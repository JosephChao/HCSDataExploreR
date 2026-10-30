#### Definitions of function inputs ####
# DAT = dataframe eg. cx5_merged()
# INFO = well_info_names()
# CALC = input$analysis
# PLT = input$plate_n
# RAW = input$raw
# COMP = input$compare
# REF = input$reference
# DIS = input$display
# AVG = input$average
# EMPT = input$ignore_empty
# UNINF = input$ignore_uninfected
# PAVG = input$plate_avg
# SAMP = input$include_sample (sample types to include on bc and sp graphs)
# SUBT = subtitle for plot (stored in plate_name())
# X = input$plot_x,
# GRP = input$group_fill,
# DRCS = unique list of compounds for dose response curve eg. dr_compounds()
# SELN = dr_choice_fn
# DRX = dr_unique_selection()  Column headers to use when multi DR plots are available eg. unique_compound/Concentration
# FCT = input$drc_fct
# DOSE = unique_dr_selected()[2,1] (Returns Concentration, Duration, or Concentration_2 based on input)
# LAB = input$manual_labels,
# X = input$x_label,
# Y = input$y_label,
# RW = row_size() (For different size plates)
# CL = col_size() (For different size plates)
# CTRL = ctrl_data_selected()  Average of controls for selected HCS measurement 
# CSD = ctrl_sd_data_selected()  SD of controls for selected HCS measurement
# UDC = uninf_def_ctrl_data_selected() Average of uninfected controls by definition for selected HCS measurement

#### Functions used in plate_layout_server.R ####
manual_header_names_fn <- function(CHN,WHN,FHN,RHN) {
  hn <- tolower(trimws(c("", CHN, WHN, FHN, RHN))) # order is important and must match list order of detectorSearch
  hn_ind <- which(nzchar(hn))
  
  if (length(hn_ind) > 0) {
    for (i in hn_ind) {
      detectorSearch[i] <- hn[i]
    }
  }
  
  return(detectorSearch)
}

convert_microplate_format_fn <- function(DAT,TF) {
  if (!is.data.frame(DAT)) {
    return(empty_plate96)
  }
  dat <- DAT[, colSums(is.na(DAT)) < nrow(DAT)]  # remove columns with NA only
  if (TF == "Microplate Format") {
    names(dat)[1] <- "Row"
    dat <- pivot_longer(dat, cols = -1, names_to = "Column", values_to = "Value")
  }
  return(dat)
}

format_hcs_data_fn <- function(DAT,HN=detectorSearch) {
  dat <- DAT
  # default_dat <- plate384[which(plate384$Column < 13 & plate384$Row %in% LETTERS[1:8]), c(1:3, 5)]
  
  if (!is.data.frame(dat)) {
    return(empty_plate96)
  }
  
  # Rename Column, Well, Field, Row
  for (i in 2:5) {
    id <- which(HN[[i]] %in% tolower(names(dat)))
    
    if (length(id) > 0) {
      names(dat)[tolower(names(dat)) == HN[[i]][id]] <- names(HN)[i]
    }
  }
  
  dat_names <- names(dat)
  
  if ("Well Name" %in% dat_names) {
    names(dat) <- gsub("\\s*\\([^()]+\\)$", "", names(dat), useBytes = TRUE)  # Remove IXM Analysis name
    names(dat) <- gsub("Cell: ", "", names(dat), useBytes = TRUE)  # Remove 'Cell: ' prefix from IXM column names
  }
  
  # if ("Row" %in% dat_names & is.numeric(dat$Row)) {
  #   dat$Row <- LETTERS[dat$Row]
  # }
  if ("Row" %in% dat_names) {
    if (is.numeric(dat$Row)) {
      dat$Row <- LETTERS[dat$Row]
    } else {
      dat$Row <- toupper(dat$Row)
    }
  }
  
  if ("Column" %in% dat_names & !all(is.numeric(dat$Column))) {
    dat$Column <- as.integer(dat$Column)  # Ensure numerical values
  }
  
  if ("Well" %in% names(dat)) {
    if (any(grepl("^[A-Za-z]0[0-9]", dat$Well))) {
      # Convert A01 to A1 format
      dat$Well <- gsub("([A-Za-z])0*([0-9]+)", "\\1\\2", dat$Well)
    }
    if (!"Row" %in% dat_names) {
      dat$Row <- gsub("[^A-Za-z]", "", dat$Well)  # Missing Row (qPCR data)
    }
    
    if (!"Column" %in% dat_names) {  # Missing Column (qPCR data)
      dat$Column <- as.integer(gsub("[^0-9]", "", dat$Well))
    }
  } else if (all(c("Row", "Column") %in% dat_names)) {
    dat$Well <- paste0(dat$Row, dat$Column)
  } else {
    dat <- empty_plate96
  }
  
  if ("Time Point" %in% dat_names) {  # IXM
    names(dat)[names(dat) == "Time Point"] <- "Timepoint"
  }
  
  #
  # if ("Time Point" %in% names(dat) & nrow(input$data_upload) == 1 & !0 %in% unique(dat[["Time Point"]])) {
  #   dat$plate <- dat[["Time Point"]]  # Note plate must be equivalent to time point for use in adding interval time points
  # }
  return(dat)
}

# Convert Time x Well
txw_extract_tables_fn <- function(DAT,PN=NULL) {
  dt <- DAT
  
  # Find empty column 2 rows
  empty_row_indices <- which(dt[, 1] == "" | is.na(dt[, 1]))
  
  # Append the start and end of the data to these indices to handle the first and last tables
  empty_row_indices <- c(0, empty_row_indices, nrow(dt) + 1)
  
  if (length(PN) == 0) {
    PN <- paste0("Value",1:(length(empty_row_indices) - 1))
  }
  
  # Initialize a list to store tables
  long_tables <- list()
  
  j <- 0
  
  # Extract tables based on empty row boundaries
  for (i in seq_along(empty_row_indices)[-length(empty_row_indices)]) {
    start <- empty_row_indices[i] + 1
    end <- empty_row_indices[i + 1]
    
    if (end - start > 2) {
      
      table <- dt[start:end, ]
      
      well_ind <- which(vapply(
        table[1, 1:10],
        function(x) any(grepl("^([A-Za-z]|[Aa][A-Fa-f])(0[1-9]|[1-9]|[1-3][0-9]|4[0-8])(, [Ii]mage [0-9]+)?$", x)),
        logical(1)
      ))[1]
      
      if (!is.na(well_ind)) {
        j <- j + 1
        # Set column names based on first row
        setnames(table, as.character(unlist(table[1, ])))
        n <- ncol(table)
        
        # reshape to long format
        long_dt <- melt(
          table[-1, ],
          id.vars = 1:(well_ind - 1),
          measure.vars = well_ind:n,
          variable.name = "Well",
          value.name = PN[j])
        
        long_dt[, (PN[j]) := as.numeric(get(PN[j]))]
        
        # add to list
        long_tables[[j]] <- long_dt
      }
    }
  }
  
  return(long_tables)
}

txw_format_tables_fn <- function(DAT) {
  dt <- as.data.table(DAT)
  
  param_id <- "Metric:"
  param_names <- dt[grepl(param_id, dt[[1]]), 1][[1]]
  param_names <- gsub(param_id, "", param_names) |>
    trimws()
  
  long_tables <- txw_extract_tables_fn(dt, param_names)
  joined_dt <- as.data.table(purrr::reduce(long_tables, left_join, by = c("Well", "Elapsed", "Date Time")))
 
  if (grepl("Image", joined_dt[1, Well])) {  # Check if Fields are included
    joined_dt[, c("Well", "Field") := tstrsplit(Well, ", ", fixed = TRUE)]
    joined_dt[, Field := as.integer(gsub("[^0-9]", "", Field))]
    # joined_dt[, Field := as.integer(substring(Field, 7))]
  }
  
  joined_dt[, c("Timepoint", "plate") := match(Elapsed, unique(Elapsed))]
  
  joined_dt[, c("Row", "Column") := .(substr(Well, 1, 1), as.integer(substring(Well, 2)))]
  
  setcolorder(joined_dt, c("plate", "Row", "Column"))
  setorder(joined_dt, plate, Row, Column)
  
  return(as.data.frame(joined_dt))
}

# Plate Selector Plot function
geom_point_fn <- function(DAT, DEF = "Sample", COND, ORNA = FALSE, ..., AES = FALSE, COL, SP = 16, SZ = 12) {
  if (AES == FALSE) {
    if (ORNA) {
      r <- which(DAT[DEF] == COND | is.na(DAT[DEF]))
    } else {
      r <- which(DAT[DEF] == COND)
    }
    geom_point(data = DAT[r, ], aes(x = Column, y = Row), color = COL, shape = SP, size = SZ)
  } else {
    geom_point(data = DAT[which(DAT[DEF] == COND), ], aes(x = Column, y = Row, ...), shape = SP, size = SZ)
  }
}

# cbind fill with NA - used in multiplate list in plate_layout_server
cbind_fill_fn <- function(...){
  nm <- list(...) 
  nm <- lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow)) 
  do.call(cbind, lapply(nm, function (x) 
    rbind(x, matrix(, n-nrow(x), ncol(x))))) 
}

# # Enhanced format_hcs_data_fn with error handling
# format_hcs_data_fn <- function(DAT) {
#   tryCatch({
#     dat <- DAT
#     dat_names <- names(dat)
#     
#     # Check if data frame is empty or has no columns
#     if (is.null(dat) || nrow(dat) == 0 || ncol(dat) == 0) {
#       stop("Data file appears to be empty or improperly formatted")
#     }
#     
#     if ("Well Name" %in% dat_names) {
#       names(dat)[dat_names == "Well Name"] <- "Well"
#       names(dat) <- gsub("\\s*\\([^()]+\\)$", "", names(dat), useBytes = TRUE)
#       names(dat) <- gsub("Cell: ", "", names(dat), useBytes = TRUE)
#       names(dat)[names(dat) == "Site ID"] <- "Field"
#     }
#     
#     if ("Row" %in% dat_names & is.numeric(dat$Row)) {
#       # Check if Row values are within valid range for LETTERS
#       if (any(dat$Row > 26 | dat$Row < 1, na.rm = TRUE)) {
#         stop("Row numbers must be between 1-26 for conversion to letters")
#       }
#       dat$Row <- LETTERS[dat$Row]
#     }
#     
#     if ("Column" %in% dat_names & !all(is.numeric(dat$Column))) {
#       # Try to convert to numeric, catch if it fails
#       column_numeric <- suppressWarnings(as.integer(dat$Column))
#       if (all(is.na(column_numeric))) {
#         stop("Column values cannot be converted to numbers")
#       }
#       dat$Column <- column_numeric
#     }
#     
#     if ("Well" %in% names(dat)) {
#       if (any(grepl("^[A-Za-z]0[0-9]", dat$Well))) {
#         dat$Well <- gsub("([A-Za-z])0*([0-9]+)", "\\1\\2", dat$Well)
#       }
#       if (!"Row" %in% dat_names) {
#         extracted_rows <- gsub("[^A-Za-z]", "", dat$Well)
#         if (any(extracted_rows == "")) {
#           stop("Cannot extract row information from Well column - check Well format")
#         }
#         dat$Row <- extracted_rows
#       }
#       
#       if (!"Column" %in% dat_names) {
#         extracted_cols <- suppressWarnings(as.integer(gsub("[^0-9]", "", dat$Well)))
#         if (all(is.na(extracted_cols))) {
#           stop("Cannot extract column information from Well column - check Well format")
#         }
#         dat$Column <- extracted_cols
#       }
#     } else {
#       # Check if Row and Column exist before creating Well
#       if (!"Row" %in% names(dat) || !"Column" %in% names(dat)) {
#         stop("Missing required columns: Well column not found and Row/Column information incomplete")
#       }
#       dat$Well <- paste0(dat$Row, dat$Column)
#     }
#     
#     if ("Timepoint" %in% dat_names) {
#       names(dat)[names(dat) == "Timepoint"] <- "Time Point"
#     }
#     
#     return(dat)
#     
#   }, error = function(e) {
#     # Return the error message to be handled by calling function
#     stop(paste("Data formatting error:", e$message))
#   })
# }
# 
# # Enhanced find_table_row_start_fn with error handling
# find_table_row_start_fn <- function(DAT) {
#   tryCatch({
#     separator <- switch (input$data_upload_separator,
#                          "comma" = ",",
#                          "tab" = "\t",
#                          "semicolon" = ";"
#     )
#     
#     if (input$data_upload_format == "Microplate Layout") {
#       r <- which(grepl(paste0(separator, "1", separator, "2", separator, "3"), DAT))
#       c <- which(grepl(paste0("A", separator), DAT))
#       
#       if (length(r) > 0) {
#         if (length(c) > 0) {
#           if (c == r + 1) {
#             return(r)
#           }
#         }
#       }
#       # Return NULL if pattern not found (will be handled in calling function)
#       return(NULL)
#       
#     } else {
#       h <- which(DAT == "[Data]")
#       i <- which(grepl(paste0("Elapsed", separator), DAT))
#       g <- which((grepl(paste0("Row", separator), DAT) & grepl(paste0("Column", separator), DAT)) | grepl(paste0("Well", separator), DAT))
#       
#       if (length(g) > 0) {
#         if (length(h) == 1) {
#           if (g[1] == h + 1) {
#             return(g[1])
#           }
#         } else if (length(i) > 0) {
#           if (g[1] == i[1]) {
#             return(g[1])
#           }
#         } else {
#           return(g[1])
#         }
#       }
#       # Return NULL if no suitable starting row found
#       return(NULL)
#     }
#   }, error = function(e) {
#     return(NULL)
#   })
# }




#### Functions used in modules.R ####
# Select Data function #
select_data_index_fn <- function(DAT, IND=NULL, INFO, RAW, REF, FLD=NULL, VAL = "Value") {
  dat <- DAT
  
  if (is.null(IND)) {
    dat <- dat[, match(c(INFO, FLD, RAW, REF), names(dat))]
  } else {
    req(IND)
    dat <- dat[IND, match(c(INFO, FLD, RAW, REF), names(dat))]
  }
  
  # Rename selected HCS data columns for easy reference
  names(dat)[names(dat) == RAW] <- VAL
  if (!is.null(REF)) {
    if (REF == RAW) {
      names(dat)[names(dat) == paste0(REF, ".1")] <- 'Ref_Val'
    } else {
      names(dat)[names(dat) == REF] <- 'Ref_Val'
    }
  }
  
  return(dat)
}

# Analysis Choice Function
analysis_choice_fn <- function(RAW,OPT,ALL=FALSE) {
  # determine Analysis selection options based on Raw Data selection
  req(RAW)
  if (ALL) {
    opt <- c("", OPT$Analysis)
  } else {
    filt_ind <- sapply(rawInputFilters, function(x) grepl(x, RAW))
    
    opt <- c("", OPT[which(OPT$filter_raw_input %in% rawInputFilters[filt_ind] | OPT$filter_raw_input == "ALL"), 1])
    
    if (grepl("%HIGH", RAW)) {
      opt <- opt[opt != "% HIGH"]
    }
  }
  return(opt)
}

# Summarize Field data
DTsummarize_field_fn <- function(DAT, FUNC, INFO) {
  # Convert to data.table if not already
  if (!is.data.table(DAT)) {
    setDT(DAT)
  }
  
  # Map the function input to the appropriate summary function
  # summary_func <- switch(FUNC,
  #                        "avg" = mean,
  #                        "sum" = sum)
  
  summary_func <- switch(FUNC,
                         "avg" = function(x) mean(x, na.rm = TRUE),
                         "sum" = function(x) as.numeric(sum(x, na.rm = TRUE))
  )
  
  # Perform the grouping and summarization
  # if ("Ref_Val" %in% names(DAT)) {
  #   dat <- DAT[, .(
  #     Value = summary_func(Value, na.rm = TRUE),
  #     Ref_Val = summary_func(Ref_Val, na.rm = TRUE)
  #   ), by = INFO]
  # } else {
  #   dat <- DAT[, .(
  #     Value = summary_func(Value, na.rm = TRUE)
  #   ), by = INFO]
  # }
  
  if ("Ref_Val" %in% names(DAT)) {
    dat <- DAT[, .(
      Value = summary_func(Value),
      Ref_Val = summary_func(Ref_Val)
    ), by = INFO]
  } else {
    dat <- DAT[, .(
      Value = summary_func(Value)
    ), by = INFO]
  }
  
  return(as.data.frame(dat))
}

#### Functions used in Utilities_analysis.R ####
ctrl_ind_fn <- function(DAT, PLT, CR, CT) {
  which(DAT$plate %in% PLT & DAT$Control_Region == CR & DAT$control_avgs & DAT$Sample == CT)
}

pct_change_fn <- function(y, k) {
  (y - k) / k * 100
}

#### Functions used in analysis_server.R ####
numeric_col_names_fn <- function(DAT) {
  
  numeric_col_ind <- as.logical(lapply(DAT, is.numeric))
  numeric_col_names <- names(DAT)[which(numeric_col_ind)]
  numeric_col_names <- numeric_col_names[!numeric_col_names %in% c("Plate", "Row", "Column", "Well", "plate", "Field", "Control_Region")]
  
  return(numeric_col_names)
}

#### Average ####
avg_fn <- function(DAT) {
  dat <- DAT |>
    group_by(plate, Sample, definition, Control_Region) |>
    mutate(Value = mean(Value, na.rm = TRUE)) |>
    ungroup()
  
  return(dat)
}

# avg_fn <- function(DAT) {
#   setDT(DAT)
#   dt <- DAT[, {
#     if (any(!is.na(Value))) {
#       Value_mean <- mean(Value, na.rm = TRUE)
#       .(Value_mean = ifelse(all(is.na(Value)), NA, Value_mean))
#     } else {
#       .(Value_mean = NA)
#     }
#   }, by = .(plate, Sample, definition)]
#   
#   # Merge the calculated means back to the original data
#   DAT <- merge(DAT, dt, by = c("plate", "Sample", "definition"), all.x = TRUE)
#   
#   # Assign the calculated means to the original column
#   DAT[, Value := ifelse(is.na(Value_mean), Value, Value_mean)]
#   DAT[, Value_mean := NULL]  # Remove the temporary column
#   
#   return(DAT)
# }

#### Round to 2 decimals ####
round_fn <- function(column) {
  has_more_than_2_decimals <- any(round(column, 2) != column)
  
  if (isTruthy(has_more_than_2_decimals)) {
    rounded_column <- round(column, 2)
    return(rounded_column)
  } else {
    return(column)
  }
}

#### Calculation analysis function ####
dat_analysis_fn <- function(DAT, DS, CTRL, CSD=NULL, UDC=NULL) {
  dat <- DAT
  if (isTruthy(DS$analysis)) {
    # Perform analysis functions
    dat <- analysis_fns[[req(DS$analysis)]](dat, DS, CTRL, CSD, UDC)
  }
  return(dat)
}

# Convert LETTERS to numbers needed for outlier coordinates
letter_to_number <- function(letter) {
  # Convert letter to its corresponding ASCII value
  ascii_value <- as.numeric(charToRaw(letter)) - 64
  return(ascii_value)
}


#### Bar Chart and Scatter Plot data ####
plate_average_fn <- function(DAT,INFO,DBL=FALSE) {
  if (DBL) {
    dat <- DAT |>
      group_by(across(all_of(INFO))) |>  # Needs to include 'plate'.
      dplyr::summarize(Value = mean(Value, na.rm = TRUE),
                Value_2 = mean(Value_2, na.rm = TRUE), .groups = 'drop')
  } else {
    dat <- DAT |>
      group_by(across(all_of(INFO))) |>  # Needs to include 'plate'.
      dplyr::summarize(Value = mean(Value, na.rm = TRUE), .groups = 'drop')
  }
}

#### Best Fit Curve Functions ####
cf_compound_fn <- function(DAT, UNM = "unique_compound", DOSE = "Concentration", EXC = TRUE) {
  
  if (!UNM %in% colnames(DAT) | !DOSE %in% colnames(DAT)) {return(NULL)}
  # dat <- DAT[which(DAT$Sample != "Uninfected Control" & DAT$plate %in% PLT), ]
  # dat <- DAT[which(DAT$plate %in% PLT), ]
  u <- unique(DAT[c(UNM, DOSE)])[UNM]
  # u <- DAT[UNM][!duplicated(DAT[, c(UNM, DOSE)]), ] |>  # names (unique_compound) of non-duplicated UNM + DOSE combinations (removes replicates)
  #   unlist()  # unlist required to prevent error when adding first new well from blank plate
  
  # v <- u[duplicated(u)] # dataframe of duplicated UNM rows (compounds with multiple concentrations)
  return(unique(u[duplicated(u) & !is.na(u)])) # unique values of duplicated names (unique_compound) = compounds with multiple concentrations
}

# Return finite values only
finite_fn <- function(DAT) {
  unlist(DAT)[is.finite(unlist(DAT))]
}

#### Predict Curve ####
predict_cf_fn <- function(DAT,SELN,FCT) {
  if (nrow(DAT) == 0) {return(NULL)}
  choice <- SELN
  dat <- DAT
  
  if (names(dat)[4] == "Value_2") {
    names(dat)[names(dat) == "Value_2"] <- "Value"
  }

  dose_seq <- expand.grid(conc=exp(seq(log(min(dat["Dose"],na.rm = TRUE)),
                                       log(max(dat["Dose"],na.rm = TRUE)), length = 1000)))$conc

  to_plot = list()
  for (i in 1:length(choice)) {
    dfs <- dat[which(dat$Selection == choice[i]), ]
    test <- nrow(dfs["Selection"]) - nrow(unique(dfs["Dose"])) != 0
    fit <- fitMod(dose = Dose, resp = Value, data=dfs, model=FCT)
    quant = qt(1 - (1 - 0.95)/2, df=fit$df)
    pm = predict(fit, doseSeq = dose_seq, predType = "ls-means", se.fit = TRUE)
    Lower = if (test) {pm$fit-quant*pm$se.fit} else {pm$fit}
    Upper <- if (test) {pm$fit+quant*pm$se.fit} else {pm$fit}
    Prediction <- pm$fit
    temp = cbind.data.frame(Prediction, Lower, Upper, dose_seq, Selection = choice[i])
    to_plot[choice[i]] = list(temp)
  }

  return(bind_rows(to_plot))
}


#### Calculate Interpolation values ####
ic_fn <- function(DAT,DAT2,CALC,ICPOS,THR,TXT="IC") {
  
  thr <- THR[1:min(length(THR), 2)]
  
  # Set IC text label position at top left of graph based on max Y value or max manual Y-axis range
  if (is.null(ICPOS)) {
    ic_pos <- max(DAT2$Value,92)*1.08
  } else {
    ic_pos <- ICPOS
  }
  
  ic_posx <- min(DAT2$Dose) * 1.1
  
  ic <- as.data.frame(do.call(rbind,lapply(thr,function(i) {
    if (max(DAT$Prediction) >= i & min(DAT$Prediction) <= i) {
      index <- which.min(abs(DAT$Prediction - i))
    } else {
      index <- 1
    }
    
    c(Target = i, Index = index, Prediction = DAT[[index,1]], dose_seq = DAT[[index,4]])
  }))) |>
    mutate(Dif = abs(Target - Prediction),
           dose_seq = ifelse((Index == 1 | Index == 1000) & Dif > 0.01, NA, round(dose_seq,2)))
  
  if (length(thr) > 1) {
      return(annotate("text", x=ic_posx, y = ic_pos,
                      label = paste0(TXT,thr[1], ": ", ic$dose_seq[1], "\n", TXT, thr[2], ": ", ic$dose_seq[2]), hjust = 0))
  } else {
    return(annotate("text", x=ic_posx, y = ic_pos,
                    label = paste0(TXT, thr, ": ", ic$dose_seq[1]), hjust = 0))
  }
}

y_title_fn <- function(Y, RAW, CALC=NULL, RAW2=NULL, CALC2=NULL) {
  if (isTruthy(Y)) {
    Y
  } else {
    paste0(ifelse(isTruthy(CALC),CALC,RAW),
           {if(!is.null(RAW2)) paste0(" / ", ifelse(isTruthy(CALC2),CALC2,RAW2))}
    )
  }
}

#### Format data for download into GraphPad Prism ####
# dl_pivot_data_fn <- function(DAT, GRP = TRUE, VAL = "Value") {
#   
#   # Create a wide dataframe of bar chart, scatter plot, or dose response data for export directly into graphpad in grouped or column format.
#   # Includes adding NA values to match maximum number of replicates - ignores plate
#   if (nrow(DAT) == 0) {return(NULL)}
#   # req(DAT)
#   if (GRP) {                        # GRP = TRUE for grouped format, FLASE for column format
#     hd <- c("Cohort", "X_axis")     # Cohort = BC SP 'Group by' input and DR Compound or Duration 'Selection'
#     # nf <- "group"
#     nf <- c("Cohort", "replicate")  # pivot_wider names_from columns
#     ar <- "X_axis"                  # sort pivoted table by X-axis
#   } else {                          # For pivoting table to graphpad Column format
#     hd <- nf <- "Cohort"            # X-axis input = Cohort input. Selection, grouping and names_from only use Cohort column
#     ar <- "replicate"               # sort pivoted table by replicate number
#   }
#   
#   # setDT(dat)
#   # x <- dat[, replicate := order(order(.I)), by = hd] # faster but error with data.table
#   df <- DAT[, match(c(hd, VAL), names(DAT))] |>
#     na.omit() |>
#     # df <- DAT |>
#     #   select(all_of(c(hd, VAL))) |>        # Select columns for pivoted table
#     group_by(across(all_of(hd))) |>      # Group by X-axis and Group by columns (Dose and Selection for DR) to add replicate number
#     mutate(replicate = row_number()) |>  # Add replicate number
#     ungroup()
#   
#   if (GRP) {
#     # xa <- data.frame(xa = unique(df$X_axis))            # identify all X-axis values
#     # co <- data.frame(co = unique(df$Cohort))
#     # replicate <- data.frame(replicate = 1:all_of(max(df$replicate)))
#     # cj <- expand.grid(co$co, replicate$replicate, xa$xa)
#     
#     cj <- expand.grid(unique(df$Cohort), 1:max(df$replicate), unique(df$X_axis))
#     
#     colnames(cj) <- c("Cohort", "replicate", "X_axis")  # Rename the columns
#     
#     df <- left_join(cj, df, by = c("Cohort", "replicate", "X_axis")) |>
#       arrange(Cohort, replicate, X_axis)
#   }
#   
#   # pivot data by Cohort and replicate for grouped data and by Cohort only for Column format
#   pivoted_data <- df |>
#     pivot_wider(names_from = all_of(nf), values_from = all_of(VAL)) |>
#     arrange(!!sym(ar))
#   
#   return(pivoted_data)
# }

dl_pivot_data_fn2 <- function(DAT, GRP = TRUE, VAL = "Value", AVGB = NULL) {
  
  # Create a wide dataframe of bar chart, scatter plot, or dose response data for export directly into graphpad in grouped or column format.
  # Includes adding NA values to match maximum number of replicates - ignores plate
  if (nrow(DAT) == 0) {return(NULL)}
  if (!is.null(AVGB)) {
    hd <- c(AVGB, "X_axis")
    lj <- c("Cohort", "replicate")
    nf <- "replicate"
    ar <- "X_axis"
  } else if (GRP) {                        # GRP = TRUE for grouped format, FLASE for column format
    hd <- c("Cohort", "X_axis")     # Cohort = BC SP 'Group by' input and DR Compound or Duration 'Selection'
    lj <- c("Cohort", "replicate", "X_axis")
    nf <- c("Cohort", "replicate")  # pivot_wider names_from columns
    ar <- "X_axis"                  # sort pivoted table by X-axis
  } else {                          # For pivoting table to graphpad Column format
    hd <- nf <- "Cohort"            # X-axis input = Cohort input. Selection, grouping and names_from only use Cohort column
    ar <- "replicate"               # sort pivoted table by replicate number
  }
  
  df <- DAT[, match(c(hd, VAL), names(DAT))] |>
    na.omit() |>
    group_by(across(all_of(hd))) |>      # Group by X-axis and Group by columns (Dose and Selection for DR) to add replicate number
    mutate(replicate = row_number()) |>  # Add replicate number
    ungroup()
  
  if (GRP) {
    if (is.null(AVGB)) {
      cj <- expand.grid(unique(df$Cohort), 1:max(df$replicate), unique(df$X_axis))
      colnames(cj) <- c("Cohort", "replicate", "X_axis")  # Rename the columns
    } else {
      if (length(AVGB) > 1) {
        df <- tidyr::unite(df, "Cohort", all_of(AVGB), na.rm = TRUE)
      } else {
        names(df)[names(df) == AVGB] <- "Cohort"
      }
      cj <- expand.grid(unique(df$Cohort), 1:max(df$replicate))
      colnames(cj) <- c("Cohort", "replicate")  # Rename the columns
    }
    
    df <- left_join(cj, df, by = all_of(lj)) |>
      arrange(Cohort, replicate, X_axis)
  }
  
  # pivot data by Cohort and replicate for grouped data and by Cohort only for Column format
  pivoted_data <- df |>
    pivot_wider(names_from = all_of(nf), values_from = all_of(VAL)) |>
    arrange(!!sym(ar)) |>
    as.data.frame()
  
  return(pivoted_data)
}

# dl_pivot_data_sp_fn <- function(DAT, GRP = TRUE, VAL = "Value", AVGB) {
#   
#   # Create a wide dataframe of bar chart, scatter plot, or dose response data for export directly into graphpad in grouped or column format.
#   # Includes adding NA values to match maximum number of replicates - ignores plate
#   if (nrow(DAT) == 0) {return(NULL)}
#   
#   ar <- "Cohort"
#   hd <- AVGB
#   VAL <- c("Value_2", "Value")
#   
#   # setDT(dat)
#   # x <- dat[, replicate := order(order(.I)), by = hd] # faster but error with data.table
#   df <- DAT[, match(c(hd, VAL), names(DAT))] |>
#     na.omit() |>
#     # df <- DAT |>
#     #   select(all_of(c(hd, VAL))) |>        # Select columns for pivoted table
#     group_by(across(all_of(hd))) |>      # Group by X-axis and Group by columns (Dose and Selection for DR) to add replicate number
#     mutate(replicate = row_number()) |>  # Add replicate number
#     ungroup()
#   
#   if (length(AVGB) > 1) {
#     df <- tidyr::unite(df, "Cohort", all_of(AVGB), na.rm = TRUE)
#   } else {
#     names(df)[names(df) == AVGB] <- "Cohort"
#   }
#   
#   if (GRP) {
#     cj <- expand.grid(unique(df$Cohort), 1:max(df$replicate))
#     colnames(cj) <- c("Cohort", "replicate")  # Rename the columns
#     
#     df <- left_join(cj, df, by = c("Cohort", "replicate")) |>
#       arrange(Cohort, replicate)
#   }
#   
#   # pivot data by Cohort and replicate for grouped data and by Cohort only for Column format
#   pivoted_data <- df |>
#     pivot_wider(names_from = "replicate", values_from = "Value") |>
#     arrange(!!sym(ar))
#   
#   return(pivoted_data)
# }

mutate_null_fn <- function(X,Y = FALSE) {
  if (is.null(X)) {
    return(Y)
  } else if (all(is.na(X))) {
    return(Y)
  } else {
    return(X)
  }
}

# split comma separated numeric text to vector of numbers
threshold_line_fn <- function(THR) {
  text <- gsub(" ", "", THR)
  split <- strsplit(text, ",", fixed = FALSE)[[1]]
  as.numeric(split)
}

#### Convert to factor and sort alphanumerically ####
factor_fn <- function(DAT, INP, ORD = NULL, LVL = INP, REV = FALSE) {
  if (isTruthy(ORD)) {
    if (REV) {
      ord <- rev(ORD)
    } else {
      ord <- ORD
    }
    factor(DAT[[INP]], levels = ord)
  } else {
    factor(DAT[[INP]], levels = unique(gtools::mixedsort(DAT[[LVL]], decreasing = REV)))
  }
}



pv_dl_data_fn <- function(DAT, SZ, HD=c("Row","Column","Value")) {
  dat <- DAT[, match(HD, names(DAT))] |>
    right_join(SZ[, -match("Well", names(SZ))], by = c("Row", "Column")) |>
    arrange(Row, Column) |>
    pivot_wider(names_from = "Column", values_from = "Value")
}


#### Plate Size Identification function ####
plate_size_ind_fn <- function(DAT, PSZ = plate_sizes, PLT = plate384) {
  max_letter <- max(as.character(DAT$Row))  # Find max Row letter
  max_row <- as.integer(charToRaw(max_letter)) - 64  # Convert Row letter to integer
  max_col <- max(DAT$Column)  # Find max Column number
  
  well_row_ind <- min(which(PSZ$Row >= max_row))  # Identify index of plate size based on max Row
  well_col_ind <- min(which(PSZ$Column >= max_col)) # Identify index of plate size based on max Column
  well_ind <- max(well_row_ind, well_col_ind)  # Identify actual index of plate size based on larger of max Row and max Column

  return(well_ind)
}

#### Shift Show Data table based on Plot height ####
shift_table_fn <- function(DIM,H=500) {
  paste0('position:relative; top:', ifelse(DIM == "auto", 0, DIM - H), 'px')
}

#### Retrieve plot dimensions based on tab ####
plot_dims_fn <- function(CI, TAB, session=getDefaultReactiveDomain()) {
  c(mutate_null_fn(CI[[tab_abbr[[TAB]]]]$graph_width_choice,
                   session$clientData[[paste0("output_render_",tab_abbr[[TAB]],"_plot_width")]]),
    mutate_null_fn(CI[[tab_abbr[[TAB]]]]$graph_height_choice,
                   session$clientData[[paste0("output_render_",tab_abbr[[TAB]],"_plot_height")]]))
}


# mean_sdl_fn <- function(x, mult = 1) {
#   m <- mean(x, na.rm = TRUE)
#   s <- sd(x, na.rm = TRUE)
#   data.frame(y = m, ymin = m - mult * s, ymax = m + mult * s)
# }
# 
# mean_cl_boot_fn <- function(x) {
#   m <- mean(x, na.rm = TRUE)
#   ci <- boot::boot.ci(boot::boot(x, function(data, indices) mean(data[indices], na.rm = TRUE), R = 1000), type = "bca")
#   data.frame(y = m, ymin = ci$bca[4], ymax = ci$bca[5])
# }

# List for using selected errorbar function
stat_summary_errorbar_functions <- list(
  SD = function(x) mean_sdl(x, mult = 1),  # Wrap mean_sdl with mult = 1
  SEM = mean_se,
  # CL = mean_cl_normal,
  CL = mean_cl_boot
)

multi_input_check_fn <- function(CALC) {
  if (mutate_null_fn(CALC, "") != "") {
    c(
      multi_input = analysisFuncs_csv$multi_input[analysisFuncs_csv$Analysis == CALC],
      filter_reference = analysisFuncs_csv$filter_reference[analysisFuncs_csv$Analysis == CALC]
    )
  } else {
    c(multi_input = "F",
         filter_reference = "F")
  }
}

#### theme() ####
labs_fn <- function(CI, XLAB=CI$x_title_choice, YLAB=CI$y_title_choice, FILL=CI$legend_title_choice, COLR=CI$legend_title_choice) {
  labs(
    title = CI$graph_title_choice,
    x = XLAB,
    y = YLAB,
    fill = FILL,
    color = COLR
  )
}

theme_fn <- function(CI, ...) {
  # theme_bw() +
  theme(
    plot.title = element_text(size = CI$graph_title_font_sz_choice, face = "bold", hjust = 0.5),
    axis.title.y = element_text(size = CI$y_title_font_sz_choice, face = "bold", margin = margin(r = 15)),
    axis.text.y = element_text(angle = CI$y_label_angle_choice, size = CI$y_label_font_sz_choice),
    axis.title.x = element_text(size = CI$x_title_font_sz_choice, face = "bold"),
    axis.text.x = element_text(angle = CI$x_label_angle_choice, hjust = 1, size = CI$x_label_font_sz_choice),
    
    legend.title = element_text(size = CI$legend_title_font_sz_choice),
    legend.text = element_text(size = CI$legend_label_font_sz_choice),
    legend.position = CI$legend_position_choice,
    ...
  )
}

theme_labs_fn2 <- function(CI, TLAB=CI$graph_title_choice, SLAB=NULL, XLAB=CI$x_title_choice, YLAB=CI$y_title_choice, FILL=CI$legend_title_choice, COLR=CI$legend_title_choice, BW=TRUE, ...) {
  list(
    labs(
      title = TLAB,
      subtitle = SLAB,
      x = XLAB,
      y = YLAB,
      fill = FILL,
      color = COLR
    ),
    { if (BW)
      theme_bw()
    },
    theme(
      plot.title = element_text(size = CI$graph_title_font_sz_choice, face = "bold", hjust = 0.5),
      axis.title.y = element_text(size = CI$y_title_font_sz_choice, face = "bold", margin = margin(r = 15)),
      axis.text.y = element_text(angle = CI$y_label_angle_choice, size = CI$y_label_font_sz_choice),
      axis.title.x = element_text(size = CI$x_title_font_sz_choice, face = "bold", margin = margin(t = 10)),
      axis.text.x = element_text(angle = CI$x_label_angle_choice, hjust = 1, size = CI$x_label_font_sz_choice),
      legend.title = element_text(size = CI$legend_title_font_sz_choice),
      legend.text = element_text(size = CI$legend_label_font_sz_choice),
      legend.position = CI$legend_position_choice,
      ...
    )
  )
}


theme_labs_fn <- function(CI, TTL="title", XLAB=CI$x_title_choice, YLAB=CI$y_title_choice, LEG="color", BW=TRUE) {
  l <- list(
    x = XLAB,
    y = YLAB
  )
  
  if (TTL == "subtitle") {
    l$subtitle <- CI$graph_title_choice
  } else {
    l$title <- CI$graph_title_choice
  }

  if (LEG == "fill") {
    l$fill <- CI$legend_title_choice
  } else {
    l$color <- CI$legend_title_choice
  }
  
  t <- list(
    axis.title.y = element_text(size = CI$y_title_font_sz_choice, face = "bold", margin = margin(r = 15)),
    axis.text.y = element_text(angle = CI$y_label_angle_choice, size = CI$y_label_font_sz_choice),
    axis.title.x = element_text(size = CI$x_title_font_sz_choice, face = "bold", margin = margin(t = 10)),
    axis.text.x = element_text(angle = CI$x_label_angle_choice, hjust = 1, size = CI$x_label_font_sz_choice),
    legend.title = element_text(size = CI$legend_title_font_sz_choice),
    legend.text = element_text(size = CI$legend_label_font_sz_choice),
    legend.position = CI$legend_position_choice
  )
  
  if (TTL == "subtitle") {
    t$plot.subtitle <- element_text(size = CI$graph_title_font_sz_choice, colour = "grey")
  } else {
    t$plot.title <- element_text(size = CI$graph_title_font_sz_choice, face = "bold", hjust = 0.5)
  }
  
  if (LEG == "fill") {
    t$legend.key.size <- unit(CI$legend_key_sz_choice, 'pt')
  }
  
  list(
    do.call(labs, l),
    { if (BW)
      theme_bw()
    },
    { if (LEG == "color")
      guides(color = guide_legend(override.aes = list(size = CI$legend_key_sz_choice)),
             fill = "none")
    },
    do.call(theme, t)
  )
}


#### Plot PV ####
pv_plot_fn <- function(DAT,DIS,AVG,EMPT,UNINF,SUBT,CI,RW,CL,OUT=FALSE) {
  dat <- req(DAT)
  val <- "Value"
  
  dat[[val]] <- round_fn(dat[[val]])
  

  
  cp_breaks <- NA
  cp_lims <- NA
  
  if (mutate_null_fn(CI$cp_scale_n == "3-colour")) {
    cp_colrs <- c(CI$cp_min_colr_choice, CI$cp_mid_colr_choice, CI$cp_max_colr_choice)
    if (mutate_null_fn(CI$cp_auto_val_choice == "value")) {
      cp_breaks <- c(CI$cp_min_val_choice, CI$cp_mid_val_choice, CI$cp_max_val_choice)
      cp_lims <- cp_breaks[c(1,3)]
    }
  } else {
    cp_colrs <- c(CI$cp_min_colr_choice, CI$cp_max_colr_choice)
    if (mutate_null_fn(CI$cp_auto_val_choice == "value")) {
      cp_breaks <- c(CI$cp_min_val_choice, CI$cp_max_val_choice)
      cp_lims <- cp_breaks
    }
  }
  
  # Check if all values are NA
  if (all(is.na(dat$Value))) {
    g = ggplot(dat, aes(x = Column, y = Row, fill = 0, label = "")) +
      scale_fill_gradient(low = "dark grey", high = "dark grey", labels = "NA")
  } else {
    g = ggplot(dat, aes(x = Column, y = Row, fill = Value, label = Value))
  }
  
  text.size <- CI$data_txt_sz_choice / 2.9
  outlier_dat <- dat[which(dat$outliers == 1), ]
  
  subt <- ifelse(isTruthy(CI$graph_title_choice), CI$graph_title_choice, SUBT)
  
  g +
    geom_tile(show.legend = CI$show_legend_choice) +
    {if (DIS) geom_text(color = CI$cp_text_colr_choice, size = text.size)} +
    {if (OUT & nrow(outlier_dat) > 0)
      geom_rect(data = outlier_dat,
                aes(xmin = Column - 0.48, xmax = Column + 0.48,
                    ymin = RW - sapply(Row, letter_to_number) + 0.52, ymax = RW - sapply(Row, letter_to_number) + 1.48),
                color = "red", fill = NA)
    } +
    scale_x_continuous(breaks = CL, limits = c(0.5,max(CL,na.rm = TRUE) + 0.5), position = "top") +
    # scale_fill_gradientn(colors = cp_colrs, limits = cp_lims, breaks = cp_breaks, labels = format(cp_breaks), oob = scales::squish) +
    
    (
      if (!all(is.na(dat$Value))) {
        if (any(is.na(cp_breaks))) {
          scale_fill_gradientn(colors = cp_colrs)
        } else {
          scale_fill_gradientn(colors = cp_colrs, limits = cp_lims, breaks = cp_breaks, labels = format(cp_breaks), oob = scales::squish)
        }
      }
    ) +
    
    theme_void() +
    
    
    # scale_fill_gradientn(colors = cp_colrs, limits = cp_lims) +
    
    ylim(rev(LETTERS[1:RW])) +
    
    # theme_labs_fn(
    #   CI,
    #   TTL = "subtitle",
    #   LEG = "fill",
    #   BW = FALSE
    # )

  
    labs(
      subtitle = subt,
      x = CI$x_title_choice,
      y = CI$y_title_choice,
      fill = CI$legend_title_choice
    ) +
  #
  theme(
    plot.subtitle = element_text(size = CI$graph_title_font_sz_choice, colour = "grey"),
    axis.title.y = element_text(angle = 90, size = CI$y_title_font_sz_choice, face = "bold", margin = margin(r = 15)),
    axis.text.y = element_text(size = CI$y_label_font_sz_choice, hjust = 0.1),
    axis.title.x = element_text(size = CI$x_title_font_sz_choice, face = "bold"),
    axis.text.x = element_text(size = CI$x_label_font_sz_choice, hjust = 1),

    legend.position = CI$legend_position_choice,
    legend.title = element_text(size = CI$legend_title_font_sz_choice),
    legend.text = element_text(size = CI$legend_label_font_sz_choice),
    legend.key.size = unit(CI$legend_key_sz_choice, 'pt')
  )
  
  # theme_labs_fn2(
  #   CI,
  #   TLAB = NULL,
  #   SLAB = subt,
  #   COLR = NULL,
  #   BW = FALSE,
  #   plot.subtitle = element_text(size = CI$graph_title_font_sz_choice, colour = "grey"),
  #   legend.key.size = unit(CI$legend_key_sz_choice, 'pt')
  # )
}

add_errorbars <- function(p, error_input, error_func_list, width = 0.15, position = "identity", aes_params = aes(), ...) {
  if (error_input != "None") {
    error_func <- error_func_list[[error_input]]
    p + stat_summary(
      aes_params,
      fun.data = error_func,
      geom = "errorbar",
      position = position,
      width = width,
      linewidth = 0.25,
      ...
    )
  } else {
    p
  }
}