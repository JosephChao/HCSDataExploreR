#### Load Saved Project ####
observeEvent(input$load_proj, {
  proj <- readRDS(input$load_proj$datapath)
  
  updateCheckboxInput(session, "pv_outlier_exclude_edge_wells", value = FALSE)
  
  dat <- proj$data
  
  if ("Time Point" %in% names(dat)) {  # used prior to v1.013
    names(dat)[names(dat) == "Time Point"] <- "Timepoint"
  }
  
  current_data$data <- dat
  current_data$data_file <- proj$data_name
  pl <- proj$layout
  pl$plate <- as.integer(pl$plate)
  current_data$layout_file <- proj$layout_name
  v <- proj$version
  
  # Add back compatibility to saved settings
  if ("version" %in% names(proj)) {  # Projects starting from v1.000
    settings <- proj$settings
    
    if (v < 1.011) {
      targets <- c("bc_customization", "sp_customization", "cf_customization",
                   "bc2_customization", "sp2_customization", "cf2_customization")
      
      for (nm in targets) {
        settings[[nm]]$data_pt_sz_choice <- 1.5
      }
      
      settings$pv_customization <- c(settings$pv_customization, list(
        cp_scale_n_choice = "2-colour",
        cp_auto_val       = "auto",
        cp_text_colr_choice = "white",
        cp_min_colr_choice  = "#56B1F7",
        cp_max_colr_choice  = "#132B43"
      ))

      settings$cf_customization$legend_position_choice <- "bottom"
      settings$cf2_customization$y_label_angle_choice <- 0
      
    }
    
    if (v < 1.018) {
      for (nm in c("layout_plate_n_choice", "multiplate_n_choice", "multiplate_n2_choice")) {
        settings[[nm]] <- as.integer(settings[[nm]])
      }
      
      for (nm in c("dataSelection", "dataSelection_pv2")) {
        settings[[nm]]$pv_plate_n_choice <- as.integer(settings[[nm]]$pv_plate_n_choice)
      }
      
      for (nm in c("dataSelection", "dataSelection_pv2", "dataSelection_dual1", "dataSelection_dual2")) {
        settings[[nm]]$field_n_choice <- as.integer(settings[[nm]]$field_n_choice)
      }
    }

    # Catch all other missed settings
    if (v < 1.020) {
      default_settings <- reactiveValuesToList(default_sv)

      missed <- names(default_settings)[!names(default_settings) %in% names(settings)]

      if (length(missed > 0)) {
        settings[missed] <- default_settings[missed]
      }
    }
    
    sv <<- settings
    
  } else {
    sv <<- default_sv
    v <- 0.99
    proj$current_tab <- list(single = "Plate View",
                           dual = "Bar Chart 2")
  }
  
  # Update tab to last saved tab
  updateTabsetPanel(session, inputId = "tabset_panel", selected = proj$current_tab$single)
  updateTabsetPanel(session, inputId = "tabset_panel_dual", selected = proj$current_tab$dual)
  
  # add back compatibility for compound_units < v1.020
  if (v < 1.020) {
    names(pl)[names(pl) == "compound_units"] <- "concentration_units"
    names(pl)[names(pl) == "compound_units_2"] <- "concentration_units_2"
  }
  
  # add back compatibility before Control_Region was added
  if (!"Control_Region" %in% names(pl)) {
    pl$Control_Region <- NA
    pl$Control_Region[which(!is.na(pl$Sample) | pl$Sample != "")] <- 1
  }
  
  # add back compatibility for projects saved before Excluded column was added
  if (!"Excluded" %in% names(pl)) {
    pl$Excluded = FALSE
  }
  
  current_data$layout <- pl
  
  req(sv)
  
  # update potentially hidden inputs
  updateSelectInput(session, inputId = "multiplate_n", selected = sv$multiplate_n_choice)
  
  # Reset data and layout upload complete messages
  reset("data_upload")
  reset("layout_upload")
  
  # Enable Analysis tab
  shinyjs::show(selector = "#navbar li a[data-value='Analysis']")
  shinyjs::show(selector = "#navbar li a[data-value='Dual Analysis']")
  
  # Identify the size of the uploaded data plate
  pl_sz <- plate_sizes$Well[plate_size_ind_fn(dat[, c("Row", "Column")])]
  
  # Update the Plate Layout size selector based on the data plate size and disable it
  updateSelectInput(session, "select_plate_size", selected = pl_sz)
  shinyjs::disable("select_plate_size")
  
  # Update customization saved inputs
  custom_inputs$pv <- sv$pv_customization
  custom_inputs$bc <- sv$bc_customization
  custom_inputs$sp <- sv$sp_customization
  custom_inputs$cf <- sv$cf_customization
  custom_inputs$zf <- sv$zf_customization
  
  # Run modules to update saved inputs
  dataSelection_moduleServer(
    id = "dataSelection",
    sv = sv,
    data_col_names = data_col_names,
    layout_info = layout_info,
    data_info = data_info
  )
  
  dataSelection_moduleServer(
    id = "dataSelection_pv2",
    sv = sv,
    data_col_names = data_col_names,
    layout_info = layout_info,
    data_info = data_info
  )
  
  dataSelection_moduleServer(
    id = "dataSelection_dual1",
    sv = sv,
    data_col_names = data_col_names,
    layout_info = layout_info,
    data_info = data_info
  )
  
  dataSelection_moduleServer(
    id = "dataSelection_dual2",
    sv = sv,
    data_col_names = data_col_names,
    layout_info = layout_info,
    data_info = data_info
  )

req(input_dataSelection_dual1)
req(input_dataSelection_dual2)  
  
}, ignoreInit = TRUE)

#### UI options for Plate Layout Tab ####
output$hcs_data_upload_setting <- renderUI({
  shinyWidgets::dropdownButton(
    inputId = "hcs_data_upload_setting",
    radioButtons("data_upload_detect_format", label = "Data Format", choices = c("Auto-Detect", "Manual"), inline = TRUE),
    uiOutput("data_upload_manual_detect"),
    
    circle = FALSE, status = "HCS_upload_settings", icon = icon("cog"), width = "600px")
})

output$data_upload_manual_detect <- renderUI({
  if (input$data_upload_detect_format == "Manual") {
    tagList(
      radioButtons("data_upload_file_ext", label = "File Extension", choices = c(".csv / .txt", ".xlsx"), inline = TRUE),
      uiOutput("data_upload_xl_sheet"),
      radioButtons("data_upload_decimal", label = "Decimal", choices = c("auto", "period", "comma"), inline = TRUE),
      div(title="'Long Table Format' requires a Well column or Row and Column columns.\n'Microplate Format' requires Row x Column table.\n'Timepoint x Well' format is a wide table format with Timepoints in rows and Wells across columns (Incucyte data)",
          radioButtons("data_upload_format", label = "Data file format", choices = c("Long Table Format", "Microplate Format", "Timepoint x Well"), inline = TRUE),
      ),
      uiOutput("data_upload_multitable"),
      div(title = "If uploading multiple files or excel sheets, data must all start on the same row. Use Auto-Detect for multi-file/sheet data on different starting rows.",
          numericInput("data_upload_start_row", label = "Data table starts on row:", value = 1, min = 1, step = 1)),
      div(title = "If no empty rows in data, select 'No' for faster uploading.",
          radioButtons("data_upload_clean_rows", label = "Clean empty rows?", choices = c("No", "Yes"), inline = TRUE))
    )
  }
})

output$data_upload_xl_sheet <- renderUI({
  if (input$data_upload_file_ext == ".xlsx") {
    div(title = "Use comma separated integers to indicate which excel sheet(s) the data can be found on. Leave blank to load all sheets.",
        textInput("data_upload_xl_sheet", label = "Data found on sheet(s):"))
        # numericInput("data_upload_xl_sheet", label = "Data found on sheet:", value = 1, min = 1, step = 1))
  } else {
    radioButtons("data_upload_separator", label = "Separator", choices = c("auto", "comma", "tab", "semicolon"), inline = TRUE)
  }
})

output$data_upload_multitable <- renderUI({
  if (input$data_upload_format != "Long Table Format") {
    tagList(
      div(title="Select 'TRUE' if multiple 'Microplate Format' or 'Timepoint x Well' tables are arranged vertically in a single .csv/.txt file or on a single .xlsx sheet. Does not support horizontally arranged tables.",
          radioButtons("data_upload_multitable", label = "Multiple Tables", choices = c(FALSE, TRUE), inline = TRUE)
      ),
      fluidRow(
        column(width = 4,
               div(title = "If uploading multiple files or excel sheets, data must all be in the same columns (empty columns are fine). Use Auto-Detect for multi-file/sheet data in different columns.",
                   numericInput("data_upload_start_col", label = "Column Start:", value = 1, min = 1, step = 1))),
        column(width = 4,
               div(title = "If uploading multiple files or excel sheets, data must all be in the same columns (empty columns are fine). Use Auto-Detect for multi-file/sheet data in different columns.",
                   numericInput("data_upload_end_col", label = "Column End:", value = 2, min = 1, step = 1)))
      )
    )
  }
})

output$Sample <- renderUI({
  opt <- c("Unknown", "Positive Control", "Negative Control", "Uninfected Control")
  cl <- current_data$layout
  
  # Check if all selected wells are defined then allow empty selection for Sample
  selected_Sample <- cl[which(cl$Well %in% input$well_selection), match("Sample", names(cl))]
  undefined_well <- any(is.na(selected_Sample) | selected_Sample == "")
  
  if(!undefined_well & isTruthy(input$well_selection)) {
    opt <- c(opt, "")
  }
  
  shinyWidgets::pickerInput(inputId = "Sample", label = "Sample",
              choices = opt,
              selected = isolate(input$Sample))
})

output$Control_Region <- renderUI({
  opt <- 1:8  # number of choices must be less than or equal to the number of greyscale_colors colors
  cl <- current_data$layout
  
  # Check if all selected wells are defined then allow empty selection for Sample
  selected_well <- cl[which(cl$Well %in% input$well_selection), match("Control_Region", names(cl))]
  undefined_well <- any(is.na(selected_well) | selected_well == "")
  
  if(!undefined_well & isTruthy(input$well_selection)) {
    opt <- c(opt, "")
  }
  
  shinyWidgets::pickerInput(inputId = "Control_Region",
                            label = "Control Region",
                            choices = opt,
                            selected = isolate(input$Control_Region))
})

# Well selection - depends on plate size
output$well_selection <- renderUI({
  saved <- isolate(input$well_selection)
  div(title = "You can also select wells by clicking on the plate layout plot. Click and drag to select groups of wells. Hold Ctrl to add well(s) to a selection. Hold Shift to remove well(s) from a selection.",
      selectizeInput(inputId = 'well_selection', label = 'Choose wells', 
                     choices = all_plate_wells()$Well,  selected = saved, multiple = TRUE)
  )
})

output$exclude_wells <- renderUI({
  pl <- current_data$layout
  
  if (mutate_null_fn(input$apply_to_all)) {
    selected_excluded <- pl$Excluded[which(pl$Well %in% input$well_selection)]
  } else {
    selected_excluded <- pl$Excluded[which(pl$plate == sv$layout_plate_n_choice & pl$Well %in% input$well_selection)]
  }
  
  if (any(selected_excluded) && any(!selected_excluded)) {
    label_name <- "Include well(s)"
    title_description <- "Some selected wells are Excluded and some are not. This will include all selected wells, after which these wells can be Excluded."
  } else if (all(!selected_excluded)) {
    label_name <- "Exclude well(s)"
    title_description <- "Click to exclude well(s) from analysis"
  } else {
    label_name <- "Include well(s)"
    title_description <- "Click to unexclude well(s) from analysis"
  }
  
  actionButton_fn("exclude_wells", HTML(paste0(strwrap(label_name, width=10), collapse="</br>")), "#636363", W=75,
                  TTL = title_description)
})



#### Set reactive values - file names, plate layout, HCS data ####
# Setup reactive values for HCS data and plate layout
current_data <- reactiveValues(
  data = plate384[which(plate384$Column < 13 & plate384$Row %in% LETTERS[1:8]), c(1:3, 5)],  # uploaded HCS data file initially stored here, filtered for 96 well plate

    # uploaded layout file or plate layout setup initially stored here
  layout = plate384[which(plate384$Column < 13 & plate384$Row %in% LETTERS[1:8]), ],
  
  data_file = NA,        # HCS data file name
  layout_file = NA      # uploaded layout file name or generic plate name
)

#### Identify number of Plates and plate selection ####
# Number of layout plates loaded
plate_n_options <- reactive({
  unique(current_data$layout$plate)
})

# Plate number selection
output$layout_plate_n <- renderUI({
  selectInput(inputId = "layout_plate_n",
              label = NULL,
              choices = plate_n_options(),
              selected = sv$layout_plate_n_choice)
})
# Save plate selection
observeEvent(input$layout_plate_n, {sv$layout_plate_n_choice <<- as.integer(input$layout_plate_n)}, ignoreInit = TRUE)

# Check if loading multiple plates
output$multiplate_output <- reactive({
  max(plate_n_options()) > 1
})
outputOptions(output, "multiplate_output", suspendWhenHidden = FALSE)

#### Data Upload ####
# upload HCS data file(s) and combine to single dataframe
data_joined <- reactive({
  req(input$data_upload)
  filename <- vector()
  l <- lapply(input$data_upload$datapath, function(file_path) {
    
    current_filename <- input$data_upload[which(input$data_upload$datapath == file_path), 1]
    
    if (input$data_upload_detect_format == "Auto-Detect") {
      
      ext <- tolower(tools::file_ext(file_path))
      
      if (ext %in% c("xlsx", "xlsm")) {
        
        sheets <- getSheetNames(file_path)
        
        dat <- lapply(sheets, function(x) {
          
          test <- suppressMessages(
            readxl::read_excel(
              file_path,
              sheet = x,
              col_names = FALSE,
              range = "A1:Z100"
            )
          )  # read.xlsx skips empty header rows causing errors finding the start row. read_excel
          
          row_strings <- apply(
            test,
            1,
            function(y) paste(trimws(y), collapse = "\t")
          )
          
          row_strings <- gsub("\\b(\\d+)\\.0+\\b", "\\1", row_strings)  # remove decimals of all zeros
          
          find_table <- autodetector_fn(row_strings, "\t")
          
          if (find_table$table_format == "Microplate Format") {
            xl_dat <- read.xlsx(file_path, sheet = x, startRow = find_table$skip_rows + 1, cols = find_table$columns, sep.names = " ")
            xl_dat <- convert_microplate_format_fn(xl_dat, find_table$table_format)
          } else {
            xl_dat <- read.xlsx(file_path, sheet = x, startRow = find_table$skip_rows + 1, sep.names = " ")
          }
          
          xl_dat <- format_hcs_data_fn(xl_dat)
          xl_dat$Plate <- x
          
          filename <<- c(filename, paste(current_filename, x, sep = "-"))
          return(xl_dat)
        })
        
        use_fread <- FALSE
        
        #close autodetect xlsx file type
      } else {
        line_data <- readLines(file_path, n = 100)
        find_table <- autodetector_fn(line_data)
        use_fread <- TRUE
      }
      
      # close auto-detect 
    } else {
      # Manual upload settings
      if (input$data_upload_file_ext == ".xlsx") {
        
        # sheets = threshold_line_fn(input$data_upload_xl_sheet)
        sheets = mutate_null_fn(threshold_line_fn(input$data_upload_xl_sheet), getSheetNames(file_path))
        
        dat <- lapply(sheets, function(x) {
          if (input$data_upload_format != "Long Table Format") {
            xl_dat <- read.xlsx(file_path,
                                sheet = x,
                                startRow = input$data_upload_start_row,
                                cols = input$data_upload_start_col:input$data_upload_end_col,
                                sep.names = " ",
                                skipEmptyRows = ifelse(input$data_upload_clean_rows == "Yes", TRUE, FALSE))
            
            xl_dat <- convert_microplate_format_fn(xl_dat, input$data_upload_format)
          } else {
            xl_dat <- read.xlsx(file_path,
                                sheet = x,
                                startRow = input$data_upload_start_row,
                                sep.names = " ",
                                skipEmptyRows = ifelse(input$data_upload_clean_rows == "Yes", TRUE, FALSE))
          }
          
          xl_dat <- format_hcs_data_fn(xl_dat)
          xl_dat$Plate <- getSheetNames(file_path)[x]
          
          filename <<- c(filename, paste(current_filename, x, sep = "-"))
          return(xl_dat)
        })
        
        use_fread <- FALSE
        
        #close manual Xlsx upload
      } else {
        find_table <- list(
          skip_rows = input$data_upload_start_row - 1,
          separator = separatorList[[input$data_upload_separator]],
          decimal = decimalList[[input$data_upload_decimal]],
          table_format = input$data_upload_format,
          columns = input$data_upload_start_col:input$data_upload_end_col
        )
        
        use_fread <- TRUE
      }
    }
    
    if (use_fread) {
      if (find_table$table_format == "Microplate Format") {
        dat <- fread(file_path,
                     skip = find_table$skip_rows,
                     sep = find_table$separator,
                     dec = find_table$decimal,
                     select = find_table$columns,  # specify columns to upload
                     header = TRUE,
                     fill = TRUE)
      } else {
        dat <- fread(file_path, skip = find_table$skip_rows, sep = find_table$separator, dec = find_table$decimal, header = TRUE, fill = TRUE)
      }
      
      if (input$data_upload_detect_format == "Manual" & mutate_null_fn(input$data_upload_clean_rows) == "Yes") {
        dat <- dat[rowSums(is.na(dat) | dat == "") < ncol(dat), ]
      }
      
      setDF(dat)
      dat <- convert_microplate_format_fn(dat, find_table$table_format)
      dat <- format_hcs_data_fn(dat)
      
      filename <<- c(filename, current_filename)
    }
    
    return(dat)
  })
  
  current_data$data_file <- filename
  
  unnested <- list()  # new unnested list of dataframes
  lc <- 1
  for (i in 1:length(l)) {
    if (is.data.frame(l[[i]])) {
      unnested[[lc]] <- l[[i]]
      lc <- lc + 1
    } else if (is.list(l[[i]])) {
      for (j in 1:length(l[[i]])) {
        unnested[[lc]] <- l[[i]][[j]]
        lc <- lc + 1
      }
    }
  }

  # Join all uploaded files into single dataframe
  for(i in seq_along(unnested)) {
    unnested[[i]]$plate <- i # add plate number
  }
  dat <- data.table::rbindlist(unnested, fill = TRUE) |>
    as.data.frame()

  if ("Timepoint" %in% names(dat) & nrow(input$data_upload) == 1 & !0 %in% unique(dat[["Timepoint"]])) {
    dat$plate <- dat[["Timepoint"]]  # Note plate must be equivalent to time point for use in adding interval time points
  }

  dat <- dat[which(!is.na(dat$Column)), ]
  
  if (all(c("Well", "Row", "Column") %in% names(dat)) ) {
    dat <- dat[, c("Well", "Row", "Column", base::setdiff(names(dat), c("Well", "Row", "Column")))]  # reorder columns
  } else {
    print("missing Well Row or Column")
  }
  
  return(dat)
})

#### Data Upload Observe ####
# assign uploaded HCS data to reactive values
observeEvent(input$data_upload, {
  shinyjs::show("clear_data_plate")
  dat <- data_joined()
  
  # Remove any columns with identical layout headers
  dup_names <- names(dat) %in% layout_cols[-(1:5)]
  if (any(dup_names)) {
    names(dat)[dup_names] <- paste0(names(dat)[dup_names],"_data")
  }

  current_data$data = dat
  pl <- isolate(current_data$layout)

  # Identify the size of the uploaded data plate
  pl_sz <- plate_sizes$Well[plate_size_ind_fn(dat[, c("Row", "Column")])]

  # Update the Plate Layout size selector based on the data plate size and disable it
  updateSelectInput(session, "select_plate_size", selected = pl_sz)
  shinyjs::disable("select_plate_size")

  # Remove fields for resizing Layout
  if ("Field" %in% names(dat)) {
    dat <- dat[which(dat$Field == 1), ]
  }

  # index for Row and Column
  cols <- match(c("Row","Column"), names(pl))

  # Update the Plate Layout based on the data plate size
  current_data$layout <- pl[which(pl$Well %in% dat$Well), -cols] |>
    right_join(dat[, c("Well","Row","Column","plate")], by = c("plate","Well")) |>
    arrange(plate, Row, Column) |>
    relocate(Row, Column, Well, Layout)

  # Enable Analysis tab
  shinyjs::show(selector = "#navbar li a[data-value='Analysis']")
  shinyjs::show(selector = "#navbar li a[data-value='Dual Analysis']")
}, ignoreInit = TRUE)

# Check if data file has been uploaded to show "Clear Data" button (conditionalPanel)
output$data_uploaded <- reactive({
  return(!is.null(input$data_upload))
})
outputOptions(output, "data_uploaded", suspendWhenHidden = FALSE)

# Clear uploaded data
observeEvent(input$clear_data_plate, {
  reset("data_upload")
  current_data$data <- plate384[which(plate384$Column < 13 & plate384$Row %in% LETTERS[1:8]), c(1:3, 5)]
  current_data$data_file = NA
  shinyjs::enable("select_plate_size")
  shinyjs::hide("clear_data_plate")
  shinyjs::hide(selector = "#navbar li a[data-value='Analysis']")
  shinyjs::hide(selector = "#navbar li a[data-value='Dual Analysis']")
}, ignoreInit = TRUE)

hcs_data <- reactive({
  dat <- current_data$data
  dat[dat == ""] <- NA  # change empty values to NA
  
  # Remove empty HCS data rows
  val <- dat[, !names(dat) %in% c("Plate", "Row", "Column", "Well", "plate", "Field")]
  
  if (NCOL(val) > 1) {
    dat <- dat[rowSums(is.na(val)) < ncol(val), ]
  } else {
    dat <- dat[which(!is.na(val)), ]
  }

  return(dat)
})

#### Plate Size ####
# Set up empty plate layout size based on HCS data
# If using different size plates, must have largest plate as plate 1
all_plate_wells <- reactive({
    well_ind <- which(plate_sizes$Well == req(input$select_plate_size))
  r <- plate_sizes$Row[well_ind]  # number of rows
  c <- plate_sizes$Column[well_ind]  # number of columns
  
  plate_wells <- plate384[which(plate384$Column <= c & plate384$Row %in% LETTERS[1:r]), 1:3]
  
  # Assign plate size as attribute name
  attr(plate_wells, "name") <- plate_sizes$Well[well_ind]
  return(plate_wells)
})

# Return the max number of rows in the plate
row_size <- reactive({
  length(unique(all_plate_wells()$Row))
})

# Return a vector of all the column numbers of the plate
col_size <- reactive({
  unique(all_plate_wells()$Column)
})

#### Load Plate Layout ####
# Upload layout file(s) and combine to single dataframe
layout_joined <- reactive({
  l <- list()
  
  l <- lapply(input$layout_upload$datapath, function(file_path) {
    dat <- readr::read_csv(file_path, locale = locale(encoding = "ISO-8859-1"), show_col_types = FALSE)
    # try other encodings if UTF-8 doesn't work, e.g., "ISO-8859-1" or "latin1"
  })

  # Join all uploaded files into a single dataframe
  for(i in seq_along(l)) {
    l[[i]]$Layout <- input$layout_upload$name[i]  # add file name
    l[[i]]$plate <- i  # add plate number
  }
  data.table::rbindlist(l, fill = TRUE) |>
    as.data.frame()
})

# Add missing columns to uploaded files
observeEvent(input$layout_upload, {
  shinyjs::show("clear_layout_plate")
  
  updateCheckboxInput(session, "pv_outlier_exclude_edge_wells", value = FALSE)
  
  df <- layout_joined()
  
  Missing <- setdiff(layout_cols, names(df))  # Find names of missing columns
  df[Missing] <- NA                      # Add them, filled with NA
  df <- df[layout_cols]
  
  # add back compatibility for layouts saved before Excluded and Control_Region columns were added
  if (any(is.na(df$Excluded))) {
    df$Excluded = FALSE
  }
  
  if (any(is.na(df$Control_Region) & !is.na(df$Sample))) {
    df$Control_Region[which(!is.na(df$Sample))] <- 1
  }
  
  if (nrow(hcs_data()) == 0) {
    # Identify the size of the uploaded layout plate
    pl_sz <- plate_sizes$Well[plate_size_ind_fn(df[, c("Row", "Column")])]
    
    # Update the Plate Layout size selector based on the layout plate size and disable it
    updateSelectInput(session, "select_plate_size", selected = pl_sz)
    shinyjs::disable("select_plate_size")
  }
  
  current_data$layout = df
  
  # Add uploaded files names to reactive value
  current_data$layout_file <- input$layout_upload$name
}, ignoreInit = TRUE)


#### Load example layout and cx5 data ####
observeEvent(input$Example_data, {
  # Show Analysis tab
  shinyjs::show(selector = "#navbar li a[data-value='Analysis']")
  shinyjs::show(selector = "#navbar li a[data-value='Dual Analysis']")
  
  # add missing columns to example layout - can be removed when example is setup
  df <- example_layout
  
  Missing <- setdiff(layout_cols, colnames(df))  # Find names of missing columns
  df[Missing] <- NA  # Add them, filled with 'NA's
  df <- df[layout_cols]
  if(any(is.na(df$Excluded))) {
    df$Excluded = FALSE
  }
  
  updateSelectInput(session, "select_plate_size", selected = 96)
  
  current_data$layout <- df
  
  # assign example data to reactive values
  # names(example_data) <- remove_multibyte(names(example_data))
  current_data$data <- example_data
  
  # Reset HCS Data, Layout and Load Project upload complete messages
  reset("data_upload")  # reset is a shinyjs function
  reset("layout_upload")
  
  # Remove uploaded files names
  current_data$data_file <- "Sample Data"
  current_data$layout_file <- "Sample Layout"
}, ignoreInit = TRUE)

#### Check for empty layout ####
non_empty_layout <- reactive({
  any(!is.na(unique(current_data$layout$Sample)))
})

#### Update Layout based on plate size selector ####
observeEvent(input$select_plate_size, {
  pl <- isolate(current_data$layout)
  pn <- req(sv$layout_plate_n_choice)
  ps <- all_plate_wells()
  clp <- pl[pl$plate == pn, ]
  
  new_size <- left_join(ps, clp, by = c("Well", "Row", "Column"))
  new_size$plate <- pn
  new_size$Excluded[is.na(new_size$Excluded)] <- FALSE
  
  pl <- pl[pl$plate != pn, ]
  pl <- rbind(pl, new_size) |>
    arrange(plate, Row, Column)

  current_data$layout <- pl
}, ignoreInit = TRUE)

#### PLATE SELECTOR ####
output$plot_selector <- renderPlot({
  pl <- if (non_empty_layout()) {
    plate_layout_def_cf()
  } else {
    current_data$layout
  }
  
  layout_plate_n <- req(sv$layout_plate_n_choice)
  
  if (layout_plate_n > max(plate_n_options())) {return(NULL)}  # prevent plate display with no wells when deleting plates
  
  pl <- pl[which(pl$plate == layout_plate_n), ]
  
  selected_well <- input$well_selection
  
  pl$definition <- factor_fn(pl, "definition")
  
  p_size <- all_plate_wells()
  r_size <- row_size()
  well_size <- 96/r_size  # creates smaller well size for larger plates
  marg <- ifelse(r_size<8, 0.5, 0)  # add 0.5 x-axis margin for 6- to 48- well plates
  
  ctrl_region <- pl[which(!is.na(pl$Control_Region)), c("Column", "Row", "Control_Region")]
  ctrl_region$Control_Region <- factor_fn(ctrl_region, "Control_Region")
  ctrl_n <- length(unique(ctrl_region$Control_Region))
  
  ggplot() +
    {if (ctrl_n > 1)
      geom_tile(data = ctrl_region, aes(x = Column, y = Row, fill = Control_Region))
    } +
    scale_fill_manual(values = greyscale_colors) +
    geom_point_fn(pl, COND = "", ORNA = TRUE, COL = "darkgrey", SP = 1, SZ = well_size) +             # Empty/NA wells
    geom_point_fn(pl, COND = "Positive Control", COL = "blue", SZ = well_size) +                      # Positive Control wells
    geom_point_fn(pl, COND = "Negative Control", COL = "purple", SZ = well_size) +                    # Negative Control wells
    geom_point_fn(pl, COND = "Uninfected Control", AES = TRUE, color = definition, SZ = well_size) +  # Uninfected Control colour
    geom_point_fn(pl, COND = "Uninfected Control", AES = TRUE, SP = "-", SZ = well_size/2) +          # Uninfected Control symbol '-'
    geom_point_fn(pl, COND = "Unknown", AES = TRUE, color = definition, SZ = well_size) +             # Unknown wells
    geom_point_fn(pl, DEF = "Excluded", COND = TRUE, COL = "darkorange", SP = 4) +                    # Excluded wells symbol
    scale_color_manual(values = d_alt_colors384, na.value = "dark grey") +                              # Colour of Unknown/Uninfected wells
    geom_point(size = well_size) +                                                                    # Vary size of well based on plate size
    scale_x_continuous(breaks = col_size(), position = "top") +                                       # X-axis based on column #s in HCS data
    coord_cartesian(xlim = c(1 - marg, max(col_size(), na.rm = TRUE) + marg)) +                       # Add 0.5 spacing on X-axis for 6 - 48 well plates
    ylim(rev(LETTERS[1:r_size])) +                                                                        # Y-axis based on plate rows in HCS data
    geom_point(data = p_size[which(p_size$Well %in% selected_well), ],
               aes(x = Column, y = Row), color = 'red', shape = 1, size = well_size + 1) +            # Highlight selected well(s)
    theme_bw() +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "none")
})

#### mouse controls ####
# Highlight wells to select
observeEvent(input$plot_brush, {
  selected_well <- brushedPoints(all_plate_wells(), input$plot_brush) |> pull()
  current_selection <- isolate(input$well_selection)
  
  if (mutate_null_fn(input$ctrlKey)) {
    selected_well <- unique(c(current_selection, selected_well))
  }
  
  if (mutate_null_fn(input$shiftKey)) {
    selected_well <- base::setdiff(current_selection, selected_well)
  }
  
  updateSelectizeInput(session, inputId = "well_selection", selected = selected_well)
  
})

# Click to deselect wells
observeEvent(input$plot_click, {
  
  click_distance <- well_px_size[[which(well_px_size$Well == attr(all_plate_wells(), "name")), 2]]
  
  selected_well <- nearPoints(all_plate_wells(), input$plot_click, maxpoints = 1, threshold = click_distance) |> pull()

  current_selection <- isolate(input$well_selection)
  
  if (mutate_null_fn(input$ctrlKey)) {
    selected_well <- unique(c(current_selection, selected_well))
  }
  
  if (mutate_null_fn(input$shiftKey)) {
    selected_well <- base::setdiff(current_selection, selected_well)
  }
  
  updateSelectizeInput(session, inputId = "well_selection", selected = selected_well)
})

#### Update wells with Add button ####
observeEvent(input$add_wells, {
  unit_defs <- Unit_defs
  defs <- Basic_defs_no_units
  pn <- req(sv$layout_plate_n_choice)
  
  pl <- isolate(current_data$layout)
  
  if (!input$apply_to_all) {
    pl <- pl[which(pl$plate == pn), ]
  }
  
  sel_well_ind <- which(pl$Well %in% input$well_selection)
  
  for (i in 1:length(defs)) {
    if (isTruthy(input[[defs[i]]])) {
      pl[sel_well_ind, defs[i]] <- input[[defs[i]]]
    }
  }

  for (i in 1:length(unit_defs)) {
    if (isTruthy(input[[Numeric_defs[i]]])) {
      pl[sel_well_ind, unit_defs[i]] <- input[[unit_defs[i]]]
    }
  }
  
  if (input$apply_to_all) {
    current_data$layout <- pl
  } else {
    current_data$layout[which(current_data$layout$plate == pn), ] <- pl
  }
}, ignoreInit = TRUE)

#### Add concentration dilutions ####
observeEvent(input$add_conc, {
  pn <- req(sv$layout_plate_n_choice)
  pl <- isolate(current_data$layout)
  
  if (!input$apply_to_all) {
    pl <- pl[which(pl$plate == pn), ]
  }
  
  sel_well_ind <- which(pl$Well %in% input$well_selection)
  
  rows <- pl$Row[sel_well_ind]
  cols <- pl$Column[sel_well_ind]

  if (all(unique(table(rows)) == length(unique(cols))) & all(unique(table(cols)) == length(unique(rows)))) {
    j = 0
    
    if (input$conc_direction %in% c("Up", "Down")) {
      if (input$conc_direction == "Down") {
        unique_rows <- unique(rows)
      } else {
        unique_rows <- rev(unique(rows))
      }
      
      for (i in unique_rows) {
        pl[sel_well_ind[sel_well_ind %in% which(pl$Row == i)], "Concentration"] <- input$conc_max / input$conc_dilution ^ j
        j <- j + 1
      }
    } else {
      if (input$conc_direction == "Right") {
        unique_cols <- unique(cols)
      } else {
        unique_cols <- rev(unique(cols))
      }
      
      for (i in unique_cols) {
        pl[sel_well_ind[sel_well_ind %in% which(pl$Column == i)], "Concentration"] <- input$conc_max / input$conc_dilution ^ j
        j <- j + 1
      }
    }
    
    if ("Sample" %in% names(pl)) {
      empty_sample_ind <- sel_well_ind[sel_well_ind %in% which(is.na(pl$Sample) | pl$Sample == "")]
    } else {
      empty_sample_ind <- sel_well_ind
    }
    
    pl$Sample[empty_sample_ind] <- "Unknown"
    pl$Control_Region[empty_sample_ind] <- 1
    
    pl$concentration_units[sel_well_ind] <- input$concentration_units
    
  }
  
  if (input$apply_to_all) {
    current_data$layout <- pl
  } else {
    current_data$layout[which(current_data$layout$plate == pn), ] <- pl
  }
}, ignoreInit = TRUE)

#### Add concentration 2 dilutions ####
observeEvent(input$add_conc_2, {
  pn <- req(sv$layout_plate_n_choice)
  pl <- isolate(current_data$layout)
  
  if (!input$apply_to_all) {
    pl <- pl[which(pl$plate == pn), ]
  }
  
  sel_well_ind <- which(pl$Well %in% input$well_selection)
  
  rows <- pl$Row[sel_well_ind]
  cols <- pl$Column[sel_well_ind]
  
  if (all(unique(table(rows)) == length(unique(cols))) & all(unique(table(cols)) == length(unique(rows)))) {
    j = 0
    
    if (input$conc_direction_2 %in% c("Up", "Down")) {
      if (input$conc_direction_2 == "Down") {
        unique_rows <- unique(rows)
      } else {
        unique_rows <- rev(unique(rows))
      }
      
      for (i in unique_rows) {
        pl[sel_well_ind[sel_well_ind %in% which(pl$Row == i)], "Concentration_2"] <- input$conc_max_2 / input$conc_dilution_2 ^ j
        j <- j + 1
      }
    } else {
      if (input$conc_direction_2 == "Right") {
        unique_cols <- unique(cols)
      } else {
        unique_cols <- rev(unique(cols))
      }
      
      for (i in unique_cols) {
        pl[sel_well_ind[sel_well_ind %in% which(pl$Column == i)], "Concentration_2"] <- input$conc_max_2 / input$conc_dilution_2 ^ j
        j <- j + 1
      }
    }
    
    if ("Sample" %in% names(pl)) {
      empty_sample_ind <- sel_well_ind[sel_well_ind %in% which(is.na(pl$Sample) | pl$Sample == "")]
    } else {
      empty_sample_ind <- sel_well_ind
    }
    
    pl$Sample[empty_sample_ind] <- "Unknown"
    pl$Control_Region[empty_sample_ind] <- 1
    
    pl$concentration_units_2[sel_well_ind] <- input$concentration_units_2
    
  }
  
  if (input$apply_to_all) {
    current_data$layout <- pl
  } else {
    current_data$layout[which(current_data$layout$plate == pn), ] <- pl
  }
}, ignoreInit = TRUE)

#### Clear selected wells ####
observeEvent(input$clear_wells, {
  pl <- isolate(current_data$layout)
  
  if (input$apply_to_all) {
    sel_well_ind <- which(pl$Well %in% input$well_selection)
  } else {
    sel_well_ind <- which(pl$plate == sv$layout_plate_n_choice & pl$Well %in% input$well_selection)
  }
  
  current_data$layout[sel_well_ind, Sample_col:(Excluded_col-1)] <- NA
  
  current_data$layout[sel_well_ind, Excluded_col] <- FALSE
}, ignoreInit = TRUE)

#### Exclude wells ####
observeEvent(input$exclude_wells, {
  pl <- isolate(current_data$layout)

  if (input$apply_to_all) {
    sel_well_ind <- which(pl$Well %in% input$well_selection)
  } else {
    sel_well_ind <- which(pl$plate == sv$layout_plate_n_choice & pl$Well %in% input$well_selection)
  }

  current_data$layout$Excluded[sel_well_ind] <- !any(pl$Excluded[sel_well_ind])
}, ignoreInit = TRUE)

#### Reset all wells ####
observeEvent(input$reset, {
  pl <- isolate(current_data$layout)
  pn <- sv$layout_plate_n_choice
  
  selected_plate <- which(pl$plate == pn)
  
  current_data$layout[selected_plate, Sample_col:(Excluded_col-1)] <- NA
  
  current_data$layout[selected_plate, Excluded_col] <- FALSE
  
  # Delete layout file name from multiplate list
  current_data$layout_file[pn] <- current_data$layout$Layout[selected_plate] <- paste("Plate", pn)
  
  reset("layout_upload") # reset layout upload complete message
}, ignoreInit = TRUE)

#### Add New Plate - Create a Copy  ####
observeEvent(input$add_plate, {
  pl <- isolate(current_data$layout)
  n <- max(pl$plate) + 1
  pl_n <- pl[which(pl$plate == sv$layout_plate_n_choice), ]
  pl_n$plate <- n
  pl_n$Layout <- paste("Layout", n)
  
  current_data$layout <- plyr::rbind.fill(pl, pl_n) |>  # add new plate to existing
    arrange((plate))
  
  sv$layout_plate_n_choice <<- n  # Update selected plate to the newly added plate number
  req(input$layout_plate_n)  # Load newly added plate
  
  # Add new plate name to multiplate layout table
  if (all(is.na(current_data$layout_file))) {  # Check if current plate has no name (not uploaded)
    current_data$layout_file <- "Plate 1"  # If no uploaded plate, name first plate
  }
  # current_data$layout_file <- append(isolate(current_data$layout_file), paste("Plate", n))  # Append new plate to multiplate layout table
  current_data$layout_file <- append(isolate(current_data$layout_file), unique(pl_n$Layout))
}, ignoreInit = TRUE)

#### Delete Plate - last plate only ####
# Enable Delete Plate button if last plate is selected
# Too lazy to add code to rename plate numbers if deleting a middle plate so not an option
observeEvent(list(input$layout_plate_n, input$add_plate), {
  pn <- req(sv$layout_plate_n_choice)
  if (pn != 1 & pn == max(current_data$layout$plate)) {
    shinyjs::enable("delete_plate")
  } else {
    shinyjs::disable("delete_plate")
  }
}, ignoreInit = TRUE)

# Delete last plate and remove file name from list
observeEvent(input$delete_plate, {
  pl <- isolate(current_data$layout)
  pn <- sv$layout_plate_n_choice
  
  current_data$layout <- pl[which(pl$plate != pn), ]

  n <- pn - 1  # Next highest plate number
  # Remove layout file name from multiplate list
  current_data$layout_file <- isolate(current_data$layout_file)[1:n]
  
  sv$layout_plate_n_choice <<- n  # Update plate selection to next highest plate number
  req(input$layout_plate_n)  # Update selected plate
}, ignoreInit = TRUE)

#### Add Time Point Layouts ####
observeEvent(input$add_time_points, {
  pl <- isolate(current_data$layout[which(current_data$layout$plate == 1), ])
  pl <- pl[, -match("plate", names(pl))]
  
  dat <- unique(current_data$data[, c("Well", "plate")])
  
  tp_start <- input$time_point_start
  tp_interval <- input$time_point_interval
  
  n <- max(dat$plate)  # This works because plate is made to be equivalent to Time Point when loading HCS data 
  
  merged <- dat |>
    left_join(pl, by = "Well") |> #, relationship = "many-to-many") |>
    arrange(plate, Row, Column) |>
    relocate(names(pl))
  
  for (i in 1:n) {
    merged$Duration[which(merged$plate == i)] <- tp_start + tp_interval * (i - 1)
  }
  
  current_data$layout <- merged
})
#### PLATE LAYOUT DEFINITION ####
plate_layout_def <- reactive ({
  pl <- isolate(current_data$layout)
  
  pl_n <- plate_n_options()

  pl[pl==""] <- NA  # change empty values to NA for easy removal during unite function
  
  basic_defs <- Basic_defs[-1] # Remove 'Sample' from basic layout definitions fields
  if (length(unique(na.omit(pl$Control_Region))) == 1) {
    basic_defs <- basic_defs[-length(basic_defs)]
  }
  
  x <- basic_defs[basic_defs != "Concentration"]    # used to unite all definitions except Concentration for Curve Fit graph
  y <- basic_defs[basic_defs != "Duration"]         # used to unite all definitions except Duration for Curve Fit graph
  z <- basic_defs[basic_defs != "Concentration_2"]  # used to unite all definitions except Concentration_2 for Curve Fit graph
  
  unite_cols <- list(
    definition = basic_defs,
    unique_compound = x,
    unique_duration = y,
    unique_compound_2 = z
  )

  pl[names(unite_cols)] <- lapply(names(unite_cols), function(col_name) {
    apply(pl[, unite_cols[[col_name]], drop = FALSE], 1, function(row) {
      paste(na.omit(row), collapse = "_")
    })
  })

  pl <- pl[, colSums(is.na(pl)) < nrow(pl)]  # remove columns with NA only
  
  pl[pl==""] <- NA
  
  return(pl)
})

cf_selection_options <- reactive({
  dat <- plate_layout_def()
  def_cols <- names(dat)
  def_cols <- def_cols[!def_cols %in% c("Row", "Column", "Well", "Layout", "Excluded")]
  
  l <- list()
  
  # Find all layout definition fields with multiple numeric entries for plotting best fit curve
  for (i in Numeric_defs) {
    for (j in def_cols) {
      opt <- cf_compound_fn(
        DAT = dat,
        UNM = j,
        DOSE = i
      )
      if (length(opt) > 0) {
        l[[i]][[j]] <- opt
      }
    }
  }
  return(l)
})

plate_layout_def_cf <- reactive({
  pl <- plate_layout_def()
  
  cf_opt <- cf_selection_options()
  
  cf <- length(names(cf_opt$Concentration)) == 0
  dur <- length(names(cf_opt$Duration)) == 0
  syn <- length(names(cf_opt$Concentration_2)) == 0
  
  # Set unique compound (2) and unique duration to NA for removal if data not available
  if (cf) {pl$unique_compound <- NULL}
  if (dur) {pl$unique_duration <- NULL}
  if (syn) {pl$unique_compound_2 <- NULL}
  
  # Hide Curve Fit tab if Curve Fit data not available
  if (!any(cf, dur, syn)) {
    if (input$tabset_panel == "Curve Fit") {  # Check if current tab = Curve Fit tab
      updateTabsetPanel(session, inputId = "tabset_panel", selected = "Plate View")  # Change tab if current tab is hidden
    }
    if (input$tabset_panel_dual == "Curve Fit 2") {  # Check if current tab = Curve Fit tab
      updateTabsetPanel(session, inputId = "tabset_panel_dual", selected = "Bar Chart 2")  # Change tab if current tab is hidden
    }
    shinyjs::hide(selector = "#tabset_panel li a[data-value='Curve Fit']")  # Use shiny.js to hide tab to prevent error if tab doesn't exist
    shinyjs::hide(selector = "#tabset_panel_dual li a[data-value='Curve Fit 2']")
  } else {
    shinyjs::show(selector = "#tabset_panel li a[data-value='Curve Fit']")
    shinyjs::show(selector = "#tabset_panel_dual li a[data-value='Curve Fit 2']")
  }
  
  # Hide Z Factor tab is positive and negative controls not present
  if (all(c("Positive Control", "Negative Control") %in% pl$Sample)) {
    shinyjs::show(selector = "#tabset_panel li a[data-value='Z Factor']")
  } else {
    if (input$tabset_panel == "Z Factor") {
      updateTabsetPanel(session, inputId = "tabset_panel", selected = "Plate View")  # Change tab if current tab is hidden
    }
    shinyjs::hide(selector = "#tabset_panel li a[data-value='Z Factor']")  # Use shiny.js to hide tab to prevent error if tab doesn't exist
  }
  
  # Disable Bar Chart and Scatter Plot until Plate Layout is added
  if (any(!is.na(pl$Sample))) {
    shinyjs::show(selector = "#tabset_panel li a[data-value='Bar Chart']")
    shinyjs::show(selector = "#tabset_panel li a[data-value='Scatter Plot']")
    shinyjs::show("pv_inputs_tl")
  } else {
    shinyjs::hide(selector = "#tabset_panel li a[data-value='Bar Chart']")
    shinyjs::hide(selector = "#tabset_panel li a[data-value='Scatter Plot']")
    shinyjs::hide("pv_inputs_tl")
  }
  
  return(pl)
})

#### Display list of uploaded file names when multiple plates are uploaded ####
output$plate_filenames <- renderTable({
  df_nm <- current_data$data_file
  pl_nm <- current_data$layout_file
  n <- max(length(df_nm), length(pl_nm), na.rm = TRUE)  # max number of loaded files

  if (any(!is.na(c(df_nm, pl_nm)))) {
    df <- cbind_fill_fn(seq.int(n), df_nm, pl_nm)  # column bind number, HCS file names, and layout file names
    colnames(df) <- c("Plate #", "HCS Data", "Layout")  # rename headers
    return(df)
  }
})

#### Download Plate Layout ####
output$downloadLayout <- downloadHandler(
  filename = "layout.csv",
  content = function(file) {
    pl <- plate_layout_def_cf()
    pl[which(pl$plate == sv$layout_plate_n_choice), ] |>
      select(!contains("unique_") & !"plate" & !"definition") |>
      write.csv(file, row.names = FALSE, fileEncoding = "ISO-8859-1")
    
  }
)

#### Reset saved selections ####
observeEvent(list(input$data_upload, input$layout_upload, input$Example_data), {
  # Reset global default settings if a new plate/layout is loaded 
  sv <<- default_sv
  
  updateTabsetPanel(session, inputId = "tabset_panel", selected = "Plot Helper")
  updateTabsetPanel(session, inputId = "tabset_panel", selected = "Plate View")
  
  # Reset Load Project Upload Complete message
  reset("load_proj")
}, ignoreInit = TRUE)

#### Sidebar Layout 2 ####
#### Create New Combined Layout Columns ####
output$create_layout_from_data <- renderUI({
  if (non_empty_layout() || length(current_data$data_file) > 0) {
    
    div(title = "Form new layout columns based on existing layout columns or HCS data columns.",
        shinyWidgets::dropdownButton(
          inputId = "combine_layout_cols",
          label = "Create columns from data",
          
          radioButtons("convert_combine_layout_cols", "Create Layout Column", c("Convert data columns", "Combine columns"), inline = TRUE),
          
          uiOutput("create_layout_col"),
          
          actionButton("combine_layout_cols_add", label = "Add",
                       style = paste0("padding: 10px; width: 100px; color: white; background-color:", "#31b0d5")),
          
          circle = FALSE, icon = icon("columns"), width = "600px")
    )
  }
})

output$create_layout_col <- renderUI({
  req(input$convert_combine_layout_cols)
  dat_names <- names(hcs_data())
  
  if (input$convert_combine_layout_cols == "Convert data columns") {
    fluidRow(
      div(style = "display:inline-block; width:41%; margin-left: 15px",
          title = "Data column(s) will be converted to Plate Layout column(s)",
          selectInput("convert_to_layout", label = "Convert to Layout", multiple = TRUE,
                      choices = dat_names[!dat_names %in% c("Row", "Column", "Well", "plate", "Timepoint", "Field")])),
      
    )
  } else {
    tagList(
      fluidRow(
        div(style = "display:inline-block; width:41%; margin-left: 15px",
            textInput("new_col_name", label = "New Column Name")),
      ),
      fluidRow(
        div(style = "display:inline-block; width:41%; margin-left: 15px",
            selectInput("col1_name", label = "Add Column", choices = c(names(plate_layout_def_cf()), dat_names))),
        div(style = "display:inline-block; width:12%",
            selectInput("combine_separator", label = "Separator", choices = c("-", "_", "."))),
        div(style = "display:inline-block; width:41%",
            selectInput("col2_name", label = "Combine with Column", choices = c("", names(plate_layout_def_cf()), dat_names))),
      ),
      fluidRow(
        div(style = "display:inline-block; width: 480px; margin-left: 15px; font-size: 10pt; margin-top: -5%;",
            title = "New column name must be unique",
            # ifelse(input$new_col_name == "", paste0(input$col1_name, input$combine_separator, input$col2_name), input$new_col_name)
            uiOutput("combine_layout_cols_preview")
        )
      ),
      br()
    )
  }
})

combine_layout_cols_name <- reactive({
  pl <- isolate(current_data$layout)
  dat <- isolate(current_data$data)

  new_col_name <- if (input$new_col_name == "") {
    if (input$col2_name == "") {
      input$col1_name
    } else {
      paste0(input$col1_name, input$combine_separator, input$col2_name)
    }
  } else {
    input$new_col_name
  }
  
  if (new_col_name %in% c(names(pl), names(dat))) {
    new_col_name <- paste0(new_col_name,"_layout")
  }
  
  return(new_col_name)
})

output$combine_layout_cols_preview <- renderUI({
  paste("New Column Name:", HTML(combine_layout_cols_name()))
})

observeEvent(input$combine_layout_cols_add, {
  pl <- isolate(current_data$layout)
  dat <- isolate(current_data$data)

  if (input$convert_combine_layout_cols == "Convert data columns") {
    new_cols <- input$convert_to_layout
  } else {
    col_names <- c(input$col1_name, input$col2_name)
    dat_names <- col_names %in% names(dat)
    new_cols <- col_names[dat_names]
  }

  if (length(new_cols) > 0) {
    num_ind <- as.logical(lapply(dat[, new_cols, drop = FALSE], is.numeric))
    
    if ("Field" %in% names(dat)) {
      if (any(num_ind)) {
        dat <- dat |>
          group_by(plate, Well, Row, Column) |>
          mutate(across(all_of(new_cols[num_ind]), ~mean(.x, na.rm = TRUE))) |>
          ungroup()
      }
      dat <- dat[which(dat$Field == 1), ]
    }

    quick_join <- if (length(dat$Well) == length(pl$Well)) {
      all(dat$Well == pl$Well) & all(dat$plate == pl$plate)
    } else {
      FALSE
    }

    if (quick_join) {
      pl[, new_cols] <- dat[, new_cols]
    } else {
      # index for Row and Column
      cols <- match(c("Row","Column"), names(pl))

      # Update the Plate Layout based on the data plate size
      pl <- pl[which(pl$Well %in% dat$Well), -cols] |>
        right_join(dat[, c("Well","Row","Column","plate", new_cols)], by = c("plate","Well")) |>
        arrange(plate, Row, Column) |>
        relocate(Row, Column, Well, Layout)
    }
  }

  if (input$convert_combine_layout_cols == "Convert data columns") {
    num_names <- c(isolate(sv$layout_numeric_cols), new_cols[num_ind])
    sv$layout_numeric_cols <- num_names  # update list of numeric columns
    current_data$data[, new_cols] <- NULL
  } else {
    new_cols <- combine_layout_cols_name()  # change new_cols variable
    pl_names <- col_names %in% names(pl)
    
    if (pl_names[2]) {
      pl[new_cols] <- paste0(pl[[col_names[1]]], input$combine_separator, pl[[col_names[2]]])
    } else {
      pl[new_cols] <- pl[[col_names[1]]]
    }
    
    pl[, col_names[dat_names]] <- NULL
  }
  
  # Update Sample and Control Region if new definitions
  pl_new_cols <- pl[, new_cols, drop = FALSE]
  if ("Sample" %in% names(pl)) {
    new_row_ind <- which(rowSums(is.na(pl_new_cols) | pl_new_cols == "") < ncol(pl_new_cols) & is.na(pl$Sample))
  } else {
    new_row_ind <- which(rowSums(is.na(pl_new_cols) | pl_new_cols == "") < ncol(pl_new_cols))
  }

  pl$Sample[new_row_ind] <- "Unknown"
  pl$Control_Region[new_row_ind] <- 1

  current_data$layout <- pl
})

#### Autofill layout ####
output$layout_autofill <- renderUI({
  div(title = "Autofill compound serial dilution concentrations or regular duration intervals",
      
      shinyWidgets::dropdownButton(
        inputId = "layout_autofill",
        label = "Autofill",
        
        br(),
        
        fluidRow(
          column(width = 4, offset = 1,
                 div(class = "green-numeric",
                     title = "Requires HCS data with multiple time points and a single plate layout loaded. Enter the first time point and the interval then click the 'Add Time Points' Button to set the time points for all loaded time-point data. Overrides Duration entries.",
                     numericInput(inputId = "time_point_start", label = "Initial Time Point", value = NULL))
          ),
          column(width = 4,
                 div(class = "green-numeric",
                     title = "Requires HCS data with multiple time points and a single plate layout loaded. Enter the first time point and the interval then click the 'Add Time Points' Button to set the time points for all loaded time-point data. Only works for equal intervals. Overrides Duration entries.",
                     numericInput(inputId = "time_point_interval", label = "Interval", value = NULL))
          ),
          column(width = 3,
                 actionButton("add_time_points", "Add Time Points",
                              style = paste0("position: relative; top: 8px; height: 50px; width: 125px; color: white; background-color:", "#31b0d5"),
                              title = "Requires HCS data with multiple time points and a single plate layout loaded. Enter the first time point and the interval then click the 'Add Time Points' Button to set the time points for all loaded time-point data. Overrides Duration entries."))
        ),
        
        
        fluidRow(
          column(width = 3, offset = 1,
                 div(class = "green-numeric",
                     title = "Fill a rectangular area of wells with Concentration serial dilutions. Set the maximum concentration value here.",
                     numericInput(inputId = "conc_max", label = "Max Conc.", value = NULL))
          ),
          column(width = 3,
                 div(class = "green-numeric",
                     title = "Requires a rectangular area selection of wells. Set the dilution factor here. For eg., '2' will dilute 100 to 50, 25, etc. for the amount of wells selected",
                     numericInput(inputId = "conc_dilution", label = "Dilution", value = NULL))
          ),
          column(width = 2,
                 div(class = "green-select",
                     title = "Requires a rectangular area selection of wells. Set the fill direction here. For example, 'Up' will enter the max concentration on the bottom row of the selected wells and fill the dilutions upwards with equal dilutions in each well of the same row.",
                     selectInput(inputId = "conc_direction", label = "Fill", choices = c("Right", "Down", "Left", "Up")))
          ),
          column(width = 3,
                 actionButton("add_conc", "Add Conc",
                              style = paste0("position: relative; top: 8px; height: 50px; width: 125px; color: white; background-color:", "#31b0d5"),
                              title = "This will update only the Concentration values (based on the Max Conc. and Dilution values) and also the Concentration Units. Wells without a defined Sample will be added as Unknown."))
        ),
        
        
        fluidRow(
          column(width = 3, offset = 1,
                 div(class = "green-numeric",
                     title = "Fill a rectangular area of wells with Concentration serial dilutions. Set the maximum concentration value here.",
                     numericInput(inputId = "conc_max_2", label = "Max Conc. 2", value = NULL))
          ),
          column(width = 3,
                 div(class = "green-numeric",
                     title = "Requires a rectangular area selection of wells. Set the dilution factor here. For eg., '2' will dilute 100 to 50, 25, etc. for the amount of wells selected",
                     numericInput(inputId = "conc_dilution_2", label = "Dilution 2", value = NULL))
          ),
          column(width = 2,
                 div(class = "green-select",
                     title = "Requires a rectangular area selection of wells. Set the fill direction here. For example, 'Up' will enter the max concentration on the bottom row of the selected wells and fill the dilutions upwards with equal dilutions in each well of the same row.",
                     selectInput(inputId = "conc_direction_2", label = "Fill 2", choices = c("Right", "Down", "Left", "Up")))
          ),
          column(width = 3,
                 actionButton("add_conc_2", "Add Conc 2",
                              style = paste0("position: relative; top: 8px; height: 50px; width: 125px; color: white; background-color:", "#31b0d5"),
                              title = "This will update only the Concentration 2 values (based on the Max Conc. and Dilution values) and also the Concentration Units. Wells without a defined Sample will be added as Unknown."))
        ),
        
        circle = FALSE, icon = icon("columns"), width = "700px")
  )
})

#### Layout table ####
# layout table format - wide/long
output$layout_table_format <- renderUI({
  if (non_empty_layout()) {
    saved <- isolate(input$layout_table_format)
    radioButtons("layout_table_format", label = "Plate Layout Table Format",
                 choices = c("Plate Layout", "Table Format"),
                 selected = saved,
                 inline = TRUE)
  }
})

output$select_definition <- renderUI({
  if (mutate_null_fn(input$layout_table_format) == "Plate Layout" & non_empty_layout()) {
    lcn <- names(plate_layout_def_cf())
    lcn <- lcn[!lcn %in% c("Row", "Column", "Well", "Layout", "plate")]
    
    shinyWidgets::pickerInput("select_definition", label = "Display Definition:",
                              choices = lcn,
                              multiple = TRUE,
                              selected = isolate(sv$select_definition_choice)
    )
  }
})

#### Display Plate Layout table ####
output$plate_layout <- renderTable({
  if (non_empty_layout()) {
    df <- plate_layout_def_cf()[which(plate_layout_def_cf()$plate == sv$layout_plate_n_choice), ]
    
    if (mutate_null_fn(input$layout_table_format) == "Table Format") {
      df
    } else {
      df[is.na(df)] <- ""
      selected_def <- req(input$select_definition)
      df$selected <- apply(df[selected_def], 1, function(x) paste(x, collapse = "\n"))  # add line break between definition
      
      df <- unique(df[, c("Row", "Column", "selected")])
      
      pivoted <- pivot_wider(df, names_from = "Column", values_from = "selected")
      
      return(pivoted[, match(c("Row", sort(as.integer(names(pivoted)[-1]))), names(pivoted))])
    }
  }
}, caption = "Plate Layout Table", caption.placement = "top")