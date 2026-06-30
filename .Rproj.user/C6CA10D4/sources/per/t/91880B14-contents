detectorSearch <- list(
  "1"    = 1:3,
  "Col"  = c("Col", "Column"),
  "Well" = c("Well", "Well Name")
)
detectorSeps <- list("," = ",", "\t" = "\t", ";" = ";")

detectorNotFound <- list(
  skip_rows = 0,
  separator = "auto",
  decimal = "auto",
  table_format = "Not Found"
)

detector_search_term_fn <- function(sep, DAT) {
  list(
    "1" = grepl(paste0(sep, "1", sep, "2", sep, "3"), DAT),
    "Col" = grepl(paste0("Row", sep), DAT, ignore.case = TRUE) & (grepl(paste0("Column", sep), DAT, ignore.case = TRUE) | grepl(paste0("Col", sep), DAT, ignore.case = TRUE)),
    "Well" = grepl(paste0("Well", sep), DAT, ignore.case = TRUE) | grepl(paste0("Well Name", sep), DAT, ignore.case = TRUE)
  )
}

autodetector_fn <- function(DAT, SEPS=detectorSeps) {

  if (any(grepl('\\"', DAT))) {
    DAT <- gsub('\\"', '', DAT)  # remove quotes found in different formats
  }
  
  # Create matrix of search terms and separators to identify type of table
  counts <- sapply(SEPS, function(sep) {
    sapply(detector_search_term_fn(sep, DAT), function(cond) {
      max(which(cond), 0)
    })
  })
  
  ind <- which(counts == max(counts), arr.ind = TRUE)
  
  start_row <- counts[ind][1]
  
  if (start_row == 0) {return(detectorNotFound)}
  
  # identify the separator
  sep <- colnames(counts)[ind[1,2]]
  
  # identify the type of table (1 = microplate, or if Well or Column header found)
  table_id <- rownames(counts)[ind[1,1]]
  
  
  # Find the column to search (Same column as "Well" or "Column", but column before if "1, 2, 3")
  col_pos <- which(str_split(DAT[start_row], sep)[[1]] %in% detectorSearch[[table_id]])[1] - ifelse(table_id == "1", 1, 0)
  
  if (table_id == "1") {
    vals <- str_split(DAT[start_row], sep)[[1]]
    end_col <- which(vals[col_pos + 1:length(vals)] == "NA" | vals[col_pos + 1:length(vals)] == "")[1]
    
    upload_cols <- col_pos:(end_col + col_pos - 1)
    
  } else {
    upload_cols <- NA
  }
  
  # Extract table data from first 5 rows for table confirmation
  check_rows <- start_row + 1:5
  test_format <- sapply(check_rows, function(x) {
    strsplit(DAT[x], sep, fixed = TRUE)[[1]][col_pos]
  })
  
  # Return the first two non-empty entries
  tf <- test_format[which(test_format != "" & test_format != "NA")][1:2]
  
  data_match <- switch(
    table_id,
    "1"    = grepl("^([A-Za-z]|A[A-F]|a[a-f])$", tf),
    "Well" = grepl("^([A-Za-z]|A[A-F]|a[a-f])(0[1-9]|[1-9]|[1-3][0-9]|4[0-8])$", tf),
    "Col"  = grepl("^(0[1-9]|[1-9]|[1-3][0-9]|4[0-8])$", tf),
    FALSE
  )
  
  if (all(data_match)) {
    return(list(
      skip_rows = start_row - 1,
      separator = sep,
      decimal = "auto",
      table_format = ifelse (table_id == "1", "Microplate Format", "Long Table Format"),
      columns = upload_cols
    ))
  } else {
    return(detectorNotFound)
  }
}
