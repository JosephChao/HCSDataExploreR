library(dplyr)
library(data.table)
library(tidyr)
library(microbenchmark)
library(shiny)
library(openxlsx)
library(stringr)

setwd("C:/Users/JDC/Downloads")
# setwd("C:/Users/JDC/Documents/Incucyte PBMC")
setwd("C:/Users/JDC/Documents/R HCS file types")
data_upload <- read.xlsx("multiwide plate reader wide headers2.xlsx", colNames = FALSE, skipEmptyRows = FALSE)
data_upload
data_upload <- read.xlsx("sadaf-data.xlsx", sheet = 1)
data_upload <- read.xlsx("Incucyte data_231124_BA.xlsx", sheet = 4, colNames = FALSE, skipEmptyRows = FALSE)

data_upload <- as.data.table(data_upload)

dt <- data_upload
View(data_upload)
param_id <- "Metric:"

param_names <- data_upload[grepl(param_id, data_upload[[1]]), 1][[1]]
param_names <- gsub(param_id, "", param_names) |>
  trimws()
param_names

# param_names <- trimws(gsub("^Metric:", "", data_upload[grepl("Metric:", data_upload[[1]]), 1][[1]]))


extract_tables <- function(DAT,PN=NULL,TXW=FALSE) {
  dt <- DAT
  
  # Find empty column 2 rows
  empty_row_indices <- which(dt[, 1] == "" | is.na(dt[, 1]))
  
  # Append the start and end of the data to these indices to handle the first and last tables
  empty_row_indices <- c(0, empty_row_indices, nrow(dt) + 1)
  empty_row_indices
  # Initialize a list to store tables
  long_tables <- list()
  
  j <- 0
  
  # Extract tables based on empty row boundaries
  for (i in seq_along(empty_row_indices)[-length(empty_row_indices)]) {
    
    start <- empty_row_indices[i] + 0
    end <- empty_row_indices[i + 1] - 1
    
    if (end - start > 2) {
      j <- j + 1
      table <- dt[start:end, ]
      
      # Set column names based on first row
      setnames(table, as.character(unlist(table[1, ])))
      n <- ncol(table)
      
      # reshape to long format
      if (TXW) {
        long_dt <- melt(
          table[-1, ],
          id.vars = 1:2,
          measure.vars = 3:n,
          variable.name = "Well",
          value.name = PN[j]
        )
      } else {
        long_dt <- melt(
          table[-1, ],
          id.vars = 1,
          measure.vars = 2:n,
          variable.name = "Column",
          value.name = paste0("Table", j)
        )
      }
      
      # add to list
      long_tables[[j]] <- long_dt
    }
  }
  
  return(long_tables)
}

table <- dt[9:17, ]
setnames(table, as.character(unlist(table[1, ])))
n <- ncol(table)
table[, X2 := as.numeric(X2)]
table

t <- copy(table)
t[, test2 := n]

t

long_tables <- extract_tables(data_upload)
long_tables <- extract_tables(data_upload, param_names)

View(long_tables[[1]])
# Join all tables
joined_dt <- as.data.table(purrr::reduce(long_tables, left_join, by = c("Well", "Elapsed", "Date Time")))

# result <- Reduce(function(x, y) merge(x, y, by = c("Well", "Elapsed", "Date Time"), all.x = TRUE), long_tables)
# 
# A <- long_tables[[1]]
# 
# for (i in 2:length(long_tables)) {
#   B <- long_tables[[i]]
#   colname <- names(B)[4]
#   A[B, on = c("Well", "Elapsed", "Date Time"), (colname) := get(colname)]
# }



if (grepl("Image", joined_dt[1, Well])) {  # Check if Fields are included
  joined_dt[, c("Well", "Field") := tstrsplit(Well, ", ", fixed = TRUE)]
  # joined_dt[, Field := as.integer(gsub("[^0-9]", "", Field))]
  joined_dt[, Field := as.integer(substring(Field, 7))]
}

joined_dt[, Timepoint := match(Elapsed, unique(Elapsed))]


joined_dt[, c("first_char", "rest") := .(substr(Well, 1, 1), as.integer(substring(Well, 2)))]



microbenchmark(
  joined_dt[, t := as.integer(gsub("[^0-9]", "", Field))],
  joined_dt[, t := as.integer(substring(Field, 7))]
)
(substring("Image 2", 7))

joined_dt[, Field := as.integer(gsub("[^0-9]", "", Field))]
joined_dt[, Field := (substring(Field, 7))]
  
View(joined_dt)

dt <- data.table(Well = c("Image_001", "Well_002"))
if (grepl("Image", result[1, "Well"])) {  # Check if Fields are included
  df <- result |>
    tidyr::separate_wider_delim(Well, delim = ", ", names = c("Well", "Field")) |>
    mutate(Field = stringr::str_extract(Field, stringr::regex("\\d+"))) |>
    group_by(Well, Field) |>
    mutate(Time_Point = row_number(),
           Row = substr(Well,1,1),
           Column = as.integer(sub(".", "", Well))) |>
    ungroup() |>
    arrange(Time_Point, Row, Column) |>
    relocate(Row,Column,Well)
} else {
  df <- result |>
    group_by(Well) |>
    mutate(Time_Point = row_number(),
           Row = substr(Well,1,1),
           Column = as.integer(sub(".", "", Well))) |>
    ungroup() |>
    arrange(Time_Point, Row, Column) |>
    relocate(Row,Column,Well)
}

sub(".", "", "W12")

View(long_tables[[1]])

sequ <- c(1,5,10)
sequ <- sequ[-length(sequ)]


for (i in sequ) {
  print(i)
}
View(result)
df <- result
dt <- as.data.table(result)

df[c("Well", "Field")] <- read.table(text = df$Well, sep = ",", strip.white = TRUE)
df$Field <- gsub("[^0-9]", "", df$Field)

dt[, c("Well", "Field") := tstrsplit(Well, ", ", fixed = TRUE)]
dt[, Field := gsub("[^0-9]", "", Field)]

sub(", .*", "", df$Well)

df$Field <- sub(".*, ", "", df$Well)
df$Well <- sub(", .*", "", df$Well)


df$Field <- gsub("[^0-9]", "", sub(".*?, ", "", df$Well))
df$Well <- gsub(", .*", "", df$Well)

View(df)



microbenchmark(
  a = {df[c("Well", "Field")] <- read.table(text = df$Well, sep = ",", strip.white = TRUE)
  df$Field <- gsub("[^0-9]", "", df$Field)},
  b = {df$Field <- gsub("[^0-9]", "", sub(".*?, ", "", df$Well))
  df$Well <- gsub(", .*", "", df$Well)},
  c = {dt[, c("Well", "Field") := tstrsplit(Well, ", ", fixed = TRUE)]
    dt[, Field := gsub("[^0-9]", "", Field)]}
)

# expr      min       lq     mean   median       uq       max neval
# a 4.016301 4.105152 4.768682 4.181100 4.317751 40.375301   100
# b 6.254801 6.453551 6.833662 6.536050 6.815900  9.768602   100
# c 4.703701 4.827952 5.281899 4.898901 5.139952 14.808100   100

microbenchmark(
  a = joined_dt <- as.data.table(purrr::reduce(long_tables, left_join, by = c("Well", "Elapsed", "Date Time"))),
  
  b = result <- Reduce(function(x, y) merge(x, y, by = c("Well", "Elapsed", "Date Time"), all.x = TRUE), long_tables),
  
  c = {A <- long_tables[[1]]
  
  for (i in 2:length(long_tables)) {
    B <- long_tables[[i]]
    colname <- names(B)[4]
    A[B, on = c("Well", "Elapsed", "Date Time"), (colname) := get(colname)]
  }}
)

long_tables2 <- long_tables
microbenchmark(
  a = {result <- Reduce(function(x, y)
    merge.data.table(x, y, by = c("Well", "Elapsed", "Date Time"), all.x = TRUE),
    long_tables)},
  b = {DT <- rbindlist(
    lapply(long_tables2, function(tbl) {
      # Identify the measurement column (the 4th column)
      measure_col <- names(tbl)[4]
      
      # Rename it to a standard name (e.g., "Value") and add a 'Variable' column
      setnames(tbl, measure_col, "Value")
      tbl[, Variable := measure_col]
      tbl
    }),
    use.names = TRUE, fill = TRUE
  )
  
  # Pivot wide: each Variable becomes a separate column
  result <- dcast(
    DT,
    Well + Elapsed + `Date Time` ~ Variable,
    value.var = "Value"
  )}
)




DT <- rbindlist(
  lapply(long_tables, function(tbl) {
    # Identify the measurement column (the 4th column)
    measure_col <- names(tbl)[4]
    
    # Rename it to a standard name (e.g., "Value") and add a 'Variable' column
    setnames(tbl, measure_col, "Value")
    tbl[, Variable := measure_col]
    tbl
  }),
  use.names = TRUE, fill = TRUE
)

# Pivot wide: each Variable becomes a separate column
result <- dcast(
  DT,
  Well + Elapsed + `Date Time` ~ Variable,
  value.var = "Value"
)

View(result)

dt[, c("Well", "Field") := tstrsplit(Well, ", ", fixed = TRUE)]
dt

View(data_upload)
table <- data_upload[4:27, ]
View(table)
# Set column names based on first row
names(table) <- table[1, ]
n <- ncol(table)
n
# pivot table
p <- tidyr::pivot_longer(table[-1, ], cols = 2:all_of(n), names_to = "Well", values_to = paste0("Value", 1))
p <- tidyr::pivot_longer(table[-1, ], cols = 2:all_of(n), names_to = "Well", values_to = MN[1])

View(p)



setnames(table, as.character(unlist(table[1, ])))
table <- table[-1, ]

# assuming `table` is already a data.table
long_dt <- melt(
  table[-1, ],                  # drop first row like you had
  id.vars = 1:2,
  measure.vars = 3:n,           # equivalent to cols = 2:all_of(n)
  variable.name = "Well",       # names_to
  value.name = param_names[1]            # values_to
)

View(long_dt)

microbenchmark(
  combined <- do.call(cbind, long_tables),
  
  combined <- do.call(cbind, lapply(long_tables, function(x) x[, 4, with = FALSE]))
)

microbenchmark(
  joined_dt[, ':=' (Timepoint = seq_len(.N)), by = c("Well", "Field")],
  joined_dt[, new_col := match(Elapsed, unique(Elapsed))],
  joined_dt[, new_co_fl := as.integer(factor(Elapsed, levels = unique(Elapsed)))]
)
