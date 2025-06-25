library(readxl)
library(dplyr)

excel_cleaner <- function(Excel) {
  Excel <- read_excel(Excel, 2, col_names = FALSE)
  row_idx <- match(TRUE, apply(Excel, 1, function(r) any(tolower(r) == "time")))
  headers <- as.vector(unlist(Excel[row_idx, ]))
  Excel <- na.omit(Excel)
  
  
  # Excel <- Excel[-(1:2), ]
  test <- sapply(headers, function(x) strsplit(x, " "))
  
  idxtime <- which(vapply(test, function(x) any(tolower(x) %in% "time"), logical(1)) )
  headers[idxtime] <- "time"
  
  idxtemp <- which(vapply(test, function(x) any(tolower(x) %in% "temperature"), logical(1)) )
  headers[idxtemp] <- "temperature"
  
  idxmodtemp <- which(vapply(test, function(x) {all(c("temperature", "modulated") %in% tolower(x))}, logical(1)))
  headers[idxmodtemp] <- "modTemp"
  
  idxhf <- which(vapply(test, function(x) {all(c("heat", "flow") %in% tolower(x))}, logical(1)))
  headers[idxhf] <- "heat_flow"
  
  idxModhf <- which(vapply(test, function(x) {all(c("heat", "flow", "modulated") %in% tolower(x))}, logical(1)))
  headers[idxModhf] <- "mod_heat_flow"
  
  if(length(idxhf) > 0) {print("There is something wrong with your input!")} 
  
  Excel <- Excel %>%
    setNames(headers) %>%                          # rename columns
    filter(if_all(everything(), ~ !grepl("[A-Za-z]", .x)) ) %>%  # drop rows with letters
    mutate(across(everything(), as.numeric))
  
}
