# Function only returns message

error_handling <- function(reactive_inputs, import) {
  
  msg <- NULL
  
  if(import == 1) {
    if (is.null(reactive_inputs$ExcelmDSC)) {
      msg <- "Error: No files were uploaded. Please upload the input files."
    }
    
    ext <- tools::file_ext(reactive_inputs$ExcelmDSC)
    valid_ext <- tolower(ext) %in% c("xls", "xlsx")
    if(!is.null(reactive_inputs$ExcelmDSC)) {
      if (!valid_ext) {
        msg <- "Error: wrong file extension"
      }    
    }
    
    if (is.null(reactive_inputs$period) || is.null(reactive_inputs$setAmplitude)) {
      msg <- "Error: one of the inputs is missing"
    }
  }

  
  if(import == 2) {
    if (is.null(reactive_inputs$ExcelDSC)) {
      msg <- "Error: No files were uploaded. Please upload the input files."
    }
    
    ext <- tools::file_ext(reactive_inputs$ExcelDSC)
    valid_ext <- tolower(ext) %in% c("xls", "xlsx")
    if(!is.null(reactive_inputs$ExcelDSC)) {
      if (!valid_ext) {
        msg <- "Error: wrong file extension"
      }    
    }
  }
  
  return(msg)  # No error 
  
  
  
}

