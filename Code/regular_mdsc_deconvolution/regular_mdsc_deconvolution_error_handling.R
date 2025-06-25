#-----------------------------------------------------------------------------------------
# Error handling functions
#-----------------------------------------------------------------------------------------
error_handling <- function(reactiveInputs, import) {
  
  msg <- NULL
  
  if(import == 1) {
    if (is.null(reactiveInputs$ExcelmDSC)) {
      msg <- "Error: No files were uploaded. Please upload the input files."
    }
    
    ext <- tools::file_ext(reactiveInputs$ExcelmDSC)
    valid_ext <- tolower(ext) %in% c("xls", "xlsx")
    if(!is.null(reactiveInputs$ExcelmDSC)) {
      if (!valid_ext) {
        msg <- "Error: wrong file extension"
      }    
    }
    
    if (is.null(reactiveInputs$period) || is.null(reactiveInputs$setAmplitude)) {
      msg <- "Error: one of the inputs is missing"
    }
  }

  
  if(import == 2) {
    if (is.null(reactiveInputs$ExcelDSC)) {
      msg <- "Error: No files were uploaded. Please upload the input files."
    }
    
    ext <- tools::file_ext(reactiveInputs$ExcelDSC)
    valid_ext <- tolower(ext) %in% c("xls", "xlsx")
    if(!is.null(reactiveInputs$ExcelDSC)) {
      if (!valid_ext) {
        msg <- "Error: wrong file extension"
      }    
    }
  }
  
  return(msg)  # No error 
}
