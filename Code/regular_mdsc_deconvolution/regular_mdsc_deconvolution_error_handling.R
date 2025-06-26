#-----------------------------------------------------------------------------------------
# Error handling functions
#-----------------------------------------------------------------------------------------
error_handling_regmDSC <- function(reactiveInputs, import) {
  
  msg <- NULL

  
  if(import == 1) {
    if (is.null(reactiveInputs$ExcelmDSC)) {
      msg <- "Error: No files were uploaded. Please upload the input files."
      return(msg)
    }
    
    ext <- tools::file_ext(reactiveInputs$ExcelmDSC)
    valid_ext <- tolower(ext) %in% c("xls", "xlsx")
    if(!is.null(reactiveInputs$ExcelmDSC)) {
      if (!valid_ext) {
        msg <- "Error: wrong file extension"
        return(msg)
      }    
    }
    
    if (anyNA(c(reactiveInputs$period, 
                    reactiveInputs$setAmplitude, 
                    reactiveInputs$heatingRate))) {
      msg <- "Error: one of your inputs (amplitude, period or heating rate) is not numerical. This is not possible."
      return(msg)
    } 
    
    if (is.null(any(c(reactiveInputs$period, 
                      reactiveInputs$setAmplitude, 
                      reactiveInputs$heatingRate)))) {
      msg <- "Error: one of the inputs is missing"
      return(msg)
    }
    
    
    if (any(c(reactiveInputs$period,
              reactiveInputs$setAmplitude,
              reactiveInputs$heatingRate) == 0)) {
      msg <- "Error: one of your inputs (amplitude, period or heating rate) is zero. This is not possible."
    }
    
    if (any(c(reactiveInputs$period,
              reactiveInputs$setAmplitude,
              reactiveInputs$heatingRate) < 0)) {
      msg <- "Error: one of your inputs (amplitude, period or heating rate) is negative. This is not possible."
    }
    
    
    if (any(c(reactiveInputs$period,
              reactiveInputs$setAmplitude,
              reactiveInputs$heatingRate) < 0)) {
      msg <- "Error: one of your inputs (amplitude, period or heating rate) is negative. This is not possible."
    }
    
    
    if (reactiveInputs$heatingRate > 20 || reactiveInputs$period > 200 || reactiveInputs$setAmplitude > 2) {
      msg <- "Error: one of your inputs (amplitude, period or heating rate) is very large. This is currently not supported."
    }
    
  }
  
  if(import == 2) {
    if (is.null(reactiveInputs$ExcelDSC)) {
      msg <- "Error: No files were uploaded. Please upload the input files."
      return(msg)
    }
    
    ext <- tools::file_ext(reactiveInputs$ExcelDSC)
    valid_ext <- tolower(ext) %in% c("xls", "xlsx")
    if(!is.null(reactiveInputs$ExcelDSC)) {
      if (!valid_ext) {
        msg <- "Error: wrong file extension"
        return(msg)
      }    
    }
  }
  
  return(msg)  # No error 
}
