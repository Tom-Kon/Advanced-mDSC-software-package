# Function only returns message

error_handling_quasiIso <- function(reactiveInputs) {
  
  msg <- NULL
  
  if (is.null(reactiveInputs$Excel)) {
    msg <- "Error: No files were uploaded. Please upload the input files."
  }
  
  ext <- tools::file_ext(reactiveInputs$Excel)
  valid_ext <- tolower(ext) %in% c("xls", "xlsx")
  if(!is.null(reactiveInputs$Excel)) {
    if (!valid_ext) {
      msg <- "Error: wrong file extension"
    }    
  }

  if (is.null(reactiveInputs$period) || is.null(reactiveInputs$stepSize) || is.null(reactiveInputs$isothermLength) || 
      is.null(reactiveInputs$startingTemp) || is.null(reactiveInputs$modulationsBack) || is.null(reactiveInputs$setAmplitude)) {
    msg <- "Error: one of the inputs is missing"
  }

  return(msg)  

}


