# Function only returns message

error_handling_quasiIso <- function(reactive_inputs) {
  
  msg <- NULL
  
  if (is.null(reactive_inputs$Excel)) {
    msg <- "Error: No files were uploaded. Please upload the input files."
  }
  
  ext <- tools::file_ext(reactive_inputs$Excel)
  valid_ext <- tolower(ext) %in% c("xls", "xlsx")
  if(!is.null(reactive_inputs$Excel)) {
    if (!valid_ext) {
      msg <- "Error: wrong file extension"
    }    
  }

  if (is.null(reactive_inputs$period) || is.null(reactive_inputs$stepSize) || is.null(reactive_inputs$isothermLength) || 
      is.null(reactive_inputs$startingTemp) || is.null(reactive_inputs$modulations_back) || is.null(reactive_inputs$setAmplitude)) {
    msg <- "Error: one of the inputs is missing"
  }

  return(msg)  

}


