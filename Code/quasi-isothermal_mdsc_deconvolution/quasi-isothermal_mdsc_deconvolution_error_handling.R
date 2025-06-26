# Function only returns message

error_handling_quasiIso <- function(reactiveInputs) {
  
  msg <- NULL
  
  if (is.null(reactiveInputs$Excel)) {
    msg <- "Error: No files were uploaded. Please upload the input files."
    return(msg)
  }
  
  ext <- tools::file_ext(reactiveInputs$Excel)
  valid_ext <- tolower(ext) %in% c("xls", "xlsx")
  if(!is.null(reactiveInputs$Excel)) {
    if (!valid_ext) {
      msg <- "Error: wrong file extension"
      return(msg)
    }    
  }
  
  if (is.null(reactiveInputs$period) ||  is.null(reactiveInputs$stepSize) || is.null(reactiveInputs$isothermLength) || is.null(reactiveInputs$startingTemp) ||
      is.null(reactiveInputs$modulationsBack) || is.null(reactiveInputs$setAmplitude) || is.null(reactiveInputs$sampling)) {
    msg <- "Error: one of the inputs is missing"
    return(msg)
  }
  
  if (anyNA(c(reactiveInputs$period, 
              reactiveInputs$stepSize, 
              reactiveInputs$isothermLength,
              reactiveInputs$startingTemp,
              reactiveInputs$modulationsBack,
              reactiveInputs$setAmplitude,
              reactiveInputs$sampling))) {
    msg <- "Error: one of your inputs (amplitude, period or heating rate) is not numerical. This is not possible."
    return(msg)
  }
  
  if (any(c(reactiveInputs$period, 
            reactiveInputs$stepSize, 
            reactiveInputs$isothermLength,
            reactiveInputs$modulationsBack,
            reactiveInputs$setAmplitude,
            reactiveInputs$sampling) == 0)) {
    msg <- "Error: one of your inputs (amplitude, period or heating rate) is zero. This is not possible."
    return(msg)
  }
  
  if (any(c(reactiveInputs$period, 
            reactiveInputs$stepSize, 
            reactiveInputs$isothermLength,
            reactiveInputs$modulationsBack,
            reactiveInputs$setAmplitude,
            reactiveInputs$sampling) < 0)) {
    msg <- "Error: one of your inputs (amplitude, period or heating rate) is negative. This is not possible."
    return(msg)
  }
  
  if (reactiveInputs$period > 500 || reactiveInputs$setAmplitude > 2  || reactiveInputs$stepSize > 100 || reactiveInputs$isothermLength > 1000 
      || reactiveInputs$sampling > 1000) {
    msg <- "Error: one of your inputs (amplitude, period, step size, isotherm length or sampling) is very large. This is currently not supported."
    return(msg)
  }

  return(msg)
}
