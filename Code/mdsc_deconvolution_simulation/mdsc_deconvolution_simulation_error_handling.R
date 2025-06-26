#-----------------------------------------------------------------------------------------
#Function containing all error handling of the app
#-----------------------------------------------------------------------------------------

simulation_error_handling <- function(reactiveInputs){
  
  if(is.null(reactiveInputs$sampling) || is.null(reactiveInputs$startTemp) || is.null(reactiveInputs$endTemp) || is.null(reactiveInputs$period) ||
     is.null(reactiveInputs$heatRate) || is.null(reactiveInputs$Atemp) || is.null(reactiveInputs$phase) || is.null(reactiveInputs$loessAlpha)) {
    
    msg <- "One of your basic parameter inputs is missing!"
    return(msg)
  }
  
  if (anyNA(c(reactiveInputs$locationTgRHF,
              reactiveInputs$locationTgTHF))) {
    msg <- "Error: The input for the Tg location must contain 3 values; the onset, the endset, and the midpoint, separated by commas. Something is wrong with your input"
    return(msg)
  }

  if (any(c(reactiveInputs$sampling,
            reactiveInputs$period,
            reactiveInputs$heatRate,
            reactiveInputs$Atemp,
            reactiveInputs$loessAlpha) < 0)) {
    msg <- "Error: one of your basic inputs is negative. This is not possible."
    return(msg)
  }
  
  if(is.null(reactiveInputs$deltaRHFPreTg) || is.null(reactiveInputs$deltaRHFPostTg) || is.null(reactiveInputs$StartRHFPreTg) ||
     is.null(reactiveInputs$deltaCpPreTg) || is.null(reactiveInputs$deltaCpPostTg) || is.null(reactiveInputs$StartCpTempPreTg) ||
     is.null(reactiveInputs$locationTgTHF) || is.null(reactiveInputs$locationTgRHF) || is.null(reactiveInputs$deltaCpTg)) {
    
    msg <- "One of your Tg inputs is missing!"
    return(msg)
  }
  
  if(length(reactiveInputs$locationTgTHF) != 3) {
    msg <- "The input for the Tg location on the THF must contain 3 values; the onset, the endset, and the midpoint. Your input contained more or fewer values"
    return(msg)
  }
  
  if(length(reactiveInputs$locationTgRHF) != 3) {
    msg <- "The input for the Tg location on the RHF must contain 3 values; the onset, the endset, and the midpoint. Your input contained more or fewer values"
    return(msg)
  }
  
  if(reactiveInputs$locationTgTHF[2] < reactiveInputs$locationTgTHF[1]) {
    msg <- "According to your input, the onset of your Tg on the THF occurs before the endset" 
    return(msg)
  }
  
  if(reactiveInputs$locationTgTHF[3] < reactiveInputs$locationTgTHF[1]) {
    msg <- "According to your input, the onset of your Tg on the THF occurs after the midpoint" 
    return(msg)
  }
  
  if(reactiveInputs$locationTgTHF[2] < reactiveInputs$locationTgTHF[3]) {
    msg <- "According to your input, the endset of your Tg on the THF occurs before the midpoint" 
    return(msg)
  }
  
  if(reactiveInputs$locationTgRHF[2] < reactiveInputs$locationTgRHF[1]) {
    msg <- "According to your input, the onset of your Tg on the RHF occurs before the endset" 
    return(msg)
  }
  
  if(reactiveInputs$locationTgRHF[3] < reactiveInputs$locationTgRHF[1]) {
    msg <- "According to your input, the onset of your Tg on the RHF occurs after the midpoint" 
    return(msg)
  }
  
  if(reactiveInputs$locationTgRHF[2] < reactiveInputs$locationTgRHF[3]) {
    msg <- "According to your input, the endset of your Tg on the RHF occurs before the midpoint" 
    return(msg)
  }
  
  if(reactiveInputs$locationTgRHF[1] < reactiveInputs$startTemp) {
    msg <- "According to your input, the onset of your Tg on the RHF occurs before the start of your mDSC run" 
    return(msg)
  }

  if(reactiveInputs$locationTgTHF[1] < reactiveInputs$startTemp) {
    msg <- "According to your input, the onset of your Tg on the THF occurs before the start of your mDSC run" 
    return(msg)
  }
  

  if (reactiveInputs$gaussianNumber != 0 &&
      isTRUE(is.na(reactiveInputs$gaussianList))) {
    
    msg <- paste(
      "The input for all Gaussian signals must contain 3 comma-separated values",
      "(onset, endset, enthalpy). Your input had missing/extra values,",
      "or non-numeric characters."
    )
    return(msg)
  }
  
  
  onsetValsGaussian <- c()
  for(i in seq_along(reactiveInputs$gaussianList)) {onsetValsGaussian[i] <- reactiveInputs$gaussianList[[i]][1]}
  
  endsetValsGaussian <- c()
  for(i in seq_along(reactiveInputs$gaussianList)) {endsetValsGaussian[i] <- reactiveInputs$gaussianList[[i]][2]}
  
  
  if(reactiveInputs$gaussianNumber == 0) {
    NULL
  } else if(reactiveInputs$gaussianNumber == 1) {
    if(is.null(reactiveInputs$gaussianList[[1]]) || length(reactiveInputs$gaussianList[[1]]) == 0) {
      msg <- "One of your Gaussian inputs is missing!"
      return (msg)
    }
    
    if(length(reactiveInputs$gaussianList[[1]]) != 3) {
      msg <- "The input for all Gaussian signals must contain 3 values; the onset, the endset, and the enthalpy. Your input contained more or fewer values"
      return(msg)
    }
    
  } else {
    
    for(i in 1:reactiveInputs$gaussianNumber) {
      if(is.null(reactiveInputs$gaussianList[[i]]) || length(reactiveInputs$gaussianList[[i]]) == 0) {
        msg <- "One of your Gaussian inputs is missing!"
        return (msg)
      }
    }
      for(i in 1:reactiveInputs$gaussianNumber) {
        if(length(reactiveInputs$gaussianList[[i]]) != 3) {
          msg <- "The input for all Gaussian signals must contain 3 values; the onset, the endset, and the enthalpy. Your input contained more or fewer values"
          return(msg)
        }
      }
    }
  

  if(reactiveInputs$gaussianNumber != 0) {
      for(i in seq_along(onsetValsGaussian)) {
        if(onsetValsGaussian[i] < reactiveInputs$startTemp) {
          msg <- "One of the onset values of your Gaussians is higher than the starting temperature of your mDSC run!"
          return(msg)
        }
      }
      
      for(i in seq_along(onsetValsGaussian)) {
        if(onsetValsGaussian[i] > endsetValsGaussian[i]) {
          msg <- "One of the onsets of your Gaussian peaks is larger than the endset"
          return(msg)
        }
      }
    }
}
