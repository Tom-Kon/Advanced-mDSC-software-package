mDSCSimErrorhandlingFunc <- function(reactive_inputs){
  
  if(is.null(reactive_inputs$sampling) || is.null(reactive_inputs$startTemp) || is.null(reactive_inputs$endTemp) || is.null(reactive_inputs$period) ||
     is.null(reactive_inputs$heatRate) || is.null(reactive_inputs$Atemp) || is.null(reactive_inputs$phase) || is.null(reactive_inputs$loessAlpha)) {
    
    msg <- "One of your basic parameter inputs is missing!"
    return(msg)
  }
  
  
  if(is.null(reactive_inputs$deltaRHFPreTg) || is.null(reactive_inputs$deltaRHFPostTg) || is.null(reactive_inputs$StartRHFPreTg) ||
     is.null(reactive_inputs$deltaCpPreTg) || is.null(reactive_inputs$deltaCpPostTg) || is.null(reactive_inputs$StartCpTempPreTg) ||
     is.null(reactive_inputs$locationTgTHF) || is.null(reactive_inputs$locationTgRHF) || is.null(reactive_inputs$deltaCpTg)) {
    
    msg <- "One of your Tg inputs is missing!"
    return(msg)
  }
  
  if(length(reactive_inputs$locationTgTHF) != 3){msg <- "The input for the Tg location on the THF must contain 3 values; the onset, the endset, and the midpoint. Your input contained more or fewer values"
  return(msg)}
  if(length(reactive_inputs$locationTgRHF) != 3){msg <- "The input for the Tg location on the RHF must contain 3 values; the onset, the endset, and the midpoint. Your input contained more or fewer values"
  return(msg)}
  
  if(reactive_inputs$locationTgTHF[2] < reactive_inputs$locationTgTHF[1]) {msg <- "According to your input, the onset of your Tg on the THF occurs before the endset" 
  return(msg)}
  
  if(reactive_inputs$locationTgTHF[3] < reactive_inputs$locationTgTHF[1]) {msg <- "According to your input, the onset of your Tg on the THF occurs after the midpoint" 
  return (msg)}
  
  if(reactive_inputs$locationTgTHF[2] < reactive_inputs$locationTgTHF[3]) {msg <- "According to your input, the endset of your Tg on the THF occurs before the midpoint" 
  return (msg)}
  
  if(reactive_inputs$locationTgRHF[2] < reactive_inputs$locationTgRHF[1]) {msg <- "According to your input, the onset of your Tg on the RHF occurs before the endset" 
  return(msg)}
  
  if(reactive_inputs$locationTgRHF[3] < reactive_inputs$locationTgRHF[1]) {msg <- "According to your input, the onset of your Tg on the RHF occurs after the midpoint" 
  return (msg)}
  
  if(reactive_inputs$locationTgRHF[2] < reactive_inputs$locationTgRHF[3]) {msg <- "According to your input, the endset of your Tg on the RHF occurs before the midpoint" 
  return (msg)}
  
  if(reactive_inputs$locationTgRHF[1] < reactive_inputs$startTemp) {msg <- "According to your input, the onset of your Tg on the RHF occurs before the start of your mDSC run" 
  return (msg)}

  if(reactive_inputs$locationTgTHF[1] < reactive_inputs$startTemp) {msg <- "According to your input, the onset of your Tg on the THF occurs before the start of your mDSC run" 
  return (msg)}
  
  
  onsetValsGaussian <- c()
  for(i in seq_along(reactive_inputs$gaussianList)) {onsetValsGaussian[i] <- reactive_inputs$gaussianList[[i]][1]}
  
  endsetValsGaussian <- c()
  for(i in seq_along(reactive_inputs$gaussianList)) {endsetValsGaussian[i] <- reactive_inputs$gaussianList[[i]][2]}
  
  
  if(reactive_inputs$gaussianNumber == 0) {
    NULL
  } else if(reactive_inputs$gaussianNumber == 1) {
      if(is.null(reactive_inputs$gaussianList[[1]]) || length(reactive_inputs$gaussianList[[1]]) == 0) {
        msg <- "One of your Gaussian inputs is missing!"
        return (msg)
      }
    
    if(length(reactive_inputs$gaussianList[[1]]) != 3) {
      msg <- "The input for all Gaussian signals must contain 3 values; the onset, the endset, and the enthalpy. Your input contained more or fewer values"
      return(msg)}  
    
    
  } else {
    
    for(i in 1:reactive_inputs$gaussianNumber) {
      if(is.null(reactive_inputs$gaussianList[[i]]) || length(reactive_inputs$gaussianList[[i]]) == 0) {
        msg <- "One of your Gaussian inputs is missing!"
        return (msg)
      }
    }
      for(i in 1:reactive_inputs$gaussianNumber) {
        if(length(reactive_inputs$gaussianList[[i]]) != 3) {
          msg <- "The input for all Gaussian signals must contain 3 values; the onset, the endset, and the enthalpy. Your input contained more or fewer values"
          return(msg)}
      }
    }
  

  if(reactive_inputs$gaussianNumber != 0) {
      for(i in seq_along(onsetValsGaussian)) {
        if(onsetValsGaussian[i] < reactive_inputs$startTemp) {
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


