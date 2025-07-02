#-----------------------------------------------------------------------------------------
#Function to export the resulting Excel file
#-----------------------------------------------------------------------------------------

download_Excel <- function(reactiveInputs) {
  
  #Time generation
  sampling <- reactiveInputs$sampling
  startTemp <- reactiveInputs$startTemp
  endTemp <- reactiveInputs$endTemp
  period <- reactiveInputs$period
  heatRate <- reactiveInputs$heatRate
  
  #MHF generation (fixed)
  Atemp <- reactiveInputs$Atemp
  phase <- reactiveInputs$phase
  deltaRHFPreTg <- reactiveInputs$deltaRHFPreTg
  deltaRHFPostTg <- reactiveInputs$deltaRHFPostTg
  StartRHFPreTg <- reactiveInputs$StartRHFPreTg
  deltaCpPreTg <- reactiveInputs$deltaCpPreTg
  deltaCpPostTg <- reactiveInputs$deltaCpPostTg
  StartCpTempPreTg <- reactiveInputs$StartCpTempPreTg
  
  #MHF generation (Tg)
  locationTgTHF <- reactiveInputs$locationTgTHF
  locationTgRHF <- reactiveInputs$locationTgRHF
  deltaCpTg <- reactiveInputs$deltaCpTg
  
  #Special melting
  specialMelt <- reactiveInputs$specialMelt
  sharpness <- reactiveInputs$sharpness
  offset <- reactiveInputs$offset
  specialMeltCheck <- reactiveInputs$specialMeltCheck
  
  #MHF generation (for loop)
  gaussianNumber <- reactiveInputs$gaussianNumber
  gaussianList <- reactiveInputs$gaussianList

  #Results
  finaldf <- reactiveInputs$finaldf
  noFTcalc <- reactiveInputs$noFTcalc
  signalGen <- reactiveInputs$signalGen
  
  onsetVals <- c()
  for(i in seq_along(gaussianList)) {onsetVals[i] <- gaussianList[[i]][1]}
  
  endsetVals <- c()
  for(i in seq_along(gaussianList)) {endsetVals[i] <- gaussianList[[i]][2]}
  
  enthalpyVals <- c()
  for(i in seq_along(gaussianList)) {enthalpyVals[i] <- gaussianList[[i]][3]}
  
  
  configFixed <- data.frame(

    "Parameters" = c("Sampling rate (pts/sec)",
                   "Starting temperature (°C)",
                   "End temperature (°C)",
                   "Period (°C)",
                   "Heating rate (°C/min)",
                   "Temperature modulation amplitude (°C)", 
                   "Phase difference (rad)",
                   "Slope of the RHF before the Tg (W/°C)",
                   "Slope of the RHF after the Tg (W/°C)",
                   "Starting value of the RHF before the Tg (W)",
                   "Slope of the Cp before the Tg (W/°C)",
                   "Slope of the Cp after the Tg (W/°C)",
                   "Starting value of the Cp before the Tg (W/°C)"),
    
    "Values" = c(sampling, startTemp, endTemp, period, heatRate*60, Atemp, phase, 
                 deltaRHFPreTg, deltaRHFPostTg, StartRHFPreTg, deltaCpPreTg, 
                 deltaCpPostTg, StartCpTempPreTg),
    
    check.names = FALSE
  )
  
  configTg <- data.frame(
    "Onset(°C)" = c(locationTgTHF[1], locationTgRHF[1]), 
    "Endset(°C)" = c(locationTgTHF[2], locationTgRHF[2]), 
    "Midpoint(°C)" = c(locationTgTHF[3], locationTgRHF[3]), 
    "Jump in heat capacity (J/g*°C)" = c(deltaCpTg, deltaCpTg),
    row.names = c("THF Tg values", "RHF Tg values"),
    check.names = FALSE
    
  )
  
  if(specialMeltCheck) {
    configSpecialMelt <- data.frame(
      "Onset special melting(°C)" = specialMelt[1], 
      "Endset special melting (°C)" = specialMelt[2], 
      "Enthalpy special melting (J/g)" = specialMelt[3],
      "Sharpness (% of the FWHM of a sine wave)" = sharpness*100,
      "Offset (with respect of the minima of the modulated heat flow, in seconds)" = offset,
      check.names = FALSE)
  }
  
  if(gaussianNumber > 0) {
    configGauss <- data.frame(
      "Onset(°C)" = onsetVals,
      "Endset(°C)" = endsetVals,
      "Enthalpy (J/g)" = enthalpyVals, 
      row.names = c(1:gaussianNumber),
      check.names = FALSE
    )
  }

  wbmDSCSim <- createWorkbook()
  
  addWorksheet(wbmDSCSim, "Settings")
  writeData(wbmDSCSim, sheet <- "Settings", configFixed, startCol = 1)
  writeData(wbmDSCSim, sheet <- "Settings", configTg, startCol = 4, rowNames =  TRUE)
  
  if(specialMeltCheck) {
    writeData(wbmDSCSim, sheet <- "Settings", configSpecialMelt, startCol = 10)
  }
  
  if(gaussianNumber > 0) {
    writeData(wbmDSCSim, sheet <- "Settings", configGauss, startCol = 13)
  }
  
  addWorksheet(wbmDSCSim, "FT Deconvoluted signals")
  writeData(wbmDSCSim, sheet <- "FT Deconvoluted signals", finaldf)
  
  addWorksheet(wbmDSCSim, "Non-FT Deconvoluted signals")
  writeData(wbmDSCSim, sheet <- "Non-FT Deconvoluted signals", noFTcalc)
  
  addWorksheet(wbmDSCSim, "Raw signals")
  writeData(wbmDSCSim, sheet <- "Raw signals", signalGen)

  
  return(wbmDSCSim)
}
