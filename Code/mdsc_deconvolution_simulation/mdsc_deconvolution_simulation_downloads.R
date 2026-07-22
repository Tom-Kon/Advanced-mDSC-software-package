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
  deltaRevCpPreTg <- reactiveInputs$deltaRevCpPreTg
  deltaRevCpPostTg <- reactiveInputs$deltaRevCpPostTg
  startRevCpPreTg <- reactiveInputs$startRevCpPreTg
  deltaCpPreTg <- reactiveInputs$deltaCpPreTg
  deltaCpPostTg <- reactiveInputs$deltaCpPostTg
  StartCpTempPreTg <- reactiveInputs$StartCpTempPreTg
  
  #MHF generation (Tg)
  locationTgTHF <- reactiveInputs$locationTgTHF
  locationTgRHF <- reactiveInputs$locationTgRHF
  deltaCpTg <- reactiveInputs$deltaCpTg
  
  #MHF generation (Peaks)
  gaussianNumber <- reactiveInputs$gaussianNumber
  gaussianList <- reactiveInputs$gaussianList
  
  #Results
  finaldf <- reactiveInputs$finaldf
  noFTcalc <- reactiveInputs$noFTcalc
  signalGen <- reactiveInputs$signalGen
  loess <- reactiveInputs$loessAlpha
  
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
                     "Slope of the RevCp before the Tg (J/°C²*g)",
                     "Slope of the RevCp after the Tg (J/°C²*g)",
                     "Starting value of the RevCp before the Tg (J/(g*°C))",
                     "Slope of the Cp before the Tg (J/°C²*g)",
                     "Slope of the Cp after the Tg (J/°C²*g)",
                     "Starting value of the Cp before the Tg (J/(°C*g)",
                     "LOESS factor"),
    
    "Values" = c(sampling, startTemp, endTemp, period, heatRate*60, Atemp, phase, 
                 deltaRevCpPreTg, deltaRevCpPostTg, startRevCpPreTg, deltaCpPreTg, 
                 deltaCpPostTg, StartCpTempPreTg, loess),
    
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
  
  
  if(gaussianNumber > 0) {
    writeData(wbmDSCSim, sheet <- "Settings", configGauss, startCol = 16)
  }
  
  addWorksheet(wbmDSCSim, "FT Deconvoluted signals")
  writeData(wbmDSCSim, sheet <- "FT Deconvoluted signals", finaldf)
  
  addWorksheet(wbmDSCSim, "Non-FT Deconvoluted signals")
  writeData(wbmDSCSim, sheet <- "Non-FT Deconvoluted signals", noFTcalc)
  
  addWorksheet(wbmDSCSim, "Raw signals")
  writeData(wbmDSCSim, sheet <- "Raw signals", signalGen)
  
  
  return(wbmDSCSim)
}
