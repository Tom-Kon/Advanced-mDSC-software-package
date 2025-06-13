downloadExcelSimDSC <- function(reactive_inputs) {
  
  #Time generation
  sampling <- reactive_inputs$sampling
  startTemp <- reactive_inputs$startTemp
  endTemp <- reactive_inputs$endTemp
  period <- reactive_inputs$period
  heatRate <- reactive_inputs$heatRate
  
  #MHF generation (fixed)
  Atemp <- reactive_inputs$Atemp
  phase <- reactive_inputs$phase
  deltaRHFPreTg <- reactive_inputs$deltaRHFPreTg
  deltaRHFPostTg <- reactive_inputs$deltaRHFPostTg
  StartRHFPreTg <- reactive_inputs$StartRHFPreTg
  deltaCpPreTg <- reactive_inputs$deltaCpPreTg
  deltaCpPostTg <- reactive_inputs$deltaCpPostTg
  StartCpTempPreTg <- reactive_inputs$StartCpTempPreTg
  
  #MHF generation (for loop)
  locationTgTHF <- reactive_inputs$locationTgTHF
  locationTgRHF <- reactive_inputs$locationTgRHF
  deltaCpTg <- reactive_inputs$deltaCpTg
  MeltEnth <- reactive_inputs$MeltEnth
  phase_melt <- reactive_inputs$phase_melt
  locationMelt <- reactive_inputs$locationMelt
  Crystalenth <- reactive_inputs$Crystalenth
  locationcrystal <- reactive_inputs$locationcrystal
  EnthrecEnth <- reactive_inputs$EnthrecEnth
  locationEnthRec <- reactive_inputs$locationEnthRec
  periodSignal <- reactive_inputs$periodSignal
  
  
  config <- data.frame(
    ParameterFixed <- c("Sampling rate (pts/sec)",
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
                   
    Value <- c(sampling,
               startTemp,
               endTemp,
               period,
               heatRate,
               Atemp, 
               phase, 
               deltaRHFPreTg,
               deltaRHFPostTg,
               StartRHFPreTg,
               deltaCpPreTg,
               deltaCpPostTg,
               StartCpTempPreTg)
  )
  
  wb <- createWorkbook()
  
  FTDeconvolution <- reactive_inputs$finaldf
  noFTDeconvolution <- reactive_inputs$noFTcalc
  rawSignal <- reactive_inputs$df2
  rawSignalFinal <- data.frame("Times" = rawSignal$times, "TRef" = rawSignal$TRef, "Modulated temperature" = rawSignal$modTemp, "Derivative of the modulated temperature" = rawSignal$modTempderiv, "Modulated temperature without the ramp" = rawSignal$modTempnoRamp, check.names = FALSE)

  addWorksheet(wb, "Settings")
  writeData(wb, sheet <- "Settings", config)
  
  addWorksheet(wb, "FT Deconvoluted signals")
  writeData(wb, sheet <- "FT Deconvoluted signals", FTDeconvolution)
  
  addWorksheet(wb, "Non-FT Deconvoluted signals")
  writeData(wb, sheet <- "Non-FT Deconvoluted signals", noFTDeconvolution)
  
  addWorksheet(wb, "Raw MHF signal")
  writeData(wb, sheet <- "Raw MHF signal", rawSignalFinal)
  
  return(wb)
  
}

