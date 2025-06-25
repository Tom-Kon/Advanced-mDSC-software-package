downloadExcelRegmDSC <- function(reactive_inputs) {
  

  period <- reactive_inputs$period
  heating_rate <- reactive_inputs$heating_rate
  setAmplitude <- reactive_inputs$setAmplitude
  tempModAmplitude <- setAmplitude*2*pi/period
  compare <- reactive_inputs$compare
  HFcalcextra <- reactive_inputs$HFcalcextra
  fileName <- reactive_inputs$fileName

  config <- data.frame(
    Parameter = c("Period (sec)",
                  "Heating rate (°C/min)",
                  "Temperature modulation amplitude (°C)", 
                  "Calculated amplitude of the derived temperature function (°C)",
                  "Did you compare with DSC?",
                  "Did you compare with the THF calculated by the software?"),
    
    Value = c(period,
              heating_rate,
              setAmplitude, 
              tempModAmplitude, 
              compare,
              HFcalcextra)
  )
  
  fileName <- unlist(strsplit(fileName, "\\."))[1]
  fileName <- paste0(fileName, ".xlsx")
  
  wb <- createWorkbook(fileName)
  
  resultsMaxMinfullmanual <- reactive_inputs$RHFdf
  resultsfft <- reactive_inputs$fftCalc
  
 
  addWorksheet(wb, "Settings")
  writeData(wb, sheet = "Settings", config)
  
  addWorksheet(wb, "Analysis Min and Max")
  writeData(wb, sheet = "Analysis Min and Max", resultsMaxMinfullmanual)
  
  addWorksheet(wb, "Analysis using FT")
  writeData(wb, sheet = "Analysis using FT", resultsfft)
  
  if(reactive_inputs$compare) {
    resultsDSC <- reactive_inputs$DSCdf
    addWorksheet(wb, "Analysis using unmodulated DSC")
    writeData(wb, sheet = "Analysis using unmodulated DSC", resultsDSC)
  }
  
  if (reactive_inputs$HFcalcextra) {
    resultsMaxMinTHF <- reactive_inputs$RHFdf2
    addWorksheet(wb, "Analysis Max and THF")
    writeData(wb, sheet = "Analysis Max and THF", resultsMaxMinTHF)
  }

  return(wb)
  
}

