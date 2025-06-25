download_excel_regular_mDSC <- function(reactiveInputs) {
  

  period <- reactiveInputs$period
  heatingRate <- reactiveInputs$heatingRate
  setAmplitude <- reactiveInputs$setAmplitude
  tempModAmplitude <- setAmplitude*2*pi/period
  compare <- reactiveInputs$compare
  HFcalcextra <- reactiveInputs$HFcalcextra
  fileName <- reactiveInputs$fileName

  config <- data.frame(
    Parameter = c("Period (sec)",
                  "Heating rate (°C/min)",
                  "Temperature modulation amplitude (°C)", 
                  "Calculated amplitude of the derived temperature function (°C)",
                  "Did you compare with DSC?",
                  "Did you compare with the THF calculated by the software?"),
    
    Value = c(period,
              heatingRate,
              setAmplitude, 
              tempModAmplitude, 
              compare,
              HFcalcextra)
  )
  
  fileName <- unlist(strsplit(fileName, "\\."))[1]
  fileName <- paste0(fileName, ".xlsx")
  
  wb <- createWorkbook(fileName)
  
  resultsMaxMinfullmanual <- reactiveInputs$calculationMinMaxResults
  calculate_fft <- reactiveInputs$calculate_fft
  
 
  addWorksheet(wb, "Settings")
  writeData(wb, sheet = "Settings", config)
  
  addWorksheet(wb, "Analysis Min and Max")
  writeData(wb, sheet = "Analysis Min and Max", resultsMaxMinfullmanual)
  
  addWorksheet(wb, "Analysis using FT")
  writeData(wb, sheet = "Analysis using FT", calculate_fft)
  
  if(reactiveInputs$compare) {
    resultsDSC <- reactiveInputs$DSCdf
    addWorksheet(wb, "Analysis using unmodulated DSC")
    writeData(wb, sheet = "Analysis using unmodulated DSC", calculationMinMaxResultsDSC)
  }
  
  if (reactiveInputs$HFcalcextra) {
    resultsMaxMinTHF <- reactiveInputs$RHFdf2
    addWorksheet(wb, "Analysis Max and THF")
    writeData(wb, sheet = "Analysis Max and THF", calculationMinMaxResultsTHF)
  }

  return(wb)
  
}

