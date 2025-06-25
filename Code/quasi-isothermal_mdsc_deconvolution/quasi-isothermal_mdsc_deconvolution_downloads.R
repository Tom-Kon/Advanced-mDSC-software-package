downloadExcel <- function(fileName, results, modulationsBack, period, setAmplitude, startingTemp, stepSize, isothermLength, sampling) {
      
  RevCpDenominator <- setAmplitude*2*pi/period
  tempMarginFirstCleanup <- 0.05
  pointsDistanceMinimumMargin <- (sampling*period*60)/2/10
  
      config <- data.frame(
        Parameter = c("Starting temperature (°C)", 
                      "Period (sec)",
                      "Isotherm length (min)",
                      "Step size (°C)", 
                      "Number of modulations used in calculation",
                      "Temperature modulation amplitude (°C)",  
                      "Margin for the first cleanup (in between steps) - user input (°C)", 
                      "Margin for the first cleanup (in between steps) - calculated (°C)", 
                      "Calculated amplitude of the derived temperature function (°C)", 
                      "Sampling interval (pts/sec)"),
        
        Value = c(starting_temp, 
                  period*60,
                  isothermLength,
                  step_size, 
                  modulationsBack, 
                  setAmplitude, 
                  tempMarginFirstCleanup, 
                  pointsDistanceMinimumMargin, 
                  RevCpDenominator, 
                  sampling)
      )
      
      
      resultsFT <- results$resultsFT
      resultsFTexport <- data.frame("Step number" = resultsFT$pattern, "Temperature at that modulation" = resultsFT$TRef, "Non-reversing heat flow" = resultsFT$NRHF, "Reversing heat flow" = resultsFT$RevCp)
      
      
      resultsNoFT <- results$resultsNoFT
      isolatedPatterns <- results$isolatedPatterns
      extremaDfIntermediate <- results$extremaDfIntermediate
      deleteLastMax <- results$deleteLastMax
      extremaDfAfterDeleteMax <- results$extremaDfAfterDeleteMax
      finalDataForAnalysis <- results$finalDataForAnalysis
      finalAnalysisExtrema <- results$finalAnalysisExtrema
      
      fileName <- unlist(strsplit(fileName, "\\."))[1]
      fileName <- paste0(fileName, " ", modulationsBack, " modulations analysed.xlsx")
      wb <- createWorkbook(fileName)

      addWorksheet(wb, "0.Settings")
      writeData(wb, sheet = "0.Settings", config)
      
      addWorksheet(wb, "1.Analysed results")
      writeData(wb, sheet = "1.Analysed results", resultsFTexport)
      
      addWorksheet(wb, "2.Non-FT calc. RevCp")
      writeData(wb, sheet = "2.Non-FT calc. RevCp", resultsNoFT)
      
      addWorksheet(wb, "3.Data with isolated patterns")
      writeData(wb, sheet = "3.Data with isolated patterns", isolatedPatterns)
      
      addWorksheet(wb, "4.Extrema of sheet 3")
      writeData(wb, sheet = "4.Extrema of sheet 3", extremaDfIntermediate)
      
      addWorksheet(wb, "5.Delete last max of sheet 3")
      writeData(wb, sheet = "5.Delete last max of sheet 3", deleteLastMax)
      
      addWorksheet(wb, "6.Extrema of sheet 5")
      writeData(wb, sheet = "6.Extrema of sheet 5", extremaDfAfterDeleteMax)
      
      addWorksheet(wb, "7.Data used in final analysis")
      writeData(wb, sheet = "7.Data used in final analysis", finalDataForAnalysis)
      
      addWorksheet(wb, "8.Extrema of sheet 7")      
      writeData(wb, sheet = "8.Extrema of sheet 7", finalAnalysisExtrema)
      
      
return(wb)
      
}

