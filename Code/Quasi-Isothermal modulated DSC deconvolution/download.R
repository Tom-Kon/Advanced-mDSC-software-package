downloadExcel <- function(fileName, sample_results, modulations_back, period, setAmplitude, starting_temp, step_size, isothermLength, sampling) {
      
  
  tempModAmplitude <- setAmplitude*2*pi/period
  temp_margin_first_cleanup <- 0.05
  points_distance_minimum_margin <- (sampling*period*60)/2/10
  
    
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
                  modulations_back, 
                  setAmplitude, 
                  temp_margin_first_cleanup, 
                  points_distance_minimum_margin, 
                  tempModAmplitude, 
                  sampling)
      )
      
      
      ft_averages <- sample_results$ft_averages
      ft_averagesexport <- data.frame("Step number" = ft_averages$pattern, "Temperature at that modulation" = ft_averages$TRef, "Non-reversing heat flow" = ft_averages$dc_value, "Reversing heat flow" = ft_averages$reversing_heat_flow)
      
      
      average_heat_flow_per_pattern <- sample_results$average_heat_flow_per_pattern
      d_steps_cleaned <- sample_results$d_steps_cleaned
      extrema_df1 <- sample_results$extrema_df1
      d_steps_cleaned_2 <- sample_results$d_steps_cleaned_2
      extrema_df2 <- sample_results$extrema_df2
      d_steps_cleaned_3 <- sample_results$d_steps_cleaned_3
      extrema_df3 <- sample_results$extrema_df3
      
      fileName <- unlist(strsplit(fileName, "\\."))[1]
      fileName <- paste0(fileName, " ", modulations_back, " modulations analysed.xlsx")
      wb <- createWorkbook(fileName)

      addWorksheet(wb, "0.Settings")
      writeData(wb, sheet = "0.Settings", config)
      
      addWorksheet(wb, "1.Analysed results")
      writeData(wb, sheet = "1.Analysed results", ft_averagesexport)
      
      addWorksheet(wb, "2.Non-FT calc. RevCp")
      writeData(wb, sheet = "2.Non-FT calc. RevCp", average_heat_flow_per_pattern)
      
      addWorksheet(wb, "3.Data with isolated patterns")
      writeData(wb, sheet = "3.Data with isolated patterns", d_steps_cleaned)
      
      addWorksheet(wb, "4.Extrema of sheet 3")
      writeData(wb, sheet = "4.Extrema of sheet 3", extrema_df1)
      
      addWorksheet(wb, "5.Delete last max of sheet 3")
      writeData(wb, sheet = "5.Delete last max of sheet 3", d_steps_cleaned_2)
      
      addWorksheet(wb, "6.Extrema of sheet 5")
      writeData(wb, sheet = "6.Extrema of sheet 5", extrema_df2)
      
      addWorksheet(wb, "7.Data used in final analysis")
      writeData(wb, sheet = "7.Data used in final analysis", d_steps_cleaned_3)
      
      addWorksheet(wb, "8.Extrema of sheet 7")      
      writeData(wb, sheet = "8.Extrema of sheet 7", extrema_df3)
      
      
      saveWorkbook(wb, fileName, overwrite = TRUE)
}

