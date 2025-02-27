library(plotly)

#Change when changing file----------
setwd("C:/Users/u0155764/OneDrive - KU Leuven/Tom Konings/Software/Quasi-isothermal mDSC analyzer")
fileName <- "DSC_data_240minanneal"
modulations_back <- 28
subtractblank <- FALSE
sample_size <- 5.79
Interactive <- FALSE
#----------------------------


source("Processing and cleaning overall function.R")
source("config.R")
source("plots.R")
source("Interactive_plots.R")


sample_results <- processDSC(file = fileName, export = TRUE,
                             rangesmin = rangesmin, rangesmax = rangesmax, 
                             starting_temp = starting_temp, step_size = step_size,
                             temp_margin_first_cleanup = temp_margin_first_cleanup,
                             modulations_back = modulations_back, period = period,
                             setAmplitude = setAmplitude, tempModAmplitude = tempModAmplitude)


if(subtractblank == FALSE) {
  sample_results$dc_value <- sample_results$dc_value/sample_size
  sample_results$reversing_heat_flow <- sample_results$reversing_heat_flow/sample_size
  }


if(subtractblank) {
  blank_results <- processDSC(file = "blank", export = FALSE, 
                              rangesmin = rangesmin, rangesmax = rangesmax, 
                              starting_temp = starting_temp, step_size = step_size,
                              temp_margin_first_cleanup = temp_margin_first_cleanup,
                              modulations_back = modulations_back, period = period,
                              setAmplitude = setAmplitude, tempModAmplitude = tempModAmplitude)
  
  sample_corrected <- sample_results %>%
    left_join(blank_results %>% select(pattern, dc_value_blank = dc_value), by = "pattern") %>%
    mutate(corrected_dc = (dc_value - dc_value_blank)/sample_size)
  
  sample_results$dc_value <- sample_corrected$corrected_dc
  sample_results$reversing_heat_flow <- sample_results$reversing_heat_flow/sample_size
  
  
  print("BLANK SUBTRACTED!")
}


NRHF_plot(sample_results)
RHF_plot(sample_results)
if (plot_datasteps4){
  Datasteps_plot(data_steps_cleaned_4)
}
Manual_RHF_plot(average_heat_flow_per_pattern)


if(saveSummaryFT == TRUE) {
  summarydf <- data.frame(sample_results$TRef, sample_results$dc_value, sample_results$reversing_heat_flow)
  names(summarydf) <- c("TRef", "NRHF", "RHF")
  write.xlsx(summarydf, paste0(fileName, " ", modulations_back, " mod analysed.xlsx"))
}

if(saveDatasteps4 == TRUE) {
  write.xlsx(data_steps_cleaned_4, paste0("datasteps4",fileName, " ", modulations_back, " mod analysed.xlsx"))
}


if(Interactive){
  runDSCShinyApp(sample_results, average_heat_flow_per_pattern)
}

if(exists("signfigerror")) {
  message(red(signfigerror))
  showDialog(title = "Warning", message = signfigerror)}


