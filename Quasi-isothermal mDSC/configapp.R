configUI <- function() {
  tagList(
    checkboxInput("plot_datasteps4", "Plot final raw data?", FALSE),
    checkboxInput("saveplotdatasteps4", "Save final raw data?", FALSE),
    checkboxInput("saveNRHFplot", "Save non-reversing heat flow plot?", FALSE),
    checkboxInput("saveRHFplot", "Save reversing heat flow plot?", FALSE),
    checkboxInput("savemanualRHFplot", "Save manually calculated reversing heat flow plot?", FALSE),
    checkboxInput("saveDatasteps4", "Save final raw data?", FALSE),
    checkboxInput("saveExtremadf3", "Plot final raw data?", FALSE),
    checkboxInput("saveSummaryFT", "Plot final raw data?", FALSE),
    
    textInput("period", "What was your modulation period (in minutes)", "2/3"), 
    textInput("step_size", "What was your step size (in °C)", "3"),
    textInput("isotherm_length", "What was your isotherm length (in minutes)", "20"),
    textInput("starting_temp", "What was your starting temperature (in °C)", "13"),
    textInput("setAmplitude", "What was your temperature modulation amplitude (in °C)", "0.212")
    
  )
}




