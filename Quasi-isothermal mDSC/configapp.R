
options(shiny.maxRequestSize = 100 * 1024^2)  # 100 MB limit

configUI1<- function() {
  tagList(
    column(6,
      checkboxInput("saveNRHFplot", "Save non-reversing heat flow plot?", FALSE),
      checkboxInput("saveRHFplot", "Save reversing heat flow plot?", FALSE),
      checkboxInput("savemanualRHFplot", "Save manually calculated reversing heat flow plot?", FALSE),
      checkboxInput("saveDatasteps3", "Save final raw data?", FALSE),
      checkboxInput("saveExtremadf3", "Save final maxima and minima list?", FALSE),
      checkboxInput("saveSummaryFT", "Save final analysed data", FALSE),
      fileInput("Excel_in", "Upload your Excel here"),    
    )
  )
}

configUI2<- function() {
  tagList(
    column(6,
      textInput("period_in", "What was your modulation period (in minutes)", "2/3"), 
      textInput("step_size_in", "What was your step size (in °C)", "3"),
      textInput("isotherm_length_in", "What was your isotherm length (in minutes)", "20"),
      textInput("starting_temp_in", "What was your starting temperature (in °C)", "13"),
      textInput("setAmplitude_in", "What was your temperature modulation amplitude (in °C)", "0.212"),
      textInput("modulations_back_in", "How many modulations should be used for the final calculation?", "15"),
    )
  )
}

configUI3<- function() {
  tagList(
    HTML("<br>"),
    HTML("<br>"),
    tags$div(
      style = "text-align: center;",
      actionButton("calculate", "Calculate", class = "btn-primary btn-lg")
    )
  )
}








