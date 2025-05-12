
options(shiny.maxRequestSize = 100 * 1024^2)  # 100 MB limit

configUI1<- function(ns) {
  tagList(
    column(6,
      checkboxInput(ns("saveNRHFplot"), "Save non-reversing heat flow plot?", FALSE),
      checkboxInput(ns("saveRevCpplot"), "Save reversing heat flow plot?", FALSE),
      checkboxInput(ns("savemanualRevCpplot"), "Save manually calculated reversing heat flow plot?", FALSE),
      checkboxInput(ns("saveExcel"), "Save Excel with all the analyses?", TRUE),
      fileInput(ns("Excel_in"), "Upload your Excel here"),    
    )
  )
}

configUI2<- function(ns) {
  tagList(
    column(6,
      textInput(ns("period_in"), "What was your modulation period (in minutes)", "2/3"), 
      textInput(ns("step_size_in"), "What was your step size (in °C)", "3"),
      textInput(ns("isotherm_length_in"), "What was your isotherm length (in minutes)", "20"),
      textInput(ns("starting_temp_in"), "What was your starting temperature (in °C)", "13"),
      textInput(ns("setAmplitude_in"), "What was your temperature modulation amplitude (in °C)", "0.212"),
      textInput(ns("modulations_back_in"), "How many modulations should be used for the final calculation?", "15"),
    )
  )
}

configUI3<- function(ns) {
  tagList(
    HTML("<br>"),
    HTML("<br>"),
    tags$div(
      style = "text-align: center;",
      actionButton(ns("calculate"), "Calculate", class = "btn-primary btn-lg")
    )
  )
}








