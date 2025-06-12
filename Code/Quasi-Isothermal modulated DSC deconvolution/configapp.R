
options(shiny.maxRequestSize = 100 * 1024^2)  # 100 MB limit


configUI1<- function(ns) {
  tagList(
    column(6,
      checkboxInput(ns("saveNRHFplot"), "Save non-reversing heat flow plot?", FALSE),
      checkboxInput(ns("saveRevCpplot"), "Save reversing heat flow plot?", FALSE),
      checkboxInput(ns("savemanualRevCpplot"), "Save manually calculated reversing heat flow plot?", FALSE),
      checkboxInput(ns("saveExcel"), "Save Excel with all the analyses?", TRUE),
      fileInput(ns("Excel_in"), "Upload your Excel here"),
      checkboxInput(ns("sheetask"), "Is your data in the first sheet of your Excel file?", TRUE),
      conditionalPanel(
        condition = sprintf("!input['%s']", ns("sheetask")),
        selectInput(ns("sheet"), "What sheet is it in then?", choices = c("2", "3", "4", "5"))
      ),
      mainPanel(
        div(
          class = "error-text",
          textOutput(ns("errorMessage"))
        )    
      )   
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
      textInput(ns("sampling"), "What was your sampling rate in pts/s?", "10"),
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

configUI4 <- function(ns) {
  sidebarLayout(
    sidebarPanel(
      h4("Plot export settings"),
      selectInput(ns("extension"), "What should the plot's extension be?", c(".png", ".jpg", ".tiff")), 
      textInput(ns("exportDpi"), "What should the plot dpi be?", value= 600),
      textInput(ns("exportWidth"), "What should the plot width be in cm?",  value= 20),
      textInput(ns("exportHeight"), "What should the plot height be in cm?", value= 20)
    ),
    
    mainPanel(
      fluidRow(
               tags$div(
                 style = "text-align: center;",
                 downloadButton(ns("excelDownload"), "Download the Excel sheet with all the analyses", class = "btn-primary btn-lg")
               )
      ),
      br(), br(),
      fluidRow(
               tags$div(
                 style = "text-align: center;",
                 downloadButton(ns("NRHFdownload"), "Download the non-reversing heat flow plot", class = "btn-primary btn-lg")
           )
      ),
      br(), br(),
      fluidRow(
               tags$div(
                 style = "text-align: center;",
                 downloadButton(ns("RevCpdownload"), "Download the reversing heat capacity plot", class = "btn-primary btn-lg")
               )
        ),
      br(), br(),
      fluidRow(
               tags$div(
                 style = "text-align: center;",
                 downloadButton(ns("nonFTrevCpdownload"), "Download the reversing heat capacity plot calculated without FT", class = "btn-primary btn-lg")
               )
      ),
      br(), br(),
      fluidRow(
        tags$div(
          style = "text-align: center;",
          downloadButton(ns("allPlotsDownload"), "Download all plots at once in a single .zip file", class = "btn-primary btn-lg")
        )
      ),
      br(), br(), br(),
      div(
        class = "succes-text",
        textOutput(ns("downloadMessage"))
      )
    )
  )
}
