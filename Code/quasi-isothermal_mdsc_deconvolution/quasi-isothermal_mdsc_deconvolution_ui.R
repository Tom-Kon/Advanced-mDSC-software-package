options(shiny.maxRequestSize = 100 * 1024^2)  # 100 MB limit

configUI1<- function(ns) {
  tagList(
    fluidRow(
      column(6,
             numericInput(ns("period"), "What was your modulation period (in seconds)", 40), 
             numericInput(ns("stepSize"), "What was your step size (in °C)", 3),
             numericInput(ns("isothermLength"), "What was your isotherm length (in minutes)", 20),
             numericInput(ns("startingTemp"), "What was your starting temperature (in °C)", 13)
            
             ),
 
      column(6,
             numericInput(ns("setAmplitude"), "What was your temperature modulation amplitude (in °C)", 0.212, step=0.001),
             numericInput(ns("modulationsBack"), "How many modulations should be used for the final calculation?", 15),
             numericInput(ns("sampling"), "What was your sampling rate in pts/s?", 10),
             fileInput(ns("Excel"), "Upload your Excel here"),
             checkboxInput(ns("sheetask"), "Is your data in the first sheet of your Excel file?", TRUE),
             conditionalPanel(
               condition = sprintf("!input['%s']", ns("sheetask")),
               selectInput(ns("sheet"), "What sheet is it in then?", choices = c("2", "3", "4", "5"))
               )
             )    
      ),
    HTML("<br><br><br>"),
    fluidRow(
      column(4),
      column(4,
             div(style = "text-align:center;",
                 actionButton(ns("analyze"), "Analyze", 
                              class = "btn-primary btn-lg",
                              style = "width: 70%; font-size: 25px; padding: 15px 30px;")
             ),
             HTML("<br><br><br>"),
             div(
               class = "error-text",
               textOutput(ns("errorMessage"))
             ),
             div(
               class = "succes-text",
               textOutput(ns("succesMessage"))
             )
      ),
      column(4)
    )
  )
}


configUI2<- function(ns) {
  tagList(
    fluidRow(
      titlePanel("Output graphs"),
      fluidRow(
        column(12, wellPanel(
          selectInput(ns("plot_choice"), "Select Plot:", 
                      choices = c("NRHF", "RevCp", "Manual RevCp", "RevCp and NRHF", 
                                  "Maxima and minima 1", "Maxima and minima prefinal", 
                                  "Maxima and minima final", "Original data", 
                                  "First cleaned up data", "Prefinal cleaned up data", 
                                  "Final data used for analysis"), 
                      selected = "RevCp"),
          fluidRow(
            column(6,
                   HTML("<br>"),
                   actionButton(ns("recalc"), 
                                "Recalculate with different number of modulations")
            ),
            column(6, 
                   numericInput(ns("modulations_back_new"), 
                                "New number of modulations", value=2)
            )
          )
        ))
      ),
      fluidRow(
        column(12, plotlyOutput(ns("plot"), height = "90vh"))
      )
    )
  )
}




configUI3 <- function(ns) {
  sidebarLayout(
    sidebarPanel(
      h4("Plot export settings"),
      selectInput(ns("extension"), "What should the plot's extension be?", 
                  c(".png", ".jpg", ".tiff")), 
      numericInput(ns("exportDpi"), "What should the plot dpi be?", value = 600),
      numericInput(ns("exportWidth"), "What should the plot width be in cm?",  
                value = 20),
      numericInput(ns("exportHeight"), "What should the plot height be in cm?", 
                value = 20)
    ),
    
    mainPanel(
      fluidRow(
               tags$div(
                 style = "text-align: center;",
                 downloadButton(ns("excelDownload"), 
                                "Download the Excel sheet with all the analyses", 
                                class = "btn-primary btn-lg")
               )
      ),
      br(), br(),
      fluidRow(
               tags$div(
                 style = "text-align: center;",
                 downloadButton(ns("NRHFdownload"), 
                                "Download the non-reversing heat flow plot", 
                                class = "btn-primary btn-lg")
               )
      ),
      br(), br(),
      fluidRow(
               tags$div(
                 style = "text-align: center;",
                 downloadButton(ns("RevCpdownload"), 
                                "Download the reversing heat capacity plot", 
                                class = "btn-primary btn-lg")
               )
      ),
      br(), br(),
      fluidRow(
               tags$div(
                 style = "text-align: center;",
                 downloadButton(ns("nonFTrevCpdownload"), 
                                "Download the reversing heat capacity plot calculated without FT", 
                                class = "btn-primary btn-lg")
               )
      ),
      br(), br(),
      fluidRow(
        tags$div(
          style = "text-align: center;",
          downloadButton(ns("allPlotsDownload"),
                         "Download all plots at once in a single .zip file", 
                         class = "btn-primary btn-lg")
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
