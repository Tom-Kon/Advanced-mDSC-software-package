#-----------------------------------------------------------------------------------------
#UI definitions
#-----------------------------------------------------------------------------------------

configUIsim1 <- function(ns) {
  tagList(
    fluidRow(
      column(6,
             textInput(ns("sampling"), "Sampling rate in points per second", "10"),
             textInput(ns("startTemp"), "Starting temperature of the mDSC run (°C)", "0"),
             textInput(ns("endTemp"), "Final temperature of the mDSC run (°C)", "180"),
             textInput(ns("period"), "Period of the modulations in seconds", "40"),
             textInput(ns("heatRate"), "Heating rate of the mDSC runs in °C/min", "2")
             
      ),
      column(6,
             textInput(ns("Atemp"), "Amplitude of the temperature modulation (°C)", "0.212"),
             textInput(ns("phase"), "Phase of the modulated heat flow with respect to the temperature modulation (rad)", "-0.2"),
             textInput(ns("loessAlpha"), "Degree of smoothing (higher = more smoothing)", "0.05"),
             selectInput(inputId = ns("gaussianNumber"), 
                         label ="How many Gaussian-shaped events do you want to add?", 
                         choices = c(0:10)),
      )
    ),
    fluidRow(
      HTML("<br>", "<br>", "<br>"),
      column(4),
      column(4,
             tags$div(
               style = "text-align: center;",
               actionButton(ns("next1"), "Next", class = "btn-primary btn-lg", 
                            style = "width: 70%; font-size: 25px; padding: 15px 30px;")
             )
      ),
      column(4)
    )
  )
}

configUIsim2 <- function(ns) {
  tagList(
    fluidRow(
      column(4,
             textInput(ns("deltaRHFPreTg"), "What is the slope of the reversing heat flow before the Tg (J/(g*°C))", "-0.0001"),
             textInput(ns("deltaRHFPostTg"), "What is the slope of the reversing heat flow after the Tg (J/(g*°C))", "-0.0001"),
             textInput(ns("StartRHFPreTg"), "What value does your reversing heat flow start at (J/g)", "-0.040"),
             
      ),
      column(4,
             textInput(ns("deltaCpPreTg"), "What is the slope of the total heat capacity before the Tg (J/(g*°C))", "0.0008"),
             textInput(ns("deltaCpPostTg"), "What is the slope of the total heat capacity after the Tg (J/(g*°C))", "0.0009"),
             textInput(ns("StartCpTempPreTg"), "What value does your total heat capacity start at (J/g)", "1.05")
             
      ),
      column(4,
             textInput(ns("locationTgTHF"), "Where is the Tg on the total heat flow? Input start, end, and midpoint separated by commas (°C)", "30, 40, 35"),
             textInput(ns("locationTgRHF"), "Where is the Tg on the reversing heat flow? Input start, end, and midpoint separated by commas (°C)", "35, 45, 40"),
             textInput(ns("deltaCpTg"), "What is the jump in heat capacity at the Tg (J/(g*°C))", "0.268")
      )
    ),
    fluidRow(
      HTML("<br>", "<br>", "<br>"),
      column(4),
      column(4,
             tags$div(
               style = "text-align: center;",
               actionButton(ns("next2"), "Next", class = "btn-primary btn-lg", 
                            style = "width: 70%; font-size: 25px; padding: 15px 30px;")
             )
      ),
      column(4)
    )
  )

}


configUIsim3 <- function(ns) {
  tagList(
    fluidRow(
      column(12,
             conditionalPanel(
               condition = paste0("input['", ns("gaussianNumber"), "'] != 0"),
               uiOutput(ns("gaussians"))
             )
      )
    ),
    fluidRow(
      HTML("<br>", "<br>", "<br>"),
      column(4),
      column(4,
             tags$div(
               style = "text-align: center;",
               actionButton(ns("analyze"), "Analyze", class = "btn-primary btn-lg", 
                            style = "width: 70%; font-size: 25px; padding: 15px 30px;"),
               HTML("<br><br><br>"),
               div(
                 class = "error-text",
                 textOutput(ns("errorMessage"))
               ),
               div(
                 class = "succes-text",
                 textOutput(ns("succesMessage"))
               )
             )
      ),
      column(4)
    )
  )
}

configUIsim4 <- function(ns) {
  tagList(
    titlePanel("Output graphs"),
    fluidRow(
      column(12, wellPanel(
        selectInput(ns("plot_choice"), "Select Plot:",
                    choices = c("MHF", "Overlay", "THF", "RHF", "RHF no FT", "NRHF"),
                    selected = "MHF")
      ))
    ),
    fluidRow(
      column(12, plotlyOutput(ns("plot"), height = "90vh"))
    )
  )
}

configUI5 <- function(ns) {
  sidebarLayout(
    sidebarPanel(
      h4("Plot export settings"),
      selectInput(ns("extension"), "What should the plot's extension be?", 
                  c(".png", ".jpg", ".tiff")), 
      textInput(ns("exportDpi"), "What should the plot dpi be?", value= 600),
      textInput(ns("exportWidth"), "What should the plot width be in cm?",  
                value= 20),
      textInput(ns("exportHeight"), "What should the plot height be in cm?", 
                value= 20)
    ),
    
    mainPanel(
      br(), br(), br(), br(), br(),
      fluidRow(
        tags$div(
          style = "text-align: center;",
          downloadButton(ns("downloadExcelSimDSC"), "Download the Excel sheet with all the analyses", 
                         class = "btn-primary btn-lg")
        )
      ),
      br(), br(), br(),
      fluidRow(
        tags$div(
          style = "text-align: center;",
          downloadButton(ns("mDSCSimplotsDownload"), "Download all the plots as a .zip", 
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
