#-----------------------------------------------------------------------------------------
#UI definitions
#-----------------------------------------------------------------------------------------

configUIsim1 <- function(ns) {
  tagList(
    fluidRow(
      column(6,
             numericInput(ns("sampling"), "Sampling rate in points per second", 10),
             numericInput(ns("startTemp"), "Starting temperature of the mDSC run (°C)", 0),
             numericInput(ns("endTemp"), "Final temperature of the mDSC run (°C)", 180),
             numericInput(ns("period"), "Period of the modulations in seconds", 40),
             numericInput(ns("heatRate"), "Heating rate of the mDSC runs in °C/min", 2)
             
      ),
      column(6,
             numericInput(ns("Atemp"), "Amplitude of the temperature modulation (°C)", 0.21, step=0.001),
             numericInput(ns("phase"), "Phase of the modulated heat flow with respect to the temperature modulation (rad)", 0, step=0.1),
             numericInput(ns("loessAlpha"), "Degree of smoothing (higher = more smoothing)", 0.05, step=0.01),
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
             numericInput(ns("deltaRevCpPreTg"), "What is the slope of the reversing heat capacity before the Tg (J/(g*°C²))", 0.003, step = 0.0005),
             numericInput(ns("deltaRevCpPostTg"), "What is the slope of the reversing heat capacity after the Tg (J/(g*°C²))", 0.003, step = 0.0005),
             numericInput(ns("startRevCpPreTg"), "What value does your reversing heat capacity start at (J/(g*°C)", 1.2, step = 0.01),
             
      ),
      column(4,
             numericInput(ns("deltaCpPreTg"), "What is the slope of the total heat capacity before the Tg (J/(g*°C²))", 0.0004, step = 0.0001),
             numericInput(ns("deltaCpPostTg"), "What is the slope of the total heat capacity after the Tg (J/(g*°C²))", 0.0005, step = 0.0001),
             numericInput(ns("StartCpTempPreTg"), "What value does your total heat capacity start at (J/(g*°C)", 1.05, step = 0.01)
             
      ),
      column(4,
             textInput(ns("locationTgTHF"), "Where is the Tg on the total heat flow? Input start, end, and midpoint separated by commas (°C)", "33.5, 44.9, 39.2"),
             textInput(ns("locationTgRHF"), "Where is the Tg on the reversing heat flow? Input start, end, and midpoint separated by commas (°C)", "36, 45.5, 43.75"),
             numericInput(ns("deltaCpTg"), "What is the jump in heat capacity at the Tg (J/(g*°C))", 0.2)
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

configUIsim5 <- function(ns) {
  sidebarLayout(
    sidebarPanel(
      actionButton(ns("analyzefreqEffect"), "Analyze!"),
      actionButton(ns("saveAnalysis"), "Save analysis"),
      actionButton(ns("exportAnalysis"), "Export analysis"),
      h4("Parameters"),
      sliderInput(ns("intlimitlower"), "Lower limit of integration/baseline correction", min = -100, max = 200, value= -50),
      sliderInput(ns("intlimithigher"), "Upper limit of integration/baseline correction", min = -100, max = 200, value= 50),
      sliderInput(ns("dCpRHF"), "Jump in Cp on RHF", min = 0.05, max = 1, value= 0.3),
      sliderInput(ns("dCpTHF"), "Jump in Cp on THF", min = 0.05, max = 1, value= 0.3),
      sliderInput(ns("ARHF"), "Starting Cp value of RHF", min = 0.01, max = 0.5, value= 0.025),
      sliderInput(ns("ATHF"), "Starting Cp value of THF", min = 0.01, max = 0.5, value= 0.025),
      sliderInput(ns("BRHF"), "Slope before Tg on RHF", min = 0.0001, max = 0.01, value= 0.0005),
      sliderInput(ns("BTHF"), "Slope before Tg on THF", min = 0.0001, max = 0.01, value= 0.0005),
      sliderInput(ns("DRHF"), "Slope after Tg on RHF", min = 0.0001, max = 0.01, value= 0.0005),
      sliderInput(ns("DTHF"), "Slope after Tg on THF", min = 0.0001, max = 0.01, value= 0.0005),
      sliderInput(ns("TTHF"), "Tg midpoint on THF", min = 0, max = 200, value= 30),
      sliderInput(ns("TRHF"), "Tg midpoint on RHF", min = 10, max = 210, value= 40),
      sliderInput(ns("k"), "k-value", min = 0.05, max = 2, value= 0.3, step = 0.025),
      sliderInput(ns("kcorr"), "Baseline correction value", min = 0.005, max = 1, value= 0.3),
    ),
    
    mainPanel(
      br(), br(), br(), br(), br(),
        fluidRow(
          column(12, plotlyOutput(ns("plotfreqeffect"), height = "90vh"))
        )
      )
    )
}




configUI6Sim <- function(ns) {
  sidebarLayout(
    sidebarPanel(
      h4("Plot export settings"),
      selectInput(ns("extension"), "What should the plot's extension be?", 
                  c(".png", ".jpg", ".tiff")), 
      numericInput(ns("exportDpi"), "What should the plot dpi be?", value= 600),
      numericInput(ns("exportWidth"), "What should the plot width be in cm?",  
                   value= 20),
      numericInput(ns("exportHeight"), "What should the plot height be in cm?", 
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
