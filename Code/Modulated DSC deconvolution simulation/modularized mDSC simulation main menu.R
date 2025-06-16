# mdsc_module.R
source("../Modulated DSC deconvolution simulation/libraries.R")
source("../Modulated DSC deconvolution simulation/configapp.R")

mdsc_sim_ui <- function(id) {
  ns <- NS(id)
  navbarPage(
    id = ns("tabs"),
    title = "Modulated DSC deconvolution simulation",
    inverse = FALSE,
    
    tabPanel(
      title = "Parameter Input",
      id = ns("paraminput"),
      icon = icon("gears", class = "fa-solid"),
      fluidPage(
        configUIsim1(ns),
        fluidRow(
          column(6, 
                 selectInput(inputId = ns("gaussianNumber"), label ="How many Gaussian-shaped events do you want to add?", choices = c(1:10)),
                 selectInput(inputId = ns("tgNumber"), label ="How many glass transitions do you want to add?", choices = c(1:10)),
                 configUIsim5(ns)
          ),
        )
      )
    ),
    
    tabPanel(
      title = "Test",
      id = ns("Test"),
      icon = icon("gears", class = "fa-solid"),
      fluidPage(
        fluidRow(
          column(4, 
                 uiOutput(ns("gaussians")),
          ),
          column(4, 
                 uiOutput(ns("tgsRHF"))
          ),
          column(4, 
                 uiOutput(ns("tgsTHF"))
          ),
        )
      )
    ),
    
    tabPanel(
      title = "Baseline Input",
      id = ns("baseInput"),
      icon = icon("gears", class = "fa-solid"),
      fluidPage(
        fluidRow(configUIsim2(ns)) 
      )
    ),
    
    tabPanel(
      id = ns("graphs"),
      title = "Graphs",
      icon = icon("chart-line", class = "fa-solid"),
      fluidPage(
        titlePanel("Output graphs"),
        fluidRow(
          column(12, wellPanel(
            selectInput(ns("plot_choice"), "Select Plot:",
                        choices = c("MHF", "Overlay", "THF", "RHF", "RHF no FT", "NRHF", "Signal closeup"),
                        selected = "MHF")
          ))
        ),
        fluidRow(
          column(12, plotlyOutput(ns("plot"), height = "90vh"))
        )
      )
    )
  )
}

mdsc_sim_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    
    output$gaussians <- renderUI({
      
      gaussianNumber <- as.numeric(input$gaussianNumber)
      tgs <- as.numeric(input$tgs)
      
      lapply(1:gaussianNumber, function(i) {
        textInput(ns(paste0("gaussian", i)), "For each of the Gaussian profiles you wish to simulate, input (in this exact order) the onset in °C, end 
                  in °C, and enthalpy in J/g. Use a dot as decimal separator.")
      })
    })
    
    
    output$tgsRHF <- renderUI({
      
      tgNumber <- as.numeric(input$tgNumber)

      lapply(1:tgNumber, function(i) {
        textInput(ns(paste0("tgsRHF", i)), "For each of the Glass transitions  you wish to simulate, input (in this exact order) the Tg onset on the RHF in °C,  
                  the Tg midpoint on the RHF in °C, and the jump in heat capacity over the Tg in J/°C.g. Use a dot as decimal separator")
      })
      
    })
    
    output$tgsTHF <- renderUI({
      tgNumber <- as.numeric(input$tgNumber)
      
      lapply(1:tgNumber, function(i) {
        textInput(ns(paste0("tgsTHF", i)), "For each of the Glass transitions  you wish to simulate, input (in this exact order) the Tg onset on the THF in °C,  
                  the Tg midpoint on the THF in °C, the Tg endset on the THF in °C. Use a dot as decimal separator")
      })
    })
    
    
    
    # Create a reactiveValues object to store inputs
    reactive_inputs <- reactiveValues()
    
    observeEvent(input$calculate, {
      showPageSpinner()
      
      
      reactive_inputs$sampling <- eval(parse(text = input$sampling))
      reactive_inputs$startTemp <- eval(parse(text = input$startTemp))
      reactive_inputs$endTemp <- eval(parse(text = input$endTemp))
      reactive_inputs$period <- eval(parse(text = input$period))
      reactive_inputs$heatRate <- eval(parse(text = input$heatRate))/60
      reactive_inputs$Atemp <- eval(parse(text = input$Atemp))
      reactive_inputs$phase <- eval(parse(text = input$phase))
      reactive_inputs$loessAlpha <- eval(parse(text = input$loessAlpha))
      reactive_inputs$deltaRHFPreTg <- eval(parse(text = input$deltaRHFPreTg))
      reactive_inputs$deltaRHFPostTg <- eval(parse(text = input$deltaRHFPostTg))
      reactive_inputs$StartRHFPreTg <- eval(parse(text = input$StartRHFPreTg))
      reactive_inputs$deltaCpPreTg <- eval(parse(text = input$deltaCpPreTg))
      reactive_inputs$deltaCpPostTg <- eval(parse(text = input$deltaCpPostTg))
      reactive_inputs$StartCpTempPreTg <- eval(parse(text = input$StartCpTempPreTg))
      
      reactive_inputs$gaussianNumber <- as.numeric(input$gaussianNumber)
      reactive_inputs$gaussianList <- list()

      for(i in 1:reactive_inputs$gaussianNumber) {
        reactive_inputs$gaussianList[[length(reactive_inputs$gaussianList) + 1]] <- input[[paste0("gaussian", i)]]
      }
      
      reactive_inputs$tgNumber <- as.numeric(input$tgNumber)
      reactive_inputs$tgRHFList <- list()
      
      for(i in 1:reactive_inputs$tgNumber) {
        reactive_inputs$tgRHFList[[length(reactive_inputs$tgRHFList) + 1]] <- input[[paste0("tgsRHF", i)]]
      }
      
      reactive_inputs$tgTHFList <- list()
      
      for(i in 1:reactive_inputs$tgNumber) {
        reactive_inputs$tgTHFList[[length(reactive_inputs$tgTHFList) + 1]] <- input[[paste0("tgsTHF", i)]]
      }

      source("../Modulated DSC deconvolution simulation/Time point generation.R")
      source("../Modulated DSC deconvolution simulation/Signal generation.R")
      source("../Modulated DSC deconvolution simulation/Equally-spaced y-values.R")
      source("../Modulated DSC deconvolution simulation/Final calculations.R")
      source("../Modulated DSC deconvolution simulation/Plot generation and control.R")
      
      
      reactive_inputs$subtitle <- paste0(
        "Sampling: ", reactive_inputs$sampling, " pts/sec. Period: ", reactive_inputs$period, " sec. ", 
        "Melting period: ", reactive_inputs$periodSignal, " sec. Heating rate: ", reactive_inputs$heatRate * 60, " °C/min. ",
        "MHF phase: ", reactive_inputs$phase, " rad. LOESS alpha: ", reactive_inputs$loessAlpha, ".\n Temp. amplitude: ", reactive_inputs$Atemp, " °C.",
        "Melting amplitude: ", reactive_inputs$MeltEnth, " W/g. Other parameters (such as start temp.) are visible on the plot.")
      
      
      
      df1 <- timegeneration(reactive_inputs)
      
      df2 <- signalgeneration(reactive_inputs, df1)
      
      reactive_inputs$df2 <- df2
      resampled_points <- equalyval(df2 = df2)
      
      results <- finalcalc(
        sampling = reactive_inputs$sampling,
        startTemp = reactive_inputs$startTemp,
        endTemp = reactive_inputs$endTemp,
        period = reactive_inputs$period,
        heatRate = reactive_inputs$heatRate,
        Atemp = reactive_inputs$Atemp,
        resampled_points = resampled_points,
        loessAlpha = reactive_inputs$loessAlpha,
        df1 = df1,
        df2 = df2)
      
      reactive_inputs$finaldf <- results[[1]]
      reactive_inputs$noFTcalc <- results[[2]]
      
      
      hidePageSpinner()
      
    })
    
    # Render the plot using the reactive sample_results
    output$plot <- renderPlotly({
      req(reactive_inputs$finaldf)  # Ensure results exist
      req(reactive_inputs$noFTcalc)
      res <- reactive_inputs$finaldf
      res2 <- reactive_inputs$noFTcalc
      
      plot_obj <- switch(input$plot_choice,
                         "MHF" = MHFplots(res, reactive_inputs$subtitle),
                         "Overlay" = overlayplot(res, reactive_inputs$subtitle),
                         "THF" = smoothedTHFplot(res, reactive_inputs$subtitle),
                         "RHF" = smoothedRHFplot(res, reactive_inputs$subtitle),
                         "RHF no FT" = RHFnoFT(res2),
                         "NRHF" = smoothedNRHFplot(res, reactive_inputs$subtitle),
                         "Signal closeup" = tempsignaloverlay(reactive_inputs$df2, reactive_inputs$subtitle))
      
      ggplotly(plot_obj, tooltip = c("x", "y", "text"))
    })
  })
}
