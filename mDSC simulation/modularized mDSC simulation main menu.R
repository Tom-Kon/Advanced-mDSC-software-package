# mdsc_module.R
source("../mDSC simulation/libraries.R")
source("../mDSC simulation/configapp.R")

mdsc_sim_ui <- function(id) {
  ns <- NS(id)
  navbarPage(
    id = ns("tabs"),
    title = "mDSC deconvolution procedure simulator",
    inverse = FALSE,
    
    tabPanel(
      title = "Parameter Input",
      id = ns("paraminput"),
      icon = icon("gears", class = "fa-solid"),
      fluidPage(configUI1(ns))
    ),
    
    tabPanel(
      title = "Experimental results Input",
      id = ns("expresinput"),
      icon = icon("gears", class = "fa-solid"),
      fluidPage(
        fluidRow(configUI2(ns), configUI3(ns), configUI4(ns)),configUI5(ns)
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
                        choices = c("MHF", "Overlay", "THF", "RHF", "NRHF", "Signal closeup"),
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
    # Create a reactiveValues object to store inputs
    reactive_inputs <- reactiveValues()
    
    observeEvent(input$calculate, {
      reactive_inputs$savetitle <- input$savetitle
      reactive_inputs$saveplots <- input$saveplots
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
      reactive_inputs$locationTgTHF <- as.numeric(eval(parse(text = unlist(trimws(strsplit(input$locationTgTHF, ","))))))
      reactive_inputs$locationTgRHF <- as.numeric(eval(parse(text = unlist(trimws(strsplit(input$locationTgRHF, ","))))))
      reactive_inputs$deltaCpTg <- eval(parse(text = input$deltaCpTg))
      reactive_inputs$MeltEnth <- eval(parse(text = input$MeltEnth))
      reactive_inputs$phase_melt <- eval(parse(text = input$phase_melt))
      reactive_inputs$locationMelt <- as.numeric(eval(parse(text = unlist(trimws(strsplit(input$locationMelt, ","))))))
      reactive_inputs$periodSignal <- eval(parse(text = input$periodSignal))
      reactive_inputs$Crystalenth <- eval(parse(text = input$Crystalenth))
      reactive_inputs$locationcrystal <- as.numeric(eval(parse(text = unlist(trimws(strsplit(input$locationcrystal, ","))))))
      reactive_inputs$EnthrecEnth <- eval(parse(text = input$EnthrecEnth))
      reactive_inputs$locationEnthRec <- as.numeric(eval(parse(text = unlist(trimws(strsplit(input$locationEnthRec, ","))))))
      
      source("../mDSC simulation/Time point generation.R")
      source("../mDSC simulation/Signal generation.R")
      source("../mDSC simulation/Equally-spaced y-values.R")
      source("../mDSC simulation/Final calculations.R")
      source("../mDSC simulation/Plot generation and control.R")
      
      
      reactive_inputs$subtitle <- paste0(
        "Sampling: ", reactive_inputs$sampling, " pts/sec. Period: ", reactive_inputs$period, " sec. ", 
        "Melting period: ", reactive_inputs$periodSignal, " sec. Heating rate: ", reactive_inputs$heatRate * 60, " °C/min. ",
        "MHF phase: ", reactive_inputs$phase, " rad. LOESS alpha: ", reactive_inputs$loessAlpha, ".\n Temp. amplitude: ", reactive_inputs$Atemp, " °C.",
        "Melting amplitude: ", reactive_inputs$MeltEnth, " W/g. Other parameters (such as start temp.) are visible on the plot.")
      
      
      
      df1 <- timegeneration(
        sampling = reactive_inputs$sampling,
        startTemp = reactive_inputs$startTemp,
        endTemp = reactive_inputs$endTemp,
        period = reactive_inputs$period,
        heatRate = reactive_inputs$heatRate)
      
      df2 <- signalgeneration(    
        sampling = reactive_inputs$sampling,
        startTemp = reactive_inputs$startTemp,
        endTemp = reactive_inputs$endTemp,
        period = reactive_inputs$period,
        heatRate = reactive_inputs$heatRate,
        Atemp = reactive_inputs$Atemp,
        phase = reactive_inputs$phase,
        deltaRHFPreTg = reactive_inputs$deltaRHFPreTg,
        deltaRHFPostTg = reactive_inputs$deltaRHFPostTg,
        StartRHFPreTg = reactive_inputs$StartRHFPreTg,
        deltaCpPreTg = reactive_inputs$deltaCpPreTg,
        deltaCpPostTg = reactive_inputs$deltaCpPostTg,
        StartCpTempPreTg = reactive_inputs$StartCpTempPreTg,
        locationTgTHF = reactive_inputs$locationTgTHF,
        locationTgRHF = reactive_inputs$locationTgRHF,
        deltaCpTg = reactive_inputs$deltaCpTg,
        MeltEnth = reactive_inputs$MeltEnth,
        phase_melt = reactive_inputs$phase_melt,
        locationMelt = reactive_inputs$locationMelt,
        Crystalenth = reactive_inputs$Crystalenth,
        locationcrystal = reactive_inputs$locationcrystal,
        EnthrecEnth = reactive_inputs$EnthrecEnth,
        locationEnthRec = reactive_inputs$locationEnthRec,
        periodSignal = reactive_inputs$periodSignal,
        df1 = df1)
      
      reactive_inputs$df2 <- df2
      resampled_points <- equalyval(df2 = df2)
      
      finaldf <- finalcalc(
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
      
      reactive_inputs$finaldf <- finaldf
      
      write.xlsx(df2, "test.xlsx")
      
      if (reactive_inputs$saveplots == TRUE) {    
        MHFplots(reactive_inputs$finaldf, reactive_inputs$subtitle, reactive_inputs$savetitle)
        overlayplot(reactive_inputs$finaldf, reactive_inputs$subtitle, reactive_inputs$savetitle)
        smoothedTHFplot(reactive_inputs$finaldf, reactive_inputs$subtitle, reactive_inputs$savetitle)
        smoothedRHFplot(reactive_inputs$finaldf, reactive_inputs$subtitle, reactive_inputs$savetitle)
        smoothedNRHFplot(reactive_inputs$finaldf, reactive_inputs$subtitle, reactive_inputs$savetitle)
      }
    })
    
    # Render the plot using the reactive sample_results
    output$plot <- renderPlotly({
      req(reactive_inputs$finaldf)  # Ensure results exist
      res <- reactive_inputs$finaldf
      
      plot_obj <- switch(input$plot_choice,
                         "MHF" = MHFplots(res, reactive_inputs$subtitle, reactive_inputs$savetitle),
                         "Overlay" = overlayplot(res, reactive_inputs$subtitle, reactive_inputs$savetitle),
                         "THF" = smoothedTHFplot(res, reactive_inputs$subtitle, reactive_inputs$savetitle),
                         "RHF" = smoothedRHFplot(res, reactive_inputs$subtitle, reactive_inputs$savetitle),
                         "NRHF" = smoothedNRHFplot(res, reactive_inputs$subtitle, reactive_inputs$savetitle),
                         "Signal closeup" = tempsignaloverlay(reactive_inputs$df2, reactive_inputs$subtitle, reactive_inputs$savetitle))
      
      ggplotly(plot_obj, tooltip = c("x", "y", "text"))
    })
  })
}
