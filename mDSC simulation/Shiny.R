source("libraries.R")
source("configapp.R")

ui <- navbarPage(
  id = "tabs",
  title = "mDSC deconvolution procedure simulator",
  inverse = FALSE,  # if you want a dark navbar style; remove if not needed
  tabPanel(
    title = "Parameter Input",
    id = "paraminput",
    icon = icon("gears", class = "fa-solid"),
    fluidPage(
      configUI1()
    )
  ),
  tabPanel(
    title = "Experimental results Input",
    id = "expresinput",
    icon = icon("gears", class = "fa-solid"),
    fluidPage(
      fluidRow(configUI2(), configUI3(), configUI4()),
      fluidRow(configUI5()),
    )
  ),
  tabPanel(
    id = "graphs",
    title = "Graphs",
    icon = icon("chart-line", class = "fa-solid"),
    fluidPage(
      titlePanel("Output graphs"),
      fluidRow(
        column(12, wellPanel(
          selectInput("plot_choice", "Select Plot:", 
                      choices = c("MHF", "Overlay", "THF", "RHF","NRHF"), 
                      selected = "MHF"),
        ))
      ),
      fluidRow(
        column(12, plotlyOutput("plot", height = "90vh"))
      )
    )
  )
)


server <- function(input, output, session) {
  
  # Create a reactiveValues object to store inputs
  reactive_inputs <- reactiveValues()
  
  # This part is separate and independent of your server logic
  observe({
    # Handle all inputs
    reactive_inputs$savetitle <- input$savetitle
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
    reactive_inputs$deltaHFPreTg <- eval(parse(text = input$deltaHFPreTg))
    reactive_inputs$deltaHFPostTg <- eval(parse(text = input$deltaHFPostTg))
    reactive_inputs$StartHFTempPreTg <- eval(parse(text = input$StartHFTempPreTg))
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
  })
  
  observeEvent(input$calculate, {
    source("Time point generation.R")
    source("Signal generation.R")
    source("Equally-spaced y-values.R")
    source("Equally-spaced y-values.R")
    source("Final calculations.R")
    source("Plot generation and control.R")
    
    
    df1 <<- timegeneration(
      sampling = reactive_inputs$sampling,
      startTemp = reactive_inputs$startTemp,
      endTemp = reactive_inputs$endTemp,
      period = reactive_inputs$period,
      heatRate = reactive_inputs$heatRate)
    
    df2 <<- signalgeneration(    
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
      deltaHFPreTg = reactive_inputs$deltaHFPreTg,
      deltaHFPostTg = reactive_inputs$deltaHFPostTg,
      StartHFTempPreTg = reactive_inputs$StartHFTempPreTg,
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
    
    resampled_points <- equalyval(df2 = df2)
    
    finaldf <<- finalcalc(
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
    
  })
  
  # Render the plot using the reactive sample_results
  output$plot <- renderPlotly({
    req(reactive_inputs$finaldf)  # Ensure results exist
    res <- reactive_inputs$finaldf

    plot_obj <- switch(input$plot_choice,
                       "MHF" = MHFplots(res),
                       "Overlay" = overlayplot(res),
                       "THF" = smoothedTHFplot(res),
                       "RHF" = smoothedRHFplot(res),
                       "NRHF" = smoothedNRHFplot(res))
    
    ggplotly(plot_obj, tooltip = c("x", "y", "text"))
  })
}

shinyApp(ui = ui, server = server)

