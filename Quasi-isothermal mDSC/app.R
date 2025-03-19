source("libraries.R")
source("configapp.R") 



ui <- navbarPage(
  id = "tabs",
  title = "Quasi-isothermal mDSC data analyzer",
  # theme = bs_theme(preset = "lumen"),
  inverse = FALSE,  # if you want a dark navbar style; remove if not needed
  tabPanel(
    title = "Input",
    id = "input",
    icon = icon("gears", class = "fa-solid"),
    fluidPage(
      configUI1(), configUI2(), configUI3()
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
                      choices = c("NRHF", "RevCp", "Manual RevCp", "RevCp and NRHF","Maxima and minima 1", "Maxima and minima prefinal", "Maxima and minima final", 
                                  "Original data", "First cleaned up data", "Prefinal cleaned up data", "Final data used for analysis"), 
                      selected = "RevCp"),
          fluidRow(  # Use a fluidRow to make sure buttons are side by side
            column(6,
                   HTML("<br>"),
                   actionButton("recalc", "Recalculate with different number of modulations")
                   ),
            column(6, textInput("modulations_back_in_new", "New number of modulations"))
          )
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
    # Handle checkboxes
    reactive_inputs$saveNRHFplot <- as.logical(input$saveNRHFplot)
    reactive_inputs$saveRevCpplot <- as.logical(input$saveRevCpplot)
    reactive_inputs$savemanualRevCpplot <- as.logical(input$savemanualRevCpplot)
    reactive_inputs$saveDatasteps3 <- as.logical(input$saveDatasteps3)
    reactive_inputs$saveExtremadf3 <- as.logical(input$saveExtremadf3)
    reactive_inputs$saveSummaryFT <- as.logical(input$saveSummaryFT)
    
    # Store numerical/text inputs with proper reactive evaluation
    reactive_inputs$period <- eval(parse(text = input$period_in))  # Assuming input is numeric
    reactive_inputs$stepSize <- eval(parse(text = input$step_size_in))
    reactive_inputs$isothermLength <- eval(parse(text = input$isotherm_length_in))
    reactive_inputs$startingTemp <- eval(parse(text = input$starting_temp_in))
    reactive_inputs$modulations_back <- eval(parse(text = input$modulations_back_in))
    reactive_inputs$setAmplitude <- eval(parse(text = input$setAmplitude_in))

  })
  
  
  # Update reactive values when calculate button is pressed
  observeEvent(input$calculate, {

    # Store file input
    reactive_inputs$Excel <- input$Excel_in$datapath
    reactive_inputs$fileName <- input$Excel_in$name
    
    # Source necessary scripts
    source("Processing and cleaning overall function.R")
    source("plots.R")

    # Call the processing function and store results in reactive value
    sample_results <- processDSC(
      file = reactive_inputs$fileName,
      Excel = reactive_inputs$Excel,
      export = TRUE,
      rangesmin = rangesmin,
      rangesmax = rangesmax,
      starting_temp = reactive_inputs$startingTemp,
      step_size = reactive_inputs$stepSize,
      modulations_back = reactive_inputs$modulations_back,
      period = reactive_inputs$period,
      setAmplitude = reactive_inputs$setAmplitude,
      saveNRHFplot = reactive_inputs$saveNRHFplot,
      saveRevCpplot = reactive_inputs$saveRevCpplot,
      savemanualRevCpplot = reactive_inputs$savemanualRevCpplot,
      saveDatasteps3 = reactive_inputs$saveDatasteps3,
      saveExtremadf3 = reactive_inputs$saveExtremadf3,
      saveSummaryFT = reactive_inputs$saveSummaryFT
    )
    
    # Store the processed results in reactiveValues()
    reactive_inputs$sample_results <- sample_results
  })
  
  # Handle recalculation with a different number of modulations
  observeEvent(input$recalc, {
    source("quickly recalculate for different mods.R")
    
    reactive_inputs$modulations_back <- eval(parse(text = input$modulations_back_in_new))
    
    reactive_inputs$sample_results <- processDSCrecalc(
      sample_results = reactive_inputs$sample_results,
      modulations_back = reactive_inputs$modulations_back,
      period = reactive_inputs$period,
      setAmplitude = reactive_inputs$setAmplitude
    )
  })
  
  # Render the plot using the reactive sample_results
  output$plot <- renderPlotly({
    req(reactive_inputs$sample_results)  # Ensure results exist
    res <- reactive_inputs$sample_results
    
    plot_obj <- switch(input$plot_choice,
                       "NRHF" = NRHF_plot(res$ft_averages, reactive_inputs$modulations_back, reactive_inputs$fileName, reactive_inputs$saveNRHFplot),
                       "RevCp" = RevCp_plot(res$ft_averages, reactive_inputs$modulations_back, reactive_inputs$fileName, reactive_inputs$saveRevCpplot),
                       "Manual RevCp" = Manual_RevCp_plot(res$average_heat_flow_per_pattern, reactive_inputs$modulations_back, reactive_inputs$fileName, reactive_inputs$savemanualRevCpplot),
                       "RevCp and NRHF" = RevCp_NRHF_plot(res$ft_averages, reactive_inputs$modulations_back, reactive_inputs$fileName),
                       "Maxima and minima 1" = Maxima_minima(res$`Extrema df1`, reactive_inputs$modulations_back, reactive_inputs$fileName),
                       "Maxima and minima prefinal" = Maxima_minima_1(res$`Extrema df2`, reactive_inputs$modulations_back, reactive_inputs$fileName),
                       "Maxima and minima final" =  Maxima_minima_2(res$`Extrema df3`, reactive_inputs$modulations_back, reactive_inputs$fileName, reactive_inputs$saveExtremadf3),
                       "Original data" = Original_data(res$`Original data`, reactive_inputs$modulations_back, reactive_inputs$fileName),
                       "First cleaned up data" = Datasteps_plot_1(res$d_steps_cleaned, reactive_inputs$modulations_back, reactive_inputs$fileName),
                       "Prefinal cleaned up data" = Datasteps_plot_prefinal(res$d_steps_cleaned_2, reactive_inputs$modulations_back, reactive_inputs$fileName),
                       "Final data used for analysis" = Datasteps_plot_final(res$d_steps_cleaned_3, reactive_inputs$modulations_back, reactive_inputs$fileName, reactive_inputs$saveDatasteps3))
    
    ggplotly(plot_obj, tooltip = c("x", "y", "text"))
  })
}


shinyApp(ui = ui, server = server)

