source("../Quasi-isothermal mDSC/libraries.R")
source("../Quasi-isothermal mDSC/configapp.R")
source("../Quasi-isothermal mDSC/error handling.R")


quasiIsotherm_ui <- function(id) {
  ns <- NS(id)
  ui <- navbarPage(
    id = ns("tabs"),
    title = "Quasi-isothermal mDSC data analyzer",
    tags$style(HTML("
    .error-text {
      font-size: 18px;
      font-weight: bold;
      padding: 10px;
      width: 150%;
      color: #e04f30;
    }
    ")
    ),

    
    # theme = bs_theme(preset = "lumen"),
    inverse = FALSE,  # if you want a dark navbar style; remove if not needed
    tabPanel(
      title = "Input",
      id = ns("input"),
      icon = icon("gears", class = "fa-solid"),
      fluidPage(
        configUI1(ns), configUI2(ns), configUI3(ns)
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
                        choices = c("NRHF", "RevCp", "Manual RevCp", "RevCp and NRHF", 
                                    "Maxima and minima 1", "Maxima and minima prefinal", "Maxima and minima final", 
                                    "Original data", "First cleaned up data", "Prefinal cleaned up data", "Final data used for analysis"), 
                        selected = "RevCp"),
            fluidRow(
              column(6,
                     HTML("<br>"),
                     actionButton(ns("recalc"), "Recalculate with different number of modulations")
              ),
              column(6, 
                     textInput(ns("modulations_back_in_new"), "New number of modulations")
              )
            )
          ))
        ),
        fluidRow(
          column(12, plotlyOutput(ns("plot"), height = "90vh"))
        )
      )
    )
    
    
  )
}


mdsc_quasiIso_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Create a reactiveValues object to store inputs
    reactive_inputs <- reactiveValues()
    
    # This part is separate and independent of your server logic
    observeEvent(input$calculate, {
      
      # Handle checkboxes
      reactive_inputs$saveNRHFplot <- as.logical(input$saveNRHFplot)
      reactive_inputs$saveRevCpplot <- as.logical(input$saveRevCpplot)
      reactive_inputs$savemanualRevCpplot <- as.logical(input$savemanualRevCpplot)
      reactive_inputs$saveExcel <- as.logical(input$saveExcel)
      if(input$sheetask) {
        reactive_inputs$sheet <- 1
      } else {
        reactive_inputs$sheet <- input$sheet
      }
      
      # Store numerical/text inputs with proper reactive evaluation
      reactive_inputs$period <- eval(parse(text = input$period_in))  # Assuming input is numeric
      reactive_inputs$stepSize <- eval(parse(text = input$step_size_in))
      reactive_inputs$isothermLength <- eval(parse(text = input$isotherm_length_in))
      reactive_inputs$startingTemp <- eval(parse(text = input$starting_temp_in))
      reactive_inputs$modulations_back <- eval(parse(text = input$modulations_back_in))
      reactive_inputs$setAmplitude <- eval(parse(text = input$setAmplitude_in))
      
    
    # Update reactive values when calculate button is pressed
    
      
      # Store file input
      reactive_inputs$Excel <- input$Excel_in$datapath
      reactive_inputs$fileName <- input$Excel_in$name
      
      # Call error_handling with the actual input value
      msg <- error_handling(reactive_inputs)
      
      # Update the error message output (this triggers UI update)
      output$errorMessage <- renderText({
        if (!is.null(msg)) msg else ""
      })
      
      # If there is an error message, stop further processing
      if (!is.null(msg)) {
        return(NULL)
      }
      
      req(is.null(msg))  # Exit here if there's an error
      
      # Source necessary scripts
      source("../Quasi-isothermal mDSC/Processing and cleaning overall function.R")
      source("../Quasi-isothermal mDSC/plots.R")
      
      # Call the processing function and store results in reactive value
      sample_results <- processDSC(
        fileName = reactive_inputs$fileName,
        sheet = reactive_inputs$sheet,
        Excel = reactive_inputs$Excel,
        export = TRUE,
        starting_temp = reactive_inputs$startingTemp,
        step_size = reactive_inputs$stepSize,
        modulations_back = reactive_inputs$modulations_back,
        period = reactive_inputs$period,
        isothermLength = reactive_inputs$isothermLength,
        setAmplitude = reactive_inputs$setAmplitude,
        saveNRHFplot = reactive_inputs$saveNRHFplot,
        saveRevCpplot = reactive_inputs$saveRevCpplot,
        savemanualRevCpplot = reactive_inputs$savemanualRevCpplot,
        saveExcel = reactive_inputs$saveExcel
      )
      
      # Store the processed results in reactiveValues()
      reactive_inputs$sample_results <- sample_results
    })
    
    # Handle recalculation with a different number of modulations
    observeEvent(input$recalc, {
      source("../Quasi-isothermal mDSC/quickly recalculate for different mods.R")
      
      reactive_inputs$modulations_back <- eval(parse(text = input$modulations_back_in_new))
      
      if (is.null(reactive_inputs$modulations_back)){
        msg <- "Error: the new value for the number of modulations is missing"
      }
      
      # Update the error message output (this triggers UI update)
      output$errorMessage <- renderText({
        if (!is.null(msg)) msg else ""
      })
      
      # If there is an error message, stop further processing
      if (!is.null(msg)) {
        return(NULL)
      }
      
      req(is.null(msg))  # Exit here if there's an error
      
      reactive_inputs$sample_results <- processDSCrecalc(
        fileName = reactive_inputs$fileName,
        sample_results = reactive_inputs$sample_results,
        modulations_back = reactive_inputs$modulations_back,
        period = reactive_inputs$period,
        setAmplitude = reactive_inputs$setAmplitude,
        starting_temp = reactive_inputs$startingTemp,
        step_size = reactive_inputs$stepSize,
        saveExcel = reactive_inputs$saveExcel
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
                         "Maxima and minima final" =  Maxima_minima_2(res$`Extrema df3`, reactive_inputs$modulations_back, reactive_inputs$fileName),
                         "Original data" = Original_data(res$`Original data`, reactive_inputs$modulations_back, reactive_inputs$fileName),
                         "First cleaned up data" = Datasteps_plot_1(res$d_steps_cleaned, reactive_inputs$modulations_back, reactive_inputs$fileName),
                         "Prefinal cleaned up data" = Datasteps_plot_prefinal(res$d_steps_cleaned_2, reactive_inputs$modulations_back, reactive_inputs$fileName),
                         "Final data used for analysis" = Datasteps_plot_final(res$d_steps_cleaned_3, reactive_inputs$modulations_back, reactive_inputs$fileName))
      
      ggplotly(plot_obj, tooltip = c("x", "y", "text"))
    })
  })
}
