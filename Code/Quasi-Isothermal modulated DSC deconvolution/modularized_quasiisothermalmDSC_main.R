source("../Quasi-Isothermal modulated DSC deconvolution/libraries.R")
source("../Quasi-Isothermal modulated DSC deconvolution/configapp.R")
source("../Quasi-Isothermal modulated DSC deconvolution/error_handling.R")
source("../Quasi-Isothermal modulated DSC deconvolution/Processing and cleaning overall function.R")
source("../Quasi-Isothermal modulated DSC deconvolution/plots.R")
source("../Quasi-Isothermal modulated DSC deconvolution/quickly recalculate for different mods.R")
source("../Quasi-Isothermal modulated DSC deconvolution/download.R")



quasiIsotherm_ui <- function(id) {
  ns <- NS(id)
  ui <- navbarPage(
    id = ns("tabs"),
    title = "Quasi-Isothermal modulated DSC deconvolution",
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
    ), 
    tabPanel(
      id = ns("downloads"),
      title = "Downloads",
      icon = icon("download", class = "fa-solid"),
      fluidPage(configUI4(ns))
    ), 
    tabPanel(
      id = ns("tutorial"),
      title = "Tutorial",
      icon = icon("book", class = "fa-solid"),
      fluidPage()
    ), 
    
    
  )
}


mdsc_quasiIso_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Create a reactiveValues object to store inputs
    reactive_inputs <- reactiveValues()
    
    # This part is separate and independent of your server logic
    observeEvent(input$calculate, {
      showPageSpinner()
      
      
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
      reactive_inputs$sampling <- eval(parse(text = input$sampling))
      reactive_inputs$recalc <- 1
      
    
    # Update reactive values when calculate button is pressed
    
      
      # Store file input
      reactive_inputs$Excel <- input$Excel_in$datapath
      reactive_inputs$fileName <- as.character(input$Excel_in$name)
      
      # Call error_handling with the actual input value
      msg <- error_handling_quasiIso(reactive_inputs)

      # Update the error message output (this triggers UI update)
      output$errorMessage <- renderText({
        if (!is.null(msg)) msg else ""
      })
      
      if (!is.null(msg)) hidePageSpinner()
      
      req(is.null(msg))  # Exit here if there's an error
      
      
      # Call the processing function and store results in reactive value
      sample_results <- processDSC(
        fileName = reactive_inputs$fileName,
        sheet = reactive_inputs$sheet,
        Excel = reactive_inputs$Excel,
        starting_temp = reactive_inputs$startingTemp,
        step_size = reactive_inputs$stepSize,
        modulations_back = reactive_inputs$modulations_back,
        period = reactive_inputs$period,
        isothermLength = reactive_inputs$isothermLength,
        setAmplitude = reactive_inputs$setAmplitude,
        sampling = reactive_inputs$sampling,
        saveNRHFplot = reactive_inputs$saveNRHFplot,
        saveRevCpplot = reactive_inputs$saveRevCpplot,
        savemanualRevCpplot = reactive_inputs$savemanualRevCpplot,
        saveExcel = reactive_inputs$saveExcel
      )
      
      if(typeof(sample_results) == "character") {
        msg <- sample_results
        # Update the error message output (this triggers UI update)
        output$errorMessage <- renderText({
          if (!is.null(msg)) msg else ""
        })
        
        if (!is.null(msg)) hidePageSpinner()
        
        req(is.null(msg))  # Exit here if there's an error
        
      }
      
      if(!is.null(attr(sample_results, "comment"))) {
        output$errorMessage <- renderText({
          attr(sample_results, "comment")
        })        
      }
      
      # Store the processed results in reactiveValues()
      reactive_inputs$sample_results <- sample_results
      hidePageSpinner()
      
    })
    
    # Handle recalculation with a different number of modulations
    observeEvent(input$recalc, {
      showPageSpinner()
      msg <- NULL
      
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
      
      hidePageSpinner()
      
    })
    

    
output$excelDownload <- downloadHandler(
  filename = function() {
    fileName <- unlist(strsplit(reactive_inputs$fileName, "\\."))[1]
    paste0(fileName, " ", reactive_inputs$modulations_back, " modulations analysed.xlsx")
  },
  
  content = function(file) {
    showPageSpinner()
    
    wb <- downloadExcel(
      sample_results = reactive_inputs$sample_results,
      fileName = reactive_inputs$fileName,
      modulations_back = reactive_inputs$modulations_back,
      period = reactive_inputs$period,
      setAmplitude = reactive_inputs$setAmplitude,
      starting_temp = reactive_inputs$startingTemp,
      step_size = reactive_inputs$stepSize,
      isothermLength = reactive_inputs$isothermLength,
      sampling = reactive_inputs$sampling
    )   
    
    saveWorkbook(wb, file = file, overwrite = TRUE)
    
    hidePageSpinner()
    
  }
)
    
    output$NRHFdownload <- downloadHandler(
      filename = function() {
        subtitle <- unlist(strsplit(reactive_inputs$fileName, "[.]"))[1]
        plotTitleNRHF <- paste0("NRHF based on FT (frequency = 0), ", reactive_inputs$modulations_back, " modulations")
        paste0(subtitle, " ", plotTitleNRHF, input$extension)
      },
      content = function(file) {
        source("../Quasi-Isothermal modulated DSC deconvolution/plots.R")
        res <- reactive_inputs$sample_results
        
        NRHF_plot(
          res$ft_averages,
          reactive_inputs$modulations_back,
          reactive_inputs$fileName,
          FALSE
        )
        
        ggsave(
          filename = file,
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
      }
    )
    
    output$RevCpdownload <- downloadHandler(
      filename = function() {
        subtitle <- unlist(strsplit(reactive_inputs$fileName, "[.]"))[1]
        plotTitleRevCp <- paste0("RevCp based on FT (1st harmonic), ", reactive_inputs$modulations_back, " modulations")
        paste0(subtitle, " ", plotTitleRevCp, input$extension)
      },
      content = function(file) {
        source("../Quasi-Isothermal modulated DSC deconvolution/plots.R")
        res <- reactive_inputs$sample_results
        
        RevCp_plot(
          res$ft_averages,
          reactive_inputs$modulations_back,
          reactive_inputs$fileName,
          FALSE
        )
        
        ggsave(
          filename = file,
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
      }
    )
    
    output$nonFTrevCpdownload <- downloadHandler(
      filename = function() {
        subtitle <- unlist(strsplit(reactive_inputs$fileName, "[.]"))[1]
        plottitleRevCpmanual <- paste0("RevCp calculated manually, ", reactive_inputs$modulations_back, " modulations")
        paste0(subtitle, " ", plottitleRevCpmanual, input$extension)
      },
      content = function(file) {
        source("../Quasi-Isothermal modulated DSC deconvolution/plots.R")
        res <- reactive_inputs$sample_results
        
        Manual_RevCp_plot(
          res$ft_averages,
          reactive_inputs$modulations_back,
          reactive_inputs$fileName,
          FALSE
        )
        
        ggsave(
          filename = file,
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
      }
    )
    
    output$allPlotsDownload <- downloadHandler(
      filename = function() {
        subtitle <- unlist(strsplit(reactive_inputs$fileName, "[.]"))[1]
        Title <- "Plots based on FT analysis"
        paste0(subtitle, " ", Title, ".zip")  # Must be .zip
      },
      content = function(file) {
        showPageSpinner()
        res <- reactive_inputs$sample_results
        
        tmpdir <- tempdir()
        
        # Output files for the plots
        plot1_file <- file.path(tmpdir, paste0("NRHF plot", input$extension))
        plot2_file <- file.path(tmpdir, paste0("RevCp plot.png", input$extension))
        plot3_file <- file.path(tmpdir, paste0("Manual RevCp plot.png", input$extension))
        # plot4_file <- file.path(tmpdir, "Overlay plot.png")
        plot5_file <- file.path(tmpdir, paste0("Maxima and minima 1.png", input$extension))
        plot6_file <- file.path(tmpdir, paste0("Maxima and minima prefinal.png", input$extension))
        plot7_file <- file.path(tmpdir, paste0("Maxima and minima final.png", input$extension))
        plot8_file <- file.path(tmpdir, paste0("Original data.png", input$extension))
        plot9_file <- file.path(tmpdir, paste0("First cleaned up data.png", input$extension))
        plot10_file <- file.path(tmpdir, paste0("Prefinal cleaned up data.png", input$extension))
        plot11_file <- file.path(tmpdir, paste0("Final data used for analysis.png", input$extension))
        
        
        
        
        # Save each plot to its file
        ggsave(
          filename = plot1_file,
          plot = NRHF_plot(res$ft_averages, reactive_inputs$modulations_back, reactive_inputs$fileName, reactive_inputs$saveNRHFplot),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        ggsave(
          filename = plot2_file,
          plot = RevCp_plot(res$ft_averages, reactive_inputs$modulations_back, reactive_inputs$fileName, reactive_inputs$saveRevCpplot),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        ggsave(
          filename = plot3_file,
          plot = Manual_RevCp_plot(res$average_heat_flow_per_pattern, reactive_inputs$modulations_back, reactive_inputs$fileName, reactive_inputs$savemanualRevCpplot),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        # ggsave(
        #   filename = plot4_file,
        #   plot = RevCp_NRHF_plotggplot(res$ft_averages, reactive_inputs$modulations_back, reactive_inputs$fileName),
        #   dpi = as.numeric(input$exportDpi),
        #   width = as.numeric(input$exportWidth),
        #   height = as.numeric(input$exportHeight),
        #   units = "cm"
        # )
        
        ggsave(
          filename = plot5_file,
          plot = Maxima_minima(res$`Extrema df1`, reactive_inputs$modulations_back, reactive_inputs$fileName),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        ggsave(
          filename = plot6_file,
          plot = Maxima_minima_1(res$`Extrema df2`, reactive_inputs$modulations_back, reactive_inputs$fileName),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        ggsave(
          filename = plot7_file,
          plot = Maxima_minima_2(res$`Extrema df3`, reactive_inputs$modulations_back, reactive_inputs$fileName),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        ggsave(
          filename = plot8_file,
          plot = Original_dataggplot(res$`Original data`, reactive_inputs$modulations_back, reactive_inputs$fileName),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        ggsave(
          filename = plot9_file,
          plot = Datasteps_plot_1ggplot(res$d_steps_cleaned, reactive_inputs$modulations_back, reactive_inputs$fileName),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        ggsave(
          filename = plot10_file,
          plot = Datasteps_plot_prefinalggplot(res$d_steps_cleaned_2, reactive_inputs$modulations_back, reactive_inputs$fileName),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        ggsave(
          filename = plot11_file,
          plot = Datasteps_plot_finalggplot(res$d_steps_cleaned_3, reactive_inputs$modulations_back, reactive_inputs$fileName),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        # Bundle into a zip file
        zip(zipfile = file, files = c(plot1_file, plot2_file, plot3_file ,plot5_file, plot6_file, plot7_file, plot8_file, plot9_file, plot10_file, plot11_file), flags = "-j")
        
        hidePageSpinner()
        
      }
    )
    
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
