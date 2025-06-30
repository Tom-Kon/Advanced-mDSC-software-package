source("quasi-isothermal_mdsc_deconvolution/quasi-isothermal_mdsc_deconvolution_libraries.R")
source("quasi-isothermal_mdsc_deconvolution/quasi-isothermal_mdsc_deconvolution_ui.R")
source("quasi-isothermal_mdsc_deconvolution/quasi-isothermal_mdsc_deconvolution_error_handling.R")
source("quasi-isothermal_mdsc_deconvolution/quasi-isothermal_mdsc_deconvolution_functions.R")
source("quasi-isothermal_mdsc_deconvolution/quasi-isothermal_mdsc_deconvolution_plots.R")
source("quasi-isothermal_mdsc_deconvolution/quasi-isothermal_mdsc_deconvolution_quickly_recalculate_function.R")
source("quasi-isothermal_mdsc_deconvolution/quasi-isothermal_mdsc_deconvolution_downloads.R")



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
        configUI1(ns)
      )
    ),
    tabPanel(
      id = ns("graphs"),
      title = "Graphs",
      icon = icon("chart-line", class = "fa-solid"),
      fluidPage(
        configUI2(ns)
      )
    ), 
    tabPanel(
      id = ns("downloads"),
      title = "Downloads",
      icon = icon("download", class = "fa-solid"),
      fluidPage(configUI3(ns))
    ), 
    tabPanel(
      title = "Tutorial",
      icon = icon("book", class = "fa-solid"),
      fluidPage(
        withMathJax(
          includeMarkdown("quasi-isothermal_mdsc_deconvolution/tutorial/quasi-isothermal_mdsc_deconvolution_tutorial.md")
        )
      )
    )
    
    
  )
}


mdsc_quasiIso_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    disable("excelDownload")
    disable("NRHFdownload")
    disable("RevCpdownload")
    disable("nonFTrevCpdownload")
    disable("allPlotsDownload")
    
    # Create a reactiveValues object to store inputs
    reactiveInputs <- reactiveValues()
    
    # This part is separate and independent of your server logic
    observeEvent(input$analyze, {
      
      showPageSpinner()
      output$errorMessage <- NULL
      output$succesMessage <- NULL
      
      # Handle checkboxes
      if(input$sheetask) {
        reactiveInputs$sheet <- 1
      } else {
        reactiveInputs$sheet <- input$sheet
      }
      
      # Store numerical/text inputs with proper reactive evaluation
      reactiveInputs$period <- as.numeric(input$period) 
      reactiveInputs$stepSize <- as.numeric(input$stepSize)
      reactiveInputs$isothermLength <- as.numeric(input$isothermLength)
      reactiveInputs$startingTemp <- as.numeric(input$startingTemp)
      reactiveInputs$modulationsBack <- as.numeric(input$modulationsBack)
      reactiveInputs$setAmplitude <- as.numeric(input$setAmplitude)
      reactiveInputs$sampling <- as.numeric(input$sampling)
      reactiveInputs$recalc <- 1
      
    
      # Update reactive values when calculate button is pressed
      # Store file input
      reactiveInputs$Excel <- input$Excel$datapath
      reactiveInputs$fileName <- as.character(input$Excel$name)
      
      # Call error_handling with the actual input value
      msg <- error_handling_quasiIso(reactiveInputs)

      # Update the error message output (this triggers UI update)
      output$errorMessage <- renderText({
        if (!is.null(msg)) msg else ""
      })
      
      if (!is.null(msg)) hidePageSpinner()
      
      req(is.null(msg))  # Exit here if there's an error
      
      # Call the processing function and store results in reactive value
      results <- processDSC(
        fileName = reactiveInputs$fileName,
        sheet = reactiveInputs$sheet,
        Excel = reactiveInputs$Excel,
        startingTemp = reactiveInputs$startingTemp,
        stepSize = reactiveInputs$stepSize,
        modulationsBack = reactiveInputs$modulationsBack,
        period = reactiveInputs$period,
        isothermLength = reactiveInputs$isothermLength,
        setAmplitude = reactiveInputs$setAmplitude,
        sampling = reactiveInputs$sampling
      )
      
      if(typeof(results) == "character") {
        msg <- results
        # Update the error message output (this triggers UI update)
        output$errorMessage <- renderText({
          if (!is.null(msg)) msg else ""
        })
        
        if (!is.null(msg)) hidePageSpinner()
        
        req(is.null(msg))  # Exit here if there's an error
      }
      
      if(!is.null(attr(results, "comment"))) {
        output$errorMessage <- renderText({
          attr(results, "comment")
        })        
      }
      
      # Store the processed results in reactiveValues()
      reactiveInputs$results <- results
      
      enable("excelDownload")
      enable("NRHFdownload")
      enable("RevCpdownload")
      enable("nonFTrevCpdownload")
      enable("allPlotsDownload")
      
      output$succesMessage <- renderText({
        "Analysis succesful! You can now head over to the \"Graphs\" or \"Downloads\" tab."
      })  
      
      hidePageSpinner()
      
    })
    
    # Handle recalculation with a different number of modulations
    observeEvent(input$recalc, {
      showPageSpinner()
      msg <- NULL
      
      reactiveInputs$modulationsBack <- input$modulations_back_new
      
      if (is.null(reactiveInputs$modulationsBack)) {
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
      
      reactiveInputs$results <- processDSCrecalc(
        fileName = reactiveInputs$fileName,
        results = reactiveInputs$results,
        modulationsBack = reactiveInputs$modulationsBack,
        period = reactiveInputs$period,
        setAmplitude = reactiveInputs$setAmplitude,
        startingTemp = reactiveInputs$startingTemp,
        stepSize = reactiveInputs$stepSize
      )
      
      hidePageSpinner()
      
    })
    
    output$excelDownload <- downloadHandler(
      filename = function() {
        fileName <- unlist(strsplit(reactiveInputs$fileName, "\\."))[1]
        paste0(fileName, " ", reactiveInputs$modulationsBack, " modulations analysed.xlsx")
      },
      
      content = function(file) {
        showPageSpinner()
        
        wb <- downloadExcel(
          results = reactiveInputs$results,
          fileName = reactiveInputs$fileName,
          modulationsBack = reactiveInputs$modulationsBack,
          period = reactiveInputs$period,
          setAmplitude = reactiveInputs$setAmplitude,
          startingTemp = reactiveInputs$startingTemp,
          stepSize = reactiveInputs$stepSize,
          isothermLength = reactiveInputs$isothermLength,
          sampling = reactiveInputs$sampling
        )   
        
        saveWorkbook(wb, file = file, overwrite = TRUE)
        
        hidePageSpinner()
        
      }
    )
    
    output$NRHFdownload <- downloadHandler(
      filename = function() {
        subtitle <- unlist(strsplit(reactiveInputs$fileName, "[.]"))[1]
        plotTitleNRHF <- paste0("NRHF based on FT, ", reactiveInputs$modulationsBack, " modulations")
        paste0(subtitle, " ", plotTitleNRHF, input$extension)
      },
      content = function(file) {
        res <- reactiveInputs$results
        
        quasi_isothermal_NRHF_plot(
          res$resultsFT,
          reactiveInputs$modulationsBack,
          reactiveInputs$fileName
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
        subtitle <- unlist(strsplit(reactiveInputs$fileName, "[.]"))[1]
        plotTitleRevCp <- paste0("RevCp based on FT, ", reactiveInputs$modulationsBack, " modulations")
        paste0(subtitle, " ", plotTitleRevCp, input$extension)
      },
      content = function(file) {
        res <- reactiveInputs$results
        
        quasi_isothermal_RevCp_plot(
          res$resultsFT,
          reactiveInputs$modulationsBack,
          reactiveInputs$fileName
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
        subtitle <- unlist(strsplit(reactiveInputs$fileName, "[.]"))[1]
        plottitleRevCpmanual <- paste0("RevCp no FT, ", reactiveInputs$modulationsBack, " modulations")
        paste0(subtitle, " ", plottitleRevCpmanual, input$extension)
      },
      content = function(file) {
        res <- reactiveInputs$results
        
        quasi_isothermal_Manual_RevCp_plot(
          res$resultsNoFT,
          reactiveInputs$modulationsBack,
          reactiveInputs$fileName
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
        subtitle <- unlist(strsplit(reactiveInputs$fileName, "[.]"))[1]
        Title <- "Quasi-isothermal mDSC analysis"
        paste0(subtitle, " ", Title, ".zip")  # Must be .zip
      },
      content = function(file) {
        showPageSpinner()
        res <- reactiveInputs$results
        
        tmpdir <- tempdir()
        
        # Output files for the plots
        plot1_file <- file.path(tmpdir, paste0("NRHF plot", input$extension))
        plot2_file <- file.path(tmpdir, paste0("RevCp plot", input$extension))
        plot3_file <- file.path(tmpdir, paste0("Manual RevCp plot", input$extension))
        # plot4_file <- file.path(tmpdir, "Overlay plot.png")
        plot5_file <- file.path(tmpdir, paste0("Maxima and minima 1", input$extension))
        plot6_file <- file.path(tmpdir, paste0("Maxima and minima prefinal", input$extension))
        plot7_file <- file.path(tmpdir, paste0("Maxima and minima final", input$extension))
        plot8_file <- file.path(tmpdir, paste0("Original data", input$extension))
        plot9_file <- file.path(tmpdir, paste0("First cleaned up data", input$extension))
        plot10_file <- file.path(tmpdir, paste0("Prefinal cleaned up data", input$extension))
        plot11_file <- file.path(tmpdir, paste0("Final data used for analysis", input$extension))
        
        # Save each plot to its file
        ggsave(
          filename = plot1_file,
          plot = quasi_isothermal_NRHF_plot(res$resultsFT, reactiveInputs$modulationsBack, reactiveInputs$fileName),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        ggsave(
          filename = plot2_file,
          plot = quasi_isothermal_RevCp_plot(res$resultsFT, reactiveInputs$modulationsBack, reactiveInputs$fileName),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        ggsave(
          filename = plot3_file,
          plot = quasi_isothermal_Manual_RevCp_plot(res$resultsNoFT, reactiveInputs$modulationsBack, reactiveInputs$fileName),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        ggsave(
          filename = plot5_file,
          plot = quasi_isothermal_Maxima_minima(res$ExtremaDf1, reactiveInputs$modulationsBack, reactiveInputs$fileName),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        ggsave(
          filename = plot6_file,
          plot = quasi_isothermal_Maxima_minima(res$ExtremaDf2, reactiveInputs$modulationsBack, reactiveInputs$fileName),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        ggsave(
          filename = plot7_file,
          plot = quasi_isothermal_Maxima_minima_2(res$ExtremaDf3, reactiveInputs$modulationsBack, reactiveInputs$fileName),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        ggsave(
          filename = plot8_file,
          plot = quasi_isothermal_Original_dataggplot(res$OriginalData, reactiveInputs$modulationsBack, reactiveInputs$fileName),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        ggsave(
          filename = plot9_file,
          plot = quasi_isothermal_Datasteps_plot_1ggplot(res$isolatedPatterns, reactiveInputs$modulationsBack, reactiveInputs$fileName),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        ggsave(
          filename = plot10_file,
          plot = quasi_isothermal_Datasteps_plot_prefinalggplot(res$deleteLastMax, reactiveInputs$modulationsBack, reactiveInputs$fileName),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        ggsave(
          filename = plot11_file,
          plot = quasi_isothermal_Datasteps_plot_finalggplot(res$finalDataForAnalysis, reactiveInputs$modulationsBack, reactiveInputs$fileName),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        # Bundle into a zip file
        utils::zip(zipfile = file, files = c(plot1_file, plot2_file, plot3_file,
                                      plot5_file, plot6_file, plot7_file, 
                                      plot8_file, plot9_file, plot10_file, 
                                      plot11_file), flags = "-j")
        
        hidePageSpinner()
        
      }
    )
    
    # Render the plot using the reactive results
    output$plot <- renderPlotly({
      req(reactiveInputs$results)  # Ensure results exist
      res <- reactiveInputs$results
      
      plot_obj <- switch(input$plot_choice,
                         "NRHF" = quasi_isothermal_NRHF_plot(res$resultsFT, reactiveInputs$modulationsBack, 
                                            reactiveInputs$fileName),
                         "RevCp" = quasi_isothermal_RevCp_plot(res$resultsFT, reactiveInputs$modulationsBack, 
                                              reactiveInputs$fileName),
                         "Manual RevCp" = quasi_isothermal_Manual_RevCp_plot(res$resultsNoFT,
                                                            reactiveInputs$modulationsBack, 
                                                            reactiveInputs$fileName),
                         "RevCp and NRHF" = quasi_isothermal_RevCp_NRHF_plot(res$resultsFT, 
                                                            reactiveInputs$modulationsBack, 
                                                            reactiveInputs$fileName),
                         "Maxima and minima 1" = quasi_isothermal_Maxima_minima(res$ExtremaDf1, 
                                                               reactiveInputs$modulationsBack, 
                                                               reactiveInputs$fileName),
                         "Maxima and minima prefinal" = quasi_isothermal_Maxima_minima_1(res$ExtremaDf2,
                                                                        reactiveInputs$modulationsBack, 
                                                                        reactiveInputs$fileName),
                         "Maxima and minima final" =  quasi_isothermal_Maxima_minima_2(res$ExtremaDf3, 
                                                                      reactiveInputs$modulationsBack,
                                                                      reactiveInputs$fileName),
                         "Original data" = quasi_isothermal_Original_data(res$OriginalData, 
                                                         reactiveInputs$modulationsBack, 
                                                         reactiveInputs$fileName),
                         "First cleaned up data" = quasi_isothermal_Datasteps_plot_1(res$isolatedPatterns, 
                                                                    reactiveInputs$modulationsBack, 
                                                                    reactiveInputs$fileName),
                         "Prefinal cleaned up data" = quasi_isothermal_Datasteps_plot_prefinal(res$deleteLastMax, 
                                                                              reactiveInputs$modulationsBack,
                                                                              reactiveInputs$fileName),
                         "Final data used for analysis" = quasi_isothermal_Datasteps_plot_final(res$finalDataForAnalysis, 
                                                                               reactiveInputs$modulationsBack, 
                                                                               reactiveInputs$fileName))
      
      ggplotly(plot_obj, tooltip = c("x", "y", "text"))
    })
  })
}
