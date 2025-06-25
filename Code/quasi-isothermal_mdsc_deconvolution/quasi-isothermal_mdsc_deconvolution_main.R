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
      
      # Handle checkboxes
      if(input$sheetask) {
        reactiveInputs$sheet <- 1
      } else {
        reactiveInputs$sheet <- input$sheet
      }
      
      # Store numerical/text inputs with proper reactive evaluation
      reactiveInputs$period <- eval(parse(text = input$period))  # Assuming input is numeric
      reactiveInputs$stepSize <- eval(parse(text = input$stepSize))
      reactiveInputs$isothermLength <- eval(parse(text = input$isothermLength))
      reactiveInputs$startingTemp <- eval(parse(text = input$startingTemp))
      reactiveInputs$modulationsBack <- eval(parse(text = input$modulationsBack))
      reactiveInputs$setAmplitude <- eval(parse(text = input$setAmplitude))
      reactiveInputs$sampling <- eval(parse(text = input$sampling))
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
      
      
      hidePageSpinner()
      
    })
    
    # Handle recalculation with a different number of modulations
    observeEvent(input$recalc, {
      showPageSpinner()
      msg <- NULL
      
      reactiveInputs$modulationsBack <- eval(parse(text = input$modulations_back_in_new))
      
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
        stepSize = reactiveInputs$stepSize,
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
        plotTitleNRHF <- paste0("NRHF based on FT (frequency = 0), ", reactiveInputs$modulationsBack, " modulations")
        paste0(subtitle, " ", plotTitleNRHF, input$extension)
      },
      content = function(file) {
        source("quasi-isothermal_mdsc_deconvolution/quasi-isothermal_mdsc_deconvolution_plots.R")
        res <- reactiveInputs$results
        
        NRHF_plot(
          res$resultsFT,
          reactiveInputs$modulationsBack,
          reactiveInputs$fileName,
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
        subtitle <- unlist(strsplit(reactiveInputs$fileName, "[.]"))[1]
        plotTitleRevCp <- paste0("RevCp based on FT (1st harmonic), ", reactiveInputs$modulationsBack, " modulations")
        paste0(subtitle, " ", plotTitleRevCp, input$extension)
      },
      content = function(file) {
        source("quasi-isothermal_mdsc_deconvolution/quasi-isothermal_mdsc_deconvolution_plots.R")
        res <- reactiveInputs$results
        
        RevCp_plot(
          res$resultsFT,
          reactiveInputs$modulationsBack,
          reactiveInputs$fileName,
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
        subtitle <- unlist(strsplit(reactiveInputs$fileName, "[.]"))[1]
        plottitleRevCpmanual <- paste0("RevCp calculated manually, ", reactiveInputs$modulationsBack, " modulations")
        paste0(subtitle, " ", plottitleRevCpmanual, input$extension)
      },
      content = function(file) {
        source("quasi-isothermal_mdsc_deconvolution/quasi-isothermal_mdsc_deconvolution_plots.R")
        res <- reactiveInputs$results
        
        Manual_RevCp_plot(
          res$resultsFT,
          reactiveInputs$modulationsBack,
          reactiveInputs$fileName,
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
        subtitle <- unlist(strsplit(reactiveInputs$fileName, "[.]"))[1]
        Title <- "Plots based on FT analysis"
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
          plot = NRHF_plot(res$resultsFT, reactiveInputs$modulationsBack, reactiveInputs$fileName),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        ggsave(
          filename = plot2_file,
          plot = RevCp_plot(res$resultsFT, reactiveInputs$modulationsBack, reactiveInputs$fileName),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        ggsave(
          filename = plot3_file,
          plot = Manual_RevCp_plot(res$resultsNoFT, reactiveInputs$modulationsBack, reactiveInputs$fileName),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        ggsave(
          filename = plot5_file,
          plot = Maxima_minima(res$`Extrema df1`, reactiveInputs$modulationsBack, reactiveInputs$fileName),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        ggsave(
          filename = plot6_file,
          plot = Maxima_minima_1(res$`Extrema df2`, reactiveInputs$modulationsBack, reactiveInputs$fileName),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        ggsave(
          filename = plot7_file,
          plot = Maxima_minima_2(res$`Extrema df3`, reactiveInputs$modulationsBack, reactiveInputs$fileName),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        ggsave(
          filename = plot8_file,
          plot = Original_dataggplot(res$`Original data`, reactiveInputs$modulationsBack, reactiveInputs$fileName),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        ggsave(
          filename = plot9_file,
          plot = Datasteps_plot_1ggplot(res$isolatedPatterns, reactiveInputs$modulationsBack, reactiveInputs$fileName),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        ggsave(
          filename = plot10_file,
          plot = Datasteps_plot_prefinalggplot(res$deleteLastMax, reactiveInputs$modulationsBack, reactiveInputs$fileName),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        ggsave(
          filename = plot11_file,
          plot = Datasteps_plot_finalggplot(res$finalDataForAnalysis, reactiveInputs$modulationsBack, reactiveInputs$fileName),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        # Bundle into a zip file
        zip(zipfile = file, files = c(plot1_file, plot2_file, plot3_file,
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
                         "NRHF" = NRHF_plot(res$resultsFT, reactiveInputs$modulationsBack, 
                                            reactiveInputs$fileName),
                         "RevCp" = RevCp_plot(res$resultsFT, reactiveInputs$modulationsBack, 
                                              reactiveInputs$fileName),
                         "Manual RevCp" = Manual_RevCp_plot(res$resultsNoFT,
                                                            reactiveInputs$modulationsBack, 
                                                            reactiveInputs$fileName),
                         "RevCp and NRHF" = RevCp_NRHF_plot(res$resultsFT, 
                                                            reactiveInputs$modulationsBack, 
                                                            reactiveInputs$fileName),
                         "Maxima and minima 1" = Maxima_minima(res$`Extrema df1`, 
                                                               reactiveInputs$modulationsBack, 
                                                               reactiveInputs$fileName),
                         "Maxima and minima prefinal" = Maxima_minima_1(res$`Extrema df2`,
                                                                        reactiveInputs$modulationsBack, 
                                                                        reactiveInputs$fileName),
                         "Maxima and minima final" =  Maxima_minima_2(res$`Extrema df3`, 
                                                                      reactiveInputs$modulationsBack,
                                                                      reactiveInputs$fileName),
                         "Original data" = Original_data(res$`Original data`, 
                                                         reactiveInputs$modulationsBack, 
                                                         reactiveInputs$fileName),
                         "First cleaned up data" = Datasteps_plot_1(res$isolatedPatterns, 
                                                                    reactiveInputs$modulationsBack, 
                                                                    reactiveInputs$fileName),
                         "Prefinal cleaned up data" = Datasteps_plot_prefinal(res$deleteLastMax, 
                                                                              reactiveInputs$modulationsBack,
                                                                              reactiveInputs$fileName),
                         "Final data used for analysis" = Datasteps_plot_final(res$finalDataForAnalysis, 
                                                                               reactiveInputs$modulationsBack, 
                                                                               reactiveInputs$fileName))
      
      ggplotly(plot_obj, tooltip = c("x", "y", "text"))
    })
  })
}
