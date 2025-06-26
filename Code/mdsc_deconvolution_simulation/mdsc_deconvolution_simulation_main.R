source("mdsc_deconvolution_simulation/mdsc_deconvolution_simulation_libraries.R")
source("mdsc_deconvolution_simulation/mdsc_deconvolution_simulation_ui.R")
source("mdsc_deconvolution_simulation/mdsc_deconvolution_simulation_downloads.R")
source("mdsc_deconvolution_simulation/mdsc_deconvolution_simulation_time_point_generation.R")
source("mdsc_deconvolution_simulation/mdsc_deconvolution_simulation_signal_generation.R")
source("mdsc_deconvolution_simulation/mdsc_deconvolution_simulation_equally_spaced_y_values.R")
source("mdsc_deconvolution_simulation/mdsc_deconvolution_simulation_final_calculations.R")
source("mdsc_deconvolution_simulation/mdsc_deconvolution_simulation_plots.R")
source("mdsc_deconvolution_simulation/mdsc_deconvolution_simulation_error_handling.R")


mdsc_sim_ui <- function(id) {
  ns <- NS(id)
  navbarPage(
    id = ns("tabs"),
    title = "Modulated DSC deconvolution simulation",
    inverse = FALSE,
    
    tabPanel(
      title = "Parameter Input",
      id = ns("paraminput"),
      icon = icon("sliders", class = "fa-solid"),
      fluidPage(
        configUIsim1(ns),
      )
    ),
    
    tabPanel(
      title = "Baseline Input",
      id = ns("baseInput"),
      value = "baseInput",
      icon = icon("gears", class = "fa-solid"),
      fluidPage(
        configUIsim2(ns)
      )
    ),
    
    tabPanel(
      title = "Gaussian peaks input",
      id = ns("gaussPeak"),
      value = "gaussPeak",
      icon = icon("chart-area", class = "fa-solid"),
      fluidPage(
          configUIsim3(ns)
      )
    ),
    
    tabPanel(
      id = ns("graphs"),
      title = "Graphs",
      icon = icon("chart-line", class = "fa-solid"),
      fluidPage(
        configUIsim4(ns)
      )
    ),
    
    tabPanel(
      id = ns("downloads"),
      title = "Downloads",
      icon = icon("download", class = "fa-solid"),
      fluidPage(configUI5(ns))
    ),
    
    tabPanel(
      title = "Tutorial",
      icon = icon("book", class = "fa-solid"),
      fluidPage(
        withMathJax(
          includeMarkdown("mdsc_deconvolution_simulation/mdsc_deconvolution_simulation_tutorial.md")
        )
      )
    )
  )
}

mdsc_sim_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    
    disable("mDSCSimplotsDownload")
    disable("downloadExcelSimDSC")
    
    observeEvent(input$next1, {
      updateNavbarPage(session, "tabs", selected = "baseInput")
    })
    
    observeEvent(input$next2, {
      updateNavbarPage(session, "tabs", selected = "gaussPeak")
    })
    
    observeEvent(input$gaussianNumber, {
      gaussianNumber <- as.numeric(input$gaussianNumber)
      
      if (!is.na(gaussianNumber) && gaussianNumber != 0) {
        output$gaussians <- renderUI({
          # Create the individual inputs
          inputs <- lapply(1:gaussianNumber, function(i) {
            textInput(
              ns(paste0("gaussian", i)),
              label = paste0("Gaussian ", i, ": Onset (°C), End (°C), Enthalpy (J/g)"),
              value = ""
            )
          })
          
          # Distribute inputs across three columns
          col1 <- inputs[seq(1, length(inputs), 3)]
          if (gaussianNumber > 1) {
            col2 <- inputs[seq(2, length(inputs), 3)]
          }
          if (gaussianNumber > 2) {
            col3 <- inputs[seq(3, length(inputs), 3)]
          }
          
          fluidRow(
            column(4, col1),
            if (gaussianNumber > 1) {
              column(4, col2)
            },
            if (gaussianNumber > 2) {
              column(4, col3)
            }
          )
        })
      } else {
        output$gaussians <- renderUI(NULL)  # Clear if zero
      }
    })
    
    
    
    # Create a reactiveValues object to store inputs
    reactiveInputs <- reactiveValues()
    
    observeEvent(input$analyze, {
      showPageSpinner()
      output$errorMessage <- NULL
      output$succesMessage <- NULL
      
      reactiveInputs$sampling <- eval(parse(text = input$sampling))
      reactiveInputs$startTemp <- eval(parse(text = input$startTemp))
      reactiveInputs$endTemp <- eval(parse(text = input$endTemp))
      reactiveInputs$period <- eval(parse(text = input$period))
      reactiveInputs$heatRate <- eval(parse(text = input$heatRate))/60
      reactiveInputs$Atemp <- eval(parse(text = input$Atemp))
      reactiveInputs$phase <- eval(parse(text = input$phase))
      reactiveInputs$loessAlpha <- eval(parse(text = input$loessAlpha))
      
      
      reactiveInputs$deltaRHFPreTg <- eval(parse(text = input$deltaRHFPreTg))
      reactiveInputs$deltaRHFPostTg <- eval(parse(text = input$deltaRHFPostTg))
      reactiveInputs$StartRHFPreTg <- eval(parse(text = input$StartRHFPreTg))
      reactiveInputs$deltaCpPreTg <- eval(parse(text = input$deltaCpPreTg))
      reactiveInputs$deltaCpPostTg <- eval(parse(text = input$deltaCpPostTg))
      reactiveInputs$StartCpTempPreTg <- eval(parse(text = input$StartCpTempPreTg))
      
      
      reactiveInputs$locationTgTHF <- tryCatch({
        vec <- as.numeric(unlist(strsplit(input$locationTgTHF, ",")))
        if (any(is.na(vec))) NA else vec
      }, error = function(e) NA)
      
      reactiveInputs$locationTgRHF <- tryCatch({
        vec <- as.numeric(unlist(strsplit(input$locationTgRHF, ",")))
        if (any(is.na(vec))) NA else vec
      }, error = function(e) NA)
      
      reactiveInputs$deltaCpTg <- as.numeric(input$deltaCpTg)
      reactiveInputs$gaussianNumber <- as.numeric(input$gaussianNumber)
      reactiveInputs$gaussianList <- list()
      
      
      if (reactiveInputs$gaussianNumber != 0) {
        for (i in 1:reactiveInputs$gaussianNumber) {
          reactiveInputs$gaussianList[[length(reactiveInputs$gaussianList) + 1]] <- tryCatch({
            vec <- as.numeric(unlist(strsplit(input[[paste0("gaussian", i)]], ",")))
            if (any(is.na(vec))) NA else vec
          }, error = function(e) NA)
        }
        
        ## error-handling only: if ANY element is NA, nuke the whole list
        if (any(vapply(reactiveInputs$gaussianList,
                       function(x) length(x) == 1 && is.na(x),
                       logical(1L)))) {
          reactiveInputs$gaussianList <- NA
        }
      } else { NULL }

      msg <- simulation_error_handling(reactiveInputs)
      
      # Update the error message output (this triggers UI update)
      output$errorMessage <- renderText({
        if (!is.null(msg)) msg else ""
      })
      
      # If there is an error message, stop further processing
      if (!is.null(msg)) {
        hidePageSpinner()
        return(NULL)
      }
      
      reactiveInputs$subtitle <- paste0(
        "Sampling: ", reactiveInputs$sampling, 
        " pts/sec. Period: ", reactiveInputs$period, " sec. ", 
        "Melting period: ", reactiveInputs$periodSignal, 
        " sec. Heating rate: ", reactiveInputs$heatRate * 60, " °C/min. ",
        "MHF phase: ", reactiveInputs$phase, 
        " rad. LOESS alpha: ", reactiveInputs$loessAlpha, 
        ".\n Temp. amplitude: ", reactiveInputs$Atemp, " °C.",
        "Melting amplitude: ", reactiveInputs$MeltEnth, 
        " W/g. Other parameters (such as start temp.) are visible on the plot."
      )
      
      
      timeGen <- time_generation(reactiveInputs)
      
      signalGen <- signal_generation(reactiveInputs, timeGen)
      
      reactiveInputs$signalGen <- signalGen
      
      resampled_points <- equal_y_val(reactiveInputs$signalGen)
      
      results <- final_calculation(
        sampling = reactiveInputs$sampling,
        startTemp = reactiveInputs$startTemp,
        endTemp = reactiveInputs$endTemp,
        period = reactiveInputs$period,
        heatRate = reactiveInputs$heatRate,
        Atemp = reactiveInputs$Atemp,
        resampled_points = resampled_points,
        loessAlpha = reactiveInputs$loessAlpha,
        timeGen = timeGen,
        signalGen = signalGen
      )
      
      reactiveInputs$finaldf <- results[[1]]
      reactiveInputs$noFTcalc <- results[[2]]
      
      enable("mDSCSimplotsDownload")
      enable("downloadExcelSimDSC")

      output$succesMessage <- renderText({
        "Analysis succesful! You can now head over to the \"Graphs\" or \"Downloads\" tab."
        })      
      
      hidePageSpinner()
    })
    
    output$downloadExcelSimDSC <- downloadHandler(
      filename = function() {
        paste0(Sys.Date(), "_mDSC simulation.xlsx")
      },
      content = function(file) {
        showPageSpinner()
        wbmDSCSim <- download_Excel(reactiveInputs)   
        saveWorkbook(wbmDSCSim, file = file, overwrite = TRUE)
        hidePageSpinner()
      }
    )
    
    output$mDSCSimplotsDownload <- downloadHandler(
      filename = function() {
        paste0(Sys.Date(), " ", "_mDSC simulation - all plots.zip")  # Must be .zip
      },
      content = function(file) {
        showPageSpinner()
        res <- reactiveInputs$finaldf
        res2 <- reactiveInputs$noFTcalc
        
        tmpdir <- tempdir()
        
        # Output files for the plots
        plot1_file <- file.path(tmpdir, paste0("MHF plot", input$extension))
        plot2_file <- file.path(tmpdir, paste0("overlay plot", input$extension))
        plot3_file <- file.path(tmpdir, paste0("THF plot", input$extension))
        plot4_file <- file.path(tmpdir, paste0("RHF plot", input$extension))
        plot5_file <- file.path(tmpdir, paste0("RHF without FT plot", input$extension))
        plot6_file <- file.path(tmpdir, paste0("NRHF plot", input$extension))
        
        # Save each plot to its file
        ggsave(
          filename = plot1_file,
          plot = MHFplots(res, reactiveInputs$subtitle),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        ggsave(
          filename = plot2_file,
          plot = overlayplot(res, reactiveInputs$subtitle),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        ggsave(
          filename = plot3_file,
          plot = smoothedTHFplot(res, reactiveInputs$subtitle),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        ggsave(
          filename = plot4_file,
          plot = smoothedRHFplot(res, reactiveInputs$subtitle),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        ggsave(
          filename = plot5_file,
          plot = RHFnoFT(res2, reactiveInputs$subtitle),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        ggsave(
          filename = plot6_file,
          plot = smoothedNRHFplot(res, reactiveInputs$subtitle),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        # Bundle into a zip file
        zip(zipfile = file, files = c(plot1_file, plot2_file, plot3_file, 
                                      plot4_file, plot5_file, plot6_file), 
            flags = "-j"
        )
        
        hidePageSpinner()
        
      }
    )
    
    
    # Render the plot using the reactive sample_results
    output$plot <- renderPlotly({
      req(reactiveInputs$finaldf)  # Ensure results exist
      req(reactiveInputs$noFTcalc)
      res <- reactiveInputs$finaldf
      res2 <- reactiveInputs$noFTcalc
      
      plot_obj <- switch(input$plot_choice,
                         "MHF" = MHFplots(res, reactiveInputs$subtitle),
                         "Overlay" = overlayplot(res, reactiveInputs$subtitle),
                         "THF" = smoothedTHFplot(res, reactiveInputs$subtitle),
                         "RHF" = smoothedRHFplot(res, reactiveInputs$subtitle),
                         "RHF no FT" = RHFnoFT(res2, reactiveInputs$subtitle),
                         "NRHF" = smoothedNRHFplot(res, reactiveInputs$subtitle))
      ggplotly(plot_obj, tooltip = c("x", "y", "text"))
    })
  })
}
