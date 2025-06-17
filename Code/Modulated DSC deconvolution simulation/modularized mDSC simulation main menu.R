# mdsc_module.R
source("../Modulated DSC deconvolution simulation/libraries.R")
source("../Modulated DSC deconvolution simulation/configapp_modDSCSim.R")
source("../Modulated DSC deconvolution simulation/downloadsDSCSim.R")
source("../Modulated DSC deconvolution simulation/Time point generation.R")
source("../Modulated DSC deconvolution simulation/Signal generation.R")
source("../Modulated DSC deconvolution simulation/Equally-spaced y-values.R")
source("../Modulated DSC deconvolution simulation/Final calculations.R")
source("../Modulated DSC deconvolution simulation/Plot generation and control.R")
source("../Modulated DSC deconvolution simulation/mDSCSimErrorHandling.R")


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
      id = ns("tutorial"),
      title = "Tutorial",
      icon = icon("book", class = "fa-solid"),
      fluidPage()
    ), 
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
    reactive_inputs <- reactiveValues()
    
    observeEvent(input$analyze, {
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
      
      
      reactive_inputs$locationTgTHF <- as.numeric(unlist(strsplit(input$locationTgTHF, ",")))
      reactive_inputs$locationTgRHF <- as.numeric(unlist(strsplit(input$locationTgRHF, ",")))
      reactive_inputs$deltaCpTg <- as.numeric(unlist(strsplit(input$deltaCpTg, ",")))
      

      reactive_inputs$gaussianNumber <- as.numeric(input$gaussianNumber)
      reactive_inputs$gaussianList <- list()
      
      
      if(reactive_inputs$gaussianNumber != 0) {
        for(i in 1:reactive_inputs$gaussianNumber) {
          reactive_inputs$gaussianList[[length(reactive_inputs$gaussianList) + 1]] <- as.numeric(unlist(strsplit(input[[paste0("gaussian", i)]], ",")))
        }
      } else {NULL}

      msg <- mDSCSimErrorhandlingFunc(reactive_inputs)
      
      # Update the error message output (this triggers UI update)
      output$errorMessage <- renderText({
        if (!is.null(msg)) msg else ""
      })
      
      # If there is an error message, stop further processing
      if (!is.null(msg)) {
        hidePageSpinner()
        return(NULL)
      }
      
      reactive_inputs$subtitle <- paste0(
        "Sampling: ", reactive_inputs$sampling, " pts/sec. Period: ", reactive_inputs$period, " sec. ", 
        "Melting period: ", reactive_inputs$periodSignal, " sec. Heating rate: ", reactive_inputs$heatRate * 60, " °C/min. ",
        "MHF phase: ", reactive_inputs$phase, " rad. LOESS alpha: ", reactive_inputs$loessAlpha, ".\n Temp. amplitude: ", reactive_inputs$Atemp, " °C.",
        "Melting amplitude: ", reactive_inputs$MeltEnth, " W/g. Other parameters (such as start temp.) are visible on the plot.")
      
      
      
      
      df1 <- timegeneration(reactive_inputs)
      
      df2 <- signalgeneration(reactive_inputs, df1)
      
      reactive_inputs$df2 <- df2
      
      resampled_points <- equalyval(reactive_inputs$df2)
      
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
      
      enable("mDSCSimplotsDownload")
      enable("downloadExcelSimDSC")
      
      
      hidePageSpinner()
      
    })
    
    output$downloadExcelSimDSC <- downloadHandler(
      filename = function() {
        paste0(Sys.Date(), "_mDSC simulation.xlsx")
      },
      content = function(file) {
        showPageSpinner()
        wbmDSCSim <- downloadExcelSimDSCFunc(reactive_inputs)   
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
        res <- reactive_inputs$finaldf
        res2 <- reactive_inputs$noFTcalc
        
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
          plot = MHFplots(res, reactive_inputs$subtitle),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        ggsave(
          filename = plot2_file,
          plot = overlayplot(res, reactive_inputs$subtitle),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        ggsave(
          filename = plot3_file,
          plot = smoothedTHFplot(res, reactive_inputs$subtitle),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        ggsave(
          filename = plot4_file,
          plot = smoothedRHFplot(res, reactive_inputs$subtitle),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        ggsave(
          filename = plot5_file,
          plot = RHFnoFT(res2, reactive_inputs$subtitle),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        ggsave(
          filename = plot6_file,
          plot = smoothedNRHFplot(res, reactive_inputs$subtitle),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        # Bundle into a zip file
        zip(zipfile = file, files = c(plot1_file, plot2_file, plot3_file, plot4_file, plot5_file, plot6_file), flags = "-j")
        
        hidePageSpinner()
        
      }
    )
    
    
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
                         "RHF no FT" = RHFnoFT(res2, reactive_inputs$subtitle),
                         "NRHF" = smoothedNRHFplot(res, reactive_inputs$subtitle))
      ggplotly(plot_obj, tooltip = c("x", "y", "text"))
    })
  })
}
