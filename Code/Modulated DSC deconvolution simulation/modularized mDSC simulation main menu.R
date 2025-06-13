# mdsc_module.R
source("../Modulated DSC deconvolution simulation/libraries.R")
source("../Modulated DSC deconvolution simulation/configapp.R")
source("../Modulated DSC deconvolution simulation/downloadsDSCSim.R")


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
      fluidPage(configUIsim1(ns))
    ),
    
    tabPanel(
      title = "Experimental results Input",
      id = ns("expresinput"),
      icon = icon("gears", class = "fa-solid"),
      fluidPage(
        fluidRow(configUIsim2(ns), configUIsim3(ns), configUIsim4(ns)),configUIsim5(ns)
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
                        choices = c("MHF", "Overlay", "THF", "RHF", "RHF no FT", "NRHF"),
                        selected = "MHF")
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
      fluidPage(
        sidebarLayout(
          sidebarPanel(
            h4("Plot export settings"),
            selectInput(ns("extension"), "What should the plot's extension be?", c(".png", ".jpg", ".tiff")), 
            textInput(ns("exportDpi"), "What should the plot dpi be?", value= 600),
            textInput(ns("exportWidth"), "What should the plot width be in cm?",  value= 20),
            textInput(ns("exportHeight"), "What should the plot height be in cm?", value= 20)
          ),
          mainPanel(
            br(), br(),br(), br(), br(), br(),

            fluidRow(
              tags$div(
                style = "text-align: center;",
                downloadButton(ns("excelDownload"), "Download the Excel sheet with all the analyses", class = "btn-primary btn-lg")
              )
            ),
            br(), br(),br(), br(), br(),
            fluidRow(
              tags$div(
                style = "text-align: center;",
                downloadButton(ns("allPlotsDownload"), "Download all plots (MHF, THF, RHF, NRHF, and RHF without FT)", class = "btn-primary btn-lg")
              )
            ),
            br(), br(), br(),
            div(
              class = "succes-text",
              textOutput(ns("downloadMessage"))
            )
          )
        )
      )
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
    disable("excelDownload")
    disable("allPlotsDownload")
    
    
    # Create a reactiveValues object to store inputs
    reactive_inputs <- reactiveValues()
    
    observeEvent(input$calculate, {
      showPageSpinner()
      
      
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
      
      source("../Modulated DSC deconvolution simulation/Time point generation.R")
      source("../Modulated DSC deconvolution simulation/Signal generation.R")
      source("../Modulated DSC deconvolution simulation/Equally-spaced y-values.R")
      source("../Modulated DSC deconvolution simulation/Final calculations.R")
      source("../Modulated DSC deconvolution simulation/Plot generation and control.R")
      
      
      reactive_inputs$subtitle <- paste0(
        "Sampling: ", reactive_inputs$sampling, " pts/sec. Period: ", reactive_inputs$period, " sec. ", 
        "Heating rate: ", reactive_inputs$heatRate * 60, " °C/min. ",
        "MHF phase: ", reactive_inputs$phase, " rad. LOESS alpha: ", reactive_inputs$loessAlpha, ".\n Temp. amplitude: ", reactive_inputs$Atemp, " °C.",
        "Other parameters (such as start temp.) are visible on the plot.")
      
      
      
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
      
      
      if (reactive_inputs$saveplots == TRUE) {    
        MHFplots(reactive_inputs$finaldf, reactive_inputs$subtitle, reactive_inputs$savetitle)
        overlayplot(reactive_inputs$finaldf, reactive_inputs$subtitle, reactive_inputs$savetitle)
        smoothedTHFplot(reactive_inputs$finaldf, reactive_inputs$subtitle, reactive_inputs$savetitle)
        smoothedRHFplot(reactive_inputs$finaldf, reactive_inputs$subtitle, reactive_inputs$savetitle)
        smoothedNRHFplot(reactive_inputs$finaldf, reactive_inputs$subtitle, reactive_inputs$savetitle)
      }
      
      enable("excelDownload")
      enable("allPlotsDownload")
      
      hidePageSpinner()
      
      
    })
    
    output$excelDownload <- downloadHandler(
      filename = function() {
        fileName <- "Analysis Excel"
        paste0(Sys.Date(), " ", fileName, ".xlsx")
      },
      content = function(file) {
        showPageSpinner()
        wb <- downloadExcelSimDSC(reactive_inputs)
        saveWorkbook(wb, file = file, overwrite = TRUE)
        hidePageSpinner()
      }
    )
    
    output$allPlotsDownload <- downloadHandler(

      filename = function() {
        Title <- "Export plots mDSC simulation"
        paste0(Sys.Date(), " ", Title, ".zip")  # Must be .zip
      },
      
      content = function(file) {
        showPageSpinner()
        res <- reactive_inputs$finaldf
        res2 <- reactive_inputs$noFTcalc

        tmpdir <- tempdir()
        
        # Output files for the plots
        plot1_file <- file.path(tmpdir, paste0("MHF plot", input$extension))
        plot2_file <- file.path(tmpdir, paste0("Overlay plot", input$extension))
        plot3_file <- file.path(tmpdir, paste0("THF plot", input$extension))
        plot4_file <- file.path(tmpdir, paste0("RHF plot", input$extension))
        plot5_file <- file.path(tmpdir, paste0("RHF without FT plot", input$extension))
        plot6_file <- file.path(tmpdir, paste0("NRHF plot", input$extension))
        
        
        # Save each plot to its file
        ggsave(
          filename = plot1_file,
          plot = MHFplots(res, reactive_inputs$subtitle, reactive_inputs$savetitle),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        ggsave(
          filename = plot2_file,
          plot = overlayplot(res, reactive_inputs$subtitle, reactive_inputs$savetitle),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        ggsave(
          filename = plot3_file,
          plot = smoothedTHFplot(res, reactive_inputs$subtitle, reactive_inputs$savetitle),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        ggsave(
          filename = plot4_file,
          plot = smoothedRHFplot(res, reactive_inputs$subtitle, reactive_inputs$savetitle),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        ggsave(
          filename = plot5_file,
          plot = RHFnoFT(res2),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        ggsave(
          filename = plot6_file,
          plot = smoothedNRHFplot(res, reactive_inputs$subtitle, reactive_inputs$savetitle),
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
                         "MHF" = MHFplots(res, reactive_inputs$subtitle, reactive_inputs$savetitle),
                         "Overlay" = overlayplot(res, reactive_inputs$subtitle, reactive_inputs$savetitle),
                         "THF" = smoothedTHFplot(res, reactive_inputs$subtitle, reactive_inputs$savetitle),
                         "RHF" = smoothedRHFplot(res, reactive_inputs$subtitle, reactive_inputs$savetitle),
                         "RHF no FT" = RHFnoFT(res2),
                         "NRHF" = smoothedNRHFplot(res, reactive_inputs$subtitle, reactive_inputs$savetitle))
      
      ggplotly(plot_obj, tooltip = c("x", "y", "text"))
    })
  })
}
