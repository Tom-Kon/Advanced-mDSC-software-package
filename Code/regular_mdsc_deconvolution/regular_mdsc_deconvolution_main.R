source("regular_mdsc_deconvolution/regular_mdsc_deconvolution_functions.R")
source("regular_mdsc_deconvolution/regular_mdsc_deconvolution_libraries.R")
source("regular_mdsc_deconvolution/regular_mdsc_deconvolution_error_handling.R")
source("regular_mdsc_deconvolution/regular_mdsc_deconvolution_downloads.R")
source("regular_mdsc_deconvolution/regular_mdsc_deconvolution_plots.R")



normal_mDSC_ui <- function(id) {
  ns <- NS(id)
  navbarPage(
    id = ns("tabs"),
    title = "Regular modulated DSC deconvolution",
    inverse = FALSE,
    
    tabPanel(
      title = "Parameter Input",
      icon = icon("gears", class = "fa-solid"),
      fluidPage(
        fluidRow(
          div(
            style = "text-align:center",
            column(6,
                   div(style = "width: 100%;",
                       textInput(ns("period"), "Period in seconds", "40")),
                   div(style = "width: 100%;",
                       textInput(ns("heatingRate"), "Heating rate in °C/min", "2")),
                   div(style = "width: 100%;",
                       textInput(ns("setAmplitude"), "Temperature amplitude set by user (°C)", 
                                 "0.212"))
            )
          ),
          column(6,
                 checkboxInput(ns("HFcalcextra"), "Do you want to calculate RHF and NRHF using the THF as well?"),
                 checkboxInput(ns("compare"), "Do you want to compare with unmodulated DSC?"),
                 fileInput(ns("ExcelmDSC"), "Upload your Excel here"),
                 checkboxInput(ns("sheetask"), "Is your data in the first sheet of your Excel file?", TRUE),
                 conditionalPanel(
                   condition = sprintf("!input['%s']", ns("sheetask")),
                   selectInput(ns("sheet"), "What sheet is it in then?", 
                               choices = c("2", "3", "4", "5"))
                 ),
                 conditionalPanel(
                   condition = sprintf("input['%s']", ns("compare")),
                   fileInput(ns("Excel_DSC"), "Upload your Excel here"),
                   checkboxInput(ns("sheetask2"), "Is your data in the first sheet of your Excel file?", TRUE),
                   conditionalPanel(
                     condition = sprintf("!input['%s']", ns("sheetask2")),
                     selectInput(ns("sheet2"), "What sheet is it in then?", 
                                 choices = c("2", "3", "4", "5"))
                   )
                 )
          )
        ),
        fluidRow(
          column(4),
          column(4,
                 div(
                   class = "error-text",
                   textOutput(ns("errorMessage"))
                 )
          ),
          column(4)
        ),
        HTML("<br><br><br>"),
        fluidRow(
          column(4),
          column(4,
                 div(style = "text-align:center;",
                     actionButton(ns("analyze"), "Analyze", 
                                  class = "btn-primary btn-lg",
                                  style = "width: 70%; font-size: 25px; padding: 15px 30px;")
                 )
          ),
          column(4)
        )
      )
    ),
    
    
    tabPanel(
      title = "Graphs",
      icon = icon("chart-line", class = "fa-solid"),
      fluidPage(
        titlePanel("Output graphs"),
        fluidRow(
          column(12, wellPanel(
            selectInput(ns("plot_choice"), "Select Plot:", 
                        choices = c("THF", "RHF","NRHF", "THF FT", "RHF FT", 
                                    "NRHF FT", "THF TRIOS", "RHF based on THF TRIOS", 
                                    "NRHF based on THF TRIOS",  "THF unmodulated DSC", 
                                    "RHF based on unmodulated DSC", 
                                    "NRHF based on unmodulated DSC"), 
                        selected = "THF"),
          ))
        ),
        fluidRow(
          column(12, plotlyOutput(ns("plot"), height = "90vh"))
        ), 
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
            selectInput(ns("extension"), "What should the plot's extension be?", 
                        c(".png", ".jpg", ".tiff")), 
            textInput(ns("exportDpi"), "What should the plot dpi be?", value= 600),
            textInput(ns("exportWidth"), "What should the plot width be in cm?",  
                      value= 20),
            textInput(ns("exportHeight"), "What should the plot height be in cm?", 
                      value= 20)
          ),
          
          mainPanel(
            fluidRow(
              tags$div(
                style = "text-align: center;",
                downloadButton(ns("excelDownload"), 
                               "Download the Excel sheet with all the analyses", 
                               class = "btn-primary btn-lg")
              )
            ),
            br(), br(),
            fluidRow(
              tags$div(
                style = "text-align: center;",
                downloadButton(ns("MaxMinAnalysisDownload"), 
                               "Download the 3 plots (THF, RHF, NRHF) based on the analysis of the maxima and minima", 
                               class = "btn-primary btn-lg")
              )
            ),
            br(), br(),
            fluidRow(
              tags$div(
                style = "text-align: center;",
                downloadButton(ns("FTAnalysisDownload"), 
                               "Download the 3 plots (THF, RHF, NRHF) based on the FT analysis", 
                               class = "btn-primary btn-lg")
              )
            ),
            br(), br(),
            fluidRow(
              tags$div(
                style = "text-align: center;",
                downloadButton(ns("MaxTHFAnalysisDownload"),
                               "Download the 3 plots (THF, RHF, NRHF) based on the analysis of the maxima and the THF",
                               class = "btn-primary btn-lg")
              )
            ),
            br(), br(),
            fluidRow(
              tags$div(
                style = "text-align: center;",
                downloadButton(ns("MaxDSCAnalysisDownload"), 
                               "Download the 3 plots (THF, RHF, NRHF) based on the analysis of the maxima and the unmodulated DSC", 
                               class = "btn-primary btn-lg")
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
      title = "Tutorial",
      icon = icon("book", class = "fa-solid"),
      fluidPage(
        withMathJax(
          includeMarkdown("regular_mdsc_deconvolution/regular_mdsc_deconvolution_tutorial.md")
        )
      )
    ),
    
  )
}

normal_mDSC_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    disable("excelDownload")
    disable("MaxMinAnalysisDownload")
    disable("FTAnalysisDownload")
    disable("MaxTHFAnalysisDownload")
    disable("MaxDSCAnalysisDownload")
    
    

    # Create a reactiveValues object to store inputs
    reactiveInputs <- reactiveValues()

    observeEvent(input$analyze,{
      showPageSpinner()
      
      # Extraction
      reactiveInputs$period <- as.numeric(input$period)
      reactiveInputs$heatingRate <- as.numeric(input$heatingRate)
      reactiveInputs$setAmplitude <- as.numeric(input$setAmplitude)
      reactiveInputs$compare <- input$compare
      reactiveInputs$ExcelmDSC <- input$ExcelmDSC$datapath
      reactiveInputs$fileName <- as.character(input$ExcelmDSC$name)
      reactiveInputs$HFcalcextra <- input$HFcalcextra
      reactiveInputs$compare <- input$compare
      
      if(input$sheetask) {
        reactiveInputs$sheet <- 1
      } else {
        reactiveInputs$sheet <- input$sheet
      }
      
      if(input$sheetask2) {
        reactiveInputs$sheet2 <- 1
      } else {
        reactiveInputs$sheet2 <- input$sheet2
      }
      
      
      # Read data from Excel sheets
      import <- 1
      msg <- NULL
      error_handling(reactiveInputs, import)
      
      output$errorMessage <- renderText({
        if (!is.null(msg)) {
          msg
        } else if (!is.null(attr(d, "comment"))) {
          (attr(d, "comment"))
        } else {""
        }
      })
      
      if (!is.null(msg)) hidePageSpinner()
      
      reactiveInputs$RHFCalcDenominator <- reactiveInputs$setAmplitude*2*pi/reactiveInputs$period
      
      d <- excel_cleaner(reactiveInputs$ExcelmDSC, reactiveInputs$sheet, reactiveInputs$HFcalcextra, reactiveInputs$compare,import)
      
      # Update the error message output (this triggers UI update)
      if(typeof(d) == "character") {
        msg <- d
      }
      
      output$errorMessage <- renderText({
        if (!is.null(msg)) {
          msg
        } else if (!is.null(attr(d, "comment"))) {
          (attr(d, "comment"))
        } else {""
          }
      })
      
      if (!is.null(msg)) hidePageSpinner()
      
      
      req(is.null(msg))  # Exit here if there's an error
      
      
      
      #Apply functions for non-FT calculation
      extremaDf <-locate_extrema(d$modHeatFlow, d$time, d$temperature)
      counts <- count_extrema(extremaDf)
      calculationMinMaxResults <- calculate_heatflow_min_max(extremaDf, 
                                                             reactiveInputs$RHFCalcDenominator, 
                                                             reactiveInputs$heatingRate)
      
      reactiveInputs$extremaDf <- extremaDf
      reactiveInputs$calculationMinMaxResults <- calculationMinMaxResults
      
      
      if(reactiveInputs$HFcalcextra) {
        calculationMinMaxResultsTHF <- calculate_heatflow_min_max_THF(reactiveInputs$extremaDf, 
                                                                      reactiveInputs$RHFCalcDenominator, 
                                                                      reactiveInputs$heatingRate, 
                                                                      d)
        reactiveInputs$calculationMinMaxResultsTHF <- calculationMinMaxResultsTHF
      }

      #Apply functions for FT calculation
      reactiveInputs$calculate_fft <- calculate_fft(reactiveInputs$period, d,
                                                    reactiveInputs$RHFCalcDenominator, 
                                                    reactiveInputs$heatingRate)
      
      
      #Compare with DSC
      if(reactiveInputs$compare) {
        reactiveInputs$ExcelDSC <- input$Excel_DSC$datapath
        import <- 2
        
        error_handling(reactiveInputs, import)
        
        output$errorMessage <- renderText({
          if (!is.null(msg)) {
            msg
          } else if (!is.null(attr(d, "comment"))) {
            (attr(d, "comment"))
          } else {""
          }
        })
        
        if (!is.null(msg)) hidePageSpinner()
        
        DSC <- excel_cleaner(reactiveInputs$ExcelmDSC, reactiveInputs$sheet, 
                             reactiveInputs$HFcalcextra, reactiveInputs$compare,
                             import)
        
        
        # Update the error message output (this triggers UI update)
        if(typeof(d) == "character") {
          msg <- d
        }
        
        output$errorMessage <- renderText({
          if (!is.null(msg)) {
            msg
          } else if (!is.null(attr(d, "comment"))) {
            (attr(d, "comment"))
          } else {""
          }
        })
        
        if (!is.null(msg)) hidePageSpinner()
        
        req(is.null(msg))  # Exit here if there's an error
        
        
        calculationMinMaxResultsDSC <- calculate_heatflow_min_max_DSC(DSC, extremaDf, 
                                                                      reactiveInputs$RHFCalcDenominator,
                                                                      reactiveInputs$heatingRate)
        reactiveInputs$calculationMinMaxResultsDSC <- calculationMinMaxResultsDSC
   
      }
      
      enable("excelDownload")
      enable("MaxMinAnalysisDownload")
      enable("FTAnalysisDownload")
      toggleState(id = "MaxTHFAnalysisDownload", condition = reactiveInputs$HFcalcextra)
      toggleState(id = "MaxDSCAnalysisDownload", condition = reactiveInputs$compare)
      
      hidePageSpinner()

    })
    
    output$excelDownload <- downloadHandler(
      filename = function() {
        fileName <- unlist(strsplit(reactiveInputs$fileName, "\\."))[1]
        paste0(fileName, ".xlsx")
      },
      content = function(file) {
        showPageSpinner()
        wb <- download_excel_regular_mDSC(reactiveInputs)
        saveWorkbook(wb, file = file, overwrite = TRUE)
        hidePageSpinner()
        
      }
    )
    
    
    output$MaxMinAnalysisDownload <- downloadHandler(
      filename = function() {
        subtitle <- unlist(strsplit(reactiveInputs$fileName, "[.]"))[1]
        Title <- "Plots based on Max-Min analysis of MHF"
        paste0(subtitle, " ", Title, ".zip")  # Must be .zip
      },
      content = function(file) {
        showPageSpinner()
        
        tmpdir <- tempdir()
        
        # Output files for the plots
        plot1_file <- file.path(tmpdir, paste0("RHF_plot", input$extension))
        plot2_file <- file.path(tmpdir, paste0("THF_plot", input$extension))
        plot3_file <- file.path(tmpdir, paste0("NRHF_plot", input$extension))
        
        # Save each plot to its file
        ggsave(
          filename = plot1_file,
          plot = RHFplot(reactiveInputs$calculationMinMaxResults),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        ggsave(
          filename = plot2_file,
          plot = THFplot(reactiveInputs$calculationMinMaxResults),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        ggsave(
          filename = plot3_file,
          plot = NRHFplot(reactiveInputs$calculationMinMaxResults),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        # Bundle into a zip file
        zip(zipfile = file, files = c(plot1_file, plot2_file, plot3_file), 
            flags = "-j")
        
        
        hidePageSpinner()
        
      }
    )
    
    output$FTAnalysisDownload <- downloadHandler(
      filename = function() {
        subtitle <- unlist(strsplit(reactiveInputs$fileName, "[.]"))[1]
        Title <- "Plots based on FT analysis"
        paste0(subtitle, " ", Title, ".zip")  # Must be .zip
      },
      content = function(file) {
        showPageSpinner()
        
        tmpdir <- tempdir()
        
        # Output files for the plots
        plot1_file <- file.path(tmpdir, paste0("RHF_plot", input$extension))
        plot2_file <- file.path(tmpdir, paste0("THF_plot", input$extension))
        plot3_file <- file.path(tmpdir, paste0("NRHF_plot", input$extension))
        
        # Save each plot to its file
        ggsave(
          filename = plot1_file,
          plot = RHFplotFT(reactiveInputs$calculate_fft),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        ggsave(
          filename = plot2_file,
          plot = THFplotFT(reactiveInputs$calculate_fft),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        ggsave(
          filename = plot3_file,
          plot = NRHFplotFT(reactiveInputs$calculate_fft),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        # Bundle into a zip file
        zip(zipfile = file, files = c(plot1_file, plot2_file, plot3_file), 
            flags = "-j")
        
        hidePageSpinner()
        
      }
    )
    
    
    output$MaxTHFAnalysisDownload <- downloadHandler(
      filename = function() {
        subtitle <- unlist(strsplit(reactiveInputs$fileName, "[.]"))[1]
        Title <- "Plots based analysis of maxima and THF"
        paste0(subtitle, " ", Title, ".zip")  # Must be .zip
      },
      content = function(file) {
        showPageSpinner()
        
        tmpdir <- tempdir()
        
        # Output files for the plots
        plot1_file <- file.path(tmpdir, paste0("RHF_plot", input$extension))
        plot2_file <- file.path(tmpdir, paste0("THF_plot", input$extension))
        plot3_file <- file.path(tmpdir, paste0("NRHF_plot", input$extension))
        
        # Save each plot to its file
        ggsave(
          filename = plot1_file,
          plot = RHFplotTRIOS(reactiveInputs$calculationMinMaxResultsTHF),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        ggsave(
          filename = plot2_file,
          plot = THFplotTRIOS(reactiveInputs$calculationMinMaxResultsTHF),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        ggsave(
          filename = plot3_file,
          plot = NRHFplotTRIOS(reactiveInputs$calculationMinMaxResultsTHF),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        # Bundle into a zip file
        zip(zipfile = file, files = c(plot1_file, plot2_file, plot3_file), 
            flags = "-j")
        
        hidePageSpinner()
        
      }
    )
    
    output$MaxDSCAnalysisDownload <- downloadHandler(
      filename = function() {
        subtitle <- unlist(strsplit(reactiveInputs$fileName, "[.]"))[1]
        Title <- "Plots based on analysis of maxima and DSC"
        paste0(subtitle, " ", Title, ".zip")  # Must be .zip
      },
      content = function(file) {
        showPageSpinner()
        
        tmpdir <- tempdir()
        
        # Output files for the plots
        plot1_file <- file.path(tmpdir, "RHF_plot.png")
        plot2_file <- file.path(tmpdir, "THF_plot.png")
        plot3_file <- file.path(tmpdir, "NRHF_plot.png")
        
        # Save each plot to its file
        ggsave(
          filename = plot1_file,
          plot = RHFplotDSC(reactiveInputs$calculationMinMaxResultsDSC),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        ggsave(
          filename = plot2_file,
          plot = THFplotDSC(reactiveInputs$calculationMinMaxResultsDSC),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        ggsave(
          filename = plot3_file,
          plot = NRHFplotDSC(reactiveInputs$calculationMinMaxResultsDSC),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        # Bundle into a zip file
        zip(zipfile = file, files = c(plot1_file, plot2_file, plot3_file), 
            flags = "-j")
        
        hidePageSpinner()
      }
    )
    
    
    # Render the plot using the reactive sample_results
    output$plot <- renderPlotly({
      req(reactiveInputs$calculate_fft)
      
      plot_obj <- switch(input$plot_choice,
                         "RHF" = RHFplot(reactiveInputs$calculationMinMaxResults),
                         "THF" = THFplot(reactiveInputs$calculationMinMaxResults),
                         "NRHF" = NRHFplot(reactiveInputs$calculationMinMaxResults),
                         "THF FT" = THFplotFT(reactiveInputs$calculate_fft),
                         "RHF FT" = RHFplotFT(reactiveInputs$calculate_fft),
                         "NRHF FT" = NRHFplotFT(reactiveInputs$calculate_fft),
                         "THF TRIOS" = {
                           validate(need(input$HFcalcextra, "Please check 'Do you want to calculate RHF and NRHF using the THF as well?' to show THF TRIOS plots."))
                           THFplotTRIOS(reactiveInputs$calculationMinMaxResultsTHF)
                         },
                         "RHF based on THF TRIOS" = {
                           validate(need(input$HFcalcextra, "Please check 'Do you want to calculate RHF and NRHF using the THF as well?' to show RHF based on THF TRIOS plots."))
                           RHFplotTRIOS(reactiveInputs$calculationMinMaxResultsTHF)
                         },
                         "NRHF based on THF TRIOS" = {
                           validate(need(input$HFcalcextra, "Please check 'Do you want to calculate RHF and NRHF using the THF as well?' to show NRHF based on THF TRIOS plots."))
                           NRHFplotTRIOS(reactiveInputs$calculationMinMaxResultsTHF)
                         },
                         "THF unmodulated DSC" = {
                           validate(need(input$compare, "Please check 'Do you want to compare with unmodulated DSC?' to show THF unmodulated DSC plots."))
                           THFplotDSC(reactiveInputs$calculationMinMaxResultsDSC)
                         },
                         "RHF based on unmodulated DSC" = {
                           validate(need(input$compare, "Please check 'Do you want to compare with unmodulated DSC?' to show RHF unmodulated DSC plots."))
                           RHFplotDSC(reactiveInputs$calculationMinMaxResultsDSC)
                         },
                         "NRHF based on unmodulated DSC" = {
                           validate(need(input$compare, "Please check 'Do you want to compare with unmodulated DSC?' to show NRHF unmodulated DSC plots."))
                           NRHFplotDSC(reactiveInputs$calculationMinMaxResultsDSC)
                         }
      )
      
      ggplotly(plot_obj)
      
    })
    
  })
}
  