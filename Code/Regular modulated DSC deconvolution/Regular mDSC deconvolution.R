source("Regular modulated DSC deconvolution/functions.R")
source("Regular modulated DSC deconvolution/Libraries.R")
source("Regular modulated DSC deconvolution/error handling.R")
source("Regular modulated DSC deconvolution/downloads.R")
source("Regular modulated DSC deconvolution/plots regular mDSC.R")



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
                       textInput(ns("heating_rate"), "Heating rate in °C/min", "2")),
                   div(style = "width: 100%;",
                       textInput(ns("setAmplitude"), "Temperature amplitude set by user (°C)", "0.212"))
            )
          ),
          column(6,
                 checkboxInput(ns("HFcalcextra"), "Do you want to calculate RHF and NRHF using the THF as well?"),
                 checkboxInput(ns("compare"), "Do you want to compare with unmodulated DSC?"),
                 fileInput(ns("Excel_mDSC"), "Upload your Excel here"),
                 checkboxInput(ns("sheetask"), "Is your data in the first sheet of your Excel file?", TRUE),
                 conditionalPanel(
                   condition = sprintf("!input['%s']", ns("sheetask")),
                   selectInput(ns("sheet"), "What sheet is it in then?", choices = c("2", "3", "4", "5"))
                 ),
                 conditionalPanel(
                   condition = sprintf("input['%s']", ns("compare")),
                   fileInput(ns("Excel_DSC"), "Upload your Excel here"),
                   checkboxInput(ns("sheetask2"), "Is your data in the first sheet of your Excel file?", TRUE),
                   conditionalPanel(
                     condition = sprintf("!input['%s']", ns("sheetask2")),
                     selectInput(ns("sheet2"), "What sheet is it in then?", choices = c("2", "3", "4", "5"))
                   )
                 )
          )
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
                        choices = c("THF", "RHF","NRHF", "THF FT", "RHF FT", "NRHF FT",
                                    "THF TRIOS", "RHF based on THF TRIOS", "NRHF based on THF TRIOS", 
                                    "THF unmodulated DSC", "RHF based on unmodulated DSC", "NRHF based on unmodulated DSC"), 
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
            selectInput(ns("extension"), "What should the plot's extension be?", c(".png", ".jpg", ".tiff")), 
            textInput(ns("exportDpi"), "What should the plot dpi be?", value= 600),
            textInput(ns("exportWidth"), "What should the plot width be in cm?",  value= 20),
            textInput(ns("exportHeight"), "What should the plot height be in cm?", value= 20)
          ),
          
          mainPanel(
            fluidRow(
              tags$div(
                style = "text-align: center;",
                downloadButton(ns("excelDownload"), "Download the Excel sheet with all the analyses", class = "btn-primary btn-lg")
              )
            ),
            br(), br(),
            fluidRow(
              tags$div(
                style = "text-align: center;",
                downloadButton(ns("MaxMinAnalysisDownload"), "Download the 3 plots (THF, RHF, NRHF) based on the analysis of the maxima and minima", class = "btn-primary btn-lg")
              )
            ),
            br(), br(),
            fluidRow(
              tags$div(
                style = "text-align: center;",
                downloadButton(ns("FTAnalysisDownload"), "Download the 3 plots (THF, RHF, NRHF) based on the FT analysis", class = "btn-primary btn-lg")
              )
            ),
            br(), br(),
            fluidRow(
              tags$div(
                style = "text-align: center;",
                downloadButton(ns("MaxTHFAnalysisDownload"), "Download the 3 plots (THF, RHF, NRHF) based on the analysis of the maxima and the THF", class = "btn-primary btn-lg")
              )
            ),
            br(), br(),
            fluidRow(
              tags$div(
                style = "text-align: center;",
                downloadButton(ns("MaxDSCAnalysisDownload"), "Download the 3 plots (THF, RHF, NRHF) based on the analysis of the maxima and the unmodulated DSC", class = "btn-primary btn-lg")
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
          includeMarkdown("Regular modulated DSC deconvolution/Regular mDSC tutorial.md")
        )
      )
    )
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
    reactive_inputs <- reactiveValues()

    observeEvent(input$analyze,{
      showPageSpinner()
      
      # Extraction
      reactive_inputs$period <- as.numeric(input$period)
      reactive_inputs$heating_rate <- as.numeric(input$heating_rate)
      reactive_inputs$setAmplitude <- as.numeric(input$setAmplitude)
      reactive_inputs$compare <- input$compare
      reactive_inputs$ExcelmDSC <- input$Excel_mDSC$datapath
      reactive_inputs$fileName <- as.character(input$Excel_mDSC$name)
      reactive_inputs$HFcalcextra <- input$HFcalcextra
      reactive_inputs$compare <- input$compare
      
      
      if(input$sheetask) {
        reactive_inputs$sheet <- 1
      } else {
        reactive_inputs$sheet <- input$sheet
      }
      
      if(input$sheetask2) {
        reactive_inputs$sheet2 <- 1
      } else {
        reactive_inputs$sheet2 <- input$sheet2
      }
      
      
      # Read data from Excel sheets
      import <- 1
      msg <- NULL
      error_handling(reactive_inputs, import)
      
      
      output$errorMessage <- renderText({
        if (!is.null(msg)) {
          msg
        } else if (!is.null(attr(d, "comment"))) {
          (attr(d, "comment"))
        } else {""
        }
      })
      
      if (!is.null(msg)) hidePageSpinner()
      
      reactive_inputs$heat_amplitude <- reactive_inputs$setAmplitude*2*pi/reactive_inputs$period
      
      d <- excel_cleaner(reactive_inputs$ExcelmDSC, reactive_inputs$sheet, reactive_inputs$HFcalcextra, reactive_inputs$compare,import)
      
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
      extrema_df <-locate_extrema_manual(d$modHeatFlow, d$time, d$temperature)
      counts <- count_extrema(extrema_df)
      RHFdf <- HFcalc(extrema_df, reactive_inputs$heat_amplitude, reactive_inputs$heating_rate)
      
      reactive_inputs$extrema_df <- extrema_df
      reactive_inputs$RHFdf <- RHFdf
      
      
      if(reactive_inputs$HFcalcextra) {
        RHFdf2 <- HFcalc2(reactive_inputs$extrema_df, reactive_inputs$heat_amplitude, reactive_inputs$heating_rate, d)
        reactive_inputs$RHFdf2 <- RHFdf2
      }



      #Apply functions for FT calculation
      reactive_inputs$fftCalc <- fftCalc(reactive_inputs$period, d, reactive_inputs$heat_amplitude, reactive_inputs$heating_rate)
      
      
      #Compare with DSC
      if(reactive_inputs$compare) {
        reactive_inputs$ExcelDSC <- input$Excel_DSC$datapath
        import <- 2
        
        error_handling(reactive_inputs, import)
        
        output$errorMessage <- renderText({
          if (!is.null(msg)) {
            msg
          } else if (!is.null(attr(d, "comment"))) {
            (attr(d, "comment"))
          } else {""
          }
        })
        
        if (!is.null(msg)) hidePageSpinner()
        
        DSC <- excel_cleaner(reactive_inputs$ExcelmDSC, reactive_inputs$sheet, reactive_inputs$HFcalcextra, reactive_inputs$compare,import)
        
        
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
        
        
        DSCdf <- funcMatchingDSCMDSC(DSC, extrema_df, reactive_inputs$heat_amplitude, reactive_inputs$heating_rate)
        reactive_inputs$DSCdf <- DSCdf
   
      }
      
      enable("excelDownload")
      enable("MaxMinAnalysisDownload")
      enable("FTAnalysisDownload")
      toggleState(id = "MaxTHFAnalysisDownload", condition = reactive_inputs$HFcalcextra)
      toggleState(id = "MaxDSCAnalysisDownload", condition = reactive_inputs$compare)
      
      hidePageSpinner()

    })
    
    output$excelDownload <- downloadHandler(
      filename = function() {
        fileName <- unlist(strsplit(reactive_inputs$fileName, "\\."))[1]
        paste0(fileName, ".xlsx")
      },
      content = function(file) {
        showPageSpinner()
        wb <- downloadExcelRegmDSC(reactive_inputs)
        saveWorkbook(wb, file = file, overwrite = TRUE)
        hidePageSpinner()
        
      }
    )
    
    
    output$MaxMinAnalysisDownload <- downloadHandler(
      filename = function() {
        subtitle <- unlist(strsplit(reactive_inputs$fileName, "[.]"))[1]
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
          plot = RHFplot(reactive_inputs$RHFdf),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        ggsave(
          filename = plot2_file,
          plot = THFplot(reactive_inputs$RHFdf),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        ggsave(
          filename = plot3_file,
          plot = NRHFplot(reactive_inputs$RHFdf),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        # Bundle into a zip file
        zip(zipfile = file, files = c(plot1_file, plot2_file, plot3_file), flags = "-j")
        
        
        hidePageSpinner()
        
      }
    )
    
    output$FTAnalysisDownload <- downloadHandler(
      filename = function() {
        subtitle <- unlist(strsplit(reactive_inputs$fileName, "[.]"))[1]
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
          plot = RHFplotFT(reactive_inputs$fftCalc),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        ggsave(
          filename = plot2_file,
          plot = THFplotFT(reactive_inputs$fftCalc),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        ggsave(
          filename = plot3_file,
          plot = NRHFplotFT(reactive_inputs$fftCalc),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        # Bundle into a zip file
        zip(zipfile = file, files = c(plot1_file, plot2_file, plot3_file), flags = "-j")
        
        hidePageSpinner()
        
      }
    )
    
    
    output$MaxTHFAnalysisDownload <- downloadHandler(
      filename = function() {
        subtitle <- unlist(strsplit(reactive_inputs$fileName, "[.]"))[1]
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
          plot = RHFplotTRIOS(reactive_inputs$RHFdf2),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        ggsave(
          filename = plot2_file,
          plot = THFplotTRIOS(reactive_inputs$RHFdf2),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        ggsave(
          filename = plot3_file,
          plot = NRHFplotTRIOS(reactive_inputs$RHFdf2),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        # Bundle into a zip file
        zip(zipfile = file, files = c(plot1_file, plot2_file, plot3_file), flags = "-j")
        
        hidePageSpinner()
        
      }
    )
    
    output$MaxDSCAnalysisDownload <- downloadHandler(
      filename = function() {
        subtitle <- unlist(strsplit(reactive_inputs$fileName, "[.]"))[1]
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
          plot = RHFplotDSC(reactive_inputs$DSCdf),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        ggsave(
          filename = plot2_file,
          plot = THFplotDSC(reactive_inputs$DSCdf),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        ggsave(
          filename = plot3_file,
          plot = NRHFplotDSC(reactive_inputs$DSCdf),
          dpi = as.numeric(input$exportDpi),
          width = as.numeric(input$exportWidth),
          height = as.numeric(input$exportHeight),
          units = "cm"
        )
        
        # Bundle into a zip file
        zip(zipfile = file, files = c(plot1_file, plot2_file, plot3_file), flags = "-j")
        
        hidePageSpinner()
      }
    )
    
    
    # Render the plot using the reactive sample_results
    output$plot <- renderPlotly({
      req(reactive_inputs$fftCalc)
      
      plot_obj <- switch(input$plot_choice,
                         "RHF" = RHFplot(reactive_inputs$RHFdf),
                         "THF" = THFplot(reactive_inputs$RHFdf),
                         "NRHF" = NRHFplot(reactive_inputs$RHFdf),
                         "THF FT" = THFplotFT(reactive_inputs$fftCalc),
                         "RHF FT" = RHFplotFT(reactive_inputs$fftCalc),
                         "NRHF FT" = NRHFplotFT(reactive_inputs$fftCalc),
                         "THF TRIOS" = {
                           validate(need(input$HFcalcextra, "Please check 'Do you want to calculate RHF and NRHF using the THF as well?' to show THF TRIOS plots."))
                           THFplotTRIOS(reactive_inputs$RHFdf2)
                         },
                         "RHF based on THF TRIOS" = {
                           validate(need(input$HFcalcextra, "Please check 'Do you want to calculate RHF and NRHF using the THF as well?' to show RHF based on THF TRIOS plots."))
                           RHFplotTRIOS(reactive_inputs$RHFdf2)
                         },
                         "NRHF based on THF TRIOS" = {
                           validate(need(input$HFcalcextra, "Please check 'Do you want to calculate RHF and NRHF using the THF as well?' to show NRHF based on THF TRIOS plots."))
                           NRHFplotTRIOS(reactive_inputs$RHFdf2)
                         },
                         "THF unmodulated DSC" = {
                           validate(need(input$compare, "Please check 'Do you want to compare with unmodulated DSC?' to show THF unmodulated DSC plots."))
                           THFplotDSC(reactive_inputs$DSCdf)
                         },
                         "RHF based on unmodulated DSC" = {
                           validate(need(input$compare, "Please check 'Do you want to compare with unmodulated DSC?' to show RHF unmodulated DSC plots."))
                           RHFplotDSC(reactive_inputs$DSCdf)
                         },
                         "NRHF based on unmodulated DSC" = {
                           validate(need(input$compare, "Please check 'Do you want to compare with unmodulated DSC?' to show NRHF unmodulated DSC plots."))
                           NRHFplotDSC(reactive_inputs$DSCdf)
                         }
      )
      
      ggplotly(plot_obj)
      
    })
    
  })
}

  