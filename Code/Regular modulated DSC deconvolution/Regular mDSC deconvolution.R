source("../Regular modulated DSC deconvolution/functions.R")
source("../Regular modulated DSC deconvolution/Libraries.R")
source("../Regular modulated DSC deconvolution/error handling.R")


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
        tagList(
          column(6,
                 textInput(ns("period"), "Period in seconds", "40"),
                 textInput(ns("heating_rate"), "Heating rate in °C/min", "2"),
                 textInput(ns("setAmplitude"), "Temperature amplitude set by user (°C)", "0.212"),
                 fileInput(ns("Excel_mDSC"), "Upload your Excel here"),
                 checkboxInput(ns("sheetask"), "Is your data in the first sheet of your Excel file?", TRUE),
                 conditionalPanel(
                   condition = sprintf("!input['%s']", ns("sheetask")),
                   selectInput(ns("sheet"), "What sheet is it in then?", choices = c("2", "3", "4", "5"))
                 ), 

                 actionButton(ns("analyze"), "Analyze", class = "btn-primary btn-lg")
          ), 
          column(6,
                 checkboxInput(ns("HFcalcextra"), "Do you want to calculate RHF and NRHF using the THF as well?"),
                 checkboxInput(ns("compare"), "Do you want to compare with unmodulated DSC?"),
                 conditionalPanel(
                   condition = sprintf("input['%s']", ns("compare")),
                   fileInput(ns("Excel_DSC"), "Upload your Excel here"),
                   checkboxInput(ns("sheetask2"), "Is your data in the first sheet of your Excel file?", TRUE),
                   conditionalPanel(
                     condition = sprintf("!input['%s']", ns("sheetask2")),
                     selectInput(ns("sheet2"), "What sheet is it in then?", choices = c("2", "3", "4", "5"))
                   ),                    
                 ),
                 mainPanel(
                   div(
                     class = "error-text",
                     textOutput(ns("errorMessage"))
                   )    
                 ) 
          )
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
                                    "THF TRIOS", "RHF based on THF TRIOS", "NRHF based on THF TRIOS", 
                                    "THF unmodulated DSC", "RHF based on unmodulated DSC", "NRHF based on unmodulated DSC"), 
                        selected = "THF"),
          ))
        ),
        fluidRow(
          column(12, plotlyOutput(ns("plot"), height = "90vh"))
        ), 
      )
    )
  )
}

normal_mDSC_server <- function(id) {
  moduleServer(id, function(input, output, session) {

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
      reactive_inputs$fileName <- input$Excel_in$name
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
      reactive_inputs$RHFdf <- RHFdf
      
      
      if(reactive_inputs$HFcalcextra) {
        RHFdf2 <- HFcalc2(extrema_df, reactive_inputs$heat_amplitude, reactive_inputs$heating_rate, d)
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
      
      hidePageSpinner()

    })
    
    # Render the plot using the reactive sample_results
    output$plot <- renderPlotly({
      req(reactive_inputs$fftCalc)
      source("../Regular modulated DSC deconvolution/plots.R")
      
      plot_obj <- switch(input$plot_choice,
                         "RHF" = RHFplot(reactive_inputs$RHFdf),
                         "THF" = THFplot(reactive_inputs$RHFdf),
                         "NRHF" = NRHFplot(reactive_inputs$RHFdf),
                         "THF FT" = THFplotFT(reactive_inputs$fftCalc),
                         "RHF FT" = RHFplotFT(reactive_inputs$fftCalc),
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

  