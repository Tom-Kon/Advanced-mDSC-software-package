library(readxl)
library(ggplot2)
library(dplyr)
library(shiny)
library(plotly)

source("../normal mDSC/functions.R")

normal_mDSC_ui <- function(id) {
  ns <- NS(id)
  navbarPage(
    id = ns("tabs"),
    title = "Regular mDSC data analyzer",
    inverse = FALSE,
    
    tabPanel(
      title = "Parameter Input",
      id = ns("paraminput"),
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
                 checkboxInput(ns("compare"), "Do you want to compare with regular DSC?"),
                 conditionalPanel(
                   condition = sprintf("input['%s']", ns("compare")),
                   fileInput(ns("Excel_DSC"), "Upload your Excel here"),
                   checkboxInput(ns("sheetask2"), "Is your data in the first sheet of your Excel file?", TRUE),
                   conditionalPanel(
                     condition = sprintf("!input['%s']", ns("sheetask2")),
                     selectInput(ns("sheet2"), "What sheet is it in then?", choices = c("2", "3", "4", "5"))
                   ),                    
                 ),
          )
        )
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
                        choices = c("THF", "RHF","NRHF"), 
                        selected = "THF"),
          ))
        ),
        fluidRow(
          column(12, plotlyOutput(ns("plot"), height = "90vh"))
        )
      )
    )
  )
}

normal_mDSC_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Create a reactiveValues object to store inputs
    reactive_inputs <- reactiveValues()

    observeEvent(input$analyze,{
      
      # Extraction
      reactive_inputs$period <- as.numeric(input$period)
      reactive_inputs$heating_rate <- as.numeric(input$heating_rate)
      reactive_inputs$setAmplitude <- as.numeric(input$setAmplitude)
      reactive_inputs$heat_amplitude <- reactive_inputs$setAmplitude*2*pi/reactive_inputs$period
      reactive_inputs$compare <- input$compare
      reactive_inputs$ExcelmDSC <- input$Excel_mDSC$datapath
      reactive_inputs$fileName <- input$Excel_in$name
      
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
      d <- excel_cleaner(reactive_inputs$ExcelmDSC, reactive_inputs$sheet)
      
      #Apply functions
      extrema_df <-locate_extrema_manual(d$modHeatFlow, d$time, d$temperature)
      counts <- count_extrema(extrema_df)
      RHFdf <- HFcalc(extrema_df, reactive_inputs$heat_amplitude, reactive_inputs$heating_rate)
      reactive_inputs$RHFdf <- RHFdf
      
      #Compare with DSC
      if(reactive_inputs$compare == TRUE) {
        
        reactive_inputs$ExcelDSC <- input$Excel_DSC$datapath
        DSC <- excel_cleaner(reactive_inputs$ExcelDSC, reactive_inputs$sheet2)
        
        # Match maxima in the modulated THF to points in the deconvoluted THF
        matchingDSCmDSC <- RHFdf %>%
          rowwise() %>%
          mutate(
            closest_index = which.min(abs(DSC$temperature - meantemp)),
            heat_flowDSC = DSC$heaFlow[closest_index],
            temperatureDSC = DSC$temperature[closest_index]
          ) %>%
          ungroup()
        
        DSCdf <- data.frame(temperatureDSC, heat_flowDSC)
      }
      
      
      print("done!")
      source("../normal mDSC/plots.R")
      
    })
    
    # Render the plot using the reactive sample_results
    output$plot <- renderPlotly({
      
      plot_obj <- switch(input$plot_choice,
                         "RHF" = RHFplot(reactive_inputs$RHFdf),
                         "THF" = THFplot(reactive_inputs$RHFdf),
                         "NRHF" = NRHFplot(reactive_inputs$RHFdf))
      
      ggplotly(plot_obj)
      
    })
    
  })
}

  