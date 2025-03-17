library(shiny)
library(shinythemes)
library(bslib)

source("libraries.R")


options(shiny.maxRequestSize = 100 * 1024^2)  # 100 MB limit

source("configapp.R")  # Source the external function

ui <- navbarPage(
  id = "tabs",
  title = "Quasi-isothermal mDSC data analyzer",
  # theme = bs_theme(preset = "lumen"),
  inverse = FALSE,  # if you want a dark navbar style; remove if not needed
  tabPanel(
    title = "Input",
    id = "input",
    icon = icon("gears", class = "fa-solid"),
    fluidPage(
      configUI1(), configUI2(), configUI3()
    )
  ),
  tabPanel(
    id = "graphs",
    title = "Graphs",
    icon = icon("chart-line", class = "fa-solid"),
    fluidPage(
      titlePanel("Output graphs"),
      fluidRow(
        column(12, wellPanel(
          selectInput("plot_choice", "Select Plot:", 
                      choices = c("NRHF", "RHF", "Manual RHF", "RHF and NRHF","Maxima and minima 1", "Maxima and minima prefinal", "Maxima and minima final", 
                                  "Original data", "First cleaned up data", "Prefinal cleaned up data", "Final data used for analysis"), 
                      selected = "RHF"),
          fluidRow(  # Use a fluidRow to make sure buttons are side by side
            column(6,
                   HTML("<br>"),
                   actionButton("recalc", "Recalculate with different number of modulations")
                   ),
            column(6, textInput("modulations_back_in_new", "New number of modulations"))
          )
        ))
      ),
      fluidRow(
        column(12, plotlyOutput("plot", height = "90vh"))
      )
    )
  )

)


server <- function(input, output, session) {
  
  # Create a reactive value for sample_results
  sample_results_reac <- reactiveVal(NULL)
  
  # Observe changes in checkboxes and update values
  observe({
    plot_datasteps4 <- input$plot_datasteps4
    saveplotdatasteps4 <- input$saveplotdatasteps4
    saveNRHFplot <- input$saveNRHFplot
    saveRHFplot <- input$saveRHFplot
    savemanualRHFplot <- input$savemanualRHFplot
    saveDatasteps4 <- input$saveDatasteps4
    saveExtremadf3 <- input$saveExtremadf3
    saveSummaryFT <- input$saveSummaryFT
  })
  

  
  observeEvent(input$calculate, {
    

    period <<- eval(parse(text = input$period_in))
    stepSize <<- eval(parse(text = input$step_size_in))
    isothermLength <<- eval(parse(text = input$isotherm_length_in))
    startingTemp <<- eval(parse(text = input$starting_temp_in))
    modulations_back <<- eval(parse(text = input$modulations_back_in))
    setAmplitude <<- eval(parse(text = input$setAmplitude_in))
    Excel <<- input$Excel_in$datapath
    fileName <<- input$Excel_in$name
    
    source("configapp-nonuser inputs.R")
    source("Processing and cleaning overall function.R")
    source("plots.R")
    
    
    sample_results <<- processDSC(file = fileName, Excel = Excel, export = TRUE,
                                 rangesmin = rangesmin, rangesmax = rangesmax, 
                                 starting_temp = starting_temp, step_size = step_size,
                                 temp_margin_first_cleanup = temp_margin_first_cleanup,
                                 modulations_back = modulations_back, period = period,
                                 setAmplitude = setAmplitude)
    
    sample_results_reac(sample_results)
    })
  
  observeEvent(input$recalc, {
    source("quickly recalculate for different mods.R")
    modulations_back <<- eval(parse(text = input$modulations_back_in_new))
    sample_results <<- processDSCrecalc(sample_results = sample_results, modulations_back = modulations_back, period = period, setAmplitude = setAmplitude)
    sample_results_reac(sample_results)
  
  })
  
  # Render the plot using the reactive sample_results
  output$plot <- renderPlotly({
    # This will re-run whenever sample_results or input$plot_choice changes
    res <- sample_results_reac()
    req(res)  # Ensure res is available
    
    plot_obj <- switch(input$plot_choice,
                       "NRHF" = NRHF_plot(res$ft_averages),
                       "RHF" = RHF_plot(res$ft_averages),
                       "Manual RHF" = Manual_RHF_plot(res$average_heat_flow_per_pattern),
                       "RHF and NRHF" = RHF_NRHF_plot(res$ft_averages),
                       "Maxima and minima 1" = Maxima_minima(res$`Extrema df1`),
                       "Maxima and minima prefinal" = Maxima_minima_1(res$`Extrema df2`),
                       "Maxima and minima final" =  Maxima_minima_2(res$`Extrema df3`),
                       "Original data" = Original_data(res$`Original data`),
                       "First cleaned up data" = Datasteps_plot_1(res$d_steps_cleaned),
                       "Prefinal cleaned up data" = Datasteps_plot_prefinal(res$d_steps_cleaned_2),
                       "Final data used for analysis" = Datasteps_plot_final(res$d_steps_cleaned_3))
    
    ggplotly(plot_obj, tooltip = c("x", "y", "text"))
  })

  
}

shinyApp(ui = ui, server = server)

