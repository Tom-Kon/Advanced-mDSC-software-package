# shiny_app.R

library(shiny)
library(ggplot2)
library(plotly)

runDSCShinyApp <- function(sample_results, average_heat_flow_per_pattern) {
  
  ui <- fluidPage(
    titlePanel("Interactive DSC Analysis"),
    
    fluidRow(
      column(12, 
             wellPanel(
               selectInput("plot_choice", "Select Plot:", 
                           choices = c("NRHF", "RHF", "Datasteps", "Manual RHF", "RHF and NRHF"), 
                           selected = "RHF")
             )
      )
    ),
    
    fluidRow(
      column(12, 
             plotlyOutput("plot", height = "90vh")  # 90% of the screen height
      )
    )
  )
  
  server <- function(input, output) {
    output$plot <- renderPlotly({
      plot_obj <- switch(input$plot_choice,
                         "NRHF" = NRHF_plot(sample_results),
                         "RHF" = RHF_plot(sample_results),
                         "Datasteps" = Datasteps_plot(data_steps_cleaned_4),
                         "Manual RHF" = Manual_RHF_plot(average_heat_flow_per_pattern),
                         "RHF and NRHF" = RHF_NRHF_plot(sample_results))
      
      ggplotly(plot_obj, tooltip = c("x", "y", "text"))  # Keep time, heat flow, and TRef!
    })
    
  }
  
  shinyApp(ui, server)
}
  