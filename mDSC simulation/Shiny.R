library(shiny)
library(plotly)

ui <- navbarPage(
  id = "tabs",
  title = "mDSC deconvolution procedure simulator",
  tabPanel(
    id = "graphs",
    title = "Graphs",
    icon = icon("chart-line", class = "fa-solid"),
    fluidPage(
      titlePanel("Output graphs"),
      fluidRow(
        column(12, wellPanel(
          selectInput("plot_choice", "Select Plot:", 
                      choices = c("MHF", "Overlay", "THF", "RHF","NRHF"), 
                      selected = "MHF"),
        ))
      ),
      fluidRow(
        column(12, plotlyOutput("plot", height = "90vh"))
      )
    )
  )
)


server <- function(input, output, session) {
  # Render the plot using the reactive sample_results
  output$plot <- renderPlotly({
    req(resampled_points)  # Ensure results exist

    plot_obj <- switch(input$plot_choice,
                       "MHF" = MHFplots(resampled_points),
                       "Overlay" = overlayplot(resampled_points),
                       "THF" = smoothedTHFplot(resampled_points),
                       "RHF" = smoothedRHFplot(resampled_points),
                       "NRHF" = smoothedNRHFplot(resampled_points))
    
    ggplotly(plot_obj, tooltip = c("x", "y", "text"))
  })
}

shinyApp(ui = ui, server = server)

