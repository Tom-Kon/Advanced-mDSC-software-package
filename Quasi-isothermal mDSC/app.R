library(shiny)
library(shinythemes)

source("configapp.R")  # Source the external function

ui <- fluidPage(
  theme = shinytheme("lumen"),  # Apply Lumen theme
  
  h1("My Shiny App"),
  
  # Main content with sourced UI function
  configUI()
)      

server <- function(input, output) {}

shinyApp(ui = ui, server = server)
