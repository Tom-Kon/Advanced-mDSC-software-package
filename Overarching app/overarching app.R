library(shiny)
library(shiny.router)

# Source the module
source("../mDSC simulation/modularized mDSC simulation main menu.R")

home_page <- div(
  titlePanel("Home page"),
  p("Welcome to the mDSC simulator app."),
  tags$div(
    style = "margin-top: 30px;",
    tags$a(
      href = route_link("another"),
      class = "btn btn-primary btn-lg",
      "Launch mDSC Simulator"
    )
  )
)

ui <- fluidPage(
  router_ui(
    route("/", home_page),
    route("another", mdsc_ui("another"))
  ),
)

# Define the server logic
server <- function(input, output, session) {
  # Start the router server logic
  router_server(root_page = "/")
  mdsc_server("another")

}

# Run the Shiny app
shinyApp(ui, server)
