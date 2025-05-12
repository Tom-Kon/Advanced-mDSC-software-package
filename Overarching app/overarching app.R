library(shiny)
library(shiny.router)

setwd("C:/Users/u0155764/OneDrive - KU Leuven/Tom Konings/Software/DSC full app (git clone)/mDSC_apps/Overarching app")


# Source the module
source("../mDSC simulation/modularized mDSC simulation main menu.R")
source("../Quasi-isothermal mDSC/modularized_quasiisothermalmDSC_main.R")

home_page <- div(
  titlePanel("Home page"),
  p("Welcome to the mDSC simulator app."),
  tags$div(
    style = "margin-top: 30px;",
    tags$a(
      href = route_link("mDScSim"),
      class = "btn btn-primary btn-lg",
      "Launch mDSC Simulator"
    ), 
    tags$a(
      href = route_link("quasiisothermal"),
      class = "btn btn-primary btn-lg",
      "Launch quasi-isothermal mDSC analyser"
    )
  )
)

ui <- fluidPage(
  router_ui(
    route("/", home_page),
    route("mDScSim", mdsc_sim_ui("mDScSim")),
    route("quasiisothermal", quasiIsotherm_ui("quasiisothermal")),
  ),
)

# Define the server logic
server <- function(input, output, session) {
  # Start the router server logic
  router_server(root_page = "/")
  mdsc_sim_server("mDScSim")
  mdsc_quasiIso_server("quasiisothermal")

}

# Run the Shiny app
shinyApp(ui, server)
