library(shiny)
library(shiny.router)
library(rstudioapi)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Source the module
source("../mDSC simulation/modularized mDSC simulation main menu.R")
source("../Quasi-isothermal mDSC/modularized_quasiisothermalmDSC_main.R")
source("../mDSC data analyzer/modularized_generaldatanalyzer.R")
source("../normal mDSC/Amplitude extractor.R")


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
    ),
    tags$a(
      href = route_link("analyzer"),
      class = "btn btn-primary btn-lg",
      "Launch mDSC data analyser"
    ),
    tags$a(
      href = route_link("normalmDSC"),
      class = "btn btn-primary btn-lg",
      "Launch normal mDSC analyser"
    )
  )
)

ui <- fluidPage(
  router_ui(
    route("/", home_page),
    route("mDScSim", mdsc_sim_ui("mDScSim")),
    route("quasiisothermal", quasiIsotherm_ui("quasiisothermal")),
    route("analyzer", mdsc_analyzer_ui("analyzer")),
    route("normalmDSC", normal_mDSC_ui("normalmDSC"))
  )
)

# Define the server logic
server <- function(input, output, session) {
  # Start the router server logic
  router_server(root_page = "/")
  mdsc_sim_server("mDScSim")
  mdsc_quasiIso_server("quasiisothermal")
  mdsc_analyzer_server("analyzer")
  normal_mDSC_server("normalmDSC")
}

# Run the Shiny app
shinyApp(ui, server)
