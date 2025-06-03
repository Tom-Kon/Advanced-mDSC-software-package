setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("router_setup.R")


home_page <- div(
  titlePanel("Home page"),
  p("Welcome to the mDSC simulator app."),
    tags$style(HTML("
      .card {
        background-color: #e6f0fa;
        padding: 20px;
        margin: 10px;
        border-radius: 10px;
        box-shadow: 2px 2px 5px rgba(0,0,0,0.1);
        height: 100%;
      }
      .card h4 {
        margin-top: 0;
      }
      .card-content {
        font-size: 14px;
      }
      .btn-title {
        margin-bottom: 15px;
        width: 100%;
        text-align: left;
        font-weight: bold;
      }
    ")
  ),
  fluidRow(
    column(6,
           div(class = "card",
               tags$a(
                 class = "btn btn-primary btn-lg",
                 href = route_link("mDScSim"),
                 "Go to modulated DSC deconvolution simulation"
               ),
               div(
                 class = "card-content",
                 "Lorem ipsum lorem ipsum."
               )
           )
    ),
    column(6,
           div(class = "card",
               tags$a(
                 class = "btn btn-primary btn-lg",
                 href = route_link("quasiisothermal"),
                 "Go to Quasi-Isothermal modulated DSC deconvolution"
               ),
               div(
                 class = "card-content",
                 "Lorem ipsum lorem ipsum."
               )
           )
    )
  ),
  fluidRow(
    column(6,
           div(class = "card",
               tags$a(
                 class = "btn btn-primary btn-lg",
                 href = route_link("analyzer"),
                 "Go to DSC descriptive statistics"
               ),
               div(
                 class = "card-content",
                 "Lorem ipsum lorem ipsum."
               )
           )
    ),
    column(6,
           div(class = "card",
               tags$a(
                 class = "btn btn-primary btn-lg",
                 href = route_link("normalmDSC"),
                 "Go to regular modulated DSC deconvolution"
               ),
               div(
                 class = "card-content",
                 "Lorem ipsum lorem ipsum."
               )
           )
    )
  ),
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
