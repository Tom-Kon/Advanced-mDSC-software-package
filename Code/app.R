if (requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()) {
  tryCatch({
    setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  }, error = function(e) {
    setwd(normalizePath("."))
  })
} else {
  setwd(normalizePath("."))
}

source("overarching_app/router_setup.R")


home_page <- div(
  tags$div(
    navbarPage(
      title = "Home page",
      
      tabPanel(
        title = "Select a sub-app",
        icon = icon("hand-pointer", class = "fa-solid"),
        tags$head(
          tags$script(src = "goBack.js"),
          tags$style(source("overarching_app/html_styling.R"))
        ),
        
        fluidPage(
          tags$div(
            style = "font-weight: bold",
            p("Welcome to the mDSC assistant app! If you are not familiar with this software or with mDSC in general, please navigate to the tutorial tab above. Otherwise, please click one of the navigation buttons below. To return to the homepage, click the rotating arrow at the top of each page."),
            class = "card-content",
          ),  
          HTML("<br>"),
          
          fluidRow(
            column(6,
                   div(class = "card",
                       tags$a(
                         class = "btn btn-primary btn-lg",
                         href = route_link("mDScSim"),
                         "Go to modulated DSC deconvolution simulation"
                       ),
                       HTML("<br>", "<br>"),
                       div(
                         class = "card-content",
                         "This app generates a modulated DSC signal based on known data and simulates its deconvolution based on different parameters. If you want to know how your output changes for a different heating rate, try it here!"
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
                       HTML("<br>", "<br>"),
                       div(
                         class = "card-content",
                         "This app helps with analyzing quasi-isothermal modulated DSC data. If you are not familiar with this technique, it is explained in this app's tutorial. To use this app, you need to have an Excel file with exported mDSC data."
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
                       HTML("<br>", "<br>"),
                       div(
                         class = "card-content",
                         "If you have analyzed your data in TRIOS and exported the resulting files as word documents, this software helps with rapidly calculating means, standard deviations, and relative standard deviations."
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
                       HTML("<br>", "<br>"),
                       div(
                         class = "card-content",
                         "For certain analyses, you might be worried that the deconvolution procedure commonly used in DSC could introduce artifacts. This app recalculates the different signals in different ways in order to let you view the output differently. To use this application, you need an Excel file with exported DSC data."
                       )
                   )
            )
          ),
          
        )
      ), 
      
      tabPanel(
        title = "Tutorial",
        icon = icon("book", class = "fa-solid"),
        fluidPage(
          withMathJax(
            includeMarkdown("overarching_app/tutorial/overarching_app_tutorial.md")
          )
        )
        
      ),
      
    )
  ),
  
)

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$script(src = "goBack.js"),
    tags$style(HTML("
      #goBack {
        position: fixed;
        top: 10px;
        right: 20px;
        z-index: 9999;
      }
    "))
  ),
  actionButton("goBack", "← Back to Home",
               style = "position: fixed; top: 10px; right: 20px; z-index: 9999; display: block; color: white; background: transparent; border: none;"),
  
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
  
  observeEvent(input$goBack, {
    session$sendCustomMessage("goBack", list())
  })
  
  # Start the router server logic
  router_server(root_page = "/")
  mdsc_sim_server("mDScSim")
  mdsc_quasiIso_server("quasiisothermal")
  mdsc_analyzer_server("analyzer")
  normal_mDSC_server("normalmDSC")
}

# Run the Shiny app
shinyApp(ui, server)
