tagList(
  titlePanel(
    tags$p(
      style = "text-align: center; color: #3c8dbc;",
      "Analysis settings: what runs did you perform?"),
    windowTitle = "Thermal Data Analyzer"),
  tags$br(), 
  tags$br(), 
  tags$p(
    style = "font-size: medium; text-align: left; color: #333;",
    "Welcome to the Thermal Data Analyzer! It will help you analyse your thermal data by taking word documents as input and spitting out the cleaned up and analysed data in  Excel tables. If you don't know how to start, navigate to the \"tutorial\" tab in the menu above. This page requires you to tell the app what your analysis looks like. Unsure about what you did exactly? Check the word documents! Press the \"next tab\" button once you're done."
  ),
  tags$br(),
  
  fluidRow(
    column(
      6,
      selectInput(
        ns("pans"),
        "How many pans did you run?",
        choices = c("2", "3", "4", "5"),
        selected = "1"), 
      selectInput(
        ns("heatingCycle"),
        "How many heating cycles did you run?",
        choices = c("1", "2", "3", "4"),
        selected= "1"),  
      uiOutput(ns("tablesDropdowns")),
      checkboxInput(
        ns("keepTitles"),
        "Are you happy with the titles you used for your table columns in your word documents? If no, uncheck this box.",
        value = TRUE),
      uiOutput(ns("coltitlesInput")),
    ),
    column(
      6,
      checkboxInput(
        ns("saveRaw"),
        "Do you want to save the raw data in an excel file too?",
        value = FALSE),
      checkboxInput(
        ns("round1"),
        "Do you want to have a different number of decimals than 2 in the final analysis output (raw data is never rounded)? If left unchecked, the program will round everything to 2 decimals.",
        value = FALSE),
      
      conditionalPanel(
        condition = sprintf("input['%s']", ns("saveRaw")),
        textInput(ns("excelName2"), "What should the excel sheet be called?")
      ),
      conditionalPanel(
        condition = sprintf("input['%s']", ns("round1")),
        selectInput(ns("round"), "To how many decimals...", choices = c("0", "1", "2", "3", "4", "5"))
      ),
      
    )
  ),
  fluidRow(
    column(
      12,
      mainPanel(
        div(
          actionButton(ns("Next"), "Next tab", class = "btn btn-primary btn-lg"),
          style = "text-align: right;"
        )
      )
    )
  ),
)