fluidPage(
  titlePanel(
    tags$p(style = "text-align: center; color: #3c8dbc;", "Input and output files"),
    windowTitle = "DSC Data Analyzer"),
  tags$br(),
  tags$br(), 
  tags$p(
    style = "font-size: medium; text-align: left; color: #333;",
    "You're almost good to go. In this tab, please upload the word documents to analyse. You also need to specify where the file is to be saved, and you'll need to give everything a suitable name. When you're all set, press \"Run Analysis\". Note that you can group several analyses in the same Excel file by simply changing the sheet name without closing the a (don't forget the raw data sheet as well in case you have one)."
  ),
  tags$br(),
  
  fluidRow(
    column(
      6,
      fileInput(
        ns("files"),
        "What files do you want to analyze?",
        multiple = TRUE),
      textInput(
        ns("outputPath"),
        "Output Folder Path",
        placeholder = "e.g., C:/Users/YourUsername/Documents"),
    ),
    column(
      6,
      textInput(ns("excelName"), "Output Excel File Name"),
      textInput(ns("excelSheet"), "Output Sheet Name"),
      textInput(ns("sampleName"), "Sample Name"),
    )
  ),
  tags$br(),
  
  fluidRow(
    column(
      12,
      mainPanel(
        # actionButton(ns("runAnalysis"), "Run Analysis"),
        downloadButton(ns("excelDownload"), "Download Results")
      ),
    )
  ),
  tags$br(),
  
  
  fluidRow(
    mainPanel(
      div(
        id = ns("analysisMessageContainer"),
        textOutput(ns("analysisMessage")
        )     
      )
    ),
    
    fluidRow(
      mainPanel(
        div(
          id = ns("errorMessageContainer"),
          textOutput("errorMessage")
        )    
      )
    ),
  )
)