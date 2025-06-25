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
      textInput(ns("sampleName"), "Sample Name"),
    ),
    column(
      6,
      textInput(ns("excelName"), "Output Excel File Name"),
      textInput(ns("excelSheet"), "Output Sheet Name"),
    )
  ),
  tags$br(),
  
  fluidRow(
    column(6,
           div(style = "text-align:center;",
               actionButton(ns("errorCheck"), HTML("Check your input for errors<br>(this is not optional)"), 
                            class = "btn-primary btn-lg",
                            style = "width: 70%; font-size: 18px; padding: 15px 30px;")
               )
           ),
    column(6,
           div(style = "text-align:center;",
               downloadButton(ns("excelDownload"), "Download Results", 
                            class = "btn-primary btn-lg",
                            style = "width: 70%; font-size: 20px; padding: 15px 30px;")
               ),
           )
  ),
  
  HTML("<br>", "<br>", "<br>"),
  fluidRow(
      div(
        class = "succes-text",
        textOutput(ns("analysisMessage")
        )     
    ),
    
    fluidRow(
      mainPanel(
        div(
          class = "error-text",
          textOutput(ns("errorMessage"))
        )    
      )
    ),
  )
)