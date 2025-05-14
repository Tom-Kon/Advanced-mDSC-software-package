source("../mDSC data analyzer/libraries.R")
source("../mDSC data analyzer/functions.R")

mdsc_analyzer_ui<- function(id) {
  ns <- NS(id)
  
  
  ui <- navbarPage(
    id= ns("tabs"),
    "Thermal Data Analyzer",
    lang = "en",
  
    #-----------------------------------------------------------
    #Static user interface: user input tabs
    #-----------------------------------------------------------
    
    tabPanel(
      title= "  Analysis settings", 
      icon = icon("gears", class = "fa-solid"),
      id = ns("analysisTab"),

      #---------------------------------------------------------------------------------------------------------------------------
      #Static user interface: all of the styling is put in a tabPanel (see above), since putting it as a separate entity results in errors
      #---------------------------------------------------------------------------------------------------------------------------
      
      tags$head(
        tags$style(
          source("../mDSC data analyzer/HTML styling.R")
        )
      ),
      
      #Actual input tabs are here---------------------------------------------------------------------------------------------------------------------------
      
      source("../mDSC data analyzer/main.R", local = TRUE)$value,
    ),
    
    tabPanel(
      "Output and input files",
      id= ns("outputInputTab"),
      icon = icon("file-import", class = "fa-solid"),
      value = "outputInputTab",
      source("../mDSC data analyzer/inputoutputtab.R", local = TRUE)$value),

    
    #-----------------------------------------------------------
    #Static user interface: tutorial tab
    #-----------------------------------------------------------
    
    tabPanel(
      id= ns("tutorial"),
      "Tutorial",
      icon = icon("book", class = "fa-solid"),
      value = "tutorialTab",
      source("../mDSC data analyzer/tutorial.R", local = TRUE)$value),
    
   )
}

#-----------------------------------------------------------
#Actual code (not stricly related to the UI) starts here
#----------------------------------------------------------
mdsc_analyzer_server <- function(id) {
  
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # make sure ns is defined for use in dynamicui.R
    
    source("../mDSC data analyzer/dynamicui.R", local= TRUE)
    
      observeEvent(input$Next, {
        updateNavbarPage(session, "tabs", selected = "outputInputTab")
      })
    
      observeEvent(input$runAnalysis, {

        #------------------------------------------------------------------------------------------------------------------------------------
        # Reset outputmessages in case user runs multiple analyses and one of them results in an error
        #------------------------------------------------------------------------------------------------------------------------------------
        
        output$errorMessage <- renderText({
          NULL
        })
        
        output$analysisMessage <- renderText({
          NULL
        })
        
        #------------------------------------------------------------------------------------------------------------------------------------
        # Extract variables: include files, numCycles, tableTitle, outputLocation, outputExcel, outputSheet, Number of pans, outputSheetRaw
        #------------------------------------------------------------------------------------------------------------------------------------
        
        
        # Extract uploaded files and get their paths
        files <- input$files
        filePaths <- files$datapath
        
        # Counting given files and assigning file path to file1, file2, or file3 based on the loop iteration
        fileCounter <- 0
        for (i in seq_along(filePaths)) {
          filePath <- filePaths[i]
          inputName <- paste0("file", i)
          assign(inputName, filePath)
          fileCounter <- fileCounter + 1
        }
        
        # Extract other input data
        numCycles <- as.numeric(input$heatingCycle)
        tableTitle <- input$sampleName
        outputLocation <- input$outputPath
        outputExcel <- input$excelName
        outputSheet <- input$excelSheet
        pans <- as.numeric(input$pans)
        outputSheetRaw <- input$excelName2
        
        # Set rounding of the values according to the user input
        if (input$round1 == FALSE) {
          round <- 2
        } else {
          round <- as.numeric(input$round)
        }
        
        source("../mDSC data analyzer/errorhandling.R", local= TRUE)

        source("../mDSC data analyzer/analysisandExcel.R", local= TRUE)
        
        
        #The users see this text if the analysis worked well. 
        output$analysisMessage <- renderText({
          "Analysis completed! Your file is now available in the directory you chose :)"
        })
      })
  })
}