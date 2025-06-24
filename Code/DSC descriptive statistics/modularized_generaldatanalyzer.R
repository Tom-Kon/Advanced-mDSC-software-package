source("DSC descriptive statistics/libraries.R")
source("DSC descriptive statistics/functions.R")
source("DSC descriptive statistics/analysisandExcel.R")


mdsc_analyzer_ui<- function(id) {
  ns <- NS(id)
  
  
  ui <- navbarPage(
    id= ns("tabs"),
    "DSC descriptive statistics",
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
      
      
      #Actual input tabs are here---------------------------------------------------------------------------------------------------------------------------
      
      source("DSC descriptive statistics/main.R", local = TRUE)$value,
    ),
    
    tabPanel(
      "Output and input files",
      id= ns("outputInputTab"),
      icon = icon("file-import", class = "fa-solid"),
      value = "outputInputTab",
      source("DSC descriptive statistics/inputoutputtab.R", local = TRUE)$value),

    
    #-----------------------------------------------------------
    #Static user interface: tutorial tab
    #-----------------------------------------------------------
    
    tabPanel(
      id= ns("tutorial"),
      "Tutorial",
      icon = icon("book", class = "fa-solid"),
      value = "tutorialTab",
      source("DSC descriptive statistics/tutorial.R", local = TRUE)$value),
    
   )
}

#-----------------------------------------------------------
#Actual code (not stricly related to the UI) starts here
#----------------------------------------------------------
mdsc_analyzer_server <- function(id) {
  
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # make sure ns is defined for use in dynamicui.R
    extraInput <- list()
    source("DSC descriptive statistics/dynamicui.R", local= TRUE)
    
    disable("excelDownload")
    

      observeEvent(input$Next, {
        updateNavbarPage(session, "tabs", selected = "outputInputTab")
      })
      
      #This file will now *define* a function
      ns <- session$ns
      
      # Define reactive values inside the scope
      numTables <- reactiveVal(NULL)
      colTitles <- reactiveVal(NULL)
      
      output$tablesDropdowns <- renderUI({
        
        numCycles <- as.numeric(input$heatingCycle)
        
        lapply(1:numCycles, function(i) {
          selectInput(ns(paste0("tables_cycle", i)),
                      paste("How many tables do you have in your", ordinalSuffix(i), "heating cycle?"),
                      choices = 1:10
          )
        })
      })
      
      
      observe({
        req(input$heatingCycle)  
        
        numCycles <- as.numeric(input$heatingCycle)
        
        numTables(sapply(1:numCycles, function(i) {
          as.numeric(input[[paste0("tables_cycle", i)]])  # Correct dynamic evaluation
        }))
        
        extraInput$numTables <<- numTables()
        
      })
      
    
      # observeEvent(input$runAnalysis, {
      #   
      # 
      #   #------------------------------------------------------------------------------------------------------------------------------------
      #   # Reset outputmessages in case user runs multiple analyses and one of them results in an error
      #   #------------------------------------------------------------------------------------------------------------------------------------
      #   
      #   output$errorMessage <- renderText({
      #     NULL
      #   })
      #   
      #   output$analysisMessage <- renderText({
      #     NULL
      #   })
      #   
      # 
      #   data_ready(TRUE)
      #   
      #   #The users see this text if the analysis worked well. 
      #   output$analysisMessage <- renderText({
      #     "Analysis completed! Your file is now available in the directory you chose :)"
      #   })
      # })
      
      
      
      observeEvent(input$errorCheck, {
        source("DSC descriptive statistics/dynamicui.R", local= TRUE)
        
        wb <<- analysisAndExcel(input, extraInput)
        if(typeof(wb) == "character") {
          output$errorMessage <- renderText({wb})
          return(NULL)
        }
        toggleState("excelDownload")
        output$analysisMessage <- renderText({"No errors, you can save the file!"})
        
      })
      
      
      output$excelDownload <- downloadHandler(
        filename = function() {
          paste0(input$excelName, ".xlsx")
        },
        content = function(file) {
          showPageSpinner()
          saveWorkbook(wb, file = file, overwrite = TRUE)
          hidePageSpinner()
          shinyjs::disable(ns("excelDownload"))
        }
      )
 
      
  })
}