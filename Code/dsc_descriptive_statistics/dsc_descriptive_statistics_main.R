source("dsc_descriptive_statistics/dsc_descriptive_statistics_libraries.R")
source("dsc_descriptive_statistics/dsc_descriptive_statistics_functions.R")
source("dsc_descriptive_statistics/dsc_descriptive_statistics_detailed_functions.R")


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
      
      source("dsc_descriptive_statistics/dsc_descriptive_statistics_ui_2.R", local = TRUE)$value,
    ),
    
    tabPanel(
      "Output and input files",
      id= ns("outputInputTab"),
      icon = icon("file-import", class = "fa-solid"),
      value = "outputInputTab",
      source("dsc_descriptive_statistics/dsc_descriptive_statistics_ui_1.R", local = TRUE)$value),

    
    #-----------------------------------------------------------
    #Static user interface: tutorial tab
    #-----------------------------------------------------------
    
    tabPanel(
      title = "Tutorial",
      icon = icon("book", class = "fa-solid"),
      fluidPage(
        withMathJax(
          includeMarkdown("dsc_descriptive_statistics/dsc_descriptive_statistics_tutorial.md")
        )
      )
    )
    
   )
}

#-----------------------------------------------------------
#Actual code (not stricly related to the UI) starts here
#----------------------------------------------------------
mdsc_analyzer_server <- function(id) {
  
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  
    extraInput <- list()
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
      
      
      observeEvent(input$errorCheck, {
        output$errorMessage <- NULL
        output$analysisMessage <- NULL
        
        
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
          output$analysisMessage <- NULL
          saveWorkbook(wb, file = file, overwrite = TRUE)
          hidePageSpinner()
          shinyjs::disable(ns("excelDownload"))
        }
      )
 
      
  })
}