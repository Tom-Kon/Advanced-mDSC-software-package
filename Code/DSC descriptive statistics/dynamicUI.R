# This file will now *define* a function
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
  })
  
  
