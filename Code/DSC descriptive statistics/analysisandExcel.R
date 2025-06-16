source("../DSC descriptive statistics/functions.R")

#---------------------------------------------------------------------------------------------------------------
# Code
# The code essentially works through a series of for loops, which allows for analysis of any data structure.
# Row 2 from table t in each document is concatenated into one single vector tempDfDocTemp. 
# All tempDfDocTemp vectors are cleaned up and grouped into a dataframe called df, this happens per heating cycle 
# All heating cycles are grouped into allcycles 
# Data is retrieved from allcycles and means, standard deviations and relative standard deviations (or spreads) are
# calculated, first by outputting them into dataFrameCycle and then combining those into CombinedStats
# Data is retrieved from CombinedStats per heating cycle in dataframes called dataFrameCycleTemp which are 
# immediately written to an excel and formatted before the loop restarts.
# The reason for this many intermediate steps is to be able to potentially expand the project in the future: it might 
# be useful to have these extra dataframes on hand. 
#---------------------------------------------------------------------------------------------------------------


#------------------------------------------------------------------------------------------------------------------------------------
# Extract variables: include files, numCycles, tableTitle, outputLocation, outputExcel, outputSheet, Number of pans, outputSheetRaw
#------------------------------------------------------------------------------------------------------------------------------------
analysisAndExcel <- function(input, extraInput) {
  numTables <- extraInput$numTables
  
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
  # outputLocation <- input$outputPath
  outputExcel <- input$excelName
  outputSheet <- input$excelSheet
  pans <- as.numeric(input$pans)
  outputSheetRaw <- input$excelName2
  
  
  
  source("../DSC descriptive statistics/errorhandling.R", local= TRUE)
  
  # Set rounding of the values according to the user input
  if (input$round1 == FALSE) {
    round <- 2
  } else {
    round <- as.numeric(input$round)
  }
  
  # Read given docx files and extract tables
  for (i in 1:pans) {
    doc <- read_docx(get(paste0("file", i)))
    assign(paste0("tablesDoc", i), docx_extract_all_tbls(doc))
  }

  # Further error handling: check the number of tables in the document and the user-specified number
  if (length(get(paste0("tablesDoc", 1))) != sum(numTables)) {
    print("Error: It seems that the sum of the number of tables you said the document has via the dropdown menus doesn't match the real number of tables in the document. Please check your input and try again.")
    output$errorMessage <- renderText({
      "Error: It seems that the sum of the number of tables you said the document has via the dropdown menus doesn't match the real number of tables in the document. Please check your input and try again."
    })
    return(NULL)
  }
  
  #Setting up integers and data frames for subsequent iterations. 
  tableStart <- 1
  allCycles <- data.frame()
  combinedStats <- data.frame()
  numColHeatingCycleTemp <- as.numeric()
  numColHeatingCycle <- as.numeric()
  dfRaw <- data.frame()
  
  # Iterate over each cycle
  for (i in 1:numCycles) {
    df <- data.frame()
    # Iterate over each table
    for (j in 1:numTables[i]) {
      t <- tableStart
      for (p in 1:pans) {
        assign(paste0("tempDfDoc", p), (get(paste0("tablesDoc", p)))[[t]])
      }
      
      # Iterate over each pan
      for (c in 1:pans) {
        tempDfDocTemp <- get(paste0("tempDfDoc", c))
        # Check the number of rows in the tables
        if (nrow(tempDfDocTemp) != 2) {
          print("Error: It seems something is wrong with the number of rows in your tables. Please keep in mind that every table in your document can only have 2 rows, and that the second row should contain your data. Something you might want to check if your table has two rows is whether the first row is considered as headers. Check this through word by selecting the table and going to table design.")
          output$errorMessage <- renderText({
            "Error: It seems something is wrong with the number of rows in your tables. Please keep in mind that every table in your document can only have 2 rows, and that the second row should contain your data. Something you might want to check if your table has two rows is whether the first row is considered as headers. Check this through word by selecting the table and going to table design."
          })
          return(NULL)
        }
      }
      
      tempDfDoc1 <- (get(paste0("tempDfDoc", 1)))
      numColHeatingCycleTemp <- (ncol(tempDfDoc1[2, ]) - 1)
      numColHeatingCycle <- c(numColHeatingCycle, numColHeatingCycleTemp)
      tempDf <- c()
      
      for (p in 1:pans) {
        # Create the variable name dynamically
        tempDfDoc <- get(paste0("tempDfDoc", p))
        
        # Check the number of columns
        if (ncol(tempDfDoc) != 2) {
          if (p == 1) {
            tempDf <- tempDfDoc[2, 2:ncol(tempDfDoc)]
          } else {
            tempDf <- c(tempDf, tempDfDoc[2, 2:ncol(tempDfDoc)])
          }
        } else {
          if (p == 1) {
            tempDf <- tempDfDoc[2, 2]
          } else {
            tempDf <- c(tempDf, tempDfDoc[2, 2])
          }
        }
      }
      
      print(tempDf)
      # Clean and convert the values in tables
      tempDf <- lapply(tempDf, cleanAndConvert)
      tempDf <- as.numeric(lapply(tempDf, as.numeric))

      
      if (j == 1) {
        # Create a df if it's the first table
        df <- rbind(df, tempDf)
      } else {
        # If it's not the first table, values will be appended to the df
        if (length(tempDf) < ncol(df)) {
          for (a in 1:((((ncol(df)) - (length(tempDf))) / pans))) {
            tempLength <- length(tempDf)
            x <- ((tempLength) / pans)
            for (p in 1:((pans))) {
              if (x + 1 == length(tempDf)) {
                tempDf <- c(tempDf[1:x], NA, tempDf[(x + 1)])
              } else if (x + 1 < length(tempDf)) {
                tempDf <- c(tempDf[1:x], NA, tempDf[(x + 1):(length(tempDf))])
              } else {
                tempDf <- c(tempDf, NA)
              }
              x <- x + 1 + ((tempLength) / pans)
            }
          }
        }
        
        if (ncol(df) < length(tempDf)) {
          for (a in 1:(((length(tempDf)) - (ncol(df))) / pans)) {
            tempLength <- ncol(df)
            x <- ((tempLength) / pans)
            for (p in 1:((pans))) {
              if (x + 1 == ncol(df)) {
                df <- cbind(df[, 1:x], NA, df[, (x + 1)])
              } else if (x + 1 < ncol(df)) {
                df <- cbind(df[, 1:x], NA, df[, (x + 1):(ncol(df))])
              } else {
                df <- cbind(df, NA)
              }
              x <- x + 1 + ((tempLength) / pans)
            }
          }
        }
        
        df <- rbind(df, tempDf)
      }
      as.data.frame(df)
      names(df) <- paste("col", 1:ncol(df), sep = "")
      tableStart <- t + 1
    }
    
    
    if (i == 1) {
      # Create allCycles df if it's the first heating cycle
      allCycles <- df
      names(allCycles) <- paste("col", 1:ncol(allCycles), sep = "")
    } else {
      # If it's not the first cycle, values will be appended to allCycles
      if (ncol(df) < ncol(allCycles)) {
        for (a in 1:((((ncol(allCycles)) - (ncol(df))) / pans))) {
          tempLength <- ncol(df)
          x <- ((tempLength) / pans)
          for (p in 1:((pans))) {
            if (x + 1 == ncol(df)) {
              df <- cbind(df[, 1:x], NA, df[, (x + 1)])
            } else if (x + 1 < ncol(df)) {
              df <- cbind(df[, 1:x], NA, df[, (x + 1):(ncol(df))])
            } else {
              df <- cbind(df, NA)
            }
            x <- x + 1 + ((tempLength) / pans)
          }
        }
      }
      
      if (ncol(allCycles) < ncol(df)) {
        for (a in 1:(((ncol(df)) - (ncol(allCycles))) / pans)) {
          tempLength <- ncol(allCycles)
          x <- ((tempLength) / pans)
          for (p in 1:((pans))) {
            if (x + 1 == ncol(allCycles)) {
              allCycles <- cbind(allCycles[, 1:x], NA, allCycles[, (x + 1)])
            } else if (x + 1 < ncol(allCycles)) {
              allCycles <- cbind(allCycles[, 1:x], NA, allCycles[, (x + 1):(ncol(allCycles))])
            } else {
              allCycles <- cbind(allCycles, NA)
            }
            x <- x + 1 + ((tempLength) / pans)
          }
        }
      }
      names(df) <- paste("col", 1:ncol(df), sep = "")
      names(allCycles) <- paste("col", 1:ncol(allCycles), sep = "")
      allCycles <- rbind(allCycles, df)
    }
    
    
    # Create dataFrameCycle df if it's the first heating cycle
    dataFrameCycle <- data.frame()
    if (numTables[i] == 1) {
      for (c in 1:((ncol(df)) / pans)) {
        tempVec <- c()
        for (d in 1:pans) {
          tempVec <- c(tempVec, df[1, c + (d - 1) * (ncol(df) / pans)])
        }
        tempRow <- c("")
        tempRow <- c(tempRow, mean(tempVec))
        #Calculate spread instead of SD whne the number of pans = 2
        if (pans != 2) {
          tempRow <- c(tempRow, sd(tempVec))
          tempRow <- c(tempRow, (sd(tempVec) / mean(tempVec) * 100))
        } else {
          tempVecDiff <- abs(tempVec[1] - tempVec[2])
          tempRow <- c(tempRow, tempVecDiff)
          tempRow <- c(tempRow, (tempVecDiff / mean(tempVec) * 100))
        }
        
        dataFrameCycle <- rbind(dataFrameCycle, tempRow)
      }
      
      # If it's not the first cycle, values will be appended to allCycles
    } else {
      for (r in 1:numTables[i]) {
        for (c in 1:((ncol(df)) / pans)) {
          tempVec <- c()
          for (d in 1:pans) {
            tempVec <- c(tempVec, df[r, c + (d - 1) * (ncol(df) / pans)])
          }
          tempRow <- c("")
          tempRow <- c(tempRow, mean(tempVec))
          if (pans != 2) {
            tempRow <- c(tempRow, sd(tempVec))
            tempRow <- c(tempRow, (sd(tempVec) / mean(tempVec) * 100))
          } else {
            tempVecDiff <- abs(tempVec[1] - tempVec[2])
            tempRow <- c(tempRow, tempVecDiff)
            tempRow <- c(tempRow, (tempVecDiff / mean(tempVec) * 100))
          }
          
          dataFrameCycle <- rbind(dataFrameCycle, tempRow)
        }
      }
    }
    
    #Add some titles to dataFrameCycle and create combinedStats
    if (pans != 2) {
      names(dataFrameCycle) <- c(paste(tableTitle, ": Heating Cycle"), "Means", "Standard deviations", "Relative standard deviations")
    } else {
      names(dataFrameCycle) <- c(paste(tableTitle, ": Heating Cycle"), "Means", "Spread", "Relative Spread")
    }
    combinedStats <- rbind(combinedStats, dataFrameCycle)
  }
  
  
  #Manipulate the colTitles so they match the rest of the data. 
  if (input$keepTitles == TRUE) {
    colTitles <- list()
    sumVal <- 0
    for (i in 1:numCycles) {
      tempColTitles <- c()
      for (j in numTables[i]) {
        if (j != 1) {
          for (s in (sumVal + 1):(sumVal + j)) {
            tempDfDoc <- data.frame()
            tempDfDoc <- tablesDoc1[[s]]
            if (ncol(tempDfDoc) == 2) {
              tempColTitles <- c(tempColTitles, tempDfDoc[1, 2])
              tempColTitles <- unlist(tempColTitles)
            } else {
              for (c in 2:ncol(tempDfDoc)) {
                tempColTitles <- c(tempColTitles, tempDfDoc[1, c])
                tempColTitles <- unlist(tempColTitles)
              }
            }
          }
          colTitles[[i]] <- tempColTitles
        } else {
          s <- (sumVal + 1)
          tempDfDoc <- data.frame()
          tempDfDoc <- tablesDoc1[[s]]
          if (ncol(tempDfDoc) == 2) {
            tempColTitles <- c(tempColTitles, tempDfDoc[1, 2])
            tempColTitles <- unlist(tempColTitles)
          } else {
            for (c in 2:ncol(tempDfDoc)) {
              tempColTitles <- c(tempColTitles, tempDfDoc[1, c])
              tempColTitles <- unlist(tempColTitles)
            }
          }
          colTitles[[i]] <- tempColTitles
        }
      }
      sumVal <- sumVal + numTables[i]
    }
  }
  
  sumColTitles <- 0
  
  if (input$keepTitles == FALSE) {
    for (i in 1:numCycles) {
      sumColTitles <- (sumColTitles + length(colTitles()[[i]]))
    }
    sumNumColHeatingCycle <- sum(numColHeatingCycle)
  } else {
    for (i in 1:numCycles) {
      sumColTitles <- (sumColTitles + length(colTitles[[i]]))
    }
    sumNumColHeatingCycle <- sum(numColHeatingCycle)
  }
  
  # Further error handling: Check the input number of titles
  if (sumColTitles != sumNumColHeatingCycle) {
    print("It seems that the number of titles you put in when setting up your method doesn't match the amount of columns in the data you're trying to save to Excel. Make sure they match and make sure to save a new template.")
    output$errorMessage <- renderText({
      "It seems that the number of titles you put in when setting up your method doesn't match the amount of columns in the data you're trying to save to Excel. Make sure they match and make sure to save a new template."
    })
    return(NULL)
  }
  
  #Create some variables to use in a subsequent loop and clean up Combinedstats. 
  combinedStats <- na.omit(combinedStats)
  sumVal <- 0
  sumCols <- 0
  colTitlesTemp <- c()
  emptyDf <- data.frame(NA)
  names(emptyDf) <- ""
  
  #Create an Excel file using the user input name in case one doesn't exist yet. 
  # excelFile <- paste(outputExcel, ".xlsx", sep = "")
  # if (file.exists(excelFile)) {
  #   wb <- loadWorkbook(paste(outputExcel, ".xlsx", sep = ""))
  # } else {
    wb <- createWorkbook()
  # }
  
  
  # Add a worksheet to the workbook in the Excel file. 
  addWorksheet(wb, outputSheet)
  
  t <- 1
  
  # Eetrieve data from combinedStats in order to prepare it for export to Excel. Some titles are added but they are not final.  
  for (i in 1:numCycles) {
    j <- numTables[i]
    if (sumVal == 0) {
      numCols <- sum(numColHeatingCycle[(sumVal):(sumVal + j)])
    } else {
      numCols <- sum(numColHeatingCycle[(sumVal + 1):(sumVal + j)])
    }
    sumVal <- (sumVal + j)
    dataFrameCycleTemp <- combinedStats[(sumCols + 1):(sumCols + numCols), ]
    if (input$keepTitles == FALSE) {
      colTitlesTemp <- colTitles()[[i]]
    } else {
      colTitlesTemp <- colTitles[[i]]
    }
    dataFrameCycleTemp[, 1] <- colTitlesTemp
    sumCols <- (sumCols + numCols)
    if (i == 1) {
      if (pans != 2) {
        names(dataFrameCycleTemp) <- c(paste(tableTitle, ": Heating Cycle 1"), "Means", "Standard deviations", "Relative standard deviations")
      } else {
        names(dataFrameCycleTemp) <- c(paste(tableTitle, ": Heating Cycle 1"), "Means", "Spread", "Relative Spread")
      }
      for (j in 2:ncol(dataFrameCycleTemp)) {
        dataFrameCycleTemp[, j] <- as.numeric(dataFrameCycleTemp[, j])
        dataFrameCycleTemp[, j] <- round(dataFrameCycleTemp[, j], digits = round)
      }
      
      #Add final titles and round values according to user input. 
      finalOutput <- dataFrameCycleTemp
    } else {
      if (pans != 2) {
        names(dataFrameCycleTemp) <- c(paste(tableTitle, ": Heating Cycle", i), "Means", "Standard deviations", "Relative standard deviations")
      } else {
        names(dataFrameCycleTemp) <- c(paste(tableTitle, ": Heating Cycle", i), "Means", "Spread", "Relative Spread")
      }
      for (j in 2:ncol(dataFrameCycleTemp)) {
        dataFrameCycleTemp[, j] <- as.numeric(dataFrameCycleTemp[, j])
        dataFrameCycleTemp[, j] <- round(dataFrameCycleTemp[, j], digits = round)
      }
    }
    
    # Write the data to the worksheet
    writeData(wb, sheet = outputSheet, dataFrameCycleTemp, startCol = t, startRow = 1)
    
    #Style the table exported to Excel
    writeDataTable(
      wb,
      outputSheet,
      dataFrameCycleTemp,
      startCol = t,
      startRow = 1,
      tableStyle = "TableStyleMedium2",
      tableName = NULL,
      headerStyle = openxlsx_getOp("headerStyle"),
      withFilter = openxlsx_getOp("withFilter", TRUE),
      keepNA = openxlsx_getOp("keepNA", FALSE),
      na.string = openxlsx_getOp("na.string"),
      sep = ", ",
      stack = FALSE,
      firstColumn = openxlsx_getOp("firstColumn", FALSE),
      lastColumn = openxlsx_getOp("lastColumn", FALSE),
      bandedRows = openxlsx_getOp("bandedRows", TRUE),
      bandedCols = openxlsx_getOp("bandedCols", FALSE),
    )
    
    widthVecHeader <- nchar(colnames(dataFrameCycleTemp)) + 3.5
    setColWidths(wb, sheet = outputSheet, cols = t:(t + 3), widths = widthVecHeader)
    setColWidths(wb, sheet = outputSheet, cols = (t + 4), widths = 3)
    
    t <- t + 5
  }
  
  # Save the workbook
  # saveWorkbook(wb, paste(outputExcel, ".xlsx", sep = ""), overwrite = TRUE)
  
  # Save raw data to the Excel sheet: this code is very similar to the code above but without the descriptive statistics. 
  if (input$saveRaw == TRUE) {
    addWorksheet(wb, outputSheetRaw)
    
    sumVal <- 0
    sumCols <- 0
    colTitlesTemp <- c()
    finalOutput <- data.frame(NA)
    emptyDf <- data.frame(NA)
    names(emptyDf) <- ""
    t <- 1
    col <- 1
    dataFrameCycleTempRaw <- data.frame()
    
    for (p in 1:pans) { # Iterate over pans
      for (i in 1:numCycles) {  # Iterate over heating cycles
        for (j in 1:numTables[i]) { # Iterate over each table
          dfRawTemp1 <- get(paste0("tablesDoc", p))
          dfRawTemp2 <- dfRawTemp1[[t]]
          dfRawTemp3 <- dfRawTemp2[2, 2:length(dfRawTemp2)]
          
          dfRawTemp3 <- as.numeric(lapply(dfRawTemp3, cleanAndConvert))
          dfRawTemp3 <- as.numeric(lapply(dfRawTemp3, as.numeric))
          
          
          if (t == 1) {
            dfRawPan <- dfRawTemp3
          } else {
            dfRawPan <- c(dfRawPan, dfRawTemp3)
          }
          
          t <- t + 1
          
          if (i == numCycles) {
            if (j == numTables[i]) {
              if (p == 1) {
                dfRaw <- dfRawPan
              } else {
                dfRaw <- cbind(dfRaw, dfRawPan)
              }
              t <- 1
            }
          }
        }
      }
    }
    
    
    for (j in 1:ncol(dfRaw)) {
      dfRaw[, j] <- as.numeric(dfRaw[, j])
    }
    
    dfRaw <- as.data.frame(dfRaw)
    
    for (i in 1:numCycles) {
      j <- numTables[i]
      
      if (sumVal == 0) {
        numCols <- sum(numColHeatingCycle[(sumVal):(sumVal + j)])
      } else {
        numCols <- sum(numColHeatingCycle[(sumVal + 1):(sumVal + j)])
      }
      sumVal <- (sumVal + j)
      dataFrameCycleTempRaw <- dfRaw[(sumCols + 1):(sumCols + numCols), ]
      
      if (input$keepTitles == FALSE) {
        colTitlesTemp <- colTitles()[[i]]
      } else {
        colTitlesTemp <- colTitles[[i]]
      }
      
      
      dataFrameCycleTempRaw <- cbind(colTitlesTemp, dataFrameCycleTempRaw)
      sumCols <- (sumCols + numCols)
      
      panTitles <- c()
      
      for (p in 1:pans) {
        pan <- paste0("pan ", p)
        panTitles <- c(panTitles, pan)
      }
      names(dataFrameCycleTempRaw) <- c(paste(tableTitle, ": Heating Cycle ", i), panTitles)
      
      
      writeData(wb, sheet = outputSheetRaw, dataFrameCycleTempRaw, startCol = col, startRow = 1)
      
      writeDataTable(
        wb,
        outputSheetRaw,
        dataFrameCycleTempRaw,
        startCol = col,
        startRow = 1,
        tableStyle = "TableStyleMedium2",
        tableName = NULL,
        headerStyle = openxlsx_getOp("headerStyle"),
        withFilter = openxlsx_getOp("withFilter", TRUE),
        keepNA = openxlsx_getOp("keepNA", FALSE),
        na.string = openxlsx_getOp("na.string"),
        sep = ", ",
        stack = FALSE,
        firstColumn = openxlsx_getOp("firstColumn", FALSE),
        lastColumn = openxlsx_getOp("lastColumn", FALSE),
        bandedRows = openxlsx_getOp("bandedRows", TRUE),
        bandedCols = openxlsx_getOp("bandedCols", FALSE),
      )
      
      widthVecHeader <- nchar(colnames(dataFrameCycleTempRaw)) + 3.5
      setColWidths(wb, sheet = outputSheetRaw, cols = col:(col + pans), widths = widthVecHeader)
      setColWidths(wb, sheet = outputSheetRaw, cols = (col + pans + 1), widths = 3)
      
      col <- col + pans + 2
    }
  }
  
  return(wb)
  
}

