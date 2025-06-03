#---------------------------------------------------------------------------------------------------------------
# General error handling: several internal checks to make sure the user didn't make any mistakes. 
# Output consists of clear error messages. 
#---------------------------------------------------------------------------------------------------------------



# Check if files were uploaded
if (is.null(files)) {
  print("Error: No files were uploaded. Please upload the input files.")
  output$errorMessage <- renderText({
    "Error: No files were uploaded. Please upload the input files."
  })
  return(NULL) # No files uploaded, exit the function
}

# Check if output location was given
if (is.null(outputLocation) || outputLocation == "") {
  print("Error: Output location not specified. Please provide the output folder path.")
  output$errorMessage <- renderText({
    "Error: Output location not specified. Please provide the output folder path."
  })
  return(NULL)
}

#Check if uploaded files are indeed word documents 
for (p in 1:pans) {
  ext <- tools::file_ext(get(paste0("file", p)))
  valid_ext <- tolower(ext) %in% c("docx", "doc")
  if (valid_ext) {
  } else {
    print("Error: It seems you didn't just upload word documents, but at least one document is another type of file.")
    output$errorMessage <- renderText({
      "Error: It seems you didn't just upload word documents, but at least one document is another type of file."
    })
    return(NULL)
  }
}

# Set working directory for excel files
if (dir.exists(outputLocation)) {
  setwd(outputLocation)
  cat("Changed working directory to:", outputLocation, "\n")
} else {
  output$errorMessage <- renderText({
    "Error: Excel file output directory does not exist or cannot be accessed."
  })
  return(NULL)
}


# Check if outputExcel was given
if (is.null(outputExcel) || outputExcel == "") {
  print("Error: Output file name not specified. Please provide the name of the output Excel file.")
  output$errorMessage <- renderText({
    "Error: Output file name not specified. Please provide the name of the output Excel file."
  })
  return(NULL)
}

# Check if the extension is already present
if (!grepl("\\.xlsx$", outputExcel)) {
  outExcel <- paste0(outputExcel, ".xlsx") # Add .xlsx extension
} else {
  outExcel <- outputExcel
}

# Check if given outputSheet already exists
if (file.exists(outExcel)) {
  sheet_names <- getSheetNames(outExcel)
  if (outputSheet %in% sheet_names) {
    print("Error: The Sheet you want to write to already exists in an excel file with the name you specified. Please choose a different file name or a different sheet name if you want another sheet to be written to the same excel.")
    output$errorMessage <- renderText({
      "Error: The Sheet you want to write to already exists in an excel file with the name you specified. Please choose a different file name or a different sheet name if you want another sheet to be written to the same excel."
    })
    return(NULL)
  } 
}

# Check if outputSheet was given
if (is.null(outputSheet) || outputSheet == "") {
  print("Error: Output sheet name not specified. Please provide the name of the sheet in the output Excel file.")
  output$errorMessage <- renderText({
    "Error: Output sheet name not specified. Please provide the name of the sheet in the output Excel file."
  })
  return(NULL)
}

# Check if the excel name isn't too long
if (nchar(outputExcel) > 31) {
  print("Error: Output Excel name is too long. The maximum length is 31 characters.")
  output$errorMessage <- renderText({
    "Error: Output Excel name is too long. The maximum length is 31 characters."
  })
  return(NULL)
}

# Check if the excel sheet name isn't too long
if (nchar(outputSheet) > 31) {
  print("Error: Output sheet name too long. The maximum length is 31 characters.")
  output$errorMessage <- renderText({
    "Error: Output sheet name too long. The maximum length is 31 characters."
  })
  return(NULL)
}

# Check if the excel file name contains special characters
if (grepl("/|#|%|&|<|>|\\?|\\*|\\$|!|:|@|\\+|\\(|\\)|\\[|\\]|\\{|\\}|\"|\'|\\=|\\;", outputExcel) == TRUE) {
  print("Error: There is a special character in your Excel file name, please remove it!")
  output$errorMessage <- renderText({
    "Error: There is a special character in your Excel sheet name, please remove it!"
  })
  return(NULL)
}

# Check if the excel sheet name contains special characters
if (grepl("/|#|%|&|<|>|\\?|\\*|\\$|!|:|@|\\+|\\(|\\)|\\[|\\]|\\{|\\}|\"|\'|\\=|\\;", outputSheet) == TRUE) {
  print("Error: There is a special character in your Excel sheet name, please remove it!")
  output$errorMessage <- renderText({
    "Error: There is a special character in your Excel sheet name, please remove it!"
  })
  return(NULL)
}

# Check if sampleName (tableTitle) was given
if (is.null(tableTitle) || tableTitle == "") {
  print("Error: Sample name not specified. Please provide the name of the sample (it will be used to create output tables).")
  output$errorMessage <- renderText({
    "Error: Sample name not specified. Please provide the name of the sample (it will be used to create output tables)."
  })
  return(NULL)
}

# Check the number of pans uploaded
if (fileCounter != pans) {
  print("Error: It seems that you have uploaded more or less files that you have pans according to your input. Please check your input and try again.")
  output$errorMessage <- renderText({
    "Error: It seems that you have uploaded more or less files that you have pans according to your input. Please check your input and try again."
  })
  return(NULL)
}

# Check if name for the raw Excel sheet was given
if (input$saveRaw == TRUE) {
  if (is.null(outputSheetRaw) || outputSheetRaw == "") {
    print("Error: Output sheet name for raw data not specified. Please provide the name of the sheet in the output Excel file.")
    output$errorMessage <- renderText({
      "Error: Output sheet name for raw data not specified. Please provide the name of the sheet in the output Excel file."
    })
    return(NULL)
  }
}

# Check if name given to raw data Excel sheet and analyzed data Excel sheet differ
if (outputSheet == outputSheetRaw) {
  print("Error: The given sheet name for the raw data output and analysis output is the same. Please make sure to give different names to these two Excel sheets")
  output$errorMessage <- renderText({
    "Error: The given sheet name for the raw data output and analysis output is the same. Please make sure to give different names to these two Excel sheets"
  })
  return(NULL)
}
