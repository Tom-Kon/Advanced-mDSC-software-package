#---------------------------------------------------------------------------------------------------------------
# General error handling: several internal checks to make sure the user didn't make any mistakes. 
# Output consists of clear error messages. 
#---------------------------------------------------------------------------------------------------------------

errorhandling <- function(files, outputExcel, outputSheet, tableTitle, pans, saveRaw, outputSheetRaw) {

  # Check if files were uploaded
  if (is.null(files)) {
    msg <- "Error: No files were uploaded. Please upload the input files."
    
    return(msg) # No files uploaded, exit the function
  }
  
  filePaths <- files$datapath
  
  fileCounter <- 0
  for (i in seq_along(filePaths)) {
    filePath <- filePaths[i]
    inputName <- paste0("file", i)
    assign(inputName, filePath)
    fileCounter <- fileCounter + 1
  }
  
  
  #Check if uploaded files are indeed word documents 
  for (p in 1:nrow(files)) {
    ext <- tools::file_ext(get(paste0("file", p)))
    valid_ext <- tolower(ext) %in% c("docx", "doc")
    if (valid_ext) {
    } else {
      msg <- "Error: It seems you didn't just upload word documents, but at least one document is another type of file."
      return(msg)
    }
  }
  
  
  # Check if outputExcel was given
  if (is.null(outputExcel) || outputExcel == "") {
    msg <- "Error: Output file name not specified. Please provide the name of the output Excel file."
    return(msg)
  }
  
  # # Check if the extension is already present
  # if (!grepl("\\.xlsx$", outputExcel)) {
  #   outExcel <- paste0(outputExcel, ".xlsx") # Add .xlsx extension
  # } else {
  #   outExcel <- outputExcel
  # }
  
  # # Check if given outputSheet already exists
  # if (file.exists(outExcel)) {
  #   sheet_names <- getSheetNames(outExcel)
  #   if (outputSheet %in% sheet_names) {
  #     print("Error: The Sheet you want to write to already exists in an excel file with the name you specified. Please choose a different file name or a different sheet name if you want another sheet to be written to the same excel.")
  #     output$errorMessage <- renderText({
  #       "Error: The Sheet you want to write to already exists in an excel file with the name you specified. Please choose a different file name or a different sheet name if you want another sheet to be written to the same excel."
  #     })
  #     return(NULL)
  #   } 
  # }
  
  # Check if outputSheet was given
  if (is.null(outputSheet) || outputSheet == "") {
    msg <- "Error: Output sheet name not specified. Please provide the name of the sheet in the output Excel file."
    return(msg)
  }
  
  # Check if the excel name isn't too long
  if (nchar(outputExcel) > 31) {
    msg <- "Error: Output Excel name is too long. The maximum length is 31 characters."
    return(msg)
  }
  
  # Check if the excel sheet name isn't too long
  if (nchar(outputSheet) > 31) {
    msg <- "Error: Output sheet name too long. The maximum length is 31 characters."
    return(msg)
  }
  
  # Check if the excel file name contains special characters
  if (grepl("/|#|%|&|<|>|\\?|\\*|\\$|!|:|@|\\+|\\(|\\)|\\[|\\]|\\{|\\}|\"|\'|\\=|\\;", outputExcel) == TRUE) {
    msg <-  "Error: There is a special character in your Excel sheet name, please remove it!"
    return(msg)
  }
  
  # Check if the excel sheet name contains special characters
  if (grepl("/|#|%|&|<|>|\\?|\\*|\\$|!|:|@|\\+|\\(|\\)|\\[|\\]|\\{|\\}|\"|\'|\\=|\\;", outputSheet) == TRUE) {
    msg <-  "Error: There is a special character in your Excel sheet name, please remove it!"
    return(msg)
  }
  
  # Check if sampleName (tableTitle) was given
  if (is.null(tableTitle) || tableTitle == "") {
    msg <- "Error: Sample name not specified. Please provide the name of the sample (it will be used to create output tables)."
    return(msg)
  }
  
  # Check the number of pans uploaded
  if (fileCounter != pans) {
    msg <-  "Error: It seems that you have uploaded more or less files that you have pans according to your input. Please check your input and try again."
    return(msg)
  }
  
  # Check if name for the raw Excel sheet was given
  if (saveRaw == TRUE) {
    if (is.null(outputSheetRaw) || outputSheetRaw == "") {
      msg <- "Error: Output sheet name for raw data not specified. Please provide the name of the sheet in the output Excel file."
      return(msg)
    }
  }
  
  # Check if name given to raw data Excel sheet and analyzed data Excel sheet differ
  if (outputSheet == outputSheetRaw) {
    msg <- "Error: The given sheet name for the raw data output and analysis output is the same. Please make sure to give different names to these two Excel sheets"
    return(msg)
  }
  
}
  

