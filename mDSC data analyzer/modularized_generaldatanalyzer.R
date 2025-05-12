#---------------------------------
#Packages
#---------------------------------

#User interface
if (require("shiny") == FALSE) {
  install.packages("shiny")
}
library(shiny)

#Reading and extracting tables from Word documents
if (require("docxtractr") == FALSE) {
  install.packages("docxtractr")
}
library(docxtractr)

#Making some data manipulation easier through functions present in these packages
if (require("tidyverse") == FALSE) {
  install.packages("tidyverse")
}
if (require("gdata") == FALSE) {
  install.packages("gdata")
}
library(tidyverse)
library(gdata)

#Writing the final excel files
if (require("openxlsx") == FALSE) {
  install.packages("openxlsx")
}
library(openxlsx)


#---------------------------------
#Custom functions
#---------------------------------

#Function for adding suffixes in the user interface (for example the "st" in "1st")
ordinalSuffix <- function(x) {
  if (x %% 100 %in% c(11, 12, 13)) {
    suffix <- "th"
  } else {
    suffix <- switch(x %% 10,
                     "st",
                     "nd",
                     "rd",
                     "th"
    )
  }
  paste0(x, suffix)
}

# Function for cleaning values
cleanAndConvert <- function(x) {
  x <- gsub("[°C]", "", x)
  x <- gsub("[J/g]", "", x)
  x <- gsub("[%]", "", x)
  x <- gsub("[mg]", "", x)
  x <- gsub("[J/(g.°C)]", "", x)
  x <- gsub(",", ".", x)
}


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
          HTML("
          /* Apply a background color to the entire page */
          body {
            background-color: #f4f7fa; /* Light blue-gray background */
            color: #333; /* Dark gray text */
            margin: 0; 
            padding: 0; 
          }
  
          .main-header {
            background-color: #3c8dbc;
            color: white;
            padding: 10px;
            font-size: 20px;
            font-weight: bold;
          }
  
          .secondary-header {
          font-size: 18px;
          color: #3c8dbc; /* Dark gray color */
          font-weight: bold;
        }
  
          .ordered-list {
          font-size: medium;
          text-align: left;
          color: #333;
          }
                .ordered-list li, .nested-list li {
          padding-bottom: 5px;
        }
              .nested-list {
          padding-left: 20px; 
          list-style-type: lower-alpha; 
          padding-bottom: 10px; 
          padding-top: 10px;
       }
  
          /* Ensure the navbar takes full width */
          .navbar {
            width: 100%;
          }
  
          /* Ensure the tab content takes full width */
          .tab-content {
            width: 100%;
          }
  
          /* Style the title panel with a blue background and white text */
          .navbar-default .navbar-brand,
          .navbar-default .navbar-nav>li>a {
            color: #fff; /* White text */
          }
          .navbar-default {
            background-color: #3c8dbc; /* Main blue color for the navbar */
          }
  
          /* Apply a white background to each tab panel */
          .navbar-default .navbar-nav>li>a:hover,
          .navbar-default .navbar-nav>li>a:focus,
          .navbar-default .navbar-nav>li>a:active,
          .navbar-default .navbar-nav>.open>a,
          .navbar-default .navbar-nav>.open>a:hover,
          .navbar-default .navbar-nav>.open>a:focus,
          .navbar-default .navbar-nav>.open>a:active {
            background-color: #fff; /* White background on hover and active */
            color: #3c8dbc; /* Main blue text on hover and active */
          }
  
          /* Style the input section with a light blue background */
          .tab-content {
            background-color: #ecf5fb;
            padding: 20px;
          }
  
          /* Style the text input and file input */
          input[type='text'],
          select {
            width: 100%;
            padding: 10px;
            margin: 8px 0;
            display: inline-block;
            border: 1px solid #ccc;
            box-sizing: border-box;
          }
  
          /* Style the file input menu */
          .file-input-label {
            background-color: #2c3e50; /* Darker blue color for the label */
            color: #fff; /* White text on the label */
            border: none; 
            padding: 10px; 
            text-align: center;
            cursor: pointer;
            border-radius: 4px; 
            display: block; 
            width: 100%; 
          }
  
          /* Style the 'Run Analysis' button */
          #runAnalysis {
            background-color: #2c3e50; /* Darker blue color for the button */
            color: #fff; /* White text on the button */
            border: none;
            padding: 10px 30px; 
            text-align: center;
            text-decoration: none;
            display: inline-block;
            font-size: 25px;
            margin: 4px 2px;
            cursor: pointer;
            border-radius: 4px;
            margin-left: 50%;
          }
          
          /* Style the 'Next' button */
          #Next {
            background-color: #2c3e50; /* Darker blue color for the button */
            color: #fff; /* White text on the button */
            border: none; 
            padding: 10px 30px; 
            text-align: center;
            text-decoration: none;
            display: inline-block;
            font-size: 25px;
            margin: 4px 2px;
            cursor: pointer;
            border-radius: 4px; 
            margin-left: 50%;
          }
  
          /* Style the analysis message */
          #analysisMessageContainer {
            font-size: 18px; 
            font-weight: bold; 
            padding: 10px;
            width: 150%; 
            color: #3c8dbc
          }
          
          /* Style the error messages */
          #errorMessageContainer {
            font-size: 18px; 
            font-weight: bold; 
            padding: 10px;
            width: 150%; 
            color: #e04f30
          }
  
                .nested-list table {
          border-collapse: collapse; 
          width: 100%; 
          }
  
        .nested-list th, .nested-list td {
          border: 1px solid #ddd; 
          padding: 8px; 
          text-align: left; 
        }
  
              .nested-list-container {
          padding-bottom: 20px; 
        }
        ")
        )
      ),
      
      #Actual input tabs are here---------------------------------------------------------------------------------------------------------------------------
      
      fluidPage(
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
              selected = "1"),  
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
              actionButton(ns("Next"), "Next tab")
            ),
          )
        ),
      )
    ),
    
    tabPanel(
      "Output and input files",
      id= ns("files"),
      icon = icon("file-import", class = "fa-solid"),
      value = "outputInputTab",
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
              actionButton(ns("runAnalysis"), "Run Analysis")
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
    ),
    
    #-----------------------------------------------------------
    #Static user interface: tutorial tab
    #-----------------------------------------------------------
    
    tabPanel(
      id= ns("tutorial"),
      "Tutorial",
      icon = icon("book", class = "fa-solid"),
      value = "tutorialTab",
      fluidPage(
        titlePanel(tags$p(style = "text-align: left; color: #3c8dbc;", "DSC data analyzer tutorial"), windowTitle = "DSC Data Analyzer"),
        tags$br(),
        tags$p(
          style = "font-size: medium; text-align: left; color: #333;",
          "Hey there fellow scientist! Welcome to the tutorial to the thermal data analyzer. This page gives some background on the app, gives some additional tips and tricks, and can be considered as a general user manual.
                As a first important point, please note that this app was developed primarily for differential scanning calorimetry – hence the terminology, such as the constant use of heating cycle. However, if you want it to use for any other technique compatible with TRIOS, it works just as well! For any further questions not answered in the tutorial, contact me at tom.konings@kuleuven.be"
        ),
        tags$br(),
        tags$p(
          style = "font-size: medium; text-align: left; color: #333; font-weight: bold",
          "In case you came here to look for troubleshooting: most errors are covered and will result in an error message. However, one notable error that will still crash the app is having the Excel file you want to write to open when you execute the app. Close the Excel file and try again."
        ),
        
        tags$br(),
        tags$div(
          class = "main-header",
          "Basic principle of the app"
        ),
        
        tags$br(),
        tags$p(
          style = "font-size: medium; text-align: left; color: #333;",
          "The app takes word documents as input and outputs formatted Excel tables. These Word files are generated using a semi-automised process in TRIOS, software by TA instruments (another section of the tutorial instructs you on how to do this). Word files are used because they are the most customizable and straightforward to generate in TRIOS. The input required in the various menus helps the program to separate your heating cycles. They are basically landmarks for the program. The main purpose of the program is to generate descriptive statistics for your data, but you can also export raw data."  
        ),
        
        tags$br(),
        tags$p(
          style = "font-size: medium; text-align: left; color: #333;",
          "Does it work? Yes it does! The program has been checked manually for accuracy. Moreover, if you happen to make a mistake in your input, the software will let you know using an error message. There are a few limitations left that are outlined in this tutorial (look for the title mentioning limiations)."
        ),
        
        tags$br(),
        tags$div(
          class = "main-header",
          "Steps in TRIOS"
        ),
        tags$br(),
        tags$p(
          style = "font-size: medium; text-align: left; color: #333;",
          "The first thing you’ll need to do is generate the word documents you’ll feed into the app in TRIOS. Here is how you do it. This part assumes you know the TRIOS software well already, and is not a full tutorial for the software."
        ),
        tags$ol(
          class = "ordered-list",
          tags$li("Open the files you want to analyze."),
          tags$li("Don't split the files into the different cycles, but simply send each cycle to a new graph. Do so by right clicking on a step in the file manager on the left and selecting \"send to new graph\"."),
          tags$li("Perform the analysis manually like you normally would for each heating or cooling cycle. When you save your analysis (see next point), TRIOS can distinguish the different cycles and knows what analysis was performed for which cycle."),
          tags$li("Go to format > save analysis. This will save a file."),
          tags$li("Now, go to format > generate report. A new screen pops up. You will see many options on the right side of the screen. In these options, you will find the analyses you did, grouped per heating cycle. Dragging one of the options (for example the onset of a certain integration you did) to the screen on the left displays the value. However, in order for the app to work, you’ll need to follow these very specific instructions."),
          tags$ol(
            class = "nested-list",
            tags$li("Every event you want to analyze is one single table. For example, if you have a glass transition and a melting point of interest in heating cycle 1, you’ll have two tables for that heating cycle."),
            tags$li("Implement tables in the TRIOS report file by clicking the “table” option in the top area."),
            tags$li("Every table has one “title column”: the first (most left) column is always considered to contain some kind of title, so do not put any values there. Besides this limitation, you may have as many columns as you wish in your tables, and different tables can have different numbers of columns."),
            tags$li("Every table has two rows. One is the “title row” containing more detailed information on the values in the row below. For example, you might want to put something like \"Tg onset (°C)\", \"Tg midpoint (°C)\", and \"Tg end (°C)\". The second row contains the values matching each title. Making your titles nicely from the start is helpful because the program can read those and use them for the output Excel."),
            tags$li("The final result should look something like this, for every table:"),
            div(
              class = "nested-list-container",
              tags$table(
                class = "nested-list",
                tags$tr(
                  tags$th("-general title-"),
                  tags$th("-title-"),
                  tags$th("-title-"),
                  tags$th("-title-"),
                  tags$th("-title-")
                ),
                tags$tr(
                  tags$td("-nothing-"),
                  tags$td("-value-"),
                  tags$td("-value-"),
                  tags$td("-value-"),
                  tags$td("-value-")
                )
              )
            ),
            tags$li(
              style = "font-weight: bold;", 
              "Important note: when making your tables in TRIOS, you’ll see an option regarding table headers. DO NOT select this option, as otherwise the code will read your table as having just one row, violating the rule above stating that every table needs two rows. Also note that when you open the documents in word afterwards and change something in the tables, word might chagne the layout for some reason and still add a header row. The program will spot this and give an error, but be aware that this can cause this particular error."),
            tags$li("As long as you follow the rules above, you may have as many tables as you wish and as many tables per heating cycle as you wish. There’s no further need for consistency.")
          ),
          tags$li("Next, you will want to save your report as a template. Do this by clicking  “save template” in the options at the top."),
          tags$li("So far, everything was manual, but here is where the automation comes in. You can apply the saved analysis and the saved report template to a new file, and the analysis will be carried out automatically, including the generation of a report. As a matter of fact, you can also only save the report as a template and apply that directly. This is a bit quicker, but the downside is that you won't be able to drag new analysis elements to add new values in the report. If one sample requires slightly different integration limits, you can simply modify the values in the report by editing the analysis in the the tab that was generated when you applied the report template (see tab list on the bottom of the screen, -your sample title- (Report 1). The Word documents you just made semi-automatically serve as the app's input."),
          tags$li("Export the reports you made as word documents by clicking the TRIOS logo on the top left, and you’re all set!"),
        ),
        tags$p(
          style = "font-size: medium; text-align: left; color: #333;",
          "Note: something that happens when you apply an analysis to a file is that the curves are superposed. If you want to avoid this, pull the curves apart BEFORE conducting the analysis; this fixes the issue. This only works when you also apply an analysis template, not when you only apply a report template"
        ),
        tags$br(),
        tags$div(
          class = "main-header",
          "Installing and running the app"
        ),
        
        tags$br(),
        tags$div(
          class = "secondary-header",
          "Running the app by installing R on your computer"
        ),
        tags$p(
          style = "font-size: medium; text-align: left; color: #333;",
          "This is the more complicated option, but certainly the more practical once you get there since you don’t have to download/upload files to the cloud. Follow these steps, in exactly this order:"
        ),
        tags$ol(
          class = "ordered-list",
          tags$li("Install Java. Make sure you pick the 64bit (x64) option if your system is x64 (offline version). This is the download link : ", tags$a(href = "https://www.java.com/en/download/manual.jsp", "https://www.java.com/en/download/manual.jsp"),"."),
          tags$li("Install RTools: ", tags$a(href = "https://cran.r-project.org/bin/windows/Rtools/", "https://cran.r-project.org/bin/windows/Rtools/"),"."),
          tags$li("Install R: follow the left link here: ", tags$a(href = "https://posit.co/download/rstudio-desktop/", "https://posit.co/download/rstudio-desktop/"),"."),
          tags$li("Install RStudio (right side): ", tags$a(href = "https://posit.co/download/rstudio-desktop/", "https://posit.co/download/rstudio-desktop/"),"."),
          tags$li("Open the code file. Running the code (Run App on the top right) will install all additional packages"),
        ),
        tags$div(
          class = "secondary-header",
          "Running the app via the cloud"
        ),
        tags$p(
          style = "font-size: medium; text-align: left; color: #333;",
          "The easiest option for people without experience. The website that is best used for this is posit, the official R website. For you to be able to see the code after clicking the link in the GitHub Readme file, you’ll still need to select the correct file on the right.
                If you’re running R locally (on your computer) and want to transition to the online version, you’ll need to remove all code setting the working directory (ctrl+ F to look up the command ‘setwd’). This is because you can’t change the working directory in posit: you'll need to download the analysed files from the environment (panel on the right)."
        ),
        tags$br(),
        tags$div(
          class = "main-header",
          "App features and limitations"
        ),
        tags$br(),
        tags$p(
          style = "font-size: medium; text-align: left; color: #333;",
          "The whole point of the app is to calculate the means, standard deviations and relative standard deviations of the files you upload. It then groups the results per heating cycle, adds all relevant titles, rounds to the amount of decimals you tell it to, and writes a nicely formatted table to an Excel file. Here is an example of an output table:"
        ),
        tags$table(
          class = "data-table",
          style = "width: 100%;",
          tags$tr(
            tags$th("-Your sample title-: heating cycle 1"),
            tags$th("Means"),
            tags$th("Standard Deviations"),
            tags$th("Relative Standard Deviations")
          ),
          tags$tr(
            tags$td("Solvent Peak Onset (°C)"),
            tags$td("8,75"),
            tags$td("0,15"),
            tags$td("1,66")
          ),
          tags$tr(
            tags$td("Solvent Peak location (°C)"),
            tags$td("46,65"),
            tags$td("0,99"),
            tags$td("2,11")
          ),
          tags$tr(
            tags$td("Melting Peak Onset (°C)"),
            tags$td("135,44"),
            tags$td("0,19"),
            tags$td("0,14")
          ),
          tags$tr(
            tags$td("Melting Peak location (°C)"),
            tags$td("146,9"),
            tags$td("0,05"),
            tags$td("0,03")
          ),
          tags$tr(
            tags$td("Melting Peak enthalpy (J/g)"),
            tags$td("17,4"),
            tags$td("0,39"),
            tags$td("2,22")
          ),
          tags$tr(
            tags$td("Tg Onset (°C)"),
            tags$td("25,61"),
            tags$td("0,28"),
            tags$td("1,08")
          ),
          tags$tr(
            tags$td("Tg midpoint (°C)"),
            tags$td("30,67"),
            tags$td("1"),
            tags$td("3,26")
          ),
          tags$tr(
            tags$td("Tg end (°C)"),
            tags$td("34,93"),
            tags$td("1,19"),
            tags$td("3,41")
          ),
        ),
        tags$br(),
        tags$p(
          style = "font-size: medium; text-align: left; color: #333;",
          "This is the main feature, and much of the app is focused around it. Your input boils down to the following:"
        ),
        tags$ul(
          class = "ordered-list",
          tags$li("In the first tab (analysis settings):"),
          tags$ol(
            class = "ordered-list",
            tags$li("How many heating cycles, pans, and tables per heating cycle you have. If you aren’t happy with the titles in each first row of every table that you generated via TRIOS, you can also untick the box asking about that and give custom titles. If you do this, you will need to input new titles for everything, however."),
            tags$li("Whether you want to save the raw data as well"),
            tags$li("The name of the excel sheet the data should be saved in"),
            tags$li("Whether you want to round to two decimals (analysed data only, not the raw data)"),
            tags$li("If the answer to the previous question was no, to how many decimals you want ro round"),
          ),
          tags$li("In the second tab (input and output files)"),
          tags$ol(
            class = "ordered-list",
            tags$li("The files you want to analyze, by uploading them."),
            tags$li("The name of the Excel file."),
            tags$li("The name of the Excel file sheet (if there is already an Excel file with the same name, but you change the sheet name, it will write to the same Excel but a different sheet!)."),
            tags$li("The sample name: this is a name displayed at the top left of all the exported tables, for example “spray dried powder”. Since the results are grouped per heating cycle, a “heating cycle X” is added after the sample name for every table, where X varies between 1 and the number of heating cycles you have."),
            tags$li("Where you want to export the excel file to, so a file directory."),
          ),
        ),
        
        tags$div(
          class = "secondary-header",
          "Error handling"
        ),
        tags$p(
          style = "font-size: medium; text-align: left; color: #333;",
          "The code is able to deal with user error and report on it in a clear way. If, for example, you have one table with only 1 row or with 3 rows, you will get an error message explaining this upon running the code. If the number of tables you say you have in your document in the input doesn’t match with the actual number of tables, you’ll get another error, and so forth."
        ),
        tags$div(
          class = "secondary-header",
          "Compatibility"
        ),
        tags$p(
          style = "font-size: medium; text-align: left; color: #333;",
          "The code is compatible with any word documents. TRIOS can analyze many different files in the manner described above, including DSC, TGA, DMA, etc. It can also analyze other CSV files. Finally, it can handle files from Universal Analysis as well. If you want to open data from Universal analysis, select \"Thermal data\" (bottom right) when opening files in TRIOS."
        ),
        tags$div(
          class = "secondary-header",
          "Limitations"
        ),
        tags$p(
          style = "font-size: medium; text-align: left; color: #333;",
          "First and foremost, the tables must be consistent in the sense that the first column is never taken into account and the first row doesn’t contain any values, as mentioned above."
        ),
        tags$p(
          style = "font-size: medium; text-align: left; color: #333;",
          "Another important point is the fact that whatever files you upload must be consistent within them. You cannot have one file where you have an additional melting peak, for example. The number of tables per file and number of columns for any given table must be the same in the different documents you calculate in order to calculate the mean."
        ),
        tags$br(),
        tags$div(
          class = "main-header",
          "How the code works"
        ),
        tags$br(),
        tags$p(
          style = "font-size: medium; text-align: left; color: #333;",
          "This section gives a very short overview of different code snippets. If you want to know the details, go have a look at the code itself 😊. "
        ),
        tags$div(
          class = "secondary-header",
          "Packages"
        ),
        tags$p(
          style = "font-size: medium; text-align: left; color: #333;",
          "Packages are essentially expansions of R that make life easier. They introduce new commands that don’t need to be coded all the way. In order to use packages, they need to be called with the (library) function."
        ),
        tags$div(
          class = "secondary-header",
          "Functions"
        ),
        tags$p(
          style = "font-size: medium; text-align: left; color: #333;",
          'There are two functions at the top of the code that are defined here and called later on. The first is clean and convert, which will remove all units from the data and convert everything to numeric values (in the original word document, everything is encoded as characters, which is a problem). The ordinal suffix helps generate the correct menus for the interactive user interface (for example if you have two heating cycles, it will ask “how many tables do you have in your 2nd heating cycle”. This function generates the "nd".'
        ),
        tags$div(
          class = "secondary-header",
          "ShinyR"
        ),
        tags$p(
          style = "font-size: medium; text-align: left; color: #333;",
          "The whole app is based on ShinyR. ShinyR has three components:"
        ),
        tags$ol(
          class = "ordered-list",
          tags$li("Code defining the user interface."),
          tags$li("Server logic (the code that does the actual analysis). "),
          tags$li("The line shinyApp(ui = ui, server = server)"),
        ),
        tags$p(
          style = "font-size: medium; text-align: left; color: #333;",
          "Some important aspects of ShinyR are the reactive values and CSS code. Look into the relevant documentation if you wish to know more, as this is outside of the scope of this tutorial."
        ),
        tags$div(
          class = "secondary-header",
          "Generating the general UI"
        ),
        tags$p(
          style = "font-size: medium; text-align: left; color: #333;",
          "CSS code and HTML are used to style and generate the user interface. This is also where parts like the three menus “input”, “methods” and “tutorial” are defined."
        ),
        tags$div(
          class = "secondary-header",
          "Generating the interactive UI "
        ),
        tags$p(
          style = "font-size: medium; text-align: left; color: #333;",
          "This piece of code changes the menus the user sees based on the values of other menus. For example, if you indicate having 3 heating cycles, you will be asked thrice about how many tables you have in each heating cycle. This is also for a large part where user input variables are defined. "
        ),
        tags$div(
          class = "secondary-header",
          "Loading functionality"
        ),
        tags$p(
          style = "font-size: medium; text-align: left; color: #333;",
          "This small block of code defines values according to a template you loaded. "
        ),
        tags$div(
          class = "secondary-header",
          "Extracting tables"
        ),
        tags$p(
          style = "font-size: medium; text-align: left; color: #333;",
          "This is the first actual analysis part of the code. Tables in word documents have assigned numbers in the xml file. You don’t see this, but the code can read this. This is the whole principle behind extracting the data.
              From every “table 1” of every file, the second row of values is extracted. The rows thus extracted are grouped in one long vector called tempDf. tempDf is then cleaned and rendered numeric.
              You’ll notice that everything works based on for-loops in the code. Important user inputs are the number of pans, the number of heating cycles, and the number of tables per heating cycles. The former two are values, while the latter is a vector. "
        ),
        tags$div(
          class = "secondary-header",
          "Grouping tables into df"
        ),
        tags$p(
          style = "font-size: medium; text-align: left; color: #333;",
          "The different tempDf vectors are then grouped into a table called df. This works smoothly when df and tempDf are of the same length, but this is often not the case. Hence, when needed, NAs are inserted at the right locations so as not to influence the descriptive statistics."
        ),
        tags$div(
          class = "secondary-header",
          "Grouping dfs into allCycles"
        ),
        tags$p(
          style = "font-size: medium; text-align: left; color: #333;",
          "All the dfs are then grouped into another data frame called allCycles, again taking into account different lengths. allCycles is printed in case the user wants to export their raw data as well. "
        ),
        tags$div(
          class = "secondary-header",
          "Generating dataFrameCycle from df"
        ),
        tags$p(
          style = "font-size: medium; text-align: left; color: #333;",
          "DataFrameCycles is composed of different rows which are in turn composed of the different statistics. This is very easy to edit, and if reader wishes to have additional statistics (more than just means, SDs and relative SDs), they can edit this part relatively quickly. "
        ),
        tags$div(
          class = "secondary-header",
          "Binding the dataFrameCycles to combinedStats"
        ),
        tags$p(
          style = "font-size: medium; text-align: left; color: #333;",
          "The different dataFrameCycles, which regroup the data per heating cycle, are combined into combinedStats."
        ),
        tags$div(
          class = "secondary-header",
          "Generating the vectors containing the titles"
        ),
        tags$p(
          style = "font-size: medium; text-align: left; color: #333;",
          "The next block of code makes a list containing all the column titles, but only if the user indicates that they want to keep the titles of their original tables."
        ),
        tags$div(
          class = "secondary-header",
          "Picking appropriate entries from combinedStats and grouping them per heating cycle, adding titles, writing to an excel"
        ),
        tags$p(
          style = "font-size: medium; text-align: left; color: #333;",
          "Finally, data is extract back to several different data frames from combined stats using some arithmetic, is bound with the appropriate title vectors, and is written to an excel. "
        ),
        tags$div(
          class = "secondary-header",
          "What’s left "
        ),
        tags$p(
          style = "font-size: medium; text-align: left; color: #333;",
          "Other code snippets are mainly error- and exception handling. Error handling gives a clear output to the user in case they did something wrong, while exception handling ensures that the code can work no matter the data structure. For example, the rest of the code would sometimes cause issues when all the tables only have two columns."
        ),
      )
    )
  )
)}

#-----------------------------------------------------------
#Actual code (not stricly related to the UI) starts here
#----------------------------------------------------------
mdsc_sim_server <- function(id) {
  ns <- NS(id)
  moduleServer(id, function(input, output, session) {

      #-----------------------------------------------------------------------------------
      # Dynamic menus: include numTables, numColHeatingCycle, colTitles, Nrheatingcycles
      #-----------------------------------------------------------------------------------
      
      #Define numTables and colTitles as reactive values
      numTables <- reactiveVal(NULL)
      colTitles <- reactiveVal(NULL)
      
      #Extract numCycles in the context of the dynamic dropdown menus - will be done again later for the rest of the code
      output[[ns("tablesDropdowns")]] <- renderUI({
        numCycles <- as.numeric(input[[ns("heatingCycle")]])
        print(numCycles)
        
        # Generate dynamic dropdown menus based on the number of cycles selected
        lapply(1:numCycles, function(i) {
          selectInput(ns(paste0("tables_cycle", i)),
                      paste("How many tables do you have in your", ordinalSuffix(i), "heating cycle?"),
                      choices = 1:10
          )
        })
      })
      
      # Update numTables vector
      observe({
        req(as.numeric(input[[ns("heatingCycle")]]))  # wait until heatingCycle exists
        
        numCycles <- as.numeric(input[[ns("heatingCycle")]])
        numTables(sapply(1:numCycles, function(i) {
          as.numeric(input[[ns(paste0("tables_cycle", i))]])
        }))
      })
      
      # Generation of the dynamic dropdown menus for column titles depending on the number of cycles and tables
      output$coltitlesInput <- renderUI({
        if (input$keepTitles == FALSE) {
          numCycles <- as.numeric(input$heatingCycle)
          if (length(numCycles) == 0) {
            NULL
          } else {
            lapply(1:numCycles, function(i) {
              numTables <- as.numeric(input[[paste0("tables_cycle", i)]])
              if (length(numTables) == 0) {
                NULL
              } else {
                lapply(1:numTables, function(j) {
                  textInput(ns(paste0("coltitles_cycle", i, "_table", j)),
                            paste("What are the column names in table", j, "of heating cycle", i, "?"),
                            placeholder = "Enter column names, separated by commas"
                  )
                })
              }
            })
          }
        }
      })
      
      #Block coding for custom user input in case the user wants to manually change the titles in their table
      observe({
        req(input[[ns("keepTitles")]])  # wait until heatingCycle exists
        
        if (input[[ns("keepTitles")]]) {
          numCycles <- as.numeric(input[[ns("heatingCycle")]])
          
          # Initialize a list to store colTitles
          colTitlesList <- list()
          
          # Update colTitles vector based on user inputs
          if (length(numCycles) == 0) {
            colTitles(NULL)
          } else {
            colTitlesList <- lapply(1:numCycles, function(i) {
              numTables <- as.numeric(input[[paste0("tables_cycle", i)]])
              if (length(numTables) == 0) {
                NULL
              } else {
                unlist(sapply(1:numTables, function(j) {
                  
                  # Split the input string at commas and remove leading/trailing spaces
                  if (!is.null(input[[ns(paste0("coltitles_cycle", i, "_table", j))]])) {
                    columnNames <- strsplit(input[[ns(paste0("coltitles_cycle", i, "_table", j))]], ",")[[1]]
                    columnNames <- trimws(columnNames)
                    columnNames <- unlist(columnNames, use.names = FALSE)
                    columnNames
                  } else {
                    NULL
                  }
                }), use.names = FALSE) %>%
                  c() 
              }
            })
            
            # Update colTitles with the list of vectors
            colTitles(colTitlesList)
          }
        }
      })
      
      
      
      #-----------------------------------------------------------------------------------
      #Switch to "Output and input files settings" tab if Next is pressed
      #-----------------------------------------------------------------------------------
      
      observeEvent(input$Next, {
        updateNavbarPage(session, "navbar", selected = "outputInputTab")
      })
      
      #-----------------------------------------------------------------------------------
      
      
      observeEvent(input$runAnalysis, {
        cat("Run analysis button clicked.\n")
        
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
        
        # Read given docx files and extract tables
        for (i in 1:pans) {
          doc <- read_docx(get(paste0("file", i)))
          assign(paste0("tablesDoc", i), docx_extract_all_tbls(doc))
        }
        
        # Further error handling: check the number of tables in the document and the user-specified number
        if (length(get(paste0("tablesDoc", 1))) != sum(numTables())) {
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
          for (j in 1:numTables()[i]) {
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
            
            # Clean and convert the values in tables
            tempDf <- as.numeric(lapply(tempDf, cleanAndConvert))
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
          if (numTables()[i] == 1) {
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
            for (r in 1:numTables()[i]) {
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
            for (j in numTables()[i]) {
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
            sumVal <- sumVal + numTables()[i]
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
        excelFile <- paste(outputExcel, ".xlsx", sep = "")
        if (file.exists(excelFile)) {
          wb <- loadWorkbook(paste(outputExcel, ".xlsx", sep = ""))
        } else {
          wb <- createWorkbook()
        }
        
        
        # Add a worksheet to the workbook in the Excel file. 
        addWorksheet(wb, outputSheet)
        
        t <- 1
        
        # Eetrieve data from combinedStats in order to prepare it for export to Excel. Some titles are added but they are not final.  
        for (i in 1:numCycles) {
          j <- numTables()[i]
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
        saveWorkbook(wb, paste(outputExcel, ".xlsx", sep = ""), overwrite = TRUE)
        
        # Save raw data to the Excel sheet: this code is very similar to the code above but without the descriptive statistics. 
        if (input$saveRaw == TRUE) {
          wb <- loadWorkbook(paste(outputExcel, ".xlsx", sep = ""))
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
              for (j in 1:numTables()[i]) { # Iterate over each table
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
                  if (j == numTables()[i]) {
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
            j <- numTables()[i]
            
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
          saveWorkbook(wb, paste(outputExcel, ".xlsx", sep = ""), overwrite = TRUE)
        }
        
        #The users see this text if the analysis worked well. 
        output$analysisMessage <- renderText({
          "Analysis completed! Your file is now available in the directory you chose :)"
        })
      })
  })
}