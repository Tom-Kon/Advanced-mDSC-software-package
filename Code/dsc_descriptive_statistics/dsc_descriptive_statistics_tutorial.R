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