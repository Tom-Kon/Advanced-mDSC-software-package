# Tutorial for DSC descriptive statistics

## Function
The main goal of this application is to speed up DSC data analysis. Since it is compatible with TRIOS, this software can also be used to analyze data from other applications, such as thermogravimetric analysis or viscosity measurements - as long as the data is generated as specified in the input section. The program simply groups the input into several categories and calculates means, standard deviations (or spreads in the case of only 2 data points), and relative standard deviations (or relative spreads).

## Input and output
The app takes word documents as input and outputs formatted Excel tables. These Word files are generated using a semi-automised process in TRIOS, software by TA instruments (another section of the tutorial instructs you on how to do this). Word files are used because they are the most customizable and straightforward to generate in TRIOS. The input required in the various menus helps the program to separate your heating cycles. They are basically landmarks for the program. The main purpose of the program is to generate descriptive statistics for your data, but you can also export raw data.

### Steps in TRIOS
The first thing you’ll need to do is generate the word documents you’ll feed into the app in TRIOS. Here is how you do it. This part assumes you know the TRIOS software well already, and is not a full tutorial for the software.

1. Open the files you want to analyze.
2. Don't split the files into the different cycles, but simply send each cycle to a new graph. Do so by right clicking on a step in the file manager on the left and selecting "send to new graph".
3. Perform the analysis manually like you normally would for each heating or cooling cycle. When you save your analysis (see next point), TRIOS can distinguish the different cycles and knows what analysis was performed for which cycle.
4. Go to format > save analysis. This will save a file.
5. Now, go to format > generate report. A new screen pops up. You will see many options on the right side of the screen. In these options, you will find the analyses you did, grouped per heating cycle. Dragging one of the options (for example the onset of a certain integration you did) to the screen on the left displays the value. However, in order for the app to work, you’ll need to follow these very specific instructions.
6. Every event you want to analyze is one single table. For example, if you have a glass transition and a melting point of interest in heating cycle 1, you’ll have two tables for that heating cycle.
7. Implement tables in the TRIOS report file by clicking the “table” option in the top area.
8. Every table has one “title column”: the first (most left) column is always considered to contain some kind of title, so do not put any values there. Besides this limitation, you may have as many columns as you wish in your tables, and different tables can have different numbers of columns.
9. Every table has two rows. One is the “title row” containing more detailed information on the values in the row below. For example, you might want to put something like "Tg onset (°C)", "Tg midpoint (°C)", and "Tg end (°C)". The second row contains the values matching each title. Making your titles nicely from the start is helpful because the program can read those and use them for the output Excel.
10. The final result should look something like this, for every table:

    <table>
    <thead>
        <tr>
        <th> -general title- </th>
        <th> -title- </th>
        <th> -title- </th>
        <th> -title- </th>
        <th> -title- </th>
        </tr>
    </thead>
    <tbody>
        <tr>
        <td> -nothing- </td>
        <td> -value- </td>
        <td> -value- </td>
        <td> -value- </td>
        <td> -value- </td>
        </tr>
    </tbody>
    </table>

- Important note: when making your tables in TRIOS, you’ll see an option regarding table headers. DO NOT select this option, as otherwise the code will read your table as having just one row, violating the rule above stating that every table needs two rows. Also note that when you open the documents in word afterwards and change something in the tables, word might chagne the layout for some reason and still add a header row. The program will spot this and give an error, but be aware that this can cause this particular error. As long as you follow the rules above, you may have as many tables as you wish and as many tables per heating cycle as you wish. There’s no further need for consistency.
11. Next, you will want to save your report as a template. Do this by clicking  “save template” in the options at the top.
12. So far, everything was manual, but here is where the automation comes in. You can apply the saved analysis and the saved report template to a new file, and the analysis will be carried out automatically, including the generation of a report. As a matter of fact, you can also only save the report as a template and apply that directly. This is a bit quicker, but the downside is that you won't be able to drag new analysis elements to add new values in the report. If one sample requires slightly different integration limits, you can simply modify the values in the report by editing the analysis in the the tab that was generated when you applied the report template (see tab list on the bottom of the screen, -your sample title- (Report 1). The Word documents you just made semi-automatically serve as the app's input."),
13. Export the reports you made as word documents by clicking the TRIOS logo on the top left, and you’re all set!

## Mathematical and theoretical background
There is not much to be said about the mathematical and theoretical background here. The arithmetic mean, sample standard deviation, relative sample standard deviation, spread and relative spread are all calculated based on standard formulae. 

## 	Details on how the software works
#### Generating the interactive UI 
This piece of code changes the menus the user sees based on the values of other menus. For example, if you indicate having 3 heating cycles, you will be asked thrice about how many tables you have in each heating cycle. This is also for a large part where user input variables are defined.

#### Extracting tables
This is the first actual analysis part of the code. Tables in word documents have assigned numbers in the xml file. You don’t see this, but the code can read this. This is the whole principle behind extracting the data.
From every “table 1” of every file, the second row of values is extracted. The rows thus extracted are grouped in one long vector called tempDf. tempDf is then cleaned and rendered numeric.
You’ll notice that everything works based on for-loops in the code. Important user inputs are the number of pans, the number of heating cycles, and the number of tables per heating cycles. The former two are values, while the latter is a vector. "

#### Grouping tables into df
The different tempDf vectors are then grouped into a table called df. This works smoothly when df and tempDf are of the same length, but this is often not the case. Hence, when needed, NAs are inserted at the right locations so as not to influence the descriptive statistics.

#### Grouping dfs into allCycles
All the dfs are then grouped into another data frame called allCycles, again taking into account different lengths. allCycles is printed in case the user wants to export their raw data as well. 

#### Generating dataFrameCycle from df
DataFrameCycles is composed of different rows which are in turn composed of the different statistics. This is very easy to edit, and if reader wishes to have additional statistics (more than just means, SDs and relative SDs), they can edit this part relatively quickly.

#### Binding the dataFrameCycles to combinedStats
The different dataFrameCycles, which regroup the data per heating cycle, are combined into combinedStats.

#### Generating the vectors containing the titles
The next block of code makes a list containing all the column titles, but only if the user indicates that they want to keep the titles of their original tables.

#### Picking appropriate entries from combinedStats and grouping them per heating cycle, adding titles, writing to an excel
Finally, data is extract back to several different data frames from combined stats using some arithmetic, is bound with the appropriate title vectors, and is written to an excel. 

#### What’s left 
Other code snippets are mainly error- and exception handling. Error handling gives a clear output to the user in case they did something wrong, while exception handling ensures that the code can work no matter the data structure. For example, the rest of the code would sometimes cause issues when all the tables only have two columns.

