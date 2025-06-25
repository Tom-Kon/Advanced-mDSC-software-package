# Regular modulated DSC deconvolution

## Function
As is mentioned in the theoretical background of the main menu, the deconvolution procedure of the modulated heat flow is normally carried out by using a Fourier transform. However, this mathematical manipulation can sometimes introduce artifacts. Hence, it might be useful to also deconvolute the data in a different way, by looking at the raw modulated heat flow signal. Additionally, comparing the results from the Fourier transform and those obtained through the alternative method can be informative when it comes to detecting artifacts. Hence, this app also offers a Fourier-transform based method for the user to compare the two outputs easily. Moreover, it can be interesting to also compare the mDSC data with unmodulated DSC data. This app also offers an option to do this. 

## Input 
The user must perform an mDSC analysis of a sample and export an Excel containing/characterized by the following: 

1.	The Excel file must be in the .xlsx or .xls format. 
2.	The Excel must not be opened on the user’s computer when loading it. 
3.	The Excel must have the **time data (in minutes)**, **the temperature data** (in °C) and the **normalized modulated heat flow data** (in W/g). It is preferable that it does not contain other heat flow data, since it might throw an error otherwise. The order of the columns does not matter. The presence of rows containing other information does not matter. These things are all detected and filtered out appropriately, since the program looks for the words “time”, “modulated” and “temperature”, and “modulated” “heat” “flow”. Note that the terms in your titles must be separated by **spaces**, e.g., write “modulated heat flow”, not “modulated_heat_flow”. If the user wishes to compute the RHF based on the deconvolution performed by the manufacturer’s software, a total heat flow column must also be present. 
4.	The user must select the sheet to be read in manually if the data is not in the first sheet of the Excel. 
5.	It is of the utmost importance that the data exported to Excel contains enough significant figures; preferably 5 or above. If not, maxima and minima will not be detected accurately (due to overlapping values), and the program will fail or produce unreliable results. A warning is printed if this is the case. 

This input is required to perform the analysis of the mDSC data, be it using the amplitudes of the modulated heat flow signal or the Fourier transform. Moreover, the user must specify the period, the heating rate, and the temperature modulation amplitude. 

If the user wishes to make a comparison with regular DSC, they tick the corresponding checkbox in the app and upload a new Excel with the same structure as what is mentioned above, but with a total heat flow instead of a modulated heat flow (refer to point 4). 

Graphs displaying the deconvoluted thermograms can be found in the corresponding tab. An Excel with the analyzed data as well as several plots can also be downloaded via the download tab, or, in the case of the plots, via the graphs tab itself (camera in top right corner of graph when hovering over it). 

## Mathematical and theoretical background
### Deconvolution based on the modulated heat flow signal
Two equations that were derived in the overall overarching background are of importance here: 

$$
THF= 〈\frac{dQ}{dt}〉  \quad \quad \text{and} \quad \quad RHF= -β\frac{A_{MHF}}{\frac{2π  A_{temp}}{T}},
$$

The software compiles a list of all the maxima and another list of all minima. The first step in calculating the average heat flow (THF) consists of adding the maximum with index “i" to the minimum with index “i”, the maximum with index “i+1” to the minimum with index "i+1”, and so forth. The average is then calculated by dividing these points by 2. Temperatures at which these averages occur are then calculated in similar fashion. These points are then plotted against temperature in the final thermogram. The amplitude required for the RHF, $A_{MHF}$, is calculated by subtracting the minima from their respective maxima without dividing anything by two. Corresponding temperature values are then found in the same way as for the THF. $β$, $T$, and $A_{temp}$ need no further calculations since they are used inputs. The calculated RHF is plotted against temperature in the final output. 

## Details on how the software works
1.	Read in the Excel.
2.	A function called “locate_extrema_manual is called: within a window of fifty points, it uses which.max to locate maxima. Minima are detected in a similar way using which.min
3.	The number of minima and maxima are counted. 
4.	The function HFcalc splits the data frame containing both minima and maxima in two dataframes containing either type. It checks the length of these dataframes and removes a row (or multiple rows) if either one is longer. It calculates THF, RHF and NRHF as was explained in the previous section. 
The rest of the code is there to generate the user interface and execute the Shiny app. 


