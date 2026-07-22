# Advanced mDSC software package

Welcome to the mDSC analysis software package. If you are not familiar with mDSC, 
we recommend you go to consult the full documentation first, since the 
following will be technical. This package is intended to help you with:

1. **DSC descriptive statistics**: Quickly calculating averages, standard deviations, 
and relative standard deviations based on mDSC analyses performed in TRIOS®. Thus,
for using this app, your data must already be in specifically formatted Word tables 
and documents.  

2. **Quasi-Isothermal modulated DSC deconvolution**: this type of analysis is not 
always present in all DSC software packages (such as TRIOS®), hence the need for 
a user-friendly app to do this. The input required here is an Excel with your
raw modulated heat flow, modulated temperature, and time. 

3. **Regular modulated DSC deconvolution**: in software packages, this is done by 
using a rolling Fourier transform to extract the amplitude and average of the 
signal. However, in certain cases, this type of deconvolution can lead to artifacts.
This is why it is useful to also calculate the amplitude and average of the signal
based on the maxima and minima in the raw data, without using a Fourier transform.
Moreover, it might be useful to compare this data to unmodulated DSC data, as well
as modulated DSC data that was deconvoluted with a Fourier transform. This package
combines all these features. It package requires an Excel file containing 
temperature, time, and modulated heat flow. 

4. **Modulated DSC deconvolution simulation**: it might be interesting, based on 
events that are already known, to mathematically simulate deconvolution of 
modulated DSC thermograms. This app requires you to already have performed 
modulated DSC on a sample, since inputs such as onset temperatures, midpoint
temperatures, heat capacities, and enthalpies are required. The app uses this
data to construct a modulated heat flow signal, which is subsequently deconvoluted 
into the reversing, total and non-reversing heat flows.

More information on how the different packages work, what input is required, and what their theoretical background is, can be found in the documentation file (.pdf), or in the tutorial tabs of each sub-application (visible after launching the application).

## Installation
There are two ways to install this software:
1. Download the Windows installer from the Github repository. It can be found under "releases", on the right side of the page. Download the .exe file and run the installer. There is no need for further action; the software will be installed and runs.
2. If you do not have a Windows PC or are comfortable tinkering with the code, you will need to install R and clone the repository to your local machine to run it. For this:
   - Make an account on Github.
   - Install R (https://cran.rstudio.com). Click the right link at the top based on your operating system.
   - Install R Studio (https://posit.co/download/rstudio-desktop).
   - Fork the Github repository to your own account. For this, login to github, return to the main page of this repository (Advanced mDSC software package), and click "fork" next to the repository title.
   - Open RStudio. On the top left, click "file", then "New project". Click the "version control" option. Click "Git". Go to the repository that you forked previously, and copy the page URL. Copy it and paste it into the "Repository URL" box. You can decide the name and folder where the clone should be saved.
   - The previous steps install the app. To run it, go to the folder that has appeared where you saved the clone. Within the folder, navigate to the "Code" folder, and within that folder, click the file "App.R". This opens the app in R. To get the user-friendly interface, click "Run App" on the top of the left upper pane. This step will install all additional dependencies and packages the app needs automatically. You will need to perform this step each time you want to use the app, but the package installation will only happen once, so the first time you start the app will take significantly longer.

## Testing
In order to test the software, test files have been included within each sub-app folder (inside the "Code" folder). Specifications on what parameters to use have been included within the test files. A folder with expected results is also included in each of the test folders. 

## Collaboration
If you wish to contribute to the software, give feedback, or contact the main author for any reason, please feel free to email me at tomkonings01@gmail.com. 





