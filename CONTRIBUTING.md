# Welcome to the Advanced mDSC software package contributing guidelines!

First and foremost, we greatly appreciate your willingness to contribute to this project. We invite you to consult our [Code of Conduct](./CODE_OF_CONDUCT.md) to ensure a welcoming and collaborative environment.

## Introduction

The latest version (1.0.2) of the software focuses entirely on different mDSC deconvolution techniques. This includes:

* Deconvolution of quasi-isothermal mDSC data.
* Deconvolution of standard mDSC data, with and without Fourier transformation.
* Computation of descriptive statistics related to mDSC data based on TRIOS templates.
* mDSC deconvolution simulation, where experimentally gathered data are used to construct a modulated heat flow, which is then deconvoluted.

Quantitative mDSC analysis, such as peak integration and peak analysis, is currently not included in the software.

This document is intended to guide users who wish to contribute to the source code. If you simply have a suggestion or have found a bug, please refer to the **Issues** tab instead.

## Code and software structure

The R code is structured as a router containing four functionally independent applications, each with its own application-specific libraries and helper functions. This structure allows the four applications to be distributed as a single software package rather than as four separate packages.

If you wish to contribute, first identify which application your contribution relates to and make the changes in the corresponding folder and `.R` file. The folder tree below shows the relevant sections of the `Code` folder, which contains the source code. Only the `dsc_descriptive_statistics` folder is shown in greater detail; the other application folders follow a similar structure.

The `obsolete` folder contains old code that is no longer part of the active application, while the `testing` folder contains files used for development and testing. These folders are therefore generally not relevant to contributors.

The main file for each sub-application contains the required `source()` statements as well as the setup for the UI, server, and download handlers. Application-specific functions are not contained in the main file. UI-related functions are located in the `UI` folder, while server-side and analytical functions are located in the other relevant `.R` files. Tutorial (`.md`) files and libraries are stored in their respective folders.

The `overarching_app` folder is an exception because it contains the code responsible for setting up the router connecting the four applications. Contributors working on an individual application therefore generally do not need to modify this folder.

```text
Advanced-mDSC-software-package/
├── Code/
│   ├── dsc_descriptive_statistics/
│   │   ├── obsolete/
│   │   ├── testing/
│   │   ├── Main file
│   │   ├── UI
│   │   ├── Libraries
│   │   ├── Tutorial
│   │   └── Helper functions/additional files
│   ├── mdsc_deconvolution_simulation/
│   ├── overarching_app/
│   │   ├── tutorial
│   │   ├── html_styling.R
│   │   └── Router setup.R
│   ├── quasi-isothermal_mdsc_deconvolution/
│   └── regular_mdsc_deconvolution/
├── app.R
└── mDSC_apps.Rproj
```

The `assets` folder contains the images used throughout the software. If you add images as part of a contribution, place them in this folder.

### Electron wrapper

To make the software accessible to users who are unfamiliar with R and programming, an Electron wrapper is provided through the **Releases** section of the repository.

Contributors do not need to modify or rebuild the Electron wrapper when making changes to the R/Shiny applications. Changes to the relevant R code are sufficient; the repository owner will update the Electron wrapper when a new application version is released.

## How to contribute

### Requirements

Please ensure that you have the following installed to ensure compatibility:

* R version 4.5.1 or higher.
* RStudio (recommended).
* Git.
* All R packages required by the application.

The required R packages are: shiny, dplyr, docxtractr, grid, tidyverse, signal, gdata, zoo, openxlsx, stats, pracma, plotly, shiny.router, rstudioapi, shinycssloaders, markdown, ggplot2, crayon, shinythemes, purrr, dplyr, tidyr, bslib, readxl, dplyr, and shinyjs.

### Getting started

After cloning or forking the repository, open the `mDSC_apps.Rproj` project in RStudio. The application can then be run from the project using `app.R`. Before making changes, ensure that the application starts correctly and that the relevant application can be opened through the router.

### Making changes

When making a contribution:

1. Identify the application or component to which the contribution relates.
2. Make the required changes in the relevant `.R` files.
3. Avoid modifying the `overarching_app` folder unless the contribution specifically concerns the router.
4. Test the affected functionality.
5. Check that the changes do not negatively affect the other applications.
6. Update the relevant documentation or tutorial files if necessary.

### Testing

Before submitting a pull request, please verify that:

* The application starts without errors.
* The relevant application can be opened through the router.
* The modified functionality behaves as expected. If the files that can be found in the /testing subfolders of the different apps do not cover your new functionality, please provide files for testing. 
* The existing functionality of the affected application remains functional.
* The changes do not unintentionally affect the other applications.
* The relevant downloads and exports continue to work.
* The documentation is updated where necessary.

## Pull requests

The repository contains a pull request template specifying the information that should be provided when submitting a pull request.

Please describe:

* What was changed.
* Why the change was made.
* Which application(s) are affected.
* How the change was tested.
* Any relevant limitations or known issues.

For changes to analytical methods, please also provide the relevant scientific reference(s) and describe any assumptions or methodological changes introduced by the contribution.
