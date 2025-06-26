
if (require("shiny.router") == FALSE) {
  install.packages("shiny.router")
}
library(shiny.router)

if (require("shiny") == FALSE) {
  install.packages("shiny")
}
library(shiny)

if (require("rstudioapi") == FALSE) {
  install.packages("rstudioapi")
}
library(rstudioapi)

if (require("shinycssloaders") == FALSE) {
  install.packages("shinycssloaders")
}
library(shinycssloaders)

if (require("markdown") == FALSE) {
  install.packages("markdown")
}
library(markdown)


if (require("zip") == FALSE) {
  install.packages("zip")
}
library(zip)

# Source the module
source("mdsc_deconvolution_simulation/mdsc_deconvolution_simulation_main.R")
source("quasi-isothermal_mdsc_deconvolution/quasi-isothermal_mdsc_deconvolution_main.R")
source("dsc_descriptive_statistics/dsc_descriptive_statistics_main.R")
source("regular_mdsc_deconvolution/regular_mdsc_deconvolution_main.R")