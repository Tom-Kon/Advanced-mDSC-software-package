#---------------------------------
#Libraries
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