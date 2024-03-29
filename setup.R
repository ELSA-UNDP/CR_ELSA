list.of.packages <- c("shiny", 
                      "shinydashboard", 
                      "shinyIncubator", 
                      "ggplot2", 
                      "sp", 
                      "rgdal", 
                      "maptools", 
                      "PBSmapping", 
                      "foreign", 
                      "sqldf", 
                      "vegan", 
                      "labdsv", 
                      "raster", 
                      "leaflet", 
                      "rhandsontable", 
                      "Matrix", 
                      "plyr", 
                      "dplyr", 
                      "tidyr", 
                      "DT", 
                      "Rsymphony",
                      "tidyverse",
                      "prioritizr",
                      "here",
                      "purrr",
                      "readxl")


new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos='https://cran.rstudio.com/')

if(!require(leaflet)){
  if (!require('devtools')) install.packages('devtools')
  devtools::install_github('rstudio/leaflet')
}

if(!require(shinyIncubator)){
  devtools::install_github("rstudio/shiny-incubator")
}

# if(!require(gurobi)){
#   install.packages("C:/gurobi902/win64/R/gurobi_9.0-2.zip", repos = NULL, type = "win.binary")
# }

source("pre_global.R")
