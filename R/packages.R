library(tidyverse)
library(prioritizr)
library(rgdal)
library(here)
library(doParallel)
library(readxl)
library(shiny)
library(shinydashboard)
library(shinyIncubator)
library(ggplot2)
library(sp)
library(rgdal)
library(maptools)
library(foreign)
library(vegan)
library(raster)
library(leaflet)
library(rhandsontable)
library(Matrix)
# library(plyr)
# library(dplyr)
# library(tidyr)
if(!require(gurobi)){
  require(Rsymphony)
}
library(prioritizr)

#get country specific target value
count_tar <- function(PU = NULL, target = NULL){
  round(cellStats(PU,"sum") / 100 * target, 0)
}
