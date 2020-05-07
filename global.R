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
library(plyr)
library(dplyr)
library(tidyr)
library(prioritizr)
library(tidyverse)
library(here)

load("pre_global.RData")

`%notin%` <- Negate(`%in%`)

category_layer_light <- function(x){
  # initialize raster layer
  out <- raster::setValues(x[[1]], 0)
  out[raster::Which(is.na(x[[1]]))] <- NA_real_
  # populate raster layer
  for (i in seq_len(raster::nlayers(x)))
    out[raster::Which(x[[i]] == 1)] <- i
  # return result
  out 
}
