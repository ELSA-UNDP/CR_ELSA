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

group_weights <- c(1.258064516,
                   1.951612903,
                   1.338709677,
                   1.366666667,
                   1.596774194,
                   1.725806452,
                   1.25,
                   1.178125,
                   1.703125,
                   1.015625,
                   0.951612903,
                   1.46875,
                   1.540322581,
                   1.1328125,
                   1.4765625,
                   1.5859375,
                   1.115,
                   1.291666667,
                   1.158333333
                   )

wgts$weight <- round(group_weights, 2)
