##################################
# Created by EPI-interactive
# 15 Sep 2021
# https://www.epi-interactive.com
##################################

library(shiny)
library(shinyjs)
library(leaflet)
library(dplyr)
library(sp)
library(sf)

g_mainData <- st_read("data/WCC_Trees/WCC_Trees.shp")
g_mainData <- st_transform(g_mainData, crs = 4326)
g_mainData <- g_mainData %>%
    arrange(desc(height)) %>%
    mutate(botanical = coalesce(botanical_, botanica_1)) %>%
    select(OBJECTID, botanical, height, girth, address_fu) %>%
    mutate(popupText = paste0(
        "<b>", botanical, "</b><br>",
        "Height: ", height, "m<br>",
        "Girth: ", girth, "cm<br>",
        address_fu
    ))
