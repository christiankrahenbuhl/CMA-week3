library(readr)        # to import tabular data (e.g. csv)
library(dplyr)        # to manipulate (tabular) data
library(ggplot2)      # to visualize data
library(sf)
library(terra)        # To handle raster data
library(lubridate)    # To handle dates and times


caro <- read_delim("caro60.csv",",") 
caro <- tibble(caro)