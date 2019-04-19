# Load libraries
library(rgdal)
library(ggplot2)
library(grid)
library(geogrid)
library(maptools)
library(viridis)
library(readxl)
library(stringr)
library(tidyr)
library(gridExtra)
library(tigris)
library(scales)
library(extrafont)

# Set working directory
setwd("D:/R/DataViz-R/PRU14")

# Read shapefile into R
input_file <- "D:/R/DataViz-R/PRU14/malaysia_par_2015_demographics/perak.shp"

original_shapes <- read_polygons(input_file)

raw <- read_polygons(input_file)
raw@data$xcentroid <- sp::coordinates(raw)[,1]
raw@data$ycentroid <- sp::coordinates(raw)[,2]

clean <- function(shape) {
  shape@data$id = rownames(shape@data)
  shape.points = fortify(shape, region="id")
  shape.df = merge(shape.points, shape@data, by="id")
}

# Function to create hexagon bins from shapefile
par(mfrow = c(2, 4), mar = c(0, 0, 2, 0))
for (i in 1:8) {
  new_cells <- calculate_grid(shape = raw, learning_rate = 0.01, grid_type = "regular", seed = i)
  plot(new_cells, main = paste("Seed", i, sep = " "))
}

new_cells_hex <- calculate_grid(shape = raw, grid_type = "regular", seed = 4)
resulthex <- assign_polygons(raw, new_cells_hex)

writeOGR(obj=resulthex, dsn="D:/R/DataViz-R/PRU14", layer="perak_parl_hex", driver="ESRI Shapefile")
