## This script is used to plot estimates of pi for each subpopulation by their geographic location
## Script written by Sean F. Ryan May 22 2018

## Clear global environment
rm(list = ls())

## Load libraries
library(readr)
library(ggplot2)

## Set working directory
setwd("C:/PI/")

## Import data
pi_data <- read_delim("pi_for_heatmap.txt",
                      "\t",
                      escape_double = FALSE,
                      trim_ws = TRUE)

colfunc <-
  colorRampPalette(c("dodgerblue3", "lightgoldenrod1", "firebrick3"))

## Plot estimages of pi for each subpopation on world map
mp <- NULL
mapWorld <-
  borders("world", colour = "white", fill = "grey90") # create a layer of borders
mp <-
  ggplot(data = pi_data, aes(x = Long, y = Lat, show_guide = T)) +   mapWorld
mp <- mp + geom_point(aes(colour = pi), size = 8) +
  scale_colour_gradientn(colours = colfunc(7)) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    plot.background = element_rect(fill = "white"),
    panel.border = element_blank(),
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(size = 22)
  )

windows()
mp
