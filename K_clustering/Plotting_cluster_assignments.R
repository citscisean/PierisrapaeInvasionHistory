## This script is used to plot individuals colored by cluster assignment on global map
## Script written by Sean F. Ryan April 16 2018

## Clear global environment
rm(list = ls())

## Load libraries
library(ggmap)
library(RCurl)
library(ggplot2)
library(readr)

## Set working directory
setwd("C:/K_clustering/")

## Import data
RAD_clusters <-
  read_csv("RADclusterassignments_plotting_K7.csv")

## Assign colors to each Cluster (K=7)
RAD_clusters$colorset[RAD_clusters$Cluster_Assignment == 1] = "#7CEDE9" #cyan
RAD_clusters$colorset[RAD_clusters$Cluster_Assignment == 2] = "#1B8724" #green
RAD_clusters$colorset[RAD_clusters$Cluster_Assignment == 3] = "#B555BA" #purple
RAD_clusters$colorset[RAD_clusters$Cluster_Assignment == 4] = "#4072ef" #blue
RAD_clusters$colorset[RAD_clusters$Cluster_Assignment == 5] = "#F0AAB4" #pink
RAD_clusters$colorset[RAD_clusters$Cluster_Assignment == 6] = "#3D1600" #brown
RAD_clusters$colorset[RAD_clusters$Cluster_Assignment == 7] = "#EDAB1D" #orange

## Plot using world map
mp <- NULL
mapWorld <-
  borders("world", colour = "white", fill = "grey80") # create a layer of borders
mp <-
  ggplot(data = RAD_clusters, aes(x = Longitude, y = Latitude, show_guide =
                                    T)) +   mapWorld
mp <-
  mp + geom_jitter(
    color = alpha("grey20", 1.0),
    fill = alpha(RAD_clusters$colorset, 0.4),
    na.rm = T,
    size = 7,
    width = 1.0,
    height = 1.0,
    pch = 21
  ) +
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
