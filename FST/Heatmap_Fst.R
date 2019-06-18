## This script creates a heatmap from estimates of Fst
## Script written by Sean F. Ryan July 23 2018

## Clear global workspace
rm(list = ls())

## Load libraries
library(ggplot2)
library(readr)

## Import Fst files
Fstdata <- read_csv(
  "C:/Fst_100kb_K8_for_heatmap.csv",
  col_types = cols(POPa = col_character(),
                   POPb = col_character())
)

## Set the order of rows/columns
Fstdata$POPa_name <- factor(
  Fstdata$POPa_name,
  levels = c(
    "Russia",
    "Asia",
    "Africa",
    "Europe",
    "USA-E",
    "USA-W",
    "New Zealand",
    "Australia"
  )
)

Fstdata$POPb_name <- factor(
  Fstdata$POPb_name,
  levels = c(
    "Russia",
    "Asia",
    "Africa",
    "Europe",
    "USA-E",
    "USA-W",
    "New Zealand",
    "Australia"
  )
)

## Plot Fst estimates
windows()
ggplot(Fstdata, aes(POPa_name, POPb_name)) +
  geom_tile(aes(fill = Fst), color = "white") +
  scale_fill_gradient(low = "#FFFC00",
                      high = "#D71E23",
                      na.value = 'white') +
  scale_x_discrete(position = "top") +
  ylab("Population") +
  xlab("Population") +
  theme(
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16),
    plot.title = element_text(size = 16),
    axis.title = element_text(size = 16),
    axis.text.x = element_text(
      angle = 45,
      hjust = 0,
      vjust = -1,
      size = 16
    ),
    axis.text.y = element_text(size = 16)
  )
labs(fill = "Weighted Fst") +
  theme(
    legend.background = element_blank(),
    legend.key = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    strip.background = element_blank(),
    plot.background = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line = element_line(colour = "black")
  )
