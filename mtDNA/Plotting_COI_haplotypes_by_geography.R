## This script is used to plot each individual colored by mtDNA haplotype
## Script written by Sean F. Ryan July 27 2018

## Clear global environment
rm(list = ls())

## Load libraries
library(ggmap)
library(RCurl)
library(ggplot2)
library(readr)
library(RColorBrewer)

## Set working directory
setwd("C:/mtDNA/")

## Import data
Pieris_mtDNA_data <- read_csv("COI_data_for_plotting.csv")

##Plot using a unique color for each haplotype
## Create list of colors
n <- 88
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
save(col_vector, file = "col_vector")

## Assign color to each haplotype
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 1] = col_vector[1]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 2] = col_vector[2]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 3] = col_vector[3]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 4] = col_vector[4]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 5] = col_vector[5]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 6] = col_vector[6]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 7] = col_vector[7]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 8] = col_vector[8]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 9] = col_vector[9]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 10] = col_vector[10]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 11] = col_vector[11]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 12] = col_vector[12]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 13] = col_vector[13]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 14] = col_vector[14]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 15] = col_vector[15]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 16] = col_vector[16]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 17] = col_vector[17]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 18] = col_vector[18]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 19] = col_vector[19]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 20] = col_vector[20]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 21] = col_vector[21]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 22] = col_vector[22]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 23] = col_vector[23]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 24] = col_vector[24]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 25] = col_vector[25]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 26] = col_vector[26]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 27] = col_vector[27]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 28] = col_vector[28]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 29] = col_vector[29]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 30] = col_vector[30]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 31] = col_vector[31]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 32] = col_vector[32]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 33] = col_vector[33]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 34] = col_vector[34]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 35] = col_vector[35]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 36] = col_vector[36]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 37] = col_vector[37]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 38] = col_vector[38]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 39] = col_vector[39]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 40] = col_vector[40]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 41] = col_vector[41]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 42] = col_vector[42]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 43] = col_vector[43]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 44] = col_vector[44]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 45] = col_vector[45]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 46] = col_vector[46]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 47] = col_vector[47]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 48] = col_vector[48]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 49] = col_vector[49]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 50] = col_vector[50]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 51] = col_vector[51]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 52] = col_vector[52]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 53] = col_vector[53]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 54] = col_vector[54]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 55] = col_vector[55]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 56] = col_vector[56]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 57] = col_vector[57]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 59] = col_vector[58]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 60] = col_vector[59]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 61] = col_vector[60]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 62] = col_vector[61]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 63] = col_vector[62]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 64] = col_vector[63]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 65] = col_vector[64]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 66] = col_vector[65]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 67] = col_vector[66]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 68] = col_vector[67]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 69] = col_vector[68]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 70] = col_vector[69]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 71] = col_vector[70]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 72] = col_vector[71]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 73] = col_vector[72]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 74] = col_vector[73]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 75] = col_vector[74]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 76] = col_vector[75]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 77] = col_vector[76]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 79] = col_vector[77]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 80] = col_vector[78]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 81] = col_vector[79]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 82] = col_vector[80]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 83] = col_vector[81]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 85] = col_vector[82]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 87] = col_vector[83]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 88] = col_vector[84]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 89] = col_vector[85]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 90] = col_vector[86]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 91] = col_vector[87]
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 92] = col_vector[88]

Pieris_mtDNA_data <-
  Pieris_mtDNA_data[sample(nrow(Pieris_mtDNA_data)),]

## Plot the data
mp <- NULL
mapWorld <-
  borders("world", colour = "white", fill = "grey90") # create a layer of borders
mp <-
  ggplot(data = Pieris_mtDNA_data, aes(x = Longitude, y = Latitude, show_guide =
                                         T)) +   mapWorld
mp <-
  mp + geom_jitter(
    color = alpha("grey20", 1.0),
    fill = alpha(Pieris_mtDNA_data$colorset, 1.0),
    na.rm = T,
    size = 5,
    width = 3.0,
    height = 3.0,
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






## Plot using a unique color for only the most common haplotypes
Pieris_mtDNA_data$pch_set[Pieris_mtDNA_data$Source == "This_study"] = 19
Pieris_mtDNA_data$pch_set[Pieris_mtDNA_data$Source == "BOLD"] = 19

Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 1] = "#88A2AA"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 2] = "#2E5339"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 3] = "#E2856E"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 4] = "#F42C04"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 5] = "#0F1A20"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 6] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 7] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 8] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 9] = "#2E5339"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 10] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 11] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 12] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 13] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 14] = "#00FDDC"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 15] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 16] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 17] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 18] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 19] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 20] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 21] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 22] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 23] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 24] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 25] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 26] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 27] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 28] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 29] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 30] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 31] = "#d7f47f"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 32] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 33] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 34] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 35] = "#e8d4f7"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 36] = "#F65BE3"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 37] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 38] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 39] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 40] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 41] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 42] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 43] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 44] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 45] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 46] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 47] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 48] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 49] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 50] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 51] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 52] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 53] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 54] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 55] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 56] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 57] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 59] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 60] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 61] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 62] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 63] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 64] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 65] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 66] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 67] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 68] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 69] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 70] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 71] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 72] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 73] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 74] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 75] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 76] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 77] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 79] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 80] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 81] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 82] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 83] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 85] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 87] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 88] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 89] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 90] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 91] = "#F9DC5C"
Pieris_mtDNA_data$colorset[Pieris_mtDNA_data$Haplotype == 92] = "#F9DC5C"

## Plot the data
mp <- NULL
mapWorld <-
  borders("world", colour = "white", fill = "grey90") # create a layer of borders
mp <-
  ggplot(data = Pieris_mtDNA_data, aes(x = Longitude, y = Latitude, show_guide =
                                         T)) +   mapWorld
mp <-
  mp + geom_jitter(
    color = alpha("grey20", 1.0),
    fill = alpha(Pieris_mtDNA_data$colorset, 1.0),
    na.rm = T,
    size = 5,
    width = 3.0,
    height = 3.0,
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



## Create plot to use for a legend
mtDNAcolors_for_legend <- read_delim(
  "mtDNAcolors_for_legend.txt",
  "\t",
  quote = "\"",
  escape_double = F,
  trim_ws = TRUE
)

mp <-
  ggplot(data = mtDNAcolors_for_legend, aes(x = X, y = Y, show_guide = T))
mp <-
  mp + geom_jitter(
    color = alpha("grey20", 1.0),
    fill = alpha(mtDNAcolors_for_legend$Color, 1.0),
    na.rm = T,
    size = 5,
    width = 0.0,
    height = 0.0,
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


## Subset to a particular haplotype (e.g., Pra1)
Pieris_mtDNA_data_1 <-
  Pieris_mtDNA_data[Pieris_mtDNA_data$Haplotype == 1, ]

mp <- NULL
mapWorld <-
  borders("world", colour = "white", fill = "grey90") # create a layer of borders
mp <-
  ggplot(data = Pieris_mtDNA_data_1, aes(x = Longitude, y = Latitude, show_guide =
                                           T)) +   mapWorld
mp <-
  mp + geom_jitter(
    color = alpha("grey20", 1.0),
    fill = alpha(Pieris_mtDNA_data_1$colorset, 1.0),
    na.rm = T,
    size = 10,
    width = 3.0,
    height = 3.0,
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
