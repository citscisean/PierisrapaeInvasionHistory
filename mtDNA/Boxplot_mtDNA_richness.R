## This script is used to create boxplots of mtDNA haplotype richness by subpopulation
## Script written by Sean F. Ryan July 27 2018

## Clear global environment
rm(list = ls())

## Load libraries
library(readr)
library(ggplot2)

## Set working directory
setwd("C:/mtDNA/")

## Import data
Hap_diversity_by_subPOP <-
  read_delim("Hap_diversity_by_subPOP.txt",
             "\t",
             escape_double = FALSE,
             trim_ws = TRUE)

Hap_diversity_by_subPOP$POP_name <-
  as.factor(Hap_diversity_by_subPOP$POP)

## Change order of populations
Hap_diversity_by_subPOP$POP_name <-
  factor(
    Hap_diversity_by_subPOP$POP_name,
    levels = levels(Hap_diversity_by_subPOP$POP_name)[c(2, 4, 7, 1, 8, 3, 6, 5)],
    ordered = FALSE
  )

## Plot data
windows()
ggplot(Hap_diversity_by_subPOP,
       aes(
         x = POP_name,
         y = as.numeric(mid),
         fill = factor(POP_name)
       )) +
  geom_boxplot()    +
  geom_point() +
  scale_fill_manual(
    values = c(
      "#EDAB1D",
      #orange
      "#1B8724",
      #green
      "#4072ef",
      #blue
      "#7CEDE9",
      #cyan
      "#F0AAB4",
      #pink
      "#94039b",
      #dark purple
      "#3D1600",
      #brown
      "#B555BA"  #purple
    )
  ) +
  scale_color_manual(
    values = c(
      "#EDAB1D",
      #orange
      "#1B8724",
      #green
      "#4072ef",
      #blue
      "#7CEDE9",
      #cyan
      "#F0AAB4",
      #pink
      "#94039b",
      #dark purple
      "#3D1600",
      #brown
      "#B555BA"  #purple
    )
  ) +
  ylab("mtDNA haplotype richness") +
  xlab("Population") +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    strip.background = element_blank(),
    plot.background = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      vjust = 0.2,
      size = 16
    ),
    axis.text.y = element_text(size = 16),
    legend.text = element_text(size = 16),
    plot.title = element_text(size = 16),
    axis.title = element_text(size = 16)
  )
