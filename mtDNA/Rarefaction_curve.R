## This script is used to estimate haplotype richness for each subpopulation
## Script written by Sean F. Ryan July 27 2018

## Clear global environment
rm(list = ls())

## Load libraries
library(iNEXT)
library(ggplot2)

## Set working directory
setwd("C:/mtDNA/")

## Import data
mtDNA_haplotypes_by_K <- read.csv("mtDNAhaplotypes_K8.txt", "\t", row.names = 1, header= TRUE)
out_K8 <- iNEXT(mtDNA_haplotypes_by_K, q=c(0), datatype="abundance", nboot = 1000, endpoint=1000)

windows()
plot <- ggiNEXT(out_K8, type=1,  grey = F, se = TRUE) 
plot + 
  scale_shape_manual(values=c(".", ".", ".", ".", ".", ".", ".", ".", ".")) +
  scale_colour_manual(values = c("#7CEDE9", "#1B8724", "#94039b", "#4072ef", "#F0AAB4","#3D1600", "#EDAB1D", "#B555BA")) +
  scale_fill_manual(values = c("#7CEDE9", "#1B8724", "#94039b", "#4072ef", "#F0AAB4","#3D1600", "#EDAB1D", "#B555BA")) +
  theme(legend.position="none",
        panel.background = element_blank(), 
        strip.background = element_blank(), 
        plot.background = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.2, size = 16),
        axis.text.y = element_text(size = 16),
        legend.text = element_text(size = 16),
        plot.title = element_text(size=16),
        axis.title=element_text(size=16)
  )

