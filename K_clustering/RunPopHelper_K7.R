## This script is used to plot output from the programs fastSTRUCTURE and ADMIXTURE
## Script written by Sean F. Ryan April 16 2018

#######fastSTRUCTURE - logistic
## Clear global environment
rm(list = ls())

## Load libraries
library(pophelper)
library(ggplot2)
library(erer)

## Set working directory
setwd("C:/K_clustering/")

##! had to make all numbers two places (01, 02, 03, etc or it wouldn't order them by K correctly)
## Get names of a files in the directory
f.files <-
  list.files(
    path = "C:/K_clustering/fastSTRUCTURE_output/",
    pattern = ".meanQ",
    all.files = TRUE,
    full.names = TRUE,
    recursive = FALSE,
    ignore.case = FALSE,
    include.dirs = FALSE
  )

## Import list of files
f.list <- readQ(filetype = "auto", f.files)

## Create table from all the files (just to look at K and sample size for each file in the list)
f.tabulated <- tabulateQ(qlist = readQ(f.files))

## Import metadata
metadata <-
  read.delim(
    "C:/K_clustering/RAD2_A_rsq02_Het06_metadata.txt",
    "\t",
    header = T,
    stringsAsFactors = F
  )


## Remove sample Pr_1159 (30th row in each list) and NC things 8, 10, 14
f.list2 <- lapply(f.list, function(x)
  x[c(-8,-10,-14,-30), ])

metadata2 <- as.data.frame(metadata[c(-8,-10,-14,-30), ])

## Need to convert each object in the list back into a dataframe
f.list3 <- lapply(f.list2, function(x)
  data.frame(x))

# if all runs are equal length, add indlab to all runs
if (length(unique(sapply(f.list3, nrow))) == 1)
  f.list3 <- lapply(f.list3, "rownames<-", metadata2$ID)


## Create files to use for labels
onelabset <-
  metadata2[, 5, drop = FALSE] # use drop=FALSE to preserve it being a dataframe
twolabset <- metadata2[, 4:5]

plotQ(
  f.list3[9],
  grplab = twolabset,
  selgrp = "Continent",
  clustercol = c(
    "#1B8724",
    #green
    "#4072ef",
    #blue
    "#B555BA",
    #purple
    "#7CEDE9",
    #cyan
    "#e3e3e3",
    #light grey
    "#F0AAB4",
    #pink
    "#929292",
    #dark grey
    "#EDAB1D",
    #orange
    "#000000" #black
  ),
  #sortind="all",
  showindlab = F,
  divtype = 1,
  ordergrp = TRUE,
  height = 1.6,
  indlabsize = 2.3,
  indlabheight = 0.08,
  indlabspacer = -1,
  barbordercolour = "white",
  barbordersize = 0,
  grplabangle = 90,
  grplabheight = 5,
  subsetgrp = c("Asia", "Europe", "Africa", "North_America", "Australasia"),
  outputfilename = "fastSTRUCTURE_K9_Autosomal_logistic",
  dpi = 1200,
  imgtype = "png"
)


plotQ(
  f.list3[2:30],
  grplab = twolabset,
  selgrp = "Continent",
  imgoutput = "join",
  #sortind="all",
  showindlab = F,
  divtype = 1,
  ordergrp = TRUE,
  height = 1.6,
  indlabsize = 2.3,
  indlabheight = 0.08,
  indlabspacer = -1,
  barbordercolour = "white",
  barbordersize = 0,
  grplabangle = 90,
  grplabheight = 5,
  subsetgrp = c("Asia", "Europe", "Africa", "North_America", "Australasia"),
  outputfilename = "fastSTRUCTURE_K2to30_twolabel_Autosomal_logistic",
  dpi = 1200,
  imgtype = "png"
)


#######Admixture
## Clear global environment
rm(list = ls())

## Load libraries
library(pophelper)
library(ggplot2)
library(erer)

## Set working directory
setwd("C:/K_clustering/")

##! had to make all numbers two places (01, 02, 03, etc or it wouldn't order them by K correctly)
## Get names of a files in the directory
f.files <-
  list.files(
    path = "C:/K_clustering/Admixture_output/",
    pattern = ".Q",
    all.files = TRUE,
    full.names = TRUE,
    recursive = FALSE,
    ignore.case = FALSE,
    include.dirs = FALSE
  )

## Import list of files
f.list <- readQ(filetype = "auto", f.files)

## Create table from all the files (just to look at K and sample size for each file in the list)
f.tabulated <- tabulateQ(qlist = readQ(f.files))

## Import metadata
metadata <-
  read.delim(
    "C:/K_clustering/RAD2_A_rsq02_Het06_metadata.txt",
    "\t",
    header = T,
    stringsAsFactors = F
  )

## Remove sample Pr_1159 (30th row in each list) and NC things 8, 10, 14
f.list2 <- lapply(f.list, function(x)
  x[c(-8,-10,-14,-30), ])
metadata2 <- as.data.frame(metadata[c(-8,-10,-14,-30), ])

## Need to convert each object in the list back into a dataframe
f.list3 <- lapply(f.list2, function(x)
  data.frame(x))

# If all runs are equal length, add indlab to all runs
if (length(unique(sapply(f.list3, nrow))) == 1)
  f.list3 <- lapply(f.list3, "rownames<-", metadata2$ID)

## Create files to use for labels
onelabset <-
  metadata2[, 5, drop = FALSE] # use drop=FALSE to preserve it being a dataframe
twolabset <- metadata2[, 4:5]

plotQ(
  f.list3[7],
  grplab = twolabset,
  selgrp = "Continent",
  clustercol = c(
    "#7CEDE9",
    #cyan
    "#1B8724",
    #green
    "#B555BA",
    #purple
    "#4072ef",
    #blue
    "#F0AAB4",
    #pink
    "#3D1600",
    #brown
    "#EDAB1D" #orange
  ),
  #sortind="all",
  showindlab = F,
  divtype = 1,
  #showdiv = F,
  ordergrp = TRUE,
  height = 1.6,
  indlabsize = 2.3,
  grplabsize = 0.5,
  indlabheight = 0.08,
  indlabspacer = -1,
  barbordercolour = "white",
  barbordersize = 0,
  grplabangle = 90,
  grplabheight = 5,
  subsetgrp = c("Asia", "Europe", "Africa", "North_America", "Australasia"),
  outputfilename = "Admixture_Autosomal_K7",
  dpi = 1200,
  imgtype = "png"
)


plotQ(
  f.list3[2:30],
  grplab = twolabset,
  selgrp = "Continent",
  imgoutput = "join",
  #sortind="all",
  showindlab = F,
  divtype = 1,
  ordergrp = TRUE,
  height = 1.6,
  indlabsize = 2.3,
  indlabheight = 0.08,
  indlabspacer = -1,
  barbordercolour = "white",
  barbordersize = 0,
  grplabangle = 90,
  grplabheight = 5,
  subsetgrp = c("Asia", "Europe", "Africa", "North_America", "Australasia"),
  outputfilename = "Admixture_K2to30_twolabel_Autosomal",
  dpi = 1200,
  imgtype = "png"
)
