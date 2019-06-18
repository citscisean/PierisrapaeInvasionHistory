## This script is used to plot expected heterozygosity using output files created from the script:
## Script written by Sean F. Ryan August 7 2018

## Clear working directory
rm(list = ls())

## Load libraries
library(readr)
library(ggplot2)

## Set working directory
setwd("C:/HET/HETobs")

## Create list of files for input
HETobs.files <-
  list.files(
    path = getwd(),
    pattern = ".txt",
    all.files = TRUE,
    full.names = TRUE,
    recursive = FALSE,
    ignore.case = FALSE,
    include.dirs = FALSE
  )

## Import files using a loop that strips unnecessary information and adds metadata
Results <- matrix(NA, nrow = length(HETobs.files), ncol = 2)

for (i in 1:length(HETobs.files)) {
  ## Load data
  HETobsdata <- read_delim(
    HETobs.files[i],
    "\t",
    escape_double = FALSE,
    na = "NA",
    trim_ws = TRUE,
    col_names = TRUE
  )
  
  ## Remove NANs
  HETobsdata2 <- HETobsdata[complete.cases(HETobsdata),]
  
  ## Strip file name down to coarse population level (K = 8)
  filename <- gsub(".*/", "", HETobs.files[i])
  filename2 <- gsub("\\..*", "", filename)
  filename3 <- gsub("RAD1_HETobs_loop_", "", filename2)
  
  ## Add pop info
  Results[i, 1] <- eval(filename3)
  
  ## Add mean for subpop
  #HETobsdata2$X1 <- as.numeric(levels(HETobsdata2$X1))[HETobsdata2$X1]
  Results[i, 2] <- quantile(HETobsdata2$V1, 0.5)
  
}

## Convert to dataframe
Results2 <- as.data.frame(Results)
colnames(Results2) <- c("POP", "HETobs")

Results2$POP_name[Results2$POP == "POP_1"] <- "Africa"
Results2$POP_name[Results2$POP == "POP_2"] <- "Europe"
Results2$POP_name[Results2$POP == "POP_3"] <- "Australia"
Results2$POP_name[Results2$POP == "POP_4"] <- "USA-E"
Results2$POP_name[Results2$POP == "POP_5"] <- "USA-W"
Results2$POP_name[Results2$POP == "POP_6"] <- "Russia"
Results2$POP_name[Results2$POP == "POP_7"] <- "Asia"
Results2$POP_name[Results2$POP == "POP_8"] <- "New Zealand"

Results2$POP_name <- as.factor(Results2$POP_name)

## Change order of populations
Results2$POP_name <-
  factor(Results2$POP_name,
         levels = levels(Results2$POP_name)[c(4, 2, 6, 1, 7, 8, 5, 3)],
         ordered = FALSE)

## Convert to numeric
Results2$HETobs <-
  as.numeric(levels(Results2$HETobs))[Results2$HETobs]

## Plot data
windows()
ggplot(Results2, aes(
  x = POP_name,
  y = as.numeric(HETobs),
  fill = factor(POP_name)
)) +
  geom_boxplot()    +
  scale_fill_manual(
    values = c(
      "#1B8724",
      #green
      "#EDAB1D",
      #orange
      "#3D1600",
      #brown
      "#7CEDE9",
      #cyan
      "#4072ef",
      #blue
      "#F0AAB4",
      #pink
      "#B555BA",
      #purple
      "#94039b" #dark purple
    )
  ) +
  scale_color_manual(
    values = c(
      "#1B8724",
      #green
      "#EDAB1D",
      #orange
      "#3D1600",
      #brown
      "#7CEDE9",
      #cyan
      "#4072ef",
      #blue
      "#F0AAB4",
      #pink
      "#B555BA",
      #purple
      "#94039b" #dark purple
    )
  ) +
  ylab("HETobs") +
  xlab("Population") +
  theme(
    legend.position = "none",
    #legend.background = element_blank(),
    #legend.key = element_blank(),
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




############################################################
##Plot using mean and error for each subpopulation separately

## Clear working directory
rm(list = ls())

## Load libraries
library(readr)
library(ggplot2)

## Set working directory
setwd("C:/HET/HETobs")

## Create list of files for input
HETobs.files <-
  list.files(
    path = getwd(),
    pattern = ".txt",
    all.files = TRUE,
    full.names = TRUE,
    recursive = FALSE,
    ignore.case = FALSE,
    include.dirs = FALSE
  )


## Import files using a loop that strips unnecessary information and adds metadata

Results <- matrix(NA, nrow = length(HETobs.files), ncol = 4)


for (i in 1:length(HETobs.files)) {
  ## Load data
  HETobsdata <- read_delim(
    HETobs.files[i],
    "\t",
    escape_double = FALSE,
    na = "NaN",
    trim_ws = TRUE,
    col_names = TRUE
  )
  
  
  ## Remove NANs
  HETobsdata2 <- HETobsdata[complete.cases(HETobsdata),]
  
  ## Strip file name down to coarse population level (K = 8)
  filename <- gsub(".*/", "", HETobs.files[i])
  filename2 <- gsub("\\..*", "", filename)
  filename3 <- gsub("RAD1_HETobs_loop_", "", filename2)
  
  ## Add pop info
  Results[i, 1] <- eval(filename3)
  
  ## Add mean for subpop
  #HETobsdata2$X1 <- as.numeric(levels(HETobsdata2$X1))[HETobsdata2$X1]
  # Results[i,2] <- quantile(HETobsdata2$V1, 0.5)
  # Results[i,3] <- quantile(HETobsdata2$V1, 0.05)
  # Results[i,4] <- quantile(HETobsdata2$V1, 0.95)
  
  Results[i, 2] <- mean(HETobsdata2$V1)
  error <-
    qt(0.975, df = length(HETobsdata2$V1) - 1) * sd(HETobsdata2$V1) / sqrt(length(HETobsdata2$V1))
  Results[i, 3] <- mean(HETobsdata2$V1) - error
  Results[i, 4] <- mean(HETobsdata2$V1) + error
}

Results2 <- as.data.frame(Results)
colnames(Results2) <- c("POP", "HETobs", "q05", "q95")

Results2$POP_name[Results2$POP == "POP_1"] <- "Africa"
Results2$POP_name[Results2$POP == "POP_2"] <- "Europe"
Results2$POP_name[Results2$POP == "POP_3"] <- "Australia"
Results2$POP_name[Results2$POP == "POP_4"] <- "USA-E"
Results2$POP_name[Results2$POP == "POP_5"] <- "USA-W"
Results2$POP_name[Results2$POP == "POP_6"] <- "Russia"
Results2$POP_name[Results2$POP == "POP_7"] <- "Asia"
Results2$POP_name[Results2$POP == "POP_8"] <- "New Zealand"

Results2$POP_name <- as.factor(Results2$POP_name)

## Change order of populations
Results2$POP_name <-
  factor(Results2$POP_name,
         levels = levels(Results2$POP_name)[c(4, 2, 6, 1, 7, 8, 5, 3)],
         ordered = FALSE)

Results2$HETobs <-
  as.numeric(levels(Results2$HETobs))[Results2$HETobs]
Results2$q05 <- as.numeric(levels(Results2$q05))[Results2$q05]
Results2$q95 <- as.numeric(levels(Results2$q95))[Results2$q95]



library(ggplot2)

windows()
ggplot(Results2, aes(
  x = POP_name,
  y = as.numeric(HETobs),
  colour = factor(POP_name)
)) +
  geom_point(size = 4)    +
  geom_errorbar(aes(ymin = q05, ymax = q95), width = .2, size = 1) +
  scale_fill_manual(
    values = c(
      "#1B8724",
      #green
      "#EDAB1D",
      #orange
      "#3D1600",
      #brown
      "#7CEDE9",
      #cyan
      "#4072ef",
      #blue
      "#F0AAB4",
      #pink
      "#B555BA",
      #purple
      "#94039b" #dark purple
    )
  ) +
  scale_color_manual(
    values = c(
      "#1B8724",
      #green
      "#EDAB1D",
      #orange
      "#3D1600",
      #brown
      "#7CEDE9",
      #cyan
      "#4072ef",
      #blue
      "#F0AAB4",
      #pink
      "#B555BA",
      #purple
      "#94039b" #dark purple
    )
  ) +
  ylab("HETobs") +
  xlab("Population") +
  theme(
    legend.position = "none",
    #legend.background = element_blank(),
    #legend.key = element_blank(),
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
