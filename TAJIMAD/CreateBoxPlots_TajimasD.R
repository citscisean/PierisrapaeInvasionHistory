## This script is used to create boxplots for estimates of Tajima's D by subpopulation
## Script written by Sean F. Ryan July 27 2018

## Clear global environment
rm(list = ls())

## Load libraries
library(readr)

## Set working directory
setwd("C:/TAJIMAD/Results")

## Create list of files for input
TajD.files <-
  list.files(
    path = getwd(),
    pattern = "_n100",
    all.files = TRUE,
    full.names = TRUE,
    recursive = FALSE,
    ignore.case = FALSE,
    include.dirs = FALSE
  )

## Load data
Results <- matrix(NA, nrow = length(TajD.files), ncol = 2)

for (i in 1:length(TajD.files)) {
  TajDdata <- read_delim(
    TajD.files[i],
    "\t",
    escape_double = FALSE,
    na = "NaN",
    trim_ws = TRUE,
    col_names = FALSE
  )
  
  
  ## Remove NANs
  TajDdata2 <- TajDdata[complete.cases(TajDdata),]
  
  ## Strip file name down to coarse population level (K = 8)
  filename <- gsub(".*/", "", TajD.files[i])
  filename2 <- gsub("\\..*", "", filename)
  
  ## Add pop info
  Results[i, 1] <- eval(filename2)
  
  ## Add mean for subpop
  Results[i, 2] <- quantile(TajDdata2$X1, 0.5)
  
}

Results2 <- as.data.frame(Results)
colnames(Results2) <- c("POP", "taj")

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

Results2$taj <- as.numeric(levels(Results2$taj))[Results2$taj]

library(ggplot2)

windows()
ggplot(Results2, aes(
  x = POP_name,
  y = as.numeric(taj),
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
  geom_hline(yintercept = 0, color = "grey") +
  ylab("Tajima's D") +
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







## Set working directory
setwd("C:/TAJIMAD/Results")

## Create list of files for input
TajD.files <-
  list.files(
    path = getwd(),
    pattern = "_n100",
    all.files = TRUE,
    full.names = TRUE,
    recursive = FALSE,
    ignore.case = FALSE,
    include.dirs = FALSE
  )

## Load data
Results <- matrix(NA, nrow = length(TajD.files), ncol = 4)

for (i in 1:length(TajD.files)) {
  TajDdata <- read_delim(
    TajD.files[i],
    "\t",
    escape_double = FALSE,
    na = "NaN",
    trim_ws = TRUE,
    col_names = FALSE
  )
  
  ## Remove NANs
  TajDdata2 <- TajDdata[complete.cases(TajDdata),]
  
  ## Strip file name down to coarse population level (K = 8)
  filename <- gsub(".*/", "", TajD.files[i])
  filename2 <- gsub("\\..*", "", filename)
  
  ## Add pop info
  Results[i, 1] <- eval(filename2)
  
  Results[i, 2] <- mean(TajDdata2$X1)
  error <-
    qt(0.975, df = length(TajDdata2$X1) - 1) * sd(TajDdata2$X1) / sqrt(length(TajDdata2$X1))
  Results[i, 3] <- mean(TajDdata2$X1) - error
  Results[i, 4] <- mean(TajDdata2$X1) + error
}

Results2 <- as.data.frame(Results)
colnames(Results2) <- c("POP", "taj", "q05", "q95")

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

Results2$taj <- as.numeric(levels(Results2$taj))[Results2$taj]
Results2$q05 <- as.numeric(levels(Results2$q05))[Results2$q05]
Results2$q95 <- as.numeric(levels(Results2$q95))[Results2$q95]



library(ggplot2)

windows()
ggplot(Results2, aes(
  x = POP_name,
  y = as.numeric(taj),
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
  geom_hline(yintercept = 0, color = "grey") +
  ylab("Tajima's D") +
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
