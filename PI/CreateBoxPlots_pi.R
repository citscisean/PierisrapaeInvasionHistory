## This script is used to create boxplots for estimates of pi, grouped by subpopulation
## Script written by Sean F. Ryan May 22 2018

## Clear global environment
rm(list = ls())

## Load libraries
library(readr)
library(ggplot2)

## Set working directory
setwd("C:/PI/windowed_pi_invariants_100kb")

## Create list of files for input
PI.files <-
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
Results <- matrix(NA, nrow = length(PI.files), ncol = 2)

for (i in 1:length(PI.files)) {
  PIdata <- read_delim(
    PI.files[i],
    "\t",
    escape_double = FALSE,
    na = "NaN",
    trim_ws = TRUE,
    col_names = FALSE
  )
  
  ## Remove NANs
  PIdata2 <- PIdata[complete.cases(PIdata),]
  
  ## Strip file name down to coarse population level (K = 8)
  filename <- gsub(".*/", "", PI.files[i])
  filename2 <- gsub("\\..*", "", filename)
  #filename2 <- gsub("_pi_n1000","", filename) #use this line instead of the one above if need to keep subpopulation resolution
  
  
  ## Add pop info
  Results[i, 1] <- eval(filename2)
  
  ## Add mean for subpop
  #PIdata2$X1 <- as.numeric(levels(PIdata2$X1))[PIdata2$X1]
  Results[i, 2] <- quantile(PIdata2$X1, 0.5)
  
}

Results2 <- as.data.frame(Results)
colnames(Results2) <- c("POP", "pi")

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

Results2$pi <- as.numeric(levels(Results2$pi))[Results2$pi]

windows()
ggplot(Results2, aes(
  x = POP_name,
  y = as.numeric(pi),
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
  ylab("pi") +
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







## Clear global environment
rm(list = ls())

## Load libraries
library(readr)
library(ggplot2)

## Set working directory
setwd("C:/PI/windowed_pi_invariants_100kb")

## Create list of files for input
PI.files <-
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
Results <- matrix(NA, nrow = length(PI.files), ncol = 4)

for (i in 1:length(PI.files)) {
  PIdata <- read_delim(
    PI.files[i],
    "\t",
    escape_double = FALSE,
    na = "NaN",
    trim_ws = TRUE,
    col_names = FALSE
  )
  
  ## Remove NANs
  PIdata2 <- PIdata[complete.cases(PIdata),]
  
  ## Strip file name down to coarse population level (K = 8)
  filename <- gsub(".*/", "", PI.files[i])
  filename2 <- gsub("\\..*", "", filename)
  
  ## Add pop info
  Results[i, 1] <- eval(filename2)
  
  Results[i, 2] <- mean(PIdata2$X1)
  error <-
    qt(0.975, df = length(PIdata2$X1) - 1) * sd(PIdata2$X1) / sqrt(length(PIdata2$X1))
  Results[i, 3] <- mean(PIdata2$X1) - error
  Results[i, 4] <- mean(PIdata2$X1) + error
}

Results2 <- as.data.frame(Results)
colnames(Results2) <- c("POP", "pi", "q05", "q95")

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

Results2$pi <- as.numeric(levels(Results2$pi))[Results2$pi]
Results2$q05 <- as.numeric(levels(Results2$q05))[Results2$q05]
Results2$q95 <- as.numeric(levels(Results2$q95))[Results2$q95]


windows()
ggplot(Results2, aes(
  x = POP_name,
  y = as.numeric(pi),
  colour = factor(POP_name)
)) +
  geom_point(size = 4)    +
  geom_errorbar(aes(ymin = q05, ymax = q95), width = .2, size = 1) +
  scale_y_continuous(limits = c(0.004,  0.008)) +
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
  ylab("pi") +
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
