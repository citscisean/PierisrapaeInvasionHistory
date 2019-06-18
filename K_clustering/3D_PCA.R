## This script is used to plot output from genetic cluster assignment programs in a 3D PCA
## Script written by Sean F. Ryan April 16 2018

## Clear global environment
rm(list = ls(all = T))

## Load libraries
library(adegenet)
library(vcfR)
library(poppr)
library(ape)
library(rgl)

## Set working directory
setwd("C:/K_clustering/")

## Import vcf file
VCF_A <- read.vcfR("RAD2_A_rsq0.2_Het06.recode.vcf")

## Import metadata
pop.data <-
  read.table("RAD2_A_rsq0.2_Het06.recode.meta.txt",
             sep = "\t",
             header = TRUE)

## You can skip the code below and load the gl.VCF if this step was done in the past
#load("gl.VCF_A_RAD2_A_rsq0.2_Het06.Rdata")

## Convert vcf to genlight object
gl.VCF_A <- vcfR2genlight(VCF_A)

## Set ploidy to diploid
ploidy(gl.VCF_A) <- 2

## Add genetic cluster assignments (K=5)
pop(gl.VCF_A) <- pop.data$K5_POP
save(gl.VCF_A, file = 'gl.VCF_A_RAD2_A_rsq0.2_Het06.Rdata')

## Load PCA if already created
#load("pca1_RAD2_A_rsq0.2_Het06.Rdata")

## Run PCA
pca1 <- glPca(gl.VCF_A,
              nf = 5,
              useC = T,
              parallel = F)
save(pca1, file = 'pca1_RAD2_A_rsq0.2_Het06.Rdata')

## Barplot of eigen values
windows()
barplot(pca1$eig[1:50], main = "PCA eigenvalues", col = heat.colors(50))

## Convert PCA results to a dataframe
PCAdata <- as.data.frame(pca1$scores)
write.table(
  PCAdata,
  file = "PierisProject_PCA.csv",
  append = FALSE,
  quote = TRUE,
  sep = ",",
  eol = "\n",
  na = "NA",
  dec = ".",
  row.names = TRUE,
  col.names = TRUE,
  qmethod = c("escape", "double"),
  fileEncoding = ""
)

## Set colors based on population assignment
pop.data$colorset[pop.data$K5_POP == "POP1"] = "#EDAB1D"
pop.data$colorset[pop.data$K5_POP == "POP2"] = "#1B8724"
pop.data$colorset[pop.data$K5_POP == "POP3"] = "#B555BA"
pop.data$colorset[pop.data$K5_POP == "POP4"] = "#F0AAB4"
pop.data$colorset[pop.data$K5_POP == "POP5"] = "#7CEDE9"

## 3D PCA plot
plot3d(
  PCAdata$PC1,
  PCAdata$PC2,
  PCAdata$PC3,
  size = 20,
  col = pop.data$colors,
  alpha = 0.8,
  xlab = "",
  ylab = "",
  zlab = ""
)
# use the following script if you want to add text labels to the ppints
par3d(cex = 0.5)
with(PCAdata, text3d(PC1, PC2, PC3, row.names(PCAdata)))
