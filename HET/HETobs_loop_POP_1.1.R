## This script uses a parallelized loop to generate bootstrapped estimates for expected heterozygosity
## The script is written so that it is specific to a given subpopulation (hard-coded); a seperate script was created for each subpopulation
## Script written by Sean F. Ryan August 23 2018

## Clear global environment
rm(list = ls())

## Load libraries
library(vcfR)
library(adegenet)
library(foreach)
library(doMC) #for linux
registerDoMC(cores = 4) #for linux

## Set working directory
setwd("C:/HET")

## Create list of files for input
vcf.files <-
  list.files(
    path = getwd(),
    pattern = "RAD1_A_Het06_POP_1.1.recode.vcf",
    all.files = TRUE,
    full.names = TRUE,
    recursive = FALSE,
    ignore.case = FALSE,
    include.dirs = FALSE
  )

## Set number of bootstraps
bootstraps = 1000

## Create empty matrix to store results
stats_i <- matrix(NA, nrow = bootstraps, ncol = 1)

## Load data
VCF_i <- read.vcfR(vcf.files[1])

#convert to genind
#supress warnings because if there are loci with all NAs they will be removed and a warning thrown
options(warn = -1)
gi.vcf_i <- vcfR2genind(VCF_i)
options(warn = 0) #reinstate warnings
ploidy(gi.vcf_i) <- 2

## Run parallelized loop
Par_Results <- foreach(j = 1:bootstraps) %dopar%  {
  ## Subset to 8 individuals
  gi.vcf_j <-
    gi.vcf_i[sample(1:nrow(gi.vcf_i@tab), 8, replace = TRUE), ]
  stats <- (summary(gi.vcf_j))
  stats_i <- mean(na.omit(stats$Hobs))
  
  ## Print result to be added to the output for the parfor loop
  stats_i
  
}

## Combine all results
Results <- do.call(rbind, Par_Results)

## Write to file
write.table(
  Results,
  paste0("./HET/Results/RAD1_HETobs_loop_POP_1.1.txt"),
  row.names = F
)
