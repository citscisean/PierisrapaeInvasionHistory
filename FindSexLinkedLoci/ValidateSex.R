## This script uses sites (SNPs) that are putatively sex linked and genetic data (vcf file) with individuals with a known sex and looks for violations of expected heterozygosity (e.g., in the case of butterflies, males should be heterozygous on Z chromosome but females should appear homozygous because they are hemizygous)
## Script written by Sean F. Ryan July 24 2018

## Clear global environment
rm(list = ls())

## Load libraries
library(vcfR)
library(readr)
library(ggplot2)

## Set working directory
setwd("C:/FindSexLinkedLoci")

## Import vcf file with males only
Males_vcf <-
  read.vcfR(
    "C:/FindSexLinkedLoci/PierisProject_101217_biSNPs_Prapae_hard_GQ20_maf01_1X50ind_95depth_75imiss_10X90ind_25imiss_Z_males.recode.vcf",
    convertNA = TRUE,
    checkFile = TRUE,
    verbose = TRUE
  )

## Import vcf file with females only
Females_vcf <-
  read.vcfR(
    "C:/FindSexLinkedLoci/PierisProject_101217_biSNPs_Prapae_hard_GQ20_maf01_1X50ind_95depth_75imiss_10X90ind_25imiss_Z_females.recode.vcf",
    convertNA = TRUE,
    checkFile = TRUE,
    verbose = TRUE
  )

## Convert vcfs to gentoype matrices
Males_gt <- extract.gt(Males_vcf)
Females_gt <- extract.gt(Females_vcf)

## Create matrices of true/false for het calls
Males_hets <- is_het(Males_gt, na_is_false = FALSE)
Males_hets[Males_hets == TRUE] <- 1
Males_hets[Males_hets == FALSE] <- 0

Females_hets <- is_het(Females_gt, na_is_false = FALSE)
Females_hets[Females_hets == TRUE] <- 1
Females_hets[Females_hets == FALSE] <- 0

windows()
heatmap(
  Males_hets,
  scale = "none",
  Rowv = NA,
  Colv = NA,
  col = c("lightcyan1", "indianred1")
)

windows()
heatmap(
  Females_hets,
  scale = "none",
  Rowv = NA,
  Colv = NA,
  col = c("lightcyan1", "indianred1")
)

## Filter SNPs with het calls in > 1% females (focus in on SNPs that should not be heterozygous)
Females_hets_1prcnt <-
  Females_hets[((rowSums(Females_hets == 1, na.rm = TRUE)) / (ncol(Females_hets))) < 0.01,]

windows()
heatmap(
  Females_hets_1prcnt,
  scale = "none",
  Rowv = NA,
  Colv = NA,
  col = c("lightcyan1", "indianred1")
)

Females_hets_ind <-
  as.data.frame(colSums(Females_hets_1prcnt == 1, na.rm = T))
colnames(Females_hets_ind) <- "TotalHet"
Females_hets_ind$Ind <- row.names(Females_hets_ind)

windows()
ggplot(data = Females_hets_ind, aes(x = Ind, y = TotalHet)) +
  geom_bar(stat = "identity")

## Which females have more than ~1% het calls (6/632 sites)
Females_hets_ind[Females_hets_ind$TotalHet > 6,]

# TotalHet     Ind
# Pr_2123        8 Pr_2123 #South Korea
# Pr_474        14  Pr_474 #Michigan, USA
# Pr_928        14  Pr_928 #Georgia

## Look at males for comparison

## Use the SNPs used for females (high likelihood they are Z-linked)
Males_hets_ind <-
  as.data.frame(colSums(Males_hets[row.names(Females_hets_1prcnt), ] == 1, na.rm = T))
colnames(Males_hets_ind) <- "TotalHet"
Males_hets_ind$Ind <- row.names(Males_hets_ind)

windows()
ggplot(data = Males_hets_ind, aes(x = Ind, y = TotalHet)) +
  geom_bar(stat = "identity")

## Which males have less than 1% het calls
Males_hets_ind[Males_hets_ind$TotalHet < 6,]

# TotalHet     Ind
# Pr_1912        3 Pr_1912  # this individual is from Carolina Biological, so it's probably just highly inbred
