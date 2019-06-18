## This script combines data and metadata files to create a DIYABC formated input file
## The .012 file can be created using a number of programs -- I used VCFtools
## Script written by Sean F. Ryan April 24 2018

## Clear global environment
rm(list = ls())

## Load libraries
library(vcfR)
library(readr)

## Set working directory
setwd(
  "C:/DIYABC"
)

## Import associated vector of individual IDs (MUST BE IN SAME ORDER AS THE GENETIC--A AND Z--MATRICES)
AZ_ind <-
  read_delim(
    "C:/RAD2_A_rsq0.2_Het06.012.indv",
    "\t",
    escape_double = FALSE,
    col_names = FALSE,
    trim_ws = TRUE
  )
as.data.frame(colnames(AZ_ind) <- rep("ID", ncol(AZ_ind)))


## Import genotype matrix
A_geno <-
  read_delim(
    "C:/RAD2_A_rsq0.2_Het06.012",
    "\t",
    escape_double = FALSE,
    col_names = FALSE,
    trim_ws = TRUE
  )

## Remove the first column, replace -1 with 9, add header
A_geno2 <- A_geno[, 2:ncol(A_geno)]
A_geno2[A_geno2 == -1] <- 9
as.data.frame(colnames(A_geno2) <- rep("A", ncol(A_geno2)))


## Import associated vector of individual IDs (in same order as genotype matrix)
AZ_meta <-
  read_delim(
    "C:/DIYABC_metadata.csv",
    ",",
    escape_double = FALSE,
    col_names = TRUE,
    trim_ws = TRUE
  )

## Combine meta data with ind file to preserver order
AZ_ind_meta <- merge(AZ_ind, AZ_meta, by = "ID")


# mtDNA not used in this study (haploid data not working in DIYABC)
# ## Import genotype matrix for mtDNA; note that alleles that were not one of the major two were converted to 9s (DIYABC can only deal with biallelic SNPs)
# mtDNA_geno <- read_csv("C:/Analyses/DIYABC/mtDNA_Biallelic_SNPs.csv")
# ## Sort to match order of A and Z data
# mtDNA_geno_sorted <- merge(AZ_meta, mtDNA_geno,by="ID")
# mtDNA_geno_sorted2 <- mtDNA_geno_sorted[, 20:ncol(mtDNA_geno_sorted)]
# as.data.frame(colnames(mtDNA_geno_sorted2) <- rep("M", ncol(mtDNA_geno_sorted2)))

### CREATE 3 DIFFERENT DATASETS

## Approach 1 dataset

## Merge them altogether (don't refer to AZ_meta directly as it is not in the correct order; refer to metadata using dataframes that have been merged with data from AZ_meta)
DIY_data_approach1 <-
  as.data.frame(
    cbind(
      mtDNA_geno_sorted$ID,
      mtDNA_geno_sorted$DIYABC_Sex,
      mtDNA_geno_sorted$Approach1,
      A_geno2,
      mtDNA_geno_sorted2
    )
  )
names(DIY_data_approach1)[1] <- "IND"
names(DIY_data_approach1)[2] <- "SEX"
names(DIY_data_approach1)[3] <- "POP"

## Remove rows with NAs (filters to just individuals represented in the sampling for that approach)
DIY_data_approach1_complete <-
  DIY_data_approach1[complete.cases(DIY_data_approach1[, 3]),]

## Sort based on POP
DIY_data_approach1_complete_sorted <-
  DIY_data_approach1_complete[order(DIY_data_approach1_complete$POP), ]

## Write to file
write.table(
  DIY_data_approach1_complete_sorted,
  file = "DIY_data_approach1.txt",
  append = FALSE,
  quote = TRUE,
  sep = " ",
  eol = "\n",
  na = "NA",
  dec = ".",
  row.names = FALSE,
  col.names = TRUE,
  qmethod = c("escape", "double"),
  fileEncoding = ""
)


## Approach 2 dataset

## Merge them altogether (don't refer to AZ_meta directly as it is not in the correct order; refer to metadata using dataframes that have been merged with data from AZ_meta)
DIY_data_approach2 <-
  as.data.frame(
    cbind(
      mtDNA_geno_sorted$ID,
      mtDNA_geno_sorted$DIYABC_Sex,
      mtDNA_geno_sorted$Approach2,
      A_geno2,
      mtDNA_geno_sorted2
    )
  )
names(DIY_data_approach2)[1] <- "IND"
names(DIY_data_approach2)[2] <- "SEX"
names(DIY_data_approach2)[3] <- "POP"

## Remove rows with NAs (filters to just individuals represented in the sampling for that approach)
DIY_data_approach2_complete <-
  DIY_data_approach2[complete.cases(DIY_data_approach2[, 3]),]

## Sort based on POP
DIY_data_approach2_complete_sorted <-
  DIY_data_approach2_complete[order(DIY_data_approach2_complete$POP), ]

## Write to file
write.table(
  DIY_data_approach2_complete_sorted,
  file = "DIY_data_approach2.txt",
  append = FALSE,
  quote = TRUE,
  sep = " ",
  eol = "\n",
  na = "NA",
  dec = ".",
  row.names = FALSE,
  col.names = TRUE,
  qmethod = c("escape", "double"),
  fileEncoding = ""
)


## Approach 3 dataset

## Merge them altogether (don't refer to AZ_meta directly as it is not in the correct order; refer to metadata using dataframes that have been merged with data from AZ_meta)
DIY_data_approach3 <-
  as.data.frame(
    cbind(
      mtDNA_geno_sorted$ID,
      mtDNA_geno_sorted$DIYABC_Sex,
      mtDNA_geno_sorted$Approach3,
      A_geno2,
      mtDNA_geno_sorted2
    )
  )
names(DIY_data_approach3)[1] <- "IND"
names(DIY_data_approach3)[2] <- "SEX"
names(DIY_data_approach3)[3] <- "POP"

## Remove rows with NAs (filters to just individuals represented in the sampling for that approach)
DIY_data_approach3_complete <-
  DIY_data_approach3[complete.cases(DIY_data_approach3[, 3]),]

## Sort based on POP
DIY_data_approach3_complete_sorted <-
  DIY_data_approach3_complete[order(DIY_data_approach3_complete$POP), ]

## Write to file
write.table(
  DIY_data_approach3_complete_sorted,
  file = "DIY_data_approach3.txt",
  append = FALSE,
  quote = TRUE,
  sep = " ",
  eol = "\n",
  na = "NA",
  dec = ".",
  row.names = FALSE,
  col.names = TRUE,
  qmethod = c("escape", "double"),
  fileEncoding = ""
)
