## This script is used to estimate the empirical number of RAD sites per bp
## Script written by Sean F. Ryan May 22 2018

## Clear global environment
rm(list = ls())

## Load libraries
library(readr)

## Import data (Use a file with ALL sites - e.g. one with coverage)
DATA <- read_delim(
  "C:/PI/RAD1_A_Het06_scaffs100kb.ldepth.mean",
  "\t",
  escape_double = FALSE,
  trim_ws = TRUE
)

## Sort data by CHROM, then by POS
DATA_sort <- DATA[order(DATA$CHROM, DATA$POS), ]

## Set initial parameters

## Select length of RAD reads
binrange = 76

## Initialize RADtag data column, starting with 1s
DATA_sort$RADtag  <- 1

## Determine column with chromosome data
CHROM_Col <- which(colnames(DATA_sort) == "CHROM")

## Determine column with position data
POS_Col <- which(colnames(DATA_sort) == "POS")

## Determine column with chromosome data
RADtag_Col <- which(colnames(DATA_sort) == "RADtag")

## Choose the position for starting (first one in the dataset)
SNP_1 = DATA_sort[1, POS_Col]

## Set counter to keep track of contigous set of SNPs (those within < binrange)
counter = 0


## Loop through each cell in the dataframe
for (cell in 2:nrow(DATA_sort)) {

  ## Determine if the scaffold is the same as before
  
  ## If a differernt scaffold as the one before
  if (DATA_sort[cell, CHROM_Col] != DATA_sort[cell - 1, CHROM_Col]) {
    ## RADtag # = previous RADtag # + 1
    DATA_sort[cell, RADtag_Col] <-
      ((DATA_sort[cell - 1, RADtag_Col]) + 1)
    
    ## Assign new SNP_1 position - this position becomes the start
    SNP_1 = DATA_sort[cell, POS_Col]
    
    # ## Reset counter to 0
    counter = 1
    
    
  ## If on the same scaffold as the one before
  } else {
    ## Need to deal with first comparison
    if (cell == 2) {
      counter = 1
    }
    
    ## If first position encountered
    if (counter == 0) {
      ##RADtag # = previous RADtag # + 1
      DATA_sort[cell, RADtag_Col] <-
        ((DATA_sort[cell - 1, RADtag_Col]) + 1)
      
      ## Assign new SNP_1 position - this position becomes the start
      SNP_1 = DATA_sort[cell, POS_Col]
      
    ## If NOT the first position encountered
    } else {
      #then determine if the POS is 76bp from the first SNP started in the count (SNP_1)
      
      ## If within the binrange
      if (abs(DATA_sort[cell, POS_Col] - SNP_1) < binrange) {
        ## SNP is part of the previous RADtag
        DATA_sort[cell, RADtag_Col] <-
          DATA_sort[cell - 1, RADtag_Col]
        
        ## Counter is 1
        counter = 1
        
      ## If SNP position > binrange from SNP_1 (not part of the same RADtag)
      } else {
        ## RADtag # = previous RADtag # + 1
        DATA_sort[cell, RADtag_Col] <-
          ((DATA_sort[cell - 1, RADtag_Col]) + 1)
        
        ## Assign new SNP_1 position - this position becomes the start
        SNP_1 = DATA_sort[cell, POS_Col]
        
        ## Counter is 1
        counter = 1
        
      }
    }
  }
}

## save output
write.table(
  DATA_sort,
  file = "RADtags_counts.csv",
  quote = TRUE,
  sep = ",",
  na = "NA",
  row.names = FALSE,
  col.names = TRUE
)


###### Need to calculate average number of RADtags per bp
#create a list of unique scaffolds
scaffs  <- as.data.frame(unique(DATA_sort$CHROM))

## save list of scaffs
write.table(
  scaffs,
  file = "RAD1_A_Het06_scaffs100kb.list",
  quote = F,
  sep = " ",
  na = "NA",
  row.names = F,
  col.names = F
)

## Create matrix to store results
Results <- matrix(nrow = nrow(scaffs), ncol = 3)

for (i in 1:nrow(scaffs)) {
  ## Subset to current scaffold
  DATA_sort_i <-
    DATA_sort[DATA_sort$CHROM == as.character(scaffs[i, 1]), ]
  
  ## Count number of RADtags within a scaffold
  Results[i, 1] <- as.numeric(length(unique(DATA_sort_i$RADtag)))
  ## Measure distance covered within the scaffold
  Results[i, 2] <-
    as.numeric(DATA_sort_i[nrow(DATA_sort_i), POS_Col] - DATA_sort_i[1, POS_Col])
  ## Calcualte number of RADtags per bp
  Results[i, 3] <- as.numeric((Results[i, 1]) / (Results[i, 2]))
  
}

## Only use those with > 3 RADtags
Results2 <- as.data.frame(Results[Results[, 1] > 3,])

colnames(Results2) <-
  c("RADtags", "Distance_Covered", "RADtags_per_bp")

## save output
write.table(
  Results2,
  file = "RADtags_stats.csv",
  quote = TRUE,
  sep = ",",
  na = "NA",
  row.names = FALSE,
  col.names = TRUE
)
