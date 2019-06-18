## The first part of this script is designed to identify sex-linked loci by comparing heterogygisty of each site (SNP) between males and females
## The second part of this script is to plot heterozygosity for each site (SNP) based on putative chromosome assignemt
## The script is written to do this for Pieris rapae
## Script written by Sean F. Ryan July 24 2018

## Clear global environment
rm(list = ls())

## Load libraries
library(vcfR)
library(dplyr)
library(readr)
library(magrittr)
library(psych)

## Set working directory
setwd("C:/FindSexLinkedLoci")

## First, need to determine putative order of Pra scaffolds using Bmo genome

## Import data (blastp data)
df <-
  read_delim(
    "C:/FindSexLinkedLoci/Hits_Nscaff_Chrom.txt",
    " ",
    escape_double = FALSE,
    trim_ws = TRUE
  )

## Create new column for protein seq ids so that they are not unique within a scaffold
df$ProtScaff <- gsub("\\..*", "", df$Pra_Pep)
df$ProtScaff <- gsub("\\pra", "", df$ProtScaff)

# ## Create new column for protein seq number (this is used to determine their order in the Pra scaffold)
df$ProtNum <- sub('.*\\.', '', df$Pra_Pep)

## Calcultate mode of Nscaff grouped by proteins
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

## Identify the most likely Bmo nscaff for Pra pep seqeuence
bynscaff <- aggregate(Bmo_Nscaff ~ ProtScaff, df, Mode)

## Rename second column to Nscaff_Mode (used for putative nscaff for all pep)
names(bynscaff)[names(bynscaff) == "Bmo_Nscaff"] <-
  "Bmo_Nscaff_Mode"

## Grouping by other factors (NOT USING THESE)
# bychrom <- aggregate(Chrom ~ ProtScaff, df, Mode)
# bynscaffchromorder <- aggregate(Nscaf_order_within_Chrom ~ ProtScaff, df, Mode)

## Combine all meta data with the Ncaff Mode
bynscaff_Bmo_meta <- (merge(df, bynscaff, by = 'ProtScaff'))

## Import filtered vcf file containing males and females (see Ryan et al. 2019 for filters that were used)
vcffile <-
  read.vcfR(
    "C:/FindSexLinkedLoci/PierisProject_101217_biSNPs_Prapae_hard_GQ30_maf01_1X50ind_95depth_75imiss_10X75ind.recode.vcf",
    convertNA = TRUE,
    checkFile = TRUE,
    verbose = TRUE
  )

## Convert to gentoype matrix
gt <- extract.gt(vcffile)

## Create dataframe with CHROM and POS
CP <- vcffile@fix[, 1:2]

## Create matrix of true/false for het calls
hets <- is_het(gt, na_is_false = FALSE)

## Import metadata file
Ind_metadata <- read_csv("C:/Ind_metadata.csv")

## Create vector of specimens
names <- as.data.frame(colnames(hets))
colnames(names) <- "ID"

## Merge the sex data with the names from the vcf file being used
Names_meta <- merge(names, Ind_metadata, by = "ID")

## Split columns into two dataframes--male and female
D.f <- hets[, Names_meta[, 9] == "F"]
D.m <- hets[, Names_meta[, 9] == "M"]


## Count hom/het/NA for each scaffold (for each sex separately)

## Females
## Count homozygotes
hom.f <- as.data.frame(rowSums(D.f == "FALSE", na.rm = TRUE))

## Count heterozygotes
het.f <- as.data.frame(rowSums(D.f == "TRUE", na.rm = TRUE))

## Count missing
miss.f <- as.data.frame(rowSums(NAs == "TRUE", na.rm = TRUE))

## Calculate percent of hets to homos
prcnthets.f <- as.data.frame(het.f / (hom.f + het.f))

## Calculate percent of missing data
totmiss.f <- as.data.frame(miss.f / (hom.f + het.f + miss.f))

## Males
## Count homozygotes
hom.m <- as.data.frame(rowSums(D.m == "FALSE", na.rm = TRUE))

## Count heterozygotes
het.m <- as.data.frame(rowSums(D.m == "TRUE", na.rm = TRUE))

## Count missing
miss.m <- as.data.frame(rowSums(NAs == "TRUE", na.rm = TRUE))

## Calculate percent of hets to homos
prcnthets.m <- as.data.frame(het.m / (hom.m + het.m))

## Calculate percent of missing data
totmiss.m <- as.data.frame(miss.m / (hom.m + het.m + miss.m))

## Combine males AND females into a table and give them a header
a.fm <-
  cbind(
    CP,
    het.f,
    hom.f,
    miss.f,
    totmiss.f,
    prcnthets.f,
    het.m,
    hom.m,
    miss.m,
    totmiss.m,
    prcnthets.m
  )
colnames(a.fm) <-
  c(
    "CHROM",
    "POS",
    "het.f",
    "hom.f",
    "miss.f",
    "totmiss.f",
    "prcnthets.f",
    "het.m",
    "hom.m",
    "miss.m",
    "totmiss.m",
    "prcnthets.m"
  )

## Remove loci with more than 25% missing data in females
a.fm.less25miss <- a.fm[which(a.fm$totmiss.f < 0.25), ]

## Create new column for protein seq ids so they can be used to join with other meta data
a.fm.less25miss$ProtScaff <-
  sub('\\_cov.*', '', a.fm.less25miss$CHROM)
a.fm.less25miss$ProtScaff <-
  sub("scaffold", "", a.fm.less25miss$ProtScaff)

## Import Bmo Nscaff meta data
Bmo_Nscaf_Chrom_Order <- read_delim("Bmo_Nscaf_Chrom_Order.txt",
                                    "\t",
                                    escape_double = FALSE,
                                    trim_ws = TRUE)

names(Bmo_Nscaf_Chrom_Order)[names(Bmo_Nscaf_Chrom_Order) == "Bmo_Nscaff"] <-
  "Bmo_Nscaff_Mode"

## Add the Nscaff_Mode
a.fm.less25miss_bynscaff <-
  (merge(a.fm.less25miss, bynscaff, by = 'ProtScaff'))

## Add Bmo chrom meta data based on selected Nscaff
a.fm.less25miss_bynscaff_Bmo_meta <-
  (merge(a.fm.less25miss_bynscaff, Bmo_Nscaf_Chrom_Order, by = 'Bmo_Nscaff_Mode'))

## Need to convert from factor to numeric (trickier than one would think)
a.fm.less25miss_bynscaff_Bmo_meta$POSnum <-
  as.numeric(as.character(a.fm.less25miss_bynscaff_Bmo_meta$POS))
a.fm.less25miss_bynscaff_Bmo_meta$CHROMnum <-
  as.numeric(as.character(a.fm.less25miss_bynscaff_Bmo_meta$ProtScaff))
a.fm.less25miss_bynscaff_Bmo_meta$Bmo_Chrom <-
  as.numeric(as.character(a.fm.less25miss_bynscaff_Bmo_meta$Bmo_Chrom))
a.fm.less25miss_bynscaff_Bmo_meta$Bmo_Nscaff_order_within_Chrom <-
  as.numeric(as.character(
    a.fm.less25miss_bynscaff_Bmo_meta$Bmo_Nscaff_order_within_Chrom
  ))

## Sort by Bmo chrom, then orientation of scaffold in Bmo chrom, then Pra CHROM and then POS
a.fm.less25miss_bynscaff_Bmo_meta_sorted <-
  a.fm.less25miss_bynscaff_Bmo_meta[order(
    a.fm.less25miss_bynscaff_Bmo_meta$Bmo_Chrom,
    a.fm.less25miss_bynscaff_Bmo_meta$Bmo_Nscaff_order_within_Chrom,
    a.fm.less25miss_bynscaff_Bmo_meta$ProtScaff,
    a.fm.less25miss_bynscaff_Bmo_meta$POSnum
  ),]

## Create SUBbins for the SNPs (this is not used in the analysis at the moment)

## Add bin ID
binrange = 200 #set the size of the bin (used RADseq so 200 seems reasonable--all within a RAD locus)

a.fm.less25miss_bynscaff_Bmo_meta_sorted$BIN <- 1
a.fm.less25miss_bynscaff_Bmo_meta_sorted$subBIN <- 1
BINcolumn <-
  which(colnames(a.fm.less25miss_bynscaff_Bmo_meta_sorted) == "BIN")
subBINcolumn <-
  which(colnames(a.fm.less25miss_bynscaff_Bmo_meta_sorted) == "subBIN")
POScolumn <-
  which(colnames(a.fm.less25miss_bynscaff_Bmo_meta_sorted) == "POSnum")
ProtScaffcolumn <-
  which(colnames(a.fm.less25miss_bynscaff_Bmo_meta_sorted) == "ProtScaff")

for (cell in 2:nrow(a.fm.less25miss_bynscaff_Bmo_meta_sorted)) {
  #if still in the same scaffold as the one before, give it the same BIN #
  if (a.fm.less25miss_bynscaff_Bmo_meta_sorted[cell, ProtScaffcolumn] == a.fm.less25miss_bynscaff_Bmo_meta_sorted[cell -
                                                                                                                  1, ProtScaffcolumn]) {
    a.fm.less25miss_bynscaff_Bmo_meta_sorted[cell, BINcolumn] <-
      as.numeric(a.fm.less25miss_bynscaff_Bmo_meta_sorted[cell - 1, BINcolumn])
    
    #then determine subBIN
    if ((
      abs(
        a.fm.less25miss_bynscaff_Bmo_meta_sorted[cell, POScolumn] - a.fm.less25miss_bynscaff_Bmo_meta_sorted[cell -
                                                                                                             1, POScolumn]
      )
    ) < binrange) {
      a.fm.less25miss_bynscaff_Bmo_meta_sorted[cell, subBINcolumn] <-
        as.numeric(a.fm.less25miss_bynscaff_Bmo_meta_sorted[cell - 1, subBINcolumn])
    } else {
      a.fm.less25miss_bynscaff_Bmo_meta_sorted[cell, subBINcolumn] <-
        (as.numeric(a.fm.less25miss_bynscaff_Bmo_meta_sorted[cell - 1, subBINcolumn]) + 1)
    }
    
  } else {
    #Bin is previous BIN # plus 1, subBIN remains 1
    a.fm.less25miss_bynscaff_Bmo_meta_sorted[cell, BINcolumn] <-
      (as.numeric(a.fm.less25miss_bynscaff_Bmo_meta_sorted[cell - 1, BINcolumn]) + 1)
  }
}

a.fm.less25miss_bynscaff_Bmo_meta_sorted$BIN.subBIN <-
  paste(
    a.fm.less25miss_bynscaff_Bmo_meta_sorted$BIN,
    a.fm.less25miss_bynscaff_Bmo_meta_sorted$subBIN,
    sep = "."
  )

## save output
write.table(
  a.fm.less25miss_bynscaff_Bmo_meta_sorted,
  file = "a.fm.less25miss_bynscaff_Bmo_meta_sorted.csv",
  quote = TRUE,
  sep = ",",
  na = "NA",
  row.names = FALSE,
  col.names = TRUE
)

## Create dataset with "list" of BINs

myvars <- c("BIN", "Bmo_Chrom", "CHROM")
BIN_CHROM <- a.fm.less25miss_bynscaff_Bmo_meta_sorted[myvars]
BIN_het <-
  as.data.frame(unique(BIN_CHROM[c("BIN", "Bmo_Chrom", "CHROM")]))
BIN_het$sum <- NA
BIN_het$total <- NA

## Within each bin, count SNPs with het < 0.01
for (i in 1:nrow(BIN_het)) {
  BIN_het[i , 4] <-
    sum(
      a.fm.less25miss_bynscaff_Bmo_meta_sorted$BIN == eval(BIN_het[i, 1]) &
        a.fm.less25miss_bynscaff_Bmo_meta_sorted$prcnthets.f < 0.01,
      na.rm = TRUE
    )
  BIN_het[i, 5] <-
    sum(a.fm.less25miss_bynscaff_Bmo_meta_sorted$BIN == eval(BIN_het[i, 1]),
        na.rm = TRUE)
}

## Calculate the percent of BINs with SNPs with het < 0.01
BIN_het$prcnt <- (BIN_het$sum / BIN_het$total) * 100

## Sort
BIN_het$Bmo_Chrom <- as.numeric(as.character(BIN_het$Bmo_Chrom))
BIN_het_sorted <- BIN_het[order(BIN_het$Bmo_Chrom, BIN_het$BIN),]


## Calculate mean (and SD) number of SNPs within each scaffold
mean(BIN_het_sorted$total)
getmode(BIN_het_sorted$total)
sd(BIN_het_sorted$total)

## Save output
write.table(
  BIN_het_sorted,
  file = "BIN_het_sorted.csv",
  quote = TRUE,
  sep = ",",
  na = "NA",
  row.names = FALSE,
  col.names = TRUE
)

windows()
plot(BIN_het_sorted$prcnt)

windows()
plot(BIN_het_sorted$prcnt ~ BIN_het_sorted$total)

## Create list of scaffolds with het >60% of SNPs within a BIN being het (seems to be reasonable #)
PutativeZlinked <- BIN_het_sorted[BIN_het_sorted$prcnt > 60, ]

## Save output
write.table(
  PutativeZlinked$CHROM,
  file = "PutativeZlinked_prcnt60.txt",
  quote = F,
  sep = " ",
  na = "NA",
  row.names = F,
  col.names = F
)



##################################################################################################################

## Plotting heterozygosity by putative chromosome assignment

## Load libraries
library(dplyr)
library(readr)
library(magrittr)
library(ggplot2)

## Load in file from above if not already in workspace environment
a_fm_less25miss_bynscaff_Bmo_meta_sorted <-
  read_csv(
    "C:/FindSexLinkedLoci/a.fm.less25miss_bynscaff_Bmo_meta_sorted.csv",
    col_types = cols(Bmo_Chrom = col_number())
  )

# ## Plot %het by chrom for all chromosomes
# windows()
# p <- ggplot(a_fm_less25miss_bynscaff_Bmo_meta_sorted, aes(factor(Bmo_Chrom), prcnthets.f))
# p + geom_violin() +
#   theme_classic()
#
# windows()
# p <- ggplot(a_fm_less25miss_bynscaff_Bmo_meta_sorted, aes(factor(Bmo_Chrom), prcnthets.m))
# p + geom_violin() +
#   theme_classic()

## Subset to chrom 1
Chrom1 <-
  na.omit(a_fm_less25miss_bynscaff_Bmo_meta_sorted[a_fm_less25miss_bynscaff_Bmo_meta_sorted$Bmo_Chrom ==
                                                     1,])

# windows()
# p <- ggplot(Chrom1, aes(factor(ProtScaff), prcnthets.f))
# p + geom_violin()  +
#   theme_classic()
#
# windows()
# p <- ggplot(Chrom1, aes(factor(ProtScaff), prcnthets.m))
# p + geom_violin()  +
#   theme_classic()

windows()
ggplot(data = Chrom1, aes(
  x = as.integer(rownames(Chrom1)),
  y = prcnthets.f,
  color = factor(BIN)
)) +
  geom_point()  +
  ylim(0, 1) +
  theme_classic()

windows()
ggplot(data = Chrom1, aes(
  x = as.integer(rownames(Chrom1)),
  y = prcnthets.m,
  color = factor(BIN)
)) +
  geom_point()  +
  ylim(0, 1) +
  theme_classic()

## Subset to chrom 2
Chrom2 <-
  na.omit(a_fm_less25miss_bynscaff_Bmo_meta_sorted[a_fm_less25miss_bynscaff_Bmo_meta_sorted$Bmo_Chrom ==
                                                     2,])
# windows()
# p <- ggplot(Chrom2, aes(factor(ProtScaff), prcnthets.f))
# p + geom_violin()  +
#   theme_classic()
#
# windows()
# p <- ggplot(Chrom2, aes(factor(ProtScaff), prcnthets.m))
# p + geom_violin()  +
#   theme_classic()

windows()
ggplot(data = Chrom2, aes(
  x = as.integer(rownames(Chrom2)),
  y = prcnthets.f,
  color = factor(BIN)
)) +
  geom_point()  +
  ylim(0, 1) +
  theme_classic()

windows()
ggplot(data = Chrom2, aes(
  x = as.integer(rownames(Chrom2)),
  y = prcnthets.m,
  color = factor(BIN)
)) +
  geom_point()  +
  ylim(0, 1) +
  theme_classic()

## Subset to chrom 3
Chrom3 <-
  na.omit(a_fm_less25miss_bynscaff_Bmo_meta_sorted[a_fm_less25miss_bynscaff_Bmo_meta_sorted$Bmo_Chrom ==
                                                     3,])

# windows()
# p <- ggplot(Chrom3, aes(factor(ProtScaff), prcnthets.f))
# p + geom_violin()  +
#   theme_classic()
#
# windows()
# p <- ggplot(Chrom3, aes(factor(ProtScaff), prcnthets.m))
# p + geom_violin()  +
#   theme_classic()

windows()
ggplot(data = Chrom3, aes(
  x = as.integer(rownames(Chrom3)),
  y = prcnthets.f,
  color = factor(BIN)
)) +
  geom_point()  +
  ylim(0, 1) +
  theme_classic()

windows()
ggplot(data = Chrom3, aes(
  x = as.integer(rownames(Chrom3)),
  y = prcnthets.m,
  color = factor(BIN)
)) +
  geom_point()  +
  ylim(0, 1) +
  theme_classic()
