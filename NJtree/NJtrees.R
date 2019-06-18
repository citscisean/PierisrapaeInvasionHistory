## This script is used to create and plot neighbor-joining trees using vcf files
## Script written by Sean F. Ryan April 16 2018

## Clear global environment
rm(list = ls())

## Load libraries
library(vcfR)
library(poppr)
library(ape)
library(readr)

## Set working directory
setwd(
  "C:/NJtree/"
)

## Read in vcf file
VCF_A <-
  read.vcfR(
    "RAD1outgroups_GQ202_miss90_15X.recode.vcf"
  )

## Read in metadata
pop.data <-
  read_delim(
    "RAD1outgroups_GQ202_miss90_15X_metadata.txt",
    "\t",
    escape_double = FALSE,
    trim_ws = TRUE
  )

## Convert vcf to genlight object
gl.vcf_A <- vcfR2genlight(VCF_A)

# ## Set ploidy to diploid
ploidy(gl.vcf_A) <- 2

## Add population data
pop(gl.vcf_A) <- pop.data$Admix_K7_pops_outgroups_num

## Save
save(gl.vcf_A, file = 'gl.vcf_RAD1outgroups_GQ202_miss90_15X.Rdata')

## Get order of specimens
indNames(gl.vcf_A)

## Assign colors to samples
pop.data$colorset[pop.data$Admix_K7_pops_outgroups_num == 1] = "#EDAB1D"
pop.data$colorset[pop.data$Admix_K7_pops_outgroups_num == 2] = "#1B8724"
pop.data$colorset[pop.data$Admix_K7_pops_outgroups_num == 3] = "#B555BA"
pop.data$colorset[pop.data$Admix_K7_pops_outgroups_num == 4] = "#7CEDE9"
pop.data$colorset[pop.data$Admix_K7_pops_outgroups_num == 5] = "#F0AAB4"
pop.data$colorset[pop.data$Admix_K7_pops_outgroups_num == 6] = "black"
pop.data$colorset[pop.data$Admix_K7_pops_outgroups_num == 7] = "grey"
pop.data$colorset[pop.data$Admix_K7_pops_outgroups_num == 8] = "white"
pop.data$colorset[pop.data$Admix_K7_pops_outgroups_num == 9] = "white"
pop.data$colorset[pop.data$Admix_K7_pops_outgroups_num == 10] = "white"

# ## Convert genlight to genind file
gi.vcf_A <- vcfR2genind(VCF_A)
ploidy(gi.vcf_A) <- 2

pop(gi.vcf_A) <- pop.data$Admix_K7_pops_outgroups_num
indNames(gi.vcf_A) <- pop.data$IND_ID
save(gi.vcf_A, file = 'gi.vcf_RAD1outgroups_GQ202_miss90_15X.Rdata')

## Creating NJ tree for populations
gp.vcf_A <- genind2genpop(gi.vcf_A)
save(gp.vcf_A, file = "gp.VCF_RAD1outgroups_GQ202_miss90_15X.Rdata")

## Load in object if already went through creating a genlight object
# load('gl.vcf_RAD1outgroups_GQ202_miss90_15X.Rdata')
# load('gi.vcf_RAD1outgroups_GQ202_miss90_15X.Rdata')
# load('gi.vcf_RAD1outgroups_GQ202_miss90_15X.Rdata')
# load('gp.vcf_RAD1outgroups_GQ202_miss90_15X.Rdata')

tree.nei <-
  aboot(
    gp.vcf_A,
    tree = "nj",
    cutoff = 50,
    quiet = TRUE,
    sample = 100,
    distance = nei.dist
  )
write.tree(tree.nei, file = "RAD1outgroups_GQ202_miss90_15X_nei_nj.tree")

tree.rey <-
  aboot(
    gp.vcf_A,
    tree = "nj",
    cutoff = 50,
    quiet = TRUE,
    sample = 100,
    distance = reynolds.dist
  )
write.tree(tree.rey, file = "RAD1outgroups_GQ202_miss90_15X_reynolds_nj.tree")



## Individual-based trees (done on cluster)
load('gi.vcf_RAD1outgroups_GQ202_miss90_15X.Rdata')

tree.gi.nei <-
  aboot(
    gi.vcf_A,
    tree = "nj",
    cutoff = 50,
    quiet = TRUE,
    sample = 100,
    distance = nei.dist
  )
write.tree(tree.gi.nei, file = "RAD1outgroups_GQ202_miss90_15X_indtree_nei_nj.tree")

tree.gi.diss <-
  aboot(
    gi.vcf_A,
    tree = "nj",
    cutoff = 50,
    quiet = TRUE,
    sample = 100,
    distance = diss.dist
  )
write.tree(tree.gi.diss, file = "RAD1outgroups_GQ202_miss90_15X_diss.dist_nj.tree")

tree.gi.rey <-
  aboot(
    gi.vcf_A,
    tree = "nj",
    cutoff = 50,
    quiet = TRUE,
    sample = 100,
    distance = reynolds.dist
  )
write.tree(tree.gi.rey, file = "RAD1outgroups_GQ202_miss90_15X_indtree_reynolds_nj.tree")


## Determine order of labels (used to determine order for colors)
tree.gp$tip.label

windows()
plot.phylo(
  tree.gp,
  cex = 1.5,
  font = 1,
  adj = 0,
  tip.color =  c(
    "gold2",
    "darkorange",
    "forestgreen",
    "darkorchid2",
    "dodgerblue4",
    "cadetblue1",
    "dodgerblue2",
    "yellow",
    "chartreuse2",
    "red ",
    "pink"
  )
)
nodelabels(
  tree.gp$node.label,
  adj = c(1.3,-0.5),
  frame = "n",
  cex = 0.8,
  font = 2,
  xpd = TRUE
)
axis(side = 1)
