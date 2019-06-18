## This script is used to run a Discriminant Analysis of Principal Components (DAPC) and plot the results
## Script written by Sean F. Ryan April 16 2018

## Clear global environment
rm(list = ls(all = T))

## Load libraries
library(adegenet)
library(vcfR)
library(poppr)
library(ape)
library(parallel)

## Set working directory
setwd("C:/K_clustering/")

## Import vcf file
VCF_A <- read.vcfR("RAD2_A_rsq0.2_Het06.recode.vcf")

## Import metadata
pop.data <-
  read.table("RAD2_A_rsq0.2_Het06.recode.meta.txt",
             sep = "\t",
             header = TRUE)

# ## Load gl.VCF if already created
# load("gl.VCF_A_RAD2_A_rsq0.2_Het06.Rdata")

# ## Convert vcf to genlight object
gl.VCF_A <- vcfR2genlight(VCF_A)
## Set ploidy to diploid
ploidy(gl.VCF_A) <- 2
## Add population data
pop(gl.VCF_A) <- pop.data$Country_Num
save(gl.VCF_A, file = 'gl.VCF_A_RAD2_A_rsq0.2_Het06.Rdata')

# ## Load PCA if already created
# load("pca1_RAD2_A_rsq0.2_Het06.Rdata")

## Run PCA
pca1 <- glPca(gl.VCF_A,
              nf = 5,
              useC = T,
              parallel = F)
save(pca1, file = 'pca1_RAD2_A_rsq0.2_Het06.Rdata')

## Barplot of eigen values
windows()
barplot(pca1$eig[1:50], main = "PCA eigenvalues", col = heat.colors(50))

## Can use sum of all eigenvalues to calculate % variance explained for each eigenvalue
(pca1$eig[1] / sum(pca1$eig)) * 100
(pca1$eig[2] / sum(pca1$eig)) * 100
(pca1$eig[3] / sum(pca1$eig)) * 100

#convert data to a dataframe
PCAdata <- as.data.frame(pca1$scores)

## DAPC Analysis

# ## import genind if already created
# load("gi.VCF_A_RAD2_A_rsq0.2_Het06.Rdata", verbose = FALSE)

## Convert genlight to genind file
gi.VCF_A <- vcfR2genind(VCF_A)
ploidy(gi.VCF_A) <- 2
pop(gi.VCF_A) <- pop.data$Country_Num
save(gi.VCF_A, file = 'gi.VCF_A_RAD2_A_rsq0.2_Het06.Rdata')

## Choose optimal K
windows()
grp <-
  find.clusters(gi.VCF_A,
                max.n.clust = 30,
                n.iter = 10e5,
                n.start = 1000)
#Choose the number PCs to retain (>=1):
# 600
#Choose the number of clusters (>=2:
# 6

## Save grouping
save(grp, file = 'grp_RAD2_A_rsq0.2_Het06_K6.Rdata')

## Load if already created
# load("grp_RAD2_A_rsq0.2_Het06_K6.Rdata", verbose = FALSE)

## Heirarchical clustering

## Use grp assigments from above as starting clustering
grp <- find.clusters(gi.VCF_A, max.n.clust = 20)
save(grp2, file = 'grp_RAD2_A_rsq0.2_Het06_K6.Rdata')

windows()
dapc1 <- dapc(gi.VCF_A, grp$grp)
#Choose the number PCs to retain (>=1):
# 186
#Choose the number discriminant functions to retain (>=1):
# 5

## try to find optimal number of PCs to retain
windows()
optimalPCs <- optim.a.score(dapc1, n.da = 5, n.sim = 100)

## redo dpca using optimal number of PCs to retain
dapc_opt <- dapc(gi.VCF_A, n.da = 5, n.pca = 24, grp$grp)

## check how well assigned group corresponds with predicted grouping
## make sure the labels are based on order of data use pop(gi.VCF_A) to find out how they are ordered
pop(gi.VCF_A)
windows()
table.value(
  table(pop(gi.VCF_A), grp$grp),
  col.lab = paste("inf", 1:17),
  row.lab = paste(
    "ori",
    c(
      "USA",
      "New_Zealand",
      "Australia",
      "Gibraltar",
      "Taiwan",
      "Turkey",
      "Canada",
      "Austria",
      "Italy",
      "England",
      "Algeria",
      "France",
      "Tunisia",
      "Finland",
      "Greece",
      "Bulgaria",
      "Romania",
      "China",
      "Spain",
      "Mexico",
      "Poland",
      "Japan",
      "Morocco",
      "Malta",
      "Russia",
      "South_Korea",
      "Georgia"
    )
  )
)

## output posterior probabilities and assingments
assignments <-
  cbind(row.names(PCAdata), as.data.frame(dapc_opt$assign))
colnames(assignments) <- c("ID", "Cluster_Assignment")
write.table(
  assignments,
  file = "assignments_RAD2_A_rsq02_grpK6.txt",
  append = FALSE,
  quote = TRUE,
  sep = " ",
  eol = "\n",
  na = "NA",
  dec = ".",
  row.names = TRUE,
  col.names = TRUE,
  qmethod = c("escape", "double"),
  fileEncoding = ""
)

posterior <-
  cbind(row.names(PCAdata), as.data.frame(dapc_opt$posterior))
colnames(posterior)[1] <- "ID"
write.table(
  posterior,
  file = "posterior_RAD2_A_rsq02_grpK6.txt",
  append = FALSE,
  quote = TRUE,
  sep = " ",
  eol = "\n",
  na = "NA",
  dec = ".",
  row.names = TRUE,
  col.names = TRUE,
  qmethod = c("escape", "double"),
  fileEncoding = ""
)


## Use grp assigments from above as starting clustering
grp2 <- find.clusters(gi.VCF_A, max.n.clust = 20, clust = grp$grp)
save(grp2, file = 'grp2_RAD2_A_rsq0.2_Het06_K6.Rdata')

windows()
dapc1 <- dapc(gi.VCF_A, grp2$grp)
#Choose the number PCs to retain (>=1):
# 186
#Choose the number discriminant functions to retain (>=1):
# 6

## try to find optimal number of PCs to retain
windows()
optimalPCs <- optim.a.score(dapc1, n.da = 6, n.sim = 100)

## redo dpca using optimal number of PCs to retain
dapc_opt <- dapc(gi.VCF_A, n.da = 6, n.pca = 23, grp2$grp)

## check how well assigned group corresponds with predicted grouping
## make sure the labels are based on order of data use pop(gi.VCF_A) to find out how they are ordered
pop(gi.VCF_A)
windows()
table.value(
  table(pop(gi.VCF_A), grp2$grp),
  col.lab = paste("inf", 1:17),
  row.lab = paste(
    "ori",
    c(
      "USA",
      "New_Zealand",
      "Australia",
      "Gibraltar",
      "Taiwan",
      "Turkey",
      "Canada",
      "Austria",
      "Italy",
      "England",
      "Algeria",
      "France",
      "Tunisia",
      "Finland",
      "Greece",
      "Bulgaria",
      "Romania",
      "China",
      "Spain",
      "Mexico",
      "Poland",
      "Japan",
      "Morocco",
      "Malta",
      "Russia",
      "South_Korea",
      "Georgia"
    )
  )
)

## output posterior probabilities and assingments
assignments <-
  cbind(row.names(PCAdata), as.data.frame(dapc_opt$assign))
colnames(assignments) <- c("ID", "Cluster_Assignment")
write.table(
  assignments,
  file = "RAD2_A_rsq02_K17.txt",
  append = FALSE,
  quote = TRUE,
  sep = " ",
  eol = "\n",
  na = "NA",
  dec = ".",
  row.names = TRUE,
  col.names = TRUE,
  qmethod = c("escape", "double"),
  fileEncoding = ""
)

posterior <-
  cbind(row.names(PCAdata), as.data.frame(dapc_opt$posterior))
colnames(posterior)[1] <- "ID"
write.table(
  posterior,
  file = "RAD2_A_rsq02_K17.txt",
  append = FALSE,
  quote = TRUE,
  sep = " ",
  eol = "\n",
  na = "NA",
  dec = ".",
  row.names = TRUE,
  col.names = TRUE,
  qmethod = c("escape", "double"),
  fileEncoding = ""
)

windows()
scatter(dapc_opt)

windows()
compoplot(dapc_opt,
          lab = "",
          posi = list(x = 12, y = -.01),
          cleg = .7)

myCol <-
  c("darkblue",
    "purple",
    "green",
    "orange",
    "red",
    "yellow",
    "pink",
    "black")

windows()
scatter(
  dapc_opt,
  scree.da = FALSE,
  bg = "white",
  pch = 20,
  cell = 0,
  cstar = 0,
  col = myCol,
  solid = .4,
  cex = 3,
  clab = 0,
  leg = TRUE,
  txt.leg = paste("Cluster", 1:17)
)
