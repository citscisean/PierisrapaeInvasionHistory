## This script is used to process output from the genetic ancestry assignment program ADMIXTURE
## Script written by Sean F. Ryan April 16 2018

## Clear global environment
rm(list = ls())

## Import .Q data for selected K
Qvalues <-
  read_delim(
    "C:/K_clustering/Admixture_output/RAD2_A_rsq0.2_Het06_12.07.Q",
    " ",
    escape_double = FALSE,
    col_names = FALSE,
    trim_ws = TRUE
  )

## Add column names
colnames(Qvalues) <- c("K1", "K2", "K3", "K4", "K5", "K6", "K7")

## Print vector of population assignment for each individaul (K with greatest proportion of assignment prob)
SelectedK <- colnames(Qvalues)[apply(Qvalues, 1, which.max)]

## Write to file
write.table(
  SelectedK,
  file = "SelectedK.txt",
  append = FALSE,
  quote = TRUE,
  sep = " ",
  eol = "\n",
  na = "NA",
  dec = ".",
  row.names = F,
  col.names = F,
  qmethod = c("escape", "double"),
  fileEncoding = ""
)
