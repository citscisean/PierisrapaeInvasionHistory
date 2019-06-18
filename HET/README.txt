Observed heterozygosity was calculated in R using the package Adegenet within an Rscript using a parallized loop to generate bootstrapped estimates
An Rscipt was created for each sub population (an example is given - HETobs_loop_POP_1.1
These Rscripts were submitted as separate jobs (for each population) using the script RunBatchR (unix environment)
Plot_HETobs.R was used to plot the output (bootstrapped estimates)
