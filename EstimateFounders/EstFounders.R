## This set of scripts estimates the number of founding individuals based on haplotype frequencies
## Haplotype frequencies are determined for the source population and then randomly sampled (with replacement) from these frequencies until all haplotypes found in the native range are present (the number of samples/individuals needed to accomplish this are then recorded)
## Script written by Sean F. Ryan September 9 2018


## Focusing on England as the source population

## Clear global environment
rm(list = ls())

## Load libraries
library(readr)

## Set working directory
setwd("C:/EstimateFounders")

## Set number of sampling events
I = 10000

## Import data
Hap_freq_table <-
  read_csv("Hap_freq_table.csv")

## Import data
Hap_freq_table_subpops <-
  read_csv("Hap_freq_table_subpops.csv")

## Create a vector of all haplotypes and their frequencies for the source population
POP2.3_v <-
  rep(Hap_freq_table_subpops$Haplotype,
      Hap_freq_table_subpops$POP_2.3)

#Identify haplotypes found in source population
POP2.3_list <-
  Hap_freq_table_subpops[Hap_freq_table_subpops$POP_2.3 > 0, 1]

#Identify haplotypes found in native population
POP4and5_list <-
  Hap_freq_table_subpops[Hap_freq_table_subpops$POP_4.1 > 0 |
                           Hap_freq_table_subpops$POP_4.2 > 0 |
                           Hap_freq_table_subpops$POP_4.3 > 0 |
                           Hap_freq_table_subpops$POP_4.4 > 0 |
                           Hap_freq_table_subpops$POP_4.5 > 0 |
                           Hap_freq_table_subpops$POP_4.6 > 0 |
                           Hap_freq_table_subpops$POP_4.7 > 0 |
                           Hap_freq_table_subpops$POP_4.8 > 0 |
                           Hap_freq_table_subpops$POP_4.9 > 0 |
                           Hap_freq_table_subpops$POP_4.x > 0 |
                           Hap_freq_table_subpops$POP_5.1 > 0 |
                           Hap_freq_table_subpops$POP_5.2 > 0 |
                           Hap_freq_table_subpops$POP_5.3 > 0, 1]

## Create list of haplotypes from North America that are found in England (POP2.3)
POP4and5_insource_list <-
  POP4and5_list[POP4and5_list$Haplotype %in% POP2.3_list$Haplotype,]

## Create an empty dataframe to store final results
POP4and5_POP2.3_boots <- data.frame()

## Create an empty dataframe to store temporary results
Hap_sample <- data.frame()

## Sample without replacement from the source population until all haplotypes from the introduced population are represented
for (n in 1:I) {
  i = 1
  while (all(POP4and5_insource_list$Haplotype %in% Hap_sample$V1) == FALSE) {
    #keep going as long as not all haplpotypes from the introduced population are present in the sample
    
    Hap_sample[i, 1] <-
      sample(POP2.3_v, 1, replace = T, prob = NULL)
    
    i = i + 1
  }
  
  ## Record result: how many founders were needed
  POP4and5_POP2.3_boots[n, 1] <- nrow(Hap_sample)
  Hap_sample <- data.frame()
  
}

## Write output to file
save(POP4and5_POP2.3_boots, file = "POP4and5_POP2.3_boots")

## Calculate some summary statistics
m <- mean(POP4and5_POP2.3_boots$V1)
s <- sd(POP4and5_POP2.3_boots$V1)
n <- I
error <- qnorm(0.975) * s / sqrt(n)
left <- m - error
right <- m + error







## Focusing on Spain & (southern) France as the source population

## Clear global environment
rm(list = ls())

## Load libraries
library(readr)

## Set working directory
setwd("C:/Analyses/Estimate_number_founders")

## Set number of sampling events
I = 10000

## Import data
Hap_freq_table <-
  read_csv("Hap_freq_table.csv")

## Import data
Hap_freq_table_subpops <-
  read_csv("Hap_freq_table_subpops.csv")

## Create a vector of all haplotypes and their frequencies for the source population
POP2.4_v <-
  rep(Hap_freq_table_subpops$Haplotype,
      Hap_freq_table_subpops$POP_2.4)


#Identify haplotypes found in source population
POP2.4_list <-
  Hap_freq_table_subpops[Hap_freq_table_subpops$POP_2.4 > 0, 1]

#Identify haplotypes found in native population
POP4and5_list <-
  Hap_freq_table_subpops[Hap_freq_table_subpops$POP_4.1 > 0 |
                           Hap_freq_table_subpops$POP_4.2 > 0 |
                           Hap_freq_table_subpops$POP_4.3 > 0 |
                           Hap_freq_table_subpops$POP_4.4 > 0 |
                           Hap_freq_table_subpops$POP_4.5 > 0 |
                           Hap_freq_table_subpops$POP_4.6 > 0 |
                           Hap_freq_table_subpops$POP_4.7 > 0 |
                           Hap_freq_table_subpops$POP_4.8 > 0 |
                           Hap_freq_table_subpops$POP_4.9 > 0 |
                           Hap_freq_table_subpops$POP_4.x > 0 |
                           Hap_freq_table_subpops$POP_5.1 > 0 |
                           Hap_freq_table_subpops$POP_5.2 > 0 |
                           Hap_freq_table_subpops$POP_5.3 > 0
                         , 1]



## Create list of haplotypes from North America that are found in Spain and France (POP2.4)
POP4and5_insource_list <-
  POP4and5_list[POP4and5_list$Haplotype %in% POP2.4_list$Haplotype,]

## Create an empty dataframe to store final results
POP4and5_POP2.4_boots <- data.frame()

## Create an empty dataframe to store temporary results
Hap_sample <- data.frame()

## Sample without replacement from the source population until all haplotypes from the introduced population are represented
for (n in 1:I) {
  i = 1
  while (all(POP4and5_insource_list$Haplotype %in% Hap_sample$V1) == FALSE) {
    #keep going as long as not all haplpotypes from the introduced population are present in the sample
    
    Hap_sample[i, 1] <-
      sample(POP2.4_v, 1, replace = T, prob = NULL)
    
    i = i + 1
  }
  
  ## Record result: how many founders were needed
  POP4and5_POP2.4_boots[n, 1] <- nrow(Hap_sample)
  Hap_sample <- data.frame()
  
}

## Write output to file
save(POP4and5_POP2.4_boots, file = "POP4and5_POP2.4_boots")

## Calculate some summary statistics
m <- mean(POP4and5_POP2.4_boots$V1)
s <- sd(POP4and5_POP2.4_boots$V1)
n <- I
error <- qnorm(0.975) * s / sqrt(n)
left <- m - error
right <- m + error
