# R script for running kmeans in parallel

# load necessary pacakges
library(foreach)
library(doParallel)
library('Rcpp')
library(tidyverse)

# load cpp file for computing the similarity measure
sourceCpp('code/correlation.cpp')
# load helper functions for running kmeans stabiltiy tests
source('code/kmeans.R')

# use 9 cores (i.e. one for each k)
nCores <- 9
registerDoParallel(nCores) 


# parameters for stability testing
N <- 100 # repeat 100 times for each k
m <- .5 # sample fraction
k_max <- 10 # maximum number of k to try


# load data
load('data/lingBinary.RData')
# select only the ID and the question columns
data <- select(lingBinary, -CITY, -STATE, -ZIP, -lat, -long)


# run kmeans stability test in parallel for k = 2, ..., k_max
result <- foreach(k = 2:k_max) %dopar% {

  kmeansStabilityTest(data, m, k, N)
  
}

# combine results into a single dataframe. Each column corresponds to a particualr k.
result_df <- result %>%
  # combine columns
  bind_cols() %>%
  # save only 3 decimal points
  format(digits = 3)


# save results
write.table(result_df, "kmeans_corr_results.txt", row.names = F, quote=F)

