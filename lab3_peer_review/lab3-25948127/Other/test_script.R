## Load Packages
library("doParallel")
library('foreach')
library('rlecuyer')
library('Rcpp')
library('microbenchmark')
library('ggplot2')
library("tidyverse")

# Generate sample data
X <- rnorm(10000, 0, 1)
Y <- rnorm(10000, 0 ,1)
Z <- rnorm(10000, 0, 1)
XYZ <- data.frame(X,Y,Z)

## Set WD and load data
setwd("~/stat215a/lab3-25948127/data")
load("lingBinary.RData")

## Retain only the numerical answers
binary_data <- lingBinary %>%
  select(-ID, -CITY, -STATE, -ZIP, -lat, -long) %>%
  drop_na()

rm(lingBinary)

## Wrapper that calculates kmeans then returns the clusters

kMeansWrapper <- function(data, k){
  clusters <- kmeans(scale(data), k)
  return(clusters$cluster)
}

# Jaccard.R: Computes Jaccard similarity
Jaccard.R <- function(data1, data2){
  M1 <- data1 %o% data2 # outer product of first matrix and second
  M2 <- data2 %o% data1 # outer product of second matrix and first
  M12 <- sum(sum(M1 * M2)) # Sum the pairwise elements, then sum again for intersected scalar distance
  J <- M12 / (sum(sum(M1)) + sum(sum(M2)) - M12) # Intersected scalar distance over total scalar distance
  return(J) # Return similarity measure
}

## Source C++
sourceCpp("~/stat215a/lab3-25948127/R/Jaccard.cpp")

## Cluster Similarity Function
ClusterSim <- function(data, fraction, clustering.method, kclusters, similarity.method){
  # Create two subsamples
  sample1 <- sort(sample(nrow(data), nrow(data) * fraction, replace=FALSE))
  sample2 <- sort(sample(nrow(data), nrow(data) * fraction, replace=FALSE))
  # Cluster them (call the kmeanswrapper for actual use)
  cluster1 <- clustering.method(sample1, kclusters)
  cluster2 <- clustering.method(sample2, kclusters)
  
  # Subset each group to the observations that appear in the other sample
  intersect1 <- sample1 %in% sample2
  intersect2 <- sample2 %in% sample1
  
  # Apply similarity method to return the result
  return(similarity.method(cluster1[intersect1], cluster2[intersect2]))
}

# Microbenchmarks
## Benchmark the relative performance of the R and C++ versions with .5 of the data
microbenchmark(ClusterSim(binary_data, fraction = .5, kMeansWrapper, kclusters = 2, similarity.method = Jaccard.R),
               ClusterSim(binary_data, .5, kMeansWrapper, 2,
                          similarity.method = JaccardCPP))

## Parallel

### Initialize the Clusters
cl <- parallel::makeCluster(8)
registerDoParallel(cl)
### Set up 100 simulations
n <- 100

## R function
### Create an empty list that will store the dataframes
listofdfs <- list()
### For loop that does 100 simulations with k = [2:10], and saves the results into
### a dataframe, providing an ID corresponding to the number of clusters the 
### result corresponds to
for (k in seq(2,10)){
  # Track which k the loop is working on
  print(sprintf("%d simulations with k=%d...", n, k))
  # foreach loop to do 100 simulations
  sims <- foreach(i = 1:n_jobs) %dopar% 
    ClusterSim(XYZ, .5, kMeansWrapper, k,
               similarity.method = Jaccard.matrix)
  # convert results into numeric
  sims <- unlist(lapply(sims[1:n_jobs], as.numeric))
  # Save results in a dataframe
  sims_df <- data.frame(sim_value=sims)
  # Mutate a "kID" variable to identify how many clusters when I merge later
  sims_df <- sims_df %>% mutate(kID = k)
  # Save each k-dataframe into a list
  listofdfs[[k]] <- sims_df
}

## Combine all of the k-dataframes in the list into one dataframe, and save a csv
sims_df <- bind_rows(listofdfs)

## CPP Function
## Repeat above with C++ function
listofdfs <- list()
for (k in seq(2,10)){
  print(sprintf("%d simulations with k=%d...", n, k))
  sims <- foreach(i = 1:n_jobs) %do% 
    ClusterSim(XYZ, .5, kMeansWrapper, k,
               similarity.method = JaccardCPP)
  sims <- unlist( lapply(sims[1:n_jobs], as.numeric) )
  sims_df <- data.frame(sim_value=sims)
  sims_df <- sims_df %>% mutate(kID = k)
  listofdfs[[k]] <- sims_df
}

sims_df <- bind_rows(listofdfs)

# Stop the Cluster
stopCluster(cl)