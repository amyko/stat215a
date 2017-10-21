# This file contains all the subfunctions needed to run kmeans stabiltiy tests

subsample <- function(data, m){
  # subsample at rate m
  # Args:
  #   data : dataframe, contains binary linguistics data.
  #         The first column is ID and the remaining columns are questions.
  #   m : double, fraction of rows to select, must be between 0 and 1
  # Returns:
  #   sample: dataframe, subsample of the original data arranged by ID
  
  # return if the sample fraction is out of bounds
  if(m <= 0 | m > 1){
    print("m out of bounds")
    return()
  }
  
  # subsample data
  sample <- data %>%
    # sample at rate m
    sample_frac(m) %>%
    # sort rows by increasing order of ID
    arrange(ID)
    
  return(sample)
    
  
}

runKmeans <- function(sample, k){
  # Run kmeans and return the cluster assignment 
  # Args:
  #   sample : dataframe, output of subsample function
  #   k : integer, number of clusters
  # Returns:
  #   cluster: dataframe, the first column is the individual ID; 
  #             the second column is the cluster assignment
  
  
  # remove the ID column, leaving just the question columns
  X <- sample %>%
    select(-ID)
  
  # run kmeans with k clusters
  kmeans <- kmeans(X, centers = k)
  
  # join ID column and cluster assignments
  cluster <- cbind(sample$ID, kmeans$cluster) %>%
    # convert to a dataframe
    as.data.frame()
  
  # rename columns to "ID" and "cluster"
  colnames(cluster) <- c("ID", "cluster")
  
  return(cluster)
  
}

filterKmeansResults <- function(x, y){
  # Filter rows of x and y so that they only contain individuals
  # who are present in both x and y.
  # Args:
  #   x : dataframe, output of runKmeans containing two columns: ID and cluster
  #   y : dataframe, output of runKmeans containing two columns: ID and cluster
  # Returns:
  #   result: list, contains x_prime and y_prime, which are integer vectors 
  #           whose rows correspond to the individuals present in both x and y
  
  
  filterRows <- function(x, ids){
    # Filter rows of x so that it only contains individuals given by ids
    # Args:
    #   x : dataframe, output of runKmeans. Contains columns (ID, cluster)
    # Returns:
    #   numeric vector, cluster assignment for individuals given by ids
    
    
    # keep rows whose ID is in ids
    x <- x %>%
      filter(ID %in% ids)
    
    # return the cluster assignment as an interger vector
    return(as.numeric(x$cluster))
    
  }
  
  
  # find common IDs between x and y
  good_ids <- intersect(x$ID, y$ID)
  
  # keep only the individuals who are in both x and y
  x_prime <- filterRows(x, good_ids)
  y_prime <- filterRows(y, good_ids)
  

  # return both vectors in a list
  return(list(x_prime, y_prime))

  
}


getCorrelation <- function(data, m, k){
  # Compute the correlation between the cluster assignments of two subsamples
  # Args:
  #   data : dataframe, contains ID and the question columns
  #   m : double, sample fraction 
  #   k : integer, cluster number
  # Returns:
  #   corr : double, correlation value
  
  # make two subsamples
  sample_1 <- subsample(data, m)
  sample_2 <- subsample(data, m)
  
  # run kmeans on the two subsamples
  x <- runKmeans(sample_1, k)
  y <- runKmeans(sample_2, k)
  
  # filter kmeans results so that they contain the same set of individuals
  result <- filterKmeansResults(x, y)
  
  # return the correlation computed by a cpp function
  return(correlationCPP(result[[1]], result[[2]]))
  
  
}

kmeansStabilityTest <- function(data, m, k, N){
  # Compute the correlation for kmeans between two subsamples. Repeat N times.
  # Args:
  #   data : dataframe, contains ID and the question columns
  #   m : double, sample fraction 
  #   k : integer, cluster number
  #   N : integer, number of experiments
  # Returns:
  #   corr : N x 1 dataframe, contains N correlation values. The column name is set to "k".
  
  # load tidyverse
  library(tidyverse)
  
  # compute the correlation N times
  result <- lapply(1:N, function(x) getCorrelation(data, m, k)) %>%
    # unpack the list
    unlist() %>%
    # convert to a dataframe
    as.data.frame()
  
  # rename the column; e.g. if k = 2, then the column name is "k2"
  colnames(result) <- paste0('k', k)
  
  return(result)

}
