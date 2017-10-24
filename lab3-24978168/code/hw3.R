# EM algorithm for HW3
library(tidyverse)

# simulation parameters: mu_0 = 10 and a range of mu_1's
n <- 100 # number of observations from each cluster
mu_0 <- 10 # mean of the first poisson distribution
mu_1 = c(12,14,16,18) # means for the second poisson

# convergence threshold: stop if each of the current estimate is 
# within .01 of the previous estimate
conv <- 0.001
# break if EM runs for more than this number of iterations
max_iter <- 5000


# run EM for mu_0 = 1 and a range of mu_1's
result <- lapply(mu_1, function(mu_1) testEM(mu_0, mu_1, n, conv, max_iter))

# extract accuracy from the reuslt
acc <- sapply(result, function(x) x[4])

# plot accuracy
ggplot() + 
  # x = mu_1, y = accuracy
  geom_point(aes(x = mu_1, y = acc)) + 
  theme_classic() + 
  labs(y = "Accuracy", x = "mu_1")


############### Below are the functions for running EM #############

# simulate data and call EM
testEM <- function(mu_0, mu_1, n, conv, max_iter){
  # Input: mu_0, mu_1 are the true means used in simulations
  #        n is the number of observations from each distribution
  #        conv and max_iter are the stopping criteria
  # Output: c(pi, mu_0, mu_1, accuracy), where accuracy is the 
  #         assignment accuracy, and the first three elements are 
  #         the estimates of the parameters from EM
  
  # simulate from two distributions
  x_1 <- rpois(n, lambda = mu_0)
  x_2 <- rpois(n, lambda = mu_1)
  # combine obsevations into a single vector
  x <- c(x_1, x_2)
  
  # get initial guess from kmeans
  init <- computeInitialTheta(x, 2*n)

  # run EM
  result <- runEM(x, init[1], init[2], init[3], conv, max_iter)    
  
  # assign to cluster
  cluster <- assignToCluster(x, result[1], result[2], result[3])
  
  # compute assignment accuracy
  cluster_true <- c(rep(0, n), rep(1,n)) # true membership
  accuracy <- sum(cluster == cluster_true) / (2*n)
  
  # add accuracy to the result vector
  result['acc'] <- accuracy
  
  return(result)
  
}


# assign to cluster according to Pr(Z | x)
assignToCluster <- function(x, pi, mu_0, mu_1){
  # Input: x is the observation vector
  #        pi, mu_0, mu_1 are the current parameters esitmates
  # Output: nx1 vector of cluster memberships
  
  # compute conditional probablity Pr(Z = 1 | x, theta)
  T_0 <- computeTij(x, 0, pi, mu_0, mu_1)
  
  # assign i to cluster 0 if T_0i > .5
  cluster <- rep(0, length(x))
  cluster[T_0 < .5] <- 1
  
  return(cluster)
  
}


# compute initial values from kmeans clustering
computeInitialTheta <- function(x, n){
  # Input: x is the observation vector
  # Output: vector containing initial guesses for pi, mu_0, mu_1
  
  
  # run kmeans with k = 2
  kmeans <- kmeans(x, centers = 2)
  
  # always label the first distribution as "1"
  if(kmeans$centers[1] > kmeans$centers[2]){
    
    new_label = kmeans$cluster
    new_label[kmeans$cluster == 1] = 2
    new_label[kmeans$cluster == 2] = 1
    kmeans$cluster <- new_label
    
  }
  
  # initial guess for the parameters
  pi <- sum(kmeans$cluster == 1) / n
  mu_0 <- mean(x[kmeans$cluster == 1])
  mu_1 <- mean(x[kmeans$cluster == 2])
  
  return(c(pi, mu_0, mu_1))
  
}


# run EM
runEM <- function(x, pi, mu_0, mu_1, conv, max_iter){
  # Input: x is the observation vector
  #        pi, mu_0, mu_1 are the initial guesses
  #        conv, max_iter are stopping criteria
  # Output: vector containing estimates for pi, mu_0, mu_1
  
  # return true if EM converged
  converged <- function(theta_prev, theta_curr, conv){
    
    dist <- abs(theta_prev - theta_curr) / theta_prev
    return(all(dist < conv))
    
  }
  

  
  theta_prev <- c(pi, mu_0, mu_1)
  T_0 <- computeTij(x, 0, pi, mu_0, mu_1)
  T_1 <- computeTij(x, 1, pi, mu_0, mu_1)
  theta_curr <- estimateParams(x, T_0, T_1)

  
  i <- 0
  
  while(!converged(theta_prev, theta_curr, conv) & i < max_iter){
  #while(i < max_iter){
    
    theta_prev <- theta_curr
    T_0 <- computeTij(x, 0, theta_curr[1], theta_curr[2], theta_curr[3])
    T_1 <- computeTij(x, 1, theta_curr[1], theta_curr[2],theta_curr[3])
    theta_curr <- estimateParams(x, T_0, T_1)
    
    i <- i + 1
    
  }
  
  return(theta_curr)
  
  
}


# compute the conditional probability T_ji
computeTij <- function(x, z, pi, mu_0, mu_1){
  # Input: x is the observation vector
  #        z is the hidden variable (0 or 1)
  #        pi, mu_0, mu_1 are the current parameters esitmates
  # Output: vector containing T_zi's
  
  # P0(x) and P1(x) with current parameter estimates
  p0_x <- dpois(x, lambda = mu_0)
  p1_x <- dpois(x, lambda = mu_1)
  
  # compute the term in the denominator
  denom <- pi * p0_x + (1 - pi) * p1_x
  
  # compute the term in the numerator
  if(z == 0)
    num <- pi * p0_x
  else
    num <- (1 - pi) * p1_x
  
  return(num / denom)
  
} 



# estimate pi, mu_0, and mu_1, given T_ji
estimateParams <- function(x, T_0, T_1){
  # Input: x is the observation vector
  #       T_0 is nx1 vector of T_0i's
  #       T_1 is nx1 vector of T_1i's
  # Output: vector containing new estimates of pi, mu_0, mu_1
  
  mu_0 <- sum(T_0 * x) / sum(T_0)
  mu_1 <- sum(T_1 * x) / sum(T_1)
  pi <- mean(T_0)
  
  return(c(pi, mu_0, mu_1))
  
}

