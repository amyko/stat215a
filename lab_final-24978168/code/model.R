# Helper functions for model selection


fitModel <- function(X, y, model, lambda_vec) {
  # Fit a linear regression model
  # parameters.
  # X - the X values of the training set (nxp)
  # y - the y values of the training set (nx1)
  # model - 1 for LASSO, 0 for ridge
  # lambda_vec - vector with all the possible regularization parameters
  #
  # Returns:
  # A glmnet object
  
  fit <- glmnet(X, y, 
                family = "gaussian", 
                alpha = model, 
                lambda = lambda_vec)
  return(fit)
  
}


predictY <- function(X, fit, lambda) {
  # Predict the response for a given data set
  # parameters.
  # X - the X values to predict the resopnce
  # fit - the previous fit created by the training set
  # lambda - lambda values at which to evaluate the predict function
  #
  # Returns:
  # A prediction object 
  
  pred <- predict(fit, newx = X,
                  type = "response",
                  s = lambda)
  
  return(pred)
  
}


computeRSS <- function(X, y, fit) {
  # Given lambda, fit a model and report the RSS
  #
  # Args:
  # X - design matrix (nxp)
  # fit - glmnet object
  # y - vector of responses (nx1)
  #
  # Return:
  # RSS vector (one value per lambda)
  
  # compute RSS 
  RSS <- function(y_hat){
    # Input:
    # y_hat - fitted values
    # Output:
    # RSS between y and y_hat
    
    rss <- (y_hat - y)^2 %>%
      sum()
    
    return(rss)
  
  }
  
  
  # prediction for each lambda (#obs x length(lambda_vec))
  pred <- predictY(X, fit, fit$lambda)
  # RSS for each lambda
  rss_vec <- sapply(1:dim(pred)[2], function(i) RSS(pred[, i]))
  
  return(rss_vec)
  
}


computeAIC <- function(rss, fit, n) {
  # Calculate the AIC for the LASSO results
  # Args:
  # rss - vector of rss (one for each lambda)
  # fit - glmnet object
  # n - number of observations
  #
  # Returns:
  # AIC
  
  df <- fit$df
  
  aic <- n*log(rss) + 2*df
  
  return(aic)
}


computeAICc <- function(rss, fit, n) {
  # Calculate the AICc for the LASSO results
  # Args:
  # rss - vector of rss (one for each lambda)
  # fit - glmnet object
  # n - number of observations
  #
  # Returns:
  # AIC
  
  df <- fit$df
  
  aic_c <- n*log(rss) + 2*df + 2*df*(df+1)/(n-df-1)

  return(aic_c)
}


computeBIC <- function(rss, fit, n) {
  # Calculate the BIC for the LASSO results
  # Args:
  # rss - vector of rss (one for each lambda)
  # fit - glmnet object
  # n - number of observations
  #
  # Returns:
  # BIC

  df <- fit$df
  
  bic <- n*log(rss) + log(n)*df

  return(bic)
}


ESCV <- function(X, y, model, k){
  # Run ESCV
  # parameters.
  # X - the X values of the training set (nxp)
  # y - the y values of the training set (nx1)
  # model - 1 for LASSO, 0 for ridge
  # k - number of folds
  #
  # Returns:
  # ESCV object
  
  fit <- escv.glmnet(X, y, 
              family = "gaussian",
              nfolds = k, 
              alpha = model)
  
  
  return(fit)
  
}


computeCorr <- function(X, y, fit, lambda){
  # Compute Pearson's correlation between the fitted values and observed values
  # parameters.
  # X - the X values of the training set (nxp)
  # y - the y values of the training set (nx1)
  # fit - glmnet object
  # lambda - best lambda for this fit
  #
  # Returns:
  # correlation
  
  # predict
  y_hat <- predictY(X, fit, lambda)
  corr <- cor(y_hat, y, method = "pearson")
  
  return(corr)

}


extractTopFeatures <- function(fit, lambda, p){
  # Extract indicies of top p features
  # parameters.
  # fit - glmnet object
  # lambda - best lambda for this fit
  # p - number of features to extract
  #
  # Returns:
  # vector containing indices of top p features
  
  # which beta column to use
  beta_col <- match(lambda, fit$lambda)
  
  # extract coefficients
  coeff <- coef(fit) %>%
    # convert to matrix
    as.matrix() %>%
    # select the model at the best lambda
    .[, beta_col] %>%
    # covert to data frame
    data.frame() %>%
    # name each feature 0 (intercept) through 10921
    mutate(feat = 0:(dim(fit$beta)[1])) %>%
    # remove intercept
    filter(feat > 0) %>%
    # sort from biggest coefficient to smallest
    arrange(desc(abs(.)))
  
  return(coeff$feat[1:p])
  
}


bootstrapFit <- function(X, y, model, lambda){
  # Fit a model on botstrapped samples 
  # Args:
  # X - design matrix
  # y - response vector for a particular voxel
  # model - 1 for LASSO, 0 for Ridge
  # lambda - best lambda for this fit
  #
  # Returns:
  # list containing 1) glmnet object for the bootstrap sample, and 
  #                 2) the correlation between y and y_hat
  
  # sample n observations wtihout replacement
  n <- nrow(X)
  ind <- sample(n, size=n, replace=TRUE)
  # form bootstrap samples for X and y
  X_bs <- X[ind, ]
  y_bs <- y[ind]
  
  # fit model
  fit <- fitModel(X_bs, y_bs, model, lambda)
  
  # compute correlation between y and fitted value
  corr <- computeCorr(X_bs, y_bs, fit, lambda)
  
  return(list(fit, corr))

  
}

