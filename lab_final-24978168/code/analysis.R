# R code for generating output and plots in the report
source('model.R')

################## Load and partition data ############
library(tidyverse)
library(ggridges)
load("../data/fMRIdata.RData")

# number of observations
n <- dim(fit_feat)[1]
# sample .25 of data for testing
test_index <- sample(1:n, size = .25*n, replace = FALSE)
# .75 of data will be used for model selection
in_training <- setdiff(1:n, test_index)
# of the .75, 1/3 of them will be for validation
val_index <- sample(in_training, size = .33*length(in_training), replace = FALSE)
# the other 2/3 are for training
train_index <- setdiff(in_training, val_index)

# partition feature and response data according to train index
train_feat <- fit_feat[train_index, ]
train_resp <- resp_dat[train_index, ]
valid_feat <- fit_feat[val_index, ]
valid_resp <- resp_dat[val_index, ]
test_feat <- fit_feat[test_index, ]
test_resp <- resp_dat[test_index, ]

#save(train_feat, train_resp, valid_feat, valid_resp, test_feat, test_resp, 
  #file = "../output/data_partitions.RData") 
#load("../output/data_partitions.RData")



################### EDA ########################
library(corrplot)
## distribution of response for each voxel
# convert to data frame
resp_dat <- data.frame(resp_dat)
# renmae columns to 1 through 20 (voxels)
colnames(resp_dat) <- sapply(1:ncol(resp_dat), as.character)

# plot (only look at the 75 percent of data)
resp_dat[in_training, ] %>%
  # make into long form
  gather(key = "Voxel", value = "Response") %>%
  # view distribution using ridge plot
  ggplot() +
  # ridge plot
  geom_density_ridges(aes(x = Response, y = Voxel, group = Voxel)) + 
  # make axes look nice
  scale_y_discrete(limits = sapply(1:ncol(resp_dat), as.character)) + 
  theme_ridges()


## correlation between voxels
resp_dat[in_training, ] %>%
  # compute correlation
  cor() %>%
  # plot
  corrplot.mixed(tl.col = "black", upper="circle", lower="circle")



# distance vs. correlation
pairs <- combn(1:20, 2) %>%
  t()

computeDist <- function(x1, x2){
  
  d <- (x1-x2)^2 %>%
    sum() %>%
    sqrt()
  
  return (d)
  
}

# indices for every pair of voxels
data <- resp_dat[in_training, ]
# Euclidean distance between every pair
dist <- sapply(1:dim(pairs)[1], function(i) computeDist(loc_dat[pairs[i,1],], loc_dat[pairs[i,2],]))
# correlation of responses for every pair
corr <- sapply(1:dim(pairs)[1], function(i) cor(data[, pairs[i,1]], data[,pairs[i,2]]))

# plot distance vs. correlation
ggplot() +
  geom_point(aes(x = dist, y = corr)) +
  xlab("Distance") +
  ylab("Correlation") + 
  theme_classic()

#################### Model selection ####################
library(glmnet)
library(HDCI)
library(gridExtra)

# function for plotting lambda vs. error
plotTrainingError <- function(lambda, error, error_label){
  # Input:
  # lambda - vector of lambda values
  # error - vector of error values
  # error_label - label for the y-axis
  # Output:
  # ggplot object
  
  p <- ggplot() +
    # plot
    geom_point(aes(x = lambda, y = error)) +
    # axis labels
    xlab(expression(lambda)) +
    ylab(error_label) +
    theme_classic() 
  
  return(p)
  
}


## ESCV for Lasso and Ridge
# number of folds
nfolds <- 10
# number of voxels
n <- dim(train_resp)[1]
# number of features
m <- dim(train_resp)[2]
# run lasso + ESCV for each voxel
lasso_escv <- lapply(1:m, 
                     function(i) ESCV(train_feat, train_resp[, i], 
                                      model = 1, k = nfolds))
# run ridge + ESCV for each voxel
ridge_escv <- lapply(1:m, 
                     function(i) ESCV(train_feat, train_resp[, i], 
                                      model = 0, k = nfolds))

#save(lasso_escv, ridge_escv, file = "../output/escv_results.RData")
#load("../output/escv_results.RData")

## Compute AIC, AICc, and BIC for LASSO
# compute RSS
rss_list <- lapply(1:m, function(i) computeRSS(train_feat, train_resp[, i], 
                                                lasso_escv[[i]]$glmnet.fit))
# AIC for each voxel
aic_list <- lapply(1:m, function(i) computeAIC(rss_list[[i]], lasso_escv[[i]]$glmnet.fit, n))
# AICc for each voxel
aic_c_list <- lapply(1:m, function(i) computeAICc(rss_list[[i]], lasso_escv[[i]]$glmnet.fit, n))
# BIC for each voxel
bic_list <- lapply(1:m, function(i) computeBIC(rss_list[[i]], lasso_escv[[i]]$glmnet.fit, n))


# plot lambda vs. metric for LASSO
i <- 1
p1 <- plotTrainingError(lasso_escv[[i]]$lambda, aic_list[[i]], "AIC")
p2 <- plotTrainingError(lasso_escv[[i]]$lambda, aic_c_list[[i]], "AICc")
p3 <- plotTrainingError(lasso_escv[[i]]$lambda, bic_list[[i]], "BIC")
p4 <- plotTrainingError(lasso_escv[[i]]$lambda, lasso_escv[[i]]$cv, "Mean CV error")
p5 <- plotTrainingError(lasso_escv[[i]]$lambda, lasso_escv[[i]]$es, "Mean ES")
grid.arrange(p1, p2, p3, p4, p5, ncol = 3)


# plot lambda vs AIC on validation data
rss_val <- computeRSS(valid_feat, valid_resp, lasso_escv[[i]]$glmnet.fit)
aic_val <- computeAIC(rss_val, lasso_escv[[i]]$glmnet.fit, dim(valid_resp)[1])
plotTrainingError(lasso_escv[[i]]$lambda, aic_val, "AIC")


# plot lambda vs. metric for Ridge
p1 <- plotTrainingError(ridge_escv[[i]]$lambda, ridge_escv[[i]]$cv, "Mean CV error")
p2 <- plotTrainingError(ridge_escv[[i]]$lambda, ridge_escv[[i]]$es, "Mean ES")
grid.arrange(p1, p2, ncol = 2)


# best lambdas for each model (20x1 vector)
lambda_lasso_cv <- sapply(1:m, function(i){lasso_escv[[i]]$lambda.cv})
lambda_lasso_escv <- sapply(1:m, function(i){lasso_escv[[i]]$lambda.escv})
lambda_lasso_aic <- sapply(1:m, function(i){lasso_escv[[i]]$lambda[which.min(aic_list[[i]])]})
lambda_lasso_aic_c <- sapply(1:m, function(i){lasso_escv[[i]]$lambda[which.min(aic_c_list[[i]])]})
lambda_lasso_bic <- sapply(1:m, function(i){lasso_escv[[i]]$lambda[which.min(bic_list[[i]])]})
lambda_ridge_cv <- sapply(1:m, function(i){ridge_escv[[i]]$lambda.cv})
lambda_ridge_escv <- sapply(1:m, function(i){ridge_escv[[i]]$lambda.escv})



################ Compare performance on validation set ######################
# compute correlation 
corr_cv <- sapply(1:m, function(i) computeCorr(valid_feat, valid_resp[,i], 
                                               lasso_escv[[i]]$glmnet.fit, lambda_lasso_cv[i]))
corr_escv <- sapply(1:m, function(i) computeCorr(valid_feat, valid_resp[,i], 
                                               lasso_escv[[i]]$glmnet.fit, lambda_lasso_escv[i]))
corr_aic <- sapply(1:m, function(i) computeCorr(valid_feat, valid_resp[,i], 
                                                lasso_escv[[i]]$glmnet.fit, lambda_lasso_aic[i]))
corr_aic_c <- sapply(1:m, function(i) computeCorr(valid_feat, valid_resp[,i], 
                                                  lasso_escv[[i]]$glmnet.fit, lambda_lasso_aic_c[i]))
corr_bic <- sapply(1:m, function(i) computeCorr(valid_feat, valid_resp[,i], 
                                                lasso_escv[[i]]$glmnet.fit, lambda_lasso_bic[i]))
corr_ridge_cv <- sapply(1:m, function(i) computeCorr(valid_feat, valid_resp[,i], 
                                                     ridge_escv[[i]]$glmnet.fit, lambda_ridge_cv[i]))
corr_ridge_escv <- sapply(1:m, function(i) computeCorr(valid_feat, valid_resp[,i], 
                                                       ridge_escv[[i]]$glmnet.fit, lambda_ridge_escv[i]))

# mean correlation for lasso + escv and ridge + cv
mean(corr_escv[c(-10, -16, -20)])
mean(corr_ridge_cv[c(-10, -16, -20)])



# table of correlation for each voxel
data <- data.frame(Voxel = as.factor(1:20), corr_aic, corr_aic_c,
                   corr_bic, corr_cv, corr_escv, corr_ridge_cv, corr_ridge_escv)
# format table
colnames(data) <- c("Voxel", "LASSO + AIC", "LASSO + AICc", "LASSO + BIC",
                    "LASSO + CV", "LASSO + ESCV", "ridge + CV",
                    "ridge + ESCV")
x <- format(round(data[,-1], 2))
y <- bind_cols(Voxel = as.factor(1:20), x)

# show table
grid.table(y, rows=NULL)


# plot correlation for each voxel
plotCorrelation <- function(corr){
  # Input:
  # corr - vector of correlation
  # Output:
  # ggplot object
  
  # plot correlation for each voxel
  data <- data.frame(Voxel = as.factor(1:20), Correlation = corr) %>%
    filter(!is.na(Correlation))
  ggplot(data) + 
    geom_point(aes(Voxel, Correlation), size = 3) + 
    theme_ridges()
  
}

# plot correlation
ggplot(data) + 
  geom_point(aes(x = Voxel, y = Correlation, shape = Method), size = 3, alpha = .7) + 
  theme_ridges()


################# Fit of Ridge + CV and Lasso + ESCV ###############

# plot residual vs. fitted
plotFittedResidual <- function(fit, lambda, voxel){
  
  fitted <- predictY(train_feat, fit, lambda) %>% as.vector()
  
  ggplot() + 
    geom_point(aes(x = fitted, y = fitted - train_resp[, voxel])) + 
    xlab("Fitted values") +
    ylab("Residual") + 
    theme_classic()
  
}

# plot residuals for voxel 1
voxel <- 1
plotFittedResidual(lasso_escv[[voxel]]$glmnet.fit, lambda_lasso_escv[[voxel]], voxel)
plotFittedResidual(ridge_escv[[voxel]]$glmnet.fit, lambda_ridge_cv[[voxel]], voxel)


################## Stability of model #################
# Bootstrap, fit, and predict voxel 1 (correlation)
voxel <- 1
B <- 100
p <- 50
lasso_bs <- lapply(1:B, function(i) bootstrapFit(train_feat, train_resp[, voxel], 
                                              1, lambda_lasso_escv[voxel]))

ridge_bs <- lapply(1:B, function(i) bootstrapFit(train_feat, train_resp[, voxel], 
                                                       0, lambda_ridge_cv[voxel]))

#save(lasso_bs, ridge_bs, file = "../output/bootstrap_fits.RData")
#load("../output/bootstrap_results.RData")

# plot distribution of correlation
plotCorrDist <- function(result){
  
  corr <- sapply(1:length(result), function(i) result[[i]][[2]][1])
  
  ggplot() + 
    geom_histogram(aes(x = corr), color = "black", alpha = .5, fill = "blue") + 
    xlab("Correlation") +
    ylab("Count") + 
    theme_classic()
}

plotCorrDist(lasso_bs)
plotCorrDist(ridge_bs)

# Are the top m features consistent across the bootstrap samples?
p <- 50
plotNumCommonFeat <- function(result){

  b <- length(result)

  # extract top p features from each bootstrap run
  feat <- sapply(1:b, function(i) extractTopFeatures(result[[i]][[1]], result[[i]][[1]]$lambda, p))

  # possible pairs of indicies 
  pairs <- combn(1:b, 2) %>% 
    t()
  
  common <- sapply(1:b, function(i) length(intersect(feat[, pairs[i,1]], feat[, pairs[i,2]])))

  ggplot() + 
    geom_histogram(aes(common), color = "black", alpha = .5, fill = "blue") + 
    scale_x_continuous(name = "Number of common features", breaks = seq(0, max(common), by = 2)) +
    scale_y_continuous(name = "Count", breaks = seq(0,p, by = 5)) + 
    theme_classic()
  
    
}

# plot histogram of number of common features
plotNumCommonFeat(lasso_bs)
plotNumCommonFeat(ridge_bs)


############## Interpret models #################
# Do the two models share any features?
p <- 10
lasso_features <- lapply(1:20, function(i) extractTopFeatures(lasso_escv[[i]]$glmnet.fit, lambda_lasso_escv[i], p))
ridge_features <- lapply(1:20, function(i) extractTopFeatures(ridge_escv[[i]]$glmnet.fit, lambda_ridge_cv[i], p))
sort(lasso_features[[1]]) # table for feat x test
sort(ridge_features[[1]])

# find common features among top 50 for each voxel
common <- sapply(1:20, function(i) length(intersect(ridge_features[[i]], lasso_features[[i]])))


# which predictors are important for which voxel?
impt_feat_lasso <- sapply(1:20, function(i) lasso_features[[i]][1])
impt_feat_ridge <- sapply(1:20, function(i) ridge_features[[i]][1])

# make a table of important features
data <- data.frame(Voxel = as.factor(1:20), Lasso = impt_feat_lasso, ridge = impt_feat_ridge)
data$Lasso[20] = "NA"
colnames(data) <- c("Voxel", "Lasso feature", "Ridge feature")
grid.table(data, rows = NULL)

# which features are stable across bootstrap samples?
#extract top 10 features for voxel 1 from bootstrap fits
p <- 10
getStableFeatures <- function(result, p){
  
  # get top p features from each bootstrap run
  top_features <- sapply(1:length(result), function(i){
    fit <- result[[i]][[1]]
    extractTopFeatures(fit, fit$lambda, p)}) %>%
    # convert to vector
    as.vector() %>%
    # convert to dataframe
    data.frame(feature = .) %>%
    # count frequency of each feature
    group_by(feature) %>% 
    summarise(count = n()) %>%
    # sort by descending count
    arrange(desc(count))

  return(top_features)
  
}

# ridge is a lot more stable
stable_feat_lasso <- getStableFeatures(lasso_bs, p)
stable_feat_ridge <- getStableFeatures(ridge_bs, p)




############## Performance of LASSO on test data ##############
# compute correlation for each voxel on test dat
test_corr <- sapply(1:m, function(i) computeCorr(test_feat, test_resp[,i], 
                                                 lasso_escv[[i]]$glmnet.fit, lambda_lasso_escv[i]))


# data frame containing voxel vs. correlation
data <- data.frame(Voxel = as.factor(1:20), Correlation = test_corr) %>%
  filter(!is.na(Correlation))

# plot correlation for each voxel
ggplot(data) + 
  geom_point(aes(Voxel, Correlation), size = 3) + 
  theme_ridges()


############# Prediction on the 120 images ################
pred_test <- predictY(val_feat, lasso_escv[[1]]$glmnet.fit, lambda_lasso_escv[[1]])
write.table(pred_test, file = "../output/predv1_amyko.txt", quote = FALSE, row.names = FALSE, col.names = FALSE)
