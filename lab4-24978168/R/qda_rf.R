# Contains R code for EDA, QDA, and random forest

library(tidyverse)
library(gridExtra)
library(caret)
library(ROCR)
library(corrplot)

#################### Data processing ##########################

# Get the data for three images
path <- "../data/"
image1 <- read.table(paste0(path, 'image1.txt'), header = F)
image2 <- read.table(paste0(path, 'image2.txt'), header = F)
image3 <- read.table(paste0(path, 'image3.txt'), header = F)


# Add informative column names.
collabs <- c('y','x','label','NDAI','SD','CORR','DF','CF','BF','AF','AN')
names(image1) <- collabs
names(image2) <- collabs
names(image3) <- collabs


# divide image into kxk blocks for cross validation
groupImage <- function(image, k, offset){
  # Appends a column named "group" that indicates which block the observation belongs to
  # INPUT: image, dataframe containing the image
  #        offset, integer indicating the start index for the groups
  #        k, image will be divided into k x k blocks
  
  
  getIndex <- function(q, k){
    
    q_range <- range(q)
    step_size <- round((q_range[2] - q_range[1]) / k)
    
    index <- rep(k, dim(image)[1])
    
    for(i in 1:(k-1)){
      
      if(i < 1) break
      
      index[q >= (i-1) * step_size + q_range[1] & 
              q <= i * step_size + q_range[1]] <- i
      
    }
    
    return(index)
    
  }
  
  
  x_index <- getIndex(image$x, k)
  y_index <- getIndex(image$y, k)
  
  
  getBooleanVectors <- function(x, y, group_num){
    
    return(group_num * (x_index == x & y_index == y))
    
    
  }
  
  grid <- expand.grid(x = 1:k, y = 1:k)
  boolean_vectors <- mapply(getBooleanVectors, grid$x, grid$y, 1:k^2)
  
  
  to_return <- image %>%
    # assign each observation into a group
    mutate(group = rowSums(boolean_vectors) + offset) %>%
    # convert to factors
    mutate(group = as.factor(group))
  
  
}


# divide each image into k x k blocks
k <- 5
train_data <- groupImage(image1, k, 0) %>%
  # filter out unlabeled observations
  filter(label != 0) %>%
  # convert group to factors
  mutate(group = as.factor(group), label = as.factor(label)) 


######################### EDA ####################################

# plot conditional density for each feature
plotConditionalDensity <- function(data, col_id){
  # Plots the conditional density of the data for the given column 
  # Input: data, dataframe that contain image data
  #       col_id, integer that indicates which column of the data should be plotted
  # Output: ggplot object
  
  plot <- data %>%
    # filter out unlabled pixels
    filter(label != 0) %>%
    ggplot() + 
      # plot density for each class
      geom_density(aes_string(x = colnames(data)[col_id], 
                              group = "factor(label)", fill = "factor(label)"), 
                 alpha = 0.5) +
      # make it pretty
      scale_fill_discrete(name = "Expert label", label = c("Not cloud", "Cloud")) +
      ylab("Density") + 
      theme_classic()
  
  return(plot)
  
}

# plot conditional densities for columns 4 thorugh 11
plots <- lapply(4:11, function(x){plotConditionalDensity(image1, x)})

# show in a grid
grid.arrange(plots[[1]], plots[[2]], plots[[3]], plots[[4]],
             plots[[5]], plots[[6]], plots[[7]], plots[[8]], nrow = 4)


###################
# Plot features on a map
plotFeature <- function(data, col_name){
  # Input: data, dataframe that contain image data
  #       col_name, name of the feature
  # Output: ggplot object
  
  p <- ggplot(data) +
    geom_point(aes_string(x = "x", y = "y", color = col_name)) + 
    theme_classic()
  
  return(p)           
  
}

#####################
# plot features
p <- lapply(colnames(image1)[4:11], function(x) plotFeature(image1, x))

# Plot the expert labels on a map
expert_plot <- image1 %>%
  ggplot() + 
  geom_point(aes(x = x, y = y, color = factor(label))) +
  scale_color_discrete(name = "Expert label", label = c("Not cloud", "Unlabeled", "Cloud")) + 
  theme_classic()


# show in a grid
grid.arrange(expert_plot, p[[1]], p[[2]],
             p[[3]], p[[4]], p[[5]],
             p[[6]], p[[7]], p[[8]],
             ncol = 3)


##################
# plot correlation matrix for all the features
# combine two images
bind_rows(image1, image2) %>% 
  # remove unlabeled pixels
  filter(label != 0) %>%
  # choose the features only
  dplyr::select(-y, -x, -label) %>%
  # compute correlation
  cor() %>%
  # plot correlation matrix
  corrplot.mixed(upper = "ellipse")


######################
## QQ plots to check normality of features
makeQQplot <- function(data, feature_name){
  # Plots the normal qq plot
  # Input: data, dataframe containing image data
  #       feature_name, name of the feature (string)
  # Output: ggplot object
  
  plot <- data %>%
    # downsample to avoid overplotting
    sample_frac(.1) %>%
    ggplot() + 
      # qq plot
      geom_qq(aes_string(sample = feature_name, color = "label")) + 
      # make it pretty
      scale_color_discrete(name = "Label",
                         labels=c("Not cloud", "Cloud")) + 
      ggtitle(feature_name) + 
      theme_classic()
  
  return(plot)
}

# QQ plots for the three features
p1 <- makeQQplot(train_data, "NDAI")
p2 <- makeQQplot(train_data, "CORR")
p3 <- makeQQplot(train_data, "SD")

# show in a panel
grid.arrange(p1,p2,p3, ncol = 2, nrow = 2)



########################### Modeling ##############################

# make group folds
group_folds <- groupKFold(train_data$group, k = 5)
# train parameters
fit_control <- trainControl(method = "cv", index = group_folds, number = 5)


# fit QDA
qda_fit <- train(as.factor(label) ~ NDAI + CORR + SD, 
                 data = train_data, 
                 method = "qda",
                 trControl = fit_control)


###############
# cross validation to choose ntrees for random forest
# set mtry = 2
rf_grid <- expand.grid(mtry = c(2))
# try fitting this with varying number of ntrees
rf_fit <- train(as.factor(label) ~ NDAI + CORR + SD, 
                 data = train_data, 
                 method = "rf",
                 trControl = fit_control,
                  ntree = 10,
                  tuneGrid = rf_grid)

print(rf_fit)

#################
# fit random forest
rf_fit <- randomForest(x = dplyr::select(train_data, NDAI, CORR, SD),
                       y = as.factor(train_data$label),
                       mtry = 2,
                       ntree = 300)


######################### TESTING ###################################

# use images 2 and 3 as test data
test_data <- bind_rows(image2, image3) %>%
  filter(label != 0)

# get true labels
truth <- test_data %>%
  mutate(new_label = ifelse(label==1, 1, 0)) %>%
  dplyr::select(new_label)


# get performance object from ROCR package
getPerf <- function(model_fit, test_data, truth){
  # Input: model_fit, fitted model object
  #       test_data, dataframe containing test images
  #       truth, dataframe containing true labels
  # Output: perf object from ROCR package

  # get probabilities for each class
  probs <- predict(model_fit, test_data, "prob")
  # predict! 
  pred <- prediction(probs[,2], truth)
  # get TPR and FPR
  perf <- performance(pred, "tpr", "fpr")
  
  return(perf)
  
}


##################### PLOTTING #######################

######## plot ROC
# get TPR and FPR values for random forest and QDA
perf <- getPerf(rf_fit, test_data, truth)
rf_roc <- data.frame(fpr = perf@x.values[[1]], tpr = perf@y.values[[1]], Test = "Random forest")
perf <- getPerf(qda_fit, test_data, truth)
qda_roc <- data.frame(fpr = perf@x.values[[1]], tpr = perf@y.values[[1]], Test = "QDA")

# plot all ROC curves in the same plot
bind_rows(qda_roc, rf_roc) %>% ggplot() +
  # plot ROC
  geom_smooth(aes(x = fpr, y = tpr, color = Test), se = FALSE) + 
  # label stuff
  xlab("False positive rate") + 
  ylab("True positive rate") + 
  scale_color_discrete(name = "Model") +
  # axis limits
  xlim(0, .2) + 
  ylim(0,1) + 
  theme_classic()


######## plot random forest CV error
# CV rate for random forest (saved results) as function of number of trees
x <- data.frame(x = c(10,25,50,100,200,300,400,500), 
                y = c(0.9202968, 0.9240015, 0.9250739,0.925718,0.9259556, 0.9263573, 0.9267023, 0.9263032))


# Plot OOB error rate
ggplot(x) + 
  # plot num_tree vs. error rate
  geom_point(aes(x = x, y = 1-y)) +
  # label stuff
  xlab("Number of trees") + 
  ylab("Cross validation error rate") + 
  ylim(0.072, .085) + 
  theme_classic()
