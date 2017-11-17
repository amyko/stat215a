
###load packages
library(tidyverse)
library(ggplot2)
library(lubridate)
library(stringr)
library(randomForest)
library(fastAdaboost)
library(pROC)
library(distrEx)
library(gridExtra)

###read in data and add names to the columns
image1 <- read.table("/Desktop/lab4/image_data/image1.txt")
image2 <- read.table("/Desktop/lab4/image_data/image2.txt")
image3 <- read.table("/Desktop/lab4/image_data/image3.txt")

image1.df <- as.data.frame(image1)
image2.df <- as.data.frame(image2)
image3.df <- as.data.frame(image3)

colnames(image1.df) <- c("Y", "X", "Label", "NDAI", "SD", "CORR", 
                         "DF", "CF", "BF", "AF", "AN")
colnames(image2.df) <- c("Y", "X", "Label", "NDAI", "SD", "CORR", 
                         "DF", "CF", "BF", "AF", "AN")
colnames(image3.df) <- c("Y", "X", "Label", "NDAI", "SD", "CORR",
                         "DF", "CF", "BF", "AF", "AN")

####function to compute the Least Directional Standard Deviation (LDSD) feature
####input: an image (as dataframe)
####output: a vector with LDSD values
compute.LDSD <- function(image.df){
  
  index.mat <- matrix(-2, 400, 400) ###matrix to record the indices of each pixel
  LDSD.mat <- matrix(0, 400, 400)   ###matrix of LDSD values
  an.mat <- matrix(0, 400, 400)     ###matrix of AN values
  
  
  for(i in 1:nrow(image.df)){
    
    x = image.df$X[i]
    y = image.df$Y[i]
    index.mat[x, y] <- i           ###use x and y coordinates to index pixels
    an.mat[x, y] = image.df$AN[i]
    
  }
  
  for(i in 10:390){         ###main loop for computation of LDSD, 
                            ###see report for definition
    for(j in 1:400){
      if(an.mat[i, j] > 0){
        if(j>=10 && j<=390){
          LDSD.mat[i, j] <- sqrt(min(var(an.mat[i, ((j-9):j)]),   ###SD_N
                                       var(an.mat[i, (j:(j+9))]), ###SD_S
                                       var(an.mat[((i-9):i), j]),  ###SD_W
                                       var(an.mat[(i:(i+9)), j])))  ###SD_E
        }
        if(j <= 9){
          LDSD.mat[i, j] <- sqrt(min(var(an.mat[i, (j:(j+9))]), 
                                       var(an.mat[((i-9):i), j]), 
                                       var(an.mat[(i:(i+9)), j])))        
        }
        if(j >= 391){
          LDSD.mat[i, j] <- sqrt(min(var(an.mat[i, ((j-9):j)]), 
                                       var(an.mat[((i-9):i), j]), 
                                       var(an.mat[(i:(i+9)), j])))
        }      
        
      }
    }
  }
  
  
  
  LDSD.vec <- rep(0, nrow(image.df))
  for(i in 1:400){
    for(j in 1:400){
      if(index.mat[i, j] > 0){
        LDSD.vec[index.mat[i, j]] <- LDSD.mat[i, j]
        ###using the recorded original index, vectorize the matrix containing LDSD values
      }
    }
  }
  
  return(LDSD.vec)
  
}

###compute LDSD for each image
image3.df <- image3.df%>%
  mutate(LDSD = compute.LDSD(image3.df))

image2.df <- image2.df%>%
  mutate(LDSD = compute.LDSD(image2.df))

image1.df <- image1.df%>%
  mutate(LDSD = compute.LDSD(image1.df))


####function to compute accuracy of the prediction algorithm (thresholding LDSD)
###input: an image (as dataframe) and a threshold
###output: prediction accuracy
LDSD.accuracy <- function(image.df, thres){
  
  TP <- 0  ###true positives, and so on
  FP <- 0 
  FN <- 0
  TN <- 0
  
  for(i in 1:nrow(image.df)){
    
    expert.label = image.df$Label[i]
    pred.label = -1
    if(image.df$LDSD[i] > thres){
      pred.label = 1
    }
    if(expert.label == 1 && pred.label == 1){
      TP <- TP + 1
    }
    if(expert.label == 1 && pred.label == -1){
      FP <- FP + 1
    }
    if(expert.label == -1 && pred.label == 1){
      FN <- FN + 1
    }
    if(expert.label == -1 && pred.label == -1){
      TN <- TN + 1
    }
    
  }
  
  (TP + TN) / (TP + FP + TN + FN)
}

LDSD.accuracy(image1.df, 1.5) ##try different threshold for image 1, 
                              ##we search for values smaller than 2.

LDSD.accuracy(image2.df, 1.5) ##92.9
LDSD.accuracy(image3.df, 1.5) ##86.2


###median filter to make the final prediction smooth
index.mat <- matrix(-2, 400, 400)
label.mat <- matrix(1, 400, 400)

###record the indices as before
for(i in 1:nrow(image3.df)){
  
  x = image3.df$X[i]
  y = image3.df$Y[i]
  index.mat[x, y] <- i
  label.mat[x, y] = 2*(image3.df$LDSD[i]>1.5) - 1
  
}

smooth.predict <- rep(0, nrow(image3.df))

for(i in 5:396){
  for(j in 5:396){
    
    if(index.mat[i, j] != -2){  
      smooth.predict[index.mat[i, j]] = 
        median(label.mat[c((i-4):(i+4)), c((j-4):(j+4))]) 
        ###replace with the median of labels in a 5*5 rectangle centered by the given pixel
    }
  }
}

image3.df <- image3.df%>%
  mutate(LDSD.pred = 2*(LDSD>1.5) - 1)

image3.df <- image3.df%>%
  mutate(LDSD.pred.smooth = smooth.predict)


###compute prediction accuracy for smoothed prediction
TP <- 0
FP <- 0 
FN <- 0
TN <- 0

for(i in 1:nrow(image3.df)){
  
  expert.label = image3.df$Label[i]
  prob = image3.df$LDV.pred.smooth[i]
  if(expert.label == 1 && prob == 1){
    TP <- TP + 1
  }
  if(expert.label == 1 && prob == -1){
    FP <- FP + 1
  }
  if(expert.label == -1 && prob == 1){
    FN <- TN + 1
  }
  if(expert.label == -1 && prob == -1){
    TN <- FN + 1
  }
  
}

(TP + TN) / (TP + FP + TN + FN)
###87.2

###ROC curve with package pROC
image3.labeled <- image3.df%>%
  filter(Label != 0)
image3roc <- roc(image3.labeled$Label, image3.labeled$LDSD)
TPR <- image3roc$sensitivities
FPR <- 1 - image3roc$specificities


####additional code for making the plots in the report
ggplot(image3.df)+
  geom_point(aes(x = X, y = Y, color = factor(Label)))+
  scale_color_discrete(name = "Expert Label")

ggplot(image3.df)+
  geom_point(aes(x = X, y = Y, color = factor(LDSD.pred)))+
  scale_color_discrete(name = "Predicted Label")

ggplot(image3.df)+
  geom_point(aes(x = X, y = Y, color = factor(LDSD.pred.smooth)))+
  scale_color_discrete(name = "Predicted Label")

ggplot(image2.df)+
  geom_point(aes(x = X, y = Y, color = AN))+
  scale_color_continuous(name = "AN")

ggplot(image2.df)+
  geom_point(aes(x = X, y = Y, color = LDSD))+
  scale_color_continuous(name = "LDSD")

ggplot(image2.df)+
  geom_point(aes(x = X, y = Y, color = factor(Label)))+
  scale_color_discrete(name = "Expert Label")


ggplot(image3.df) + 
  geom_density(aes_string(x = "LDSD", group = "factor(Label)", fill = "factor(Label)"), 
               alpha = 0.5) +
  scale_fill_discrete(name = "Expert label") +
  theme_classic()

image2roc <- roc(image2.labeled$Label, image2.labeled$LDV)
TPR <- image2roc$sensitivities
FPR <- 1 - image2roc$specificities
plot(x= FPR, y = TPR, type = 'l', xlab = "False Positive Rate")

pdf("image2ROC.pdf", width = 5, height = 5)
plot(x= FPR, y = TPR, type = 'l', 
     xlab = "False Positive Rate", ylab = "True Positive Rate")
abline(0, 1, lty = 2)
dev.off()


pdf("image3ROC.pdf", width = 5, height = 5)
plot(x= FPR, y = TPR, type = 'l', 
     xlab = "False Positive Rate", ylab = "True Positive Rate")
abline(0, 1, lty = 2)
dev.off()


