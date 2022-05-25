#Loading the data
proj4_tr <- read.csv("C:/Users/alexa/Downloads/proj3_tr")
proj4_te <- read.csv("C:/Users/alexa/Downloads/proj3_te")
#Loading packages needed for analysis 
library(caret)
library(plyr)
library(rpart)
library(rattle)

#Setting seed for reproducibility 
set.seed(12345)

#Deleting an index variable so it doesn't cause problems later
proj4_tr$X <- NULL
proj4_te$X <- NULL

#Setting up the training method for the model. It will use a 10 fold cross validation
TC <- trainControl(method = "CV",
                   number = 10)

#Training the model to predict income based on the other 4 variables in the data. It will be decision tree
fit <- train(income ~ . ,
             data = proj4_tr,
             method = "rpart2",
             trControl = TC)

#Plotting the decision tree
fancyRpartPlot(fit$finalModel)

#Checking for overfitting among the cross validation folds
fit$resample

#Reordering the folds to find the highest and lowest values among them
fold_values <- fit$resample
order(fold_values$Accuracy)

#Calculating the variance between the highest and lowest folds. We want this value below 4
abs((.8089 - .8403) * 100)

#Using the trained model to make predictions on the set aside test data
testsetpreds <- predict(fit, proj4_te)

#Making the predictions into a table which will include the predicted category out model chose and it's actual category
table(proj4_te$income, testsetpreds)

thing <- table(proj4_te$income, testsetpreds)
#Calculating metrics for the model
cartacc <- (13357 / 16280)
carterr <- 1 - cartacc
cartspec <- 10630 / 12360
cartsens <- 2727 / 3920

#Calculating evaluation metrics for the positive baseline
positiveaccuracy <- 3921 / 16281
positiveerror <- 1 - posacc

#Evaluation methods for the negative baseline
negativeaccuracy <- 12360 / 16281
negativeerror <- 1 - negacc

#Calculating the relative decrease in error rate from the chosen baseline to the trained model
24.10 - 17.95
6.15 / 24.10

#Getting counts for income classes 
summary(as.factor(proj4_tr$income))
summary(as.factor(proj4_te$income))

#Calculating the proportion of high income customers across all data 
(3921 + 3920) / (16280 + 16281)

3921 + 3920

16280 + 16281

#Writing a function to calculate model metrics 
metrics <- function(t,beta) {
  GT <- t[1,1] + t[1,2] + t[2,1] + t[2,2]
  TAP <- t[2,2] + t[1,2]
  TAN <- t[1,1] + t[1,2]
  TPP <- t[2,1] + t[2,2] 
  Accuracy <- (t[1,1] + t[2,2]) / GT
  Error_rate <- (t[1,2] + t[2,1]) / GT
  Sensitivity <- t[2,2] / TAP
  Specificity <- t[1,1] / TAN
  Precision <- t[2,2] / TPP
  Fbeta <- (1 + beta^2) * ((Precision * Sensitivity) / ((beta^2 * Precision) + Sensitivity))
  print("Accuracy")
  print(Accuracy)
  print("Error_rate")
  print(Error_rate)
  print("Sensitivity") 
  print(Sensitivity)
  print("Precision")
  print(Precision)
  print("Fbeta")
  print(Fbeta)
}

