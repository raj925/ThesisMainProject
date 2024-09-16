install.packages("caret")
library(caret)

set.seed(101)

#######################################
# Creating a random training dataset
# The multinomial model will be predicting one of three classes in the outcome variable:
# A, B, C
trainingdf <- as.data.frame(matrix(sample(0:1, 100 * 25, replace = TRUE), nrow = 100, ncol = 25))
colnames(trainingdf) <- paste0("T", 1:25) # Binary input predictors, named T1-T25
labels <- c(rep("A", 50), rep("B", 30), rep("C", 20)) # The classes are imbalanced in the training dataset
labels <- sample(labels) # Generate a random shuffle of labels
trainingdf$Label <- labels
trainingdf$Label <- as.factor(trainingdf$Label)
#######################################

#######################################
# Creating a test dataset
# This is larger than the training set, same predictors
testdf <- as.data.frame(matrix(sample(0:1, 500 * 25, replace = TRUE), nrow = 500, ncol = 25))
colnames(testdf) <- paste0("T", 1:25) 
#######################################

#######################################
# Specify the cross-validation method
# I am using leave one out cross validation and smote resampling to make up for the 
# imbalanced training data
ctrl <- trainControl(method = "LOOCV", savePredictions = "all", sampling="smote")
#######################################

#######################################
# Model 1: Multinomial model using glmnet

# Training
model1 <- caret::train(Label ~ ., method = "glmnet", family="multinomial", data = trainingdf, trControl = ctrl)

# Now apply the model to the test using the predict() function

# Predict labels for test dataset
predictions <- predict(model, newdata = testdf, type="raw")
# Also a note, I assume this is pulling the final model

predTableForModel1 <- table(predictions)
# You will see there is a decent spread of As, Bs and Cs, but the proportions
# do seem to somewhat track the proportions of each in the training set.
# ie. More As, fewer Cs.

# A   B   C 
# 186 183 131 

#######################################

########################################

# Model 2: Multinomial model using glmnet and regularisation
model2 <- caret::train(Label ~ ., method = "glmnet", family="multinomial", 
                       data = trainingdf, trControl = ctrl, 
                       tuneGrid = expand.grid(alpha = seq(0, 1, by = 0.1), 
                              lambda = seq(0, 1, by = 0.1))) # find best values for alpha and lambda

# Predict labels for test dataset
predictions <- predict(model2, newdata = testdf, type="raw")

predTableForModel2 <- table(predictions)
# You will see that one class is completely ignored!
# The model never predicts B. 

#A   B   C 
#251 0 249 

########################################
