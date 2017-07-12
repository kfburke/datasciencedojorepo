#=======================================================================
# This code is part of Data Science Dojo's Bootcamp
# Copyright (C) 2017
#
# Objective: Code sample illustrating the use of the skills we have 
#            learned so far to build the best binary classification 
#            model we can for the adult census income dataset under 
#            the constraint that we only have 20 minutes.
#
# NOTE - This script was actually built and executed in 20 minutes!
#        Comments were added after due to time constraints. Also, this
#        script was created quickly via "cut-and-paste reuse" and 
#        illustrates the art of the possible!
#
#=======================================================================


# Load data and inspect.
# NOTE - Set working directory to bootcamp root folder!
adult.train <- read.csv("Datasets/AdultCensusIncome.csv")
str(adult.train)


# Leverage the mighty random forest to explore the data!
#install.packages("randomForest")
library(randomForest)


# Use seed so everyone sees the same thing.
set.seed(453)


# We are predicting income from all other predictors.
rf.1 <- randomForest(income ~ ., data = adult.train, 
                     importance = TRUE)
rf.1


# What features does the RF think are important?
varImpPlot(rf.1)


# Start looking at each of the important features - looking for
# clean separation of the two income classes.
#install.packages("ggplot2")
library(ggplot2)


# The capital.gain feature was the top-ranked feature by the RF.
# The density plot indicates very clean separation of the income labels.
ggplot(adult.train, aes(x = capital.gain, fill = income)) +
  theme_bw() +
  geom_density(alpha = 0.5)


# Next up, the relationship feature.
# For many levels of the relationship variable, there is very clean separation.
ggplot(adult.train, aes(x = relationship, fill = income)) +
  theme_bw() +
  geom_bar()


# And the age feature shows some nice "bump-outs".
ggplot(adult.train, aes(x = age, fill = income)) +
  theme_bw() +
  geom_density(alpha = 0.5)


# The fnlwgt doesn't seem to be as powerful as age in terms of
# "bump-outs"!
ggplot(adult.train, aes(x = fnlwgt, fill = income)) +
  theme_bw() +
  geom_density(alpha = 0.5)


# Many levels of marital.status show very strong separation of
# income levels.
ggplot(adult.train, aes(x = marital.status, fill = income)) +
  theme_bw() +
  geom_bar()


# Many levels of occupation show very strong separation of
# income levels.
ggplot(adult.train, aes(x = occupation, fill = income)) +
  theme_bw() +
  geom_bar()


# Set up caret to perform 10-fold cross validation repeated 3 times.
# Do this for better estimates of generalization error.
#install.packages("caret")
library(caret)
caret.control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3)



# Since simple models are more likely to generalize, use just the first 5
# good features we analyzed to build the model.
features <- c("capital.gain", "relationship", "age", "marital.status", "occupation", "income")
adult.train.1 <- adult.train[, features]


# Perform 10-fold CV with rpart trees and explore the effectiveness of various
# values for cp (i.e., the "tuneLength" parameter below). Using rpart here so
# that we can train fast when compared to doing CV with Random Forests. Use a 
# random seed to ensure we all see the same thing.
set.seed(43987)
rpart.cv.1 <- train(income ~ ., 
                    data = adult.train.1,
                    method = "rpart",
                    trControl = caret.control,
                    tuneLength = 7)
# Other algorithms to be used as "methods" in caret.
# https://topepo.github.io/caret/available-models.html

# Display the results of the cross validation run - 84.39% mean accuracy! 
rpart.cv.1


# What is the standard deviation? 0.4837%! Nice!
cat(paste("\nCross validation standard deviation:",  
          sd(rpart.cv.1$resample$Accuracy), "\n", sep = " "))


# Take a look at the model
#install.packages("rpart.plot")
library(rpart.plot)
prp(rpart.cv.1$finalModel, type = 0, extra = 1, under = TRUE)


#=======================================================================
#
# TODO - Evaluate remaining features visually for clean separation and
#        explore adding them to the model to see if it improves.
#
#=======================================================================