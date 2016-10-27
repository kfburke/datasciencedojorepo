###################################################################################
## This code is part of Data Science Dojo's bootcamp
## Copyright (C) 2015~2016

## Objective: Machine learning of iris species classification with decision tree
## Data source: iris data set (included in R)
## Please install "rpart" package: install.packages("rpart")
## Please install "party" package: install.packages("party")
###################################################################################

## load the library
library(rpart)

## DATA EXPLORATION
## load the iris data in R
data(iris)
## explore the data set
str(iris)
dim(iris)
summary(iris)

## BUILD MODEL
## randomly choose 70% of the data set as training data
set.seed(777)
train.index <- sample(1:nrow(iris), 0.7*nrow(iris))
iris.train <- iris[train.index,]
dim(iris.train)
## select the 30% left as the testing data
iris.test <- iris[-train.index,]
dim(iris.test)

# Default decision tree model
    # Builds a decision tree from the iris dataset to predict
    # species given all other columns as predictors
iris.tree <- rpart(Species~.,data=iris.train)

# Reports the model
print(iris.tree)

## VISUALIZE THE MODEL
## plot the tree structure
plot(iris.tree, margin=c(.25))
title(main = "Decision Tree Model of Iris Data")
text(iris.tree, use.n = TRUE)
## print the tree structure
summary(iris.tree)

## MODEL EVALUATION
## make prediction using decision model
iris.predictions <- predict(iris.tree, iris.test, type = "class")
head(iris.predictions)

## Comparison table
iris.comparison <- iris.test
iris.comparison$Predictions <- iris.predictions
iris.comparison[ , c("Species", "Predictions")]

## View misclassified rows
disagreement.index <- iris.comparison$Species != iris.comparison$Predictions
iris.comparison[disagreement.index,]

## If instead you wanted probabilities.
# iris.predictions <- predict(iris.tree, iris.test)

## Extract the test data species to build the confusion matrix
iris.confusion <- table(iris.predictions, iris.test$Species)
print(iris.confusion)
## calculate accuracy, precision, recall, F1
iris.accuracy <- sum(diag(iris.confusion)) / sum(iris.confusion)
print(iris.accuracy)

iris.precision <- iris.confusion[2,2] / sum(iris.confusion[2,])
print(iris.precision)

iris.recall <- iris.confusion[2,2] / sum(iris.confusion[,2])
print(iris.recall)

iris.f1 <- 2 * iris.precision * iris.recall / (iris.precision + iris.recall)
print(iris.f1)


#### Parameter Tuning ####

## Setting control parameters for rpart
## Check ?rpart.control for what the parameters do
tree.params <- rpart.control(minsplit=20, minbucket=7, maxdepth=30, cp=0.01)

## Fit decision model to training set
## Use parameters from above and Gini index for splitting
iris.tree <- rpart(Species ~ ., data = iris.train, 
                       control=tree.params, parms=list(split="gini"))

### Regression Decision Tree ####
# Use method as "anova" as a parameter.
iris.tree <- rpart(Petal.Length ~ ., data = iris.train, method="anova")

## EXERCISE
## Another library called "party" can be also used to build decision trees.
## It provides nonparametric regression trees for nominal, ordinal,
## numeric, censored, and multivariate responses. Tree growth is based on statistical 
## stopping rules, so pruning should not be required. 
## party manual: http://cran.r-project.org/web/packages/party/party.pdf
## Instead of rpart(), try to use ctree() in "party" for the same data. 
## They implement a different algorithm for building the tree. 
## But for this small amount of data, do these different functions (with different algorithms) 
## actually give us different trees?