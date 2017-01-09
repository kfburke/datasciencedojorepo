######################################################
## This code is part of Data Science Dojo's bootcamp #
## Copyright (C) 2016                                #
######################################################

##################################
# Global Input and output fields #
##################################

# My working directry and my file names
working.directory <- "C:/Users/phuc/Downloads/titanic"
train.filename <- "titanic.csv"
test.filename  <- "test.csv"

###############
# Import Data #
###############

# Reading in the train and test set, set
#    the working directory for all future
#    inputs and outputs
setwd(working.directory)
titanic.train <- read.csv(
    file = train.filename,
    stringsAsFactors = FALSE,
    header = TRUE
  )
titanic.test <- read.csv(
  file = test.filename,
  stringsAsFactors = FALSE,
  header = TRUE
)

########################
# Combine two datasets #
########################

# Mark rows as either train or test
titanic.train$IsTrainSet <- TRUE
titanic.test$IsTrainSet  <- FALSE

# In order to combine the two datasets together,
#    both test and train sets need to have the
#    same columns.
# Give the test set a survived column
titanic.test$Survived <- NA

# Combine the two datasets into one
titanic.full <- rbind(titanic.train, titanic.test)

########################
# Clean Missing Values #
########################

## Missing Values of  Embarked ##
table(titanic.full$Embarked)
#     C   Q   S 
# 2 270 123 914
# Two blank strings
table(titanic.full$Embarked=='')
# FALSE  TRUE 
# 1307     2

# Fill in the missing values of embarked with
#   the mode.
titanic.full[titanic.full$Embarked=='', "Embarked"] <- 'S'

## Missing Values of  Age ##

# Fills age with the global median (28)
age.median <- median(titanic.full$Age, na.rm = TRUE)
titanic.full[is.na(titanic.full$Age),"Age"] <- age.median

#######################
# Categorical Casting #
#######################
titanic.full$Pclass <- as.factor(titanic.full$Pclass)
titanic.full$Embarked <- as.factor(titanic.full$Embarked)
titanic.full$Sex <- as.factor(titanic.full$Sex)

###########################
# Linear model to predict #
# missing values of Fare  #
###########################

# Finds outlier threshold in fare
quantiles <- boxplot.stats(titanic.full$Fare)$stats
# Anything greater than the upper bound 
#    whisker is an outlier
upper.whisker <- quantiles[5]
outlier.filter <- titanic.full$Fare < upper.whisker

# Build a linear model to predict fare,
#    where outliers of fare were filtered out
fare.equation <- "Fare ~ Pclass + Sex + Age + SibSp + Parch + Embarked"
fare.model <- lm(
  formula = fare.equation,
  data = titanic.full[outlier.filter,]
)

# Find the missing row of fare
fare.row <- titanic.full[
  is.na(titanic.full$Fare),
  c("Pclass", "Sex", "Age", "SibSp", "Parch", "Embarked")
]
fare.predictions  <- predict(
                          fare.model,
                          newdata = fare.row
                        )
titanic.full[is.na(titanic.full$Fare),"Fare"] <- fare.predictions

##############################
# Separate datasets back out #
##############################

titanic.train <- titanic.full[titanic.full$IsTrainSet==TRUE,]
titanic.test <- titanic.full[titanic.full$IsTrainSet==FALSE,]

####################
# Predictive Model #
####################

# Classification task
titanic.train$Survived <- as.factor(titanic.train$Survived)

# Setup predictors and response class
survive.equation<- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
survive.formula <- as.formula(survive.equation)

require(randomForest)
survive.model <- randomForest(
  formula = survive.formula,
  data = titanic.train,
  ntree = 500,
  mtry = 3,
  nodesize = 0.01 * nrow(titanic.test),
  importance = TRUE
)

###############
# Predictions #
###############
features = c("Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked")
Survived <- predict(
  survive.model,
  newdata = titanic.test[,features]
)

##############################
# Binding outputs for Kaggle #
##############################
PassengerId <- titanic.test$PassengerId
output <- as.data.frame(PassengerId)
output$Survived <- Survived
write.csv(
  output,
  file= "kaggle_submission.csv",
  row.names = FALSE
)
