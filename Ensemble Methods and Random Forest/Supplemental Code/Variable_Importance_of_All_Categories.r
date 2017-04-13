setwd("C:/repos/bootcamp/Datasets")

titanic <- read.csv(
              file = "titanic.csv",
              stringsAsFactors = FALSE
              )

titanic$Survived <- as.factor(titanic$Survived)

# Discretizing categories

# Pclass discretizing
titanic$pclass_one <- 0
titanic$pclass_two <- 0
titanic$pclass_three <- 0
titanic[titanic$Pclass==1,"pclass_one"] <- 1
titanic[titanic$Pclass==2,"pclass_two"] <- 1
titanic[titanic$Pclass==3,"pclass_three"] <- 1

titanic$embarked_q <- 0
titanic$embarked_s <- 0
titanic$embarked_c <- 0
titanic[titanic$Embarked=="Q","embarked_q"] <- 1
titanic[titanic$Embarked=="S","embarked_s"] <- 1
titanic[titanic$Embarked=="C","embarked_c"] <- 1

titanic$sex_m <- 0
titanic$sex_f <- 0
titanic[titanic$Sex=="male","sex_m"] <- 1
titanic[titanic$Sex=="female","sex_f"] <- 1

titanic[is.na(titanic$Age),"Age"] <- 28

library(randomForest)
features <- c("Survived","Age", "SibSp", "Parch", "Fare", "pclass_one","pclass_two","pclass_three","embarked_q","embarked_s","embarked_c","sex_m","sex_f")
titanic.forest <- randomForest(Survived~., data = titanic[,features], importance=TRUE)
varImpPlot(titanic.forest)
