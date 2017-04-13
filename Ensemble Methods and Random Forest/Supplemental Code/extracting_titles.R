setwd("C:/repos/bootcamp/Datasets")

titanic <- read.csv(
  file = "titanic.csv",
  stringsAsFactors = FALSE
)

library(stringr)
# Splits name into last, title, and name
name_list <- str_split(titanic$Name, pattern="\\. |, ")
names_df <- data.frame(Reduce(rbind, name_list), stringsAsFactors = FALSE)
names(names_df) <- c("Last", "Title", "First")

# Creates a new data frame with survive
# with titles
Survived <- titanic$Survived
Title <- names_df$Title
titanic <- data.frame(
          Survived,
          Title,
          stringsAsFactors = FALSE)

titanic$Mr <- 0
titanic$Mrs <- 0
titanic$Miss <- 0
titanic$Master <- 0
titanic$Don <- 0
titanic$Rev <- 0
titanic$Dr <- 0
titanic$Mme <- 0
titanic$Ms <- 0
titanic$Major <- 0
titanic$Lady <- 0
titanic$Sir <- 0
titanic$Mlle <- 0
titanic$Col <- 0
titanic$Capt <- 0
titanic$the_Countess <- 0
titanic$Jonkheer <- 0

titanic[titanic$Title=="Mr","Mr"] <- 1
titanic[titanic$Title=="Mrs","Mrs"] <- 1
titanic[titanic$Title=="Miss","Miss"] <- 1
titanic[titanic$Title=="Master","Master"] <- 1
titanic[titanic$Title=="Don","Don"] <- 1
titanic[titanic$Title=="Rev","Rev"] <- 1
titanic[titanic$Title=="Dr","Dr"] <- 1
titanic[titanic$Title=="Mme","Mme"] <- 1
titanic[titanic$Title=="Ms","Ms"] <- 1
titanic[titanic$Title=="Major","Major"] <- 1
titanic[titanic$Title=="Lady","Lady"] <- 1
titanic[titanic$Title=="Sir","Sir"] <- 1
titanic[titanic$Title=="Mlle","Mlle"] <- 1
titanic[titanic$Title=="Col","Col"] <- 1
titanic[titanic$Title=="Capt","Capt"] <- 1
titanic[titanic$Title=="the Countess","the_Countess"] <- 1
titanic[titanic$Title=="Jonkheer","Jonkheer"] <- 1

features <- c(
  "Survived",
  "Mr",
  "Mrs",
  "Miss",
  "Master",
  "Don",
  "Rev",
  "Dr",
  "Mme",
  "Ms",
  "Major",
  "Lady",
  "Sir",
  "Mlle",
  "Col",
  "Capt",
  "the_Countess",
  "Jonkheer"
)

titanic$Survived <- as.factor(titanic$Survived)

library(randomForest)
title.forest <- randomForest(
                  Survived~.,
                  titanic[,features],
                  importance=TRUE)

varImpPlot(title.forest)
