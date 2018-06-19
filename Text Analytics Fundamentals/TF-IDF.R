###################################################################################
## This code is part of Data Science Dojo's bootcamp
## Copyright (c) 2015-2018

## Objective: Perform Text Analysis preprocessing tasks and evaluate document
##            similarity via TF-IDF.
## Packages: tm, lsa
###################################################################################

# Load the library
library(tm)
library(lsa)


# Load the test dataset
data(crude)


# Look at the help file of the dataset
?crude


# Display the raw text of the first document in the corpus and display.
writeLines(as.character(crude[[1]]))


# Transform document words to lower case
crude <- tm_map(crude, content_transformer(tolower))
writeLines(as.character(crude[[1]]))


# Remove punctuation from documents
crude <- tm_map(crude, removePunctuation)
writeLines(as.character(crude[[1]]))


# Remove stopwords from the corpus
crude <- tm_map(crude, removeWords, stopwords("english"))
writeLines(as.character(crude[[1]]))


# Remove numbers from the corpus
crude <- tm_map(crude, removeNumbers)
writeLines(as.character(crude[[1]]))


# Stem the corpus
crude <- tm_map(crude, stemDocument, language = "english")
writeLines(as.character(crude[[1]]))


# Build a document-term matrix using TF-IDF and inspect
crude.dt <- DocumentTermMatrix(crude, control=list(weighting=weightTfIdf))
crude.dt
inspect(crude.dt[1:8, 1:8])


# Compute a matrix of cosine similarity scores between each document pair 
# and inspect
crude.cos <- cosine(as.matrix(t(crude.dt)))
crude.cos[1:8, 1:8]
