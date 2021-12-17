### Knowledge Discovery and Data Mining (CS 513) ###
#  Homework 7 - Artificial Neural Network  #

# Course      : CS 513A
# First Name  : SHREEYA
# Last Name   : KOKATE
# CWID        : 20005256


rm(list=ls())
install.packages('neuralnet')
library(neuralnet)

# select the downloaded wisc_bc_ContinuousVar.csv dataset from canvas
filename <- file.choose()
dataSet <- read.csv(filename, na.strings = "?") 
View(dataSet)
table(dataSet$diagnosis)

#To factor the data set
#dataSet<-data.frame(lapply(na.omit(dataSet),as.numeric))

# Split training and testing dataset
index <- sort(sample(nrow(dataSet), as.integer(.70 * nrow(dataSet))))
train_set <- dataSet[index, ]
test_set <- dataSet[-index, ]

# Artificial Neural Network model
model <- neuralnet(diagnosis~., train_set[-1], hidden = 5, threshold = 0.01)

#Plot the neural network
plot(model)

# Test
ann <- compute(model, test_set)
ann$net.result 

ann_cat <- ifelse(ann$net.result < 1.5, 1, 2)
length(ann_cat)
length(test_set$diagnosis)

# Error rate 
wrong <- (test_set$diagnosis != ann_cat)
error <- sum(wrong) / length(wrong)
error