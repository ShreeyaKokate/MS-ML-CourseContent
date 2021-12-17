### Knowledge Discovery and Data Mining (CS 513) ###
#  Homework 9 - SVM  #

# Course      : CS 513A
# First Name  : SHREEYA
# Last Name   : KOKATE
# CWID        : 20005256


rm(list=ls())


# Importing the SVM library
library(e1071)

# select the downloaded wisc_bc_ContinuousVar.csv dataset from canvas
file_name <- file.choose()
cancer_data <- read.csv(file_name)

#Factoring the data
#summary(cancer_data)
table(cancer_data$diagnosis)
data<-na.omit(cancer_data)

data$diagnosis <- factor(data$diagnosis)

#Splitting the data in 70% train and 30% test 
idx<-sort(sample(nrow(data),as.integer(.70*nrow(data))))
training<-data[idx,]
test<-data[-idx,]

library(e1071)
#Using SVM for prediction
svm_model <- svm( diagnosis~ ., data =training  )
svm_pred <- predict(svm_model,  test )

#Creating Confusion Matrix
conf_matrix = table(actual=test$diagnosis,svm_pred )
conf_matrix

#Checking Error Rate
SVM_wrong<- (test$diagnosis!=svm_pred)
errorRate<-sum(SVM_wrong)/length(SVM_wrong)
errorRate 

#Checking accuracy of the model
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(conf_matrix)

