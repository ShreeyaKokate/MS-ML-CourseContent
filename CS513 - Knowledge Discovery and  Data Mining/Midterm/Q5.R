### Knowledge Discovery and Data Mining (CS 513) ###
#  MIDTERM EXAM - Q5  #

# Course      : CS 513A
# First Name  : SHREEYA
# Last Name   : KOKATE
# CWID        : 20005256

rm(list = ls())

library(e1071)
library(class)

#select the COVID19_v4.csv dataset from canvas
name <-file.choose()
dataset = read.csv(name)

#Summarize all the columns 
summary(dataset)

#Lets replace '?' with 'NA' for easy operations
dataset[, 1:7][dataset[, 1:7] == "?"] <- NA

#Identify and view the missing values
is.na(dataset)
View(dataset)

#Remove the rows having missing values 
data <- na.omit(dataset)

#Discretize values
data$Age<-cut(data$Age , c(0,35,50,60))
data$Age<- factor(data$Age, levels = c("(0,35]","(35,50]","(50,60]") , labels = c("Less 35","35 to 50","More than 50"))

data$MonthAtHospital<-cut(data$MonthAtHospital, c(0,6,32))
data$MonthAtHospital<- factor(data$MonthAtHospital, levels = c("(0,6]","(6,32]") , labels = c("Less than 6 Months","6 months or more"))

samplef <- sort(sample(nrow(data), round(0.30*nrow(data))))
training_data <- data[-samplef,]
testing_data <- data[samplef,]

# Train the model with Naive Bayes Algorithm 
model<-naiveBayes(Infected~., data = training_data)

# Predict using the testing data
prediction <-predict(model, testing_data)

#confusion matrix
conf_matrix <- table(predict_nb = prediction,Class = testing_data$Infected)
print(conf_matrix)

# Calculate Error Rate
NB_wrong <- sum(prediction != testing_data$Infected)
NB_errorrate <- NB_wrong/length(prediction)

accuracy <- 1 - NB_errorrate
accuracy
