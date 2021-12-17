### Knowledge Discovery and Data Mining (CS 513) ###
#  Homework 4 - Naive Bayes  #

# Course      : CS 513A
# First Name  : SHREEYA
# Last Name   : KOKATE
# CWID        : 20005256


rm(list=ls())

library(e1071)
library(class)

#select the downloaded breast-cancer-wisconsin.csv dataset from canvas
name <-file.choose()
df = read.csv(name)

n <- as.numeric(as.character(df$F6))
df$F6 <- n

#Remove the rows having missing values 
df <- na.omit(df)

df$Class<- factor(df$Class , levels = c("2", "4") , labels = c("Benign", "Malignant"))

new<- df[2:11]
samplef <- floor(0.70 * nrow(new))

set.seed(123)
x_train <- sample(seq_len(nrow(newData)), size = samplef)

#train dataset
training_data <- newData[x_train, ]

#test dataset
testing_data <- newData[-x_train, ]

#generate model
model <- naiveBayes(Class ~ ., data = training_data)

#Validation
prediction <- predict(model, testing_data)

#metrics
conf_matrix <- table(predict_nb = prediction, class = testing_data$Class)
print(conf_matrix)

#Output of Naive Bayes Classifier
accuracy <- function(x){sum(diag(x) / (sum(rowSums(x)))) * 100}
accuracy(conf_matrix)