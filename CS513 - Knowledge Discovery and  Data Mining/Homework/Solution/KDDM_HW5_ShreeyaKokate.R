### Knowledge Discovery and Data Mining (CS 513) ###
#  Homework 5 - DTree  #

# Course      : CS 513A
# First Name  : SHREEYA
# Last Name   : KOKATE
# CWID        : 20005256


rm(list=ls())

library(e1071)
library(class)
library('rpart')

#select the downloaded breast-cancer-wisconsin.csv dataset from canvas
name <-file.choose()
df = read.csv(name)

#Summarizing
summary(df)

n <- as.numeric(as.character(df$F6))
summary(n, na.rm = TRUE)
df$F6 <- n
summary(df, na.rm = TRUE)

#Remove rows with missing values 
df <- na.omit(df)

#Convert labels to factor class
df$Class<- factor(df$Class , levels = c("2","4") , labels = c("Benign","Malignant"))
is.factor(df$Class)

#Generate train and test in the ratio 70% to 30%
df<- df[2:11]
size <- floor(0.70 * nrow(df))

#Set seed 
set.seed(123)
random <- sample(seq_len(nrow(df)), size = size)

#train dataset
train_data <- df[random, ]

#test dataset
test_data <- df[-random, ]

#CART 
cart <- rpart(Class ~ ., data = train_data)

#Predict class
prediction <- predict(cart, test_data, type = "class")
print(prediction)
print(length(test_data$Class))

#Confusion Matrix
conf_matrix <- table(prediction, test_data$Class)
print(conf_matrix)

#Accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(conf_matrix)