### Knowledge Discovery and Data Mining (CS 513) ###
#  Homework 6 - C50 & Random Forest  #
# Course      : CS 513A
# First Name  : SHREEYA
# Last Name   : KOKATE
# CWID        : 20005256
# PART 6.1 : Use the C5.0 methodology to develop a classification model for the Diagnosis.

rm(list=ls())

install.packages('C50')
library(C50)

# Select the downloaded breast-cancer-wisconsin.csv dataset from canvas
name <-file.choose()
df = read.csv(name)
table(df$Class)

# Factor the data set
df$Class <- factor(df$Class, levels = c(2, 4),labels = c("Benign", "Malignant"))

# Split training and testing dataset
index <- sort(sample(nrow(df), as.integer(.70 * nrow(df))))
train_set1 <- df[index, ]
test_set1 <- df[-index, ]

dev.off()

# C 5.0 model
model1 <- C5.0(Class~., train_set1[ , -1])
summary(model1)
plot(model1)

# Prediction using testing dataset
prediction1 <- predict(model1, test_set1[ , -1], type = "class")

# Confusion matrix
conf_matrix1 <- table(test_set1[ , 11], prediction1)
conf_matrix1
str(prediction)

# Error rate 
wrong <- sum(test_set1[ , 11] != prediction1)
error <- wrong / length(test_set1[ , 11])
error

# Accuracy
accuracy1 <- sum(diag(conf_matrix1) / nrow(test_set1)) * 100
accuracy1



# PART 6.2 : Use the Random Forest methodology to develop a classification model for the Diagnosis and identify important features.

install.packages('randomForest')
library(randomForest)

# We will use the above dataset df 
# Remove the unwanted columns
dataset <- df[ , -1]
df[df == "?"] <- NA
dataset[dataset == 'NA'] <- 0

# Factor the data set
sapply(dataset, class)
new_data <- transform(dataset, F1 = as.factor(F1), F2 = as.factor(F2), F3 = as.factor(F3), F4 = as.factor(F4), F5 = as.factor(F5), F6 = as.factor(F6), F7 = as.factor(F7), F8 = as.factor(F8), F9 = as.factor(F9), Class = as.factor(Class))

# Convert 2,4 to Benign and Malignant
new_data$Class <- factor(new_data$Class, levels = c("2", "4") , labels = c("Benign", "Malignant"))

# Split training and testing dataset
set.seed(125)
index <- sort(sample(nrow(new_data), round(0.30 * nrow(new_data))))
train_set2 <- new_data[-index, ]
test_set2 <- new_data[index, ]

# Random Forest model
model2 <- randomForest(Class~., data = train_set2)
summary(model2)
plot(model2)

# Prediction using testing dataset
prediction2 <- predict( model2, test_set2, type = "class" )
prediction2

# Confusion Matrix
conf_matrix2 <- table(actual = test_set2[, 10], Random_Forest = prediction2)
conf_matrix2

# Accuracy
accuracy <- sum(diag(conf_matrix2) / nrow(test_set2)) * 100
accuracy
