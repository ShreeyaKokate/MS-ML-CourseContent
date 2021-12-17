### Knowledge Discovery and Data Mining (CS 513) ###
#  MIDTERM EXAM - Q6  #

# Course      : CS 513A
# First Name  : SHREEYA
# Last Name   : KOKATE
# CWID        : 20005256

rm(list = ls())

library(e1071)
library(class)
library(rpart)


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

#Train the model on the rpart library
cart_class <- rpart(Infected~., data = training_data)

# Plot the trained class
rpart.plot(cart_class)

# Predict using the trained model based on class.
cart_predict2 <- predict(cart_class, testing_data, type = 'class')

# Print the confusion matrix
conf_matrix <- table(Actual = testing_data[, 7], CART = cart_predict2)
print(conf_matrix)

# Predict using the trained model based on probabilities.
cart_predict <- predict(cart_class, testing_data)

#cart_predict
str(cart_predict)
cart_predict_cat <- ifelse(cart_predict[, 1] <= 0.5,'Yes','No')
table(Actual = testing_data[, 7], CART = cart_predict_cat)

#Calculate the error rate
cart_wrong <- sum(testing_data[, 7] != cart_predict_cat)
cart_error_rate <- cart_wrong / length(testing_data[, 7])

cart_predict2 <- predict(cart_class, testing_data, type = "class")
cart_wrong2 <- sum(testing_data[, 7] != cart_predict2)
cart_error_rate2 <- cart_wrong2 / length(testing_data[, 7])
 
#Plotting the decision tree
library(rpart.plot)
prp(cart_class)
