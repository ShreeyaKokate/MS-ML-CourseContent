### Knowledge Discovery and Data Mining (CS 513) ###
#  (Homework 3 - K Nearest Neighbors(KNN))  #

# Course      : CS 513A
# First Name  : SHREEYA
# Last Name   : KOKATE
# CWID        : 20005256

###### ******************************************** ######


rm(list = ls())

#select the breast-cancer-wisconsin.csv dataset from canvas
name <-file.choose()
df = read.csv(name)

n <- as.numeric(as.character(df$F6))
df$F6 <- n

#Remove the rows with missing values 
df <- na.omit(df)

#Convert labels to factor 
df$Class<- factor(df$Class , levels = c("2", "4") , labels = c("Benign", "Malignant"))

#KNN
#Generate training and testing data in the ratio 70% : 30%
size <- sample(1:nrow(df), 0.7 * nrow(df)) 
norm <- function(x) { (x - min(x)) / (max(x) - min(x)) }

#normalization
norm <- as.data.frame(lapply(df[,c(2,3,4,5,6,7,8,9,10)], norm))
df = df['Class']

#training set
train <- norm[size,] 
x_train <- df['Class'][size,] 

#testing set
test <- norm[-size,] 
x_test <- df['Class'][-size,]

#load the package class
library(class)

#knn function for k = 3
result1 <- knn(train=train, test=test, cl=x_train, k=3)

#confusion matrix
conf_matrix1 <- table(result1, x_test)
print(conf_matrix1)

#accuracy
accuracy1 <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy1(conf_matrix1)

#knn function for k = 5
result2 <- knn(train=train, test=test, cl=x_train,k=5)

#create confusion matrix
conf_matrix2 <- table(result2, x_test)
print(conf_matrix2)

#accuracy
accuracy2 <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy2(conf_matrix2)

#knn function for k = 10
result3 <- knn(train=train, test=test, cl=x_train,k=5)

#confusion matrix
conf_matrix3 <- table(result3, x_test)
print(conf_matrix3)

#accuracy
accuracy3 <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy3(conf_matrix3)

