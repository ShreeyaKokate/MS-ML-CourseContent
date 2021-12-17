### Knowledge Discovery and Data Mining (CS 513) ###
#  (Homework 2 - EDA)  #

# Course      : CS 513A
# First Name  : SHREEYA
# Last Name   : KOKATE
# CWID        : 20005256

###### ******************************************** ######

#Question 1:
#Load the dataset into R to perform the EDA 
dataset <- read.csv("/Users/shree/Downloads/KDD_HW2/breast-cancer-wisconsin.data.csv", na.string = "?")

#Summarize all the columns 
summary(dataset)

#Lets replace '?' with 'NA' for easy operations
dataset[, 1:11][dataset[, 1:11] == "?"] <- NA

#Identify and view the missing values
is.na(dataset)
View(dataset)

#Now, replace the missing values with the "mean" of the column.
for(i in 1:ncol(dataset)){
  dataset[is.na(dataset[, i]), i] <- mean(dataset[, i], na.rm = TRUE)
}

#Round off the Values to first two decimal places
dataset[,-1] <- round(dataset[, -1], 2)

#Display the frequency table of "Class" vs. F6
freq_table <- table(dataset$Class, dataset$F6)
ftable(freq_table)

#Display the scatter plot for F1 to F6
plot(dataset[2:7], main = "Scatter Plot for F1 to F6", ph = 10, col = 2)

#Show histogram box plot for columns F7 to F9
boxplot(dataset[8:10], main = "Histogram Box Plot for F7 to F9")

#Delete all the objects from the R- environment. 
rm(list = ls())


# Question 2:
#Reload the dataset from into R. 
dataset <- read.csv("/Users/shree/Downloads/KDD_HW2/breast-cancer-wisconsin.data.csv", na.string = "?")

#Remove any row with a missing value in any of the columns.
dataset[, 1:11][dataset[, 1:11] == "?"] <- NA
dataset <- na.omit(dataset)
