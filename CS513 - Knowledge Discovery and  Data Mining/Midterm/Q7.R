### Knowledge Discovery and Data Mining (CS 513) ###
#  MIDTERM EXAM - Q7  #

# Course      : CS 513A
# First Name  : SHREEYA
# Last Name   : KOKATE
# CWID        : 20005256

rm(list = ls())

#select the COVID19_v4.csv dataset from canvas
name <-file.choose()
dataset = read.csv(name)

clean_data<-na.omit(dataset)

set.seed(123)

#split the dataset into training and testing
samplef <- sort(sample(nrow(data), round(0.30*nrow(data))))
training_data <- data[-samplef,]
testing_data <- data[samplef,]

#install the KNN package
#install.packages("kknn") 
library(kknn)

predict_k5 <- kknn(formula = Infected~., training_data, testing_data, k=5, kernel = "rectangular")
fit <- fitted(predict_k5)
table(Actual = testing_data$Infected, Fitted=fit)

#confusion matrix
conf_matrix<-table(Actual = testing_data$Infected, Fitted = fit)
conf_matrix

#accuracy
accuracy <- sum(diag(conf_mat1) / nrow(testing_data)) * 100 
accuracy 
