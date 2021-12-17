### Knowledge Discovery and Data Mining (CS 513) ###
#  Homework 8 - HCluster and KMeans  #

# Course      : CS 513A
# First Name  : SHREEYA
# Last Name   : KOKATE
# CWID        : 20005256


rm(list=ls())

# select the downloaded wisc_bc_ContinuousVar.csv dataset from canvas
filename <- file.choose()
dataSet <- read.csv(filename, na.strings = "?") 
View(dataSet)
table(dataSet$diagnosis)

#To factor the data set
dataset<-na.omit(dataSet)
dataset<-dataset[-1]
dataset_dist<-dist(dataset[,-1])

#Implementing Hclust Algorithm
hclust_results<-hclust(dataset_dist)
plot(hclust_results)
hclust_2<-cutree(hclust_results,2)
conf_mat<-table(hclust_2,dataset[,1])
accuracy<-sum(diag(conf_mat))/nrow(dataset) * 100
accuracy



rm(list=ls())

# select the downloaded wisc_bc_ContinuousVar.csv dataset from canvas
filename <- file.choose()
dataSet <- read.csv(filename, na.strings = "?") 
View(dataSet)
table(dataSet$diagnosis)

#To factor the data set
dataset<-na.omit(dataSet)
dataset<-dataset[-1]


# Implement K means algorithm
kmeans_2<- kmeans(dataset[,-1],2,nstart = 10)
kmeans_2$cluster
conf_mat<-table(kmeans_2$cluster,dataset[,1])
accuracy<-sum(diag(conf_mat))/nrow(dataset) * 100
accuracy
