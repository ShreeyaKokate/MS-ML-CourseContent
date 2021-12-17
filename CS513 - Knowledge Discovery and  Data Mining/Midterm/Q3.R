### Knowledge Discovery and Data Mining (CS 513) ###
#  MIDTERM EXAM - Q3  #

# Course      : CS 513A
# First Name  : SHREEYA
# Last Name   : KOKATE
# CWID        : 20005256

rm(list = ls())

#select the COVID19_v4.csv dataset from canvas
name <-file.choose()
dataset = read.csv(name)

#Summarize all the columns 
summary(dataset)

#Lets replace '?' with 'NA' for easy operations
dataset[, 1:6][dataset[, 1:6] == "?"] <- NA

#Identify and view the missing values
is.na(dataset)
View(dataset)

#Now, replace the missing values with the "mode" of the column.
for(i in 1:ncol(dataset)){
  dataset[is.na(dataset[, i]), i] <- mode(dataset[, i])
}

clean_data<-(transform(dataset, ID=as.factor(ID), Age=as.factor(Age), Exposure=as.factor(Exposure), MaritalStatus=as.factor(MaritalStatus), Cases=as.factor(Cases), MonthAtHospital=as.factor(MonthAtHospital), Infected=as.factor(Infected)))
sapply(clean_data, class)

#Plotting one pair at a time
#Scatterplot
plot(clean_data$Age, clean_data$Exposure)
plot(clean_data$Age, clean_data$MonthAtHospital)
plot(clean_data$MonthAtHospital, clean_data$Exposure)

#Boxplot
boxplot(clean_data$Age, clean_data$MonthAtHospital)
