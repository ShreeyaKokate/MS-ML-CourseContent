################################################################################
# KDDM FINAL EXAM - DEC 15 2021
# Shreeya Kokate
# CWID: 20005256
# SECTION A
#
# Problem 1 : (20 points)
#Use R/python to cluster (Algorithm=K-means; K=2) the seven (7) already normalized points 
# in the accompanying table and answer a and b below: (20 points)
# X Y Z
#a 1 1 1
#b 5 3 4
#c 4 4 5
#d 4 3 4
#e 1 2 1
#f 4 4 4
#g 2 1 2

#a. What are the members of each cluster?
#b. What are the coordinates for the cluster centers?
################################################################################

rm(list = ls())

# Create Vector x, y and z for coordinates
x <- c(1,5,4,4,1,4,2)
y <- c(1,3,4,3,2,4,1)
z <- c(1,4,5,4,1,4,2)

#Create new vector which is used to combine x and y
xyz <- cbind(x,y,z)

#Find Euclidean distance
dxyz <- dist(xyz)

#For k=2
c2<-kmeans(xyz,2)

# a. Members of each cluster
c2$cluster

# b. Coordinates for the cluster centers
c2$centers

#plot the clusters
plot(xyz, col=c2$cluster)
