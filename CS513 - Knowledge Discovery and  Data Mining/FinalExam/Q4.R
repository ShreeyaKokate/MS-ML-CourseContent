################################################################################
# KDDM FINAL EXAM - DEC 15 2021
# Shreeya Kokate
# CWID: 20005256
# SECTION A
#
# Problem 4 : (20 points)
# Use R/python and the above table (problem #3) to cluster (Algorithm=hierarchical; two clusters) 
#the seven (7) already normalized points in the accompanying table and answer a and b below: (20 points)
# a. What are the members of each cluster?
# b. What are the coordinates for the cluster centers?

#################################################################################

#clear the environment
rm(list=ls())

X <- c(1,5,4,4,1,4,2)
Y <- c(1,3,4,3,2,4,1)
Z <- c(1,4,5,4,1,4,2)

df <- data.frame(X,Y,Z)
df

#1->a,2->b,3->c,4->d,5>e,6->f,7->g
#Hirarchichal Clustering 
df_dist<-dist(df)
hclust_results<-hclust(df_dist)
plot(hclust_results)
#### 4.a. Cluster members ####
#Cluster 1 has members: A, E and G
#Cluster 2 has members: B, C, D and F

#### 4.b Cluster center co-ordinates ####
clusters <- cutree(hclust_results, 2)
cluster_center = aggregate(df,list(cluster=clusters), mean)
cluster_center
plot(cluster_center[2:4])
