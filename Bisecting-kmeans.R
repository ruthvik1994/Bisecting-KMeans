######
# Bisecting K-Means Implementation By Ruthvik Mandava
# Coursework - Automated Learning and Data analysis - CSC 522 - Dr.Raju
# North Carolina State University
#####
rm(list = ls(all = T))

library(RColorBrewer)
set.seed(100)
data.df <- read.csv('hw2-data.csv')

# Implementation bisecting k means.
# Input:
# data.df: data frame based on hw2-data.csv
# trials.max: Max. number of trials for kmeans 
# k-means function is used to split at every step
# k: Number of clusters to find in bisecting k-means

# Output:
# Output/function return value will be a list containing 2 elements
# first element of the list is a vector containing cluster assignments (i.e., values 1 to k assigned to each data point)
# second element of the list is a vector containing SSE of each cluster
# Additional Information:
# When identifying which cluster to split, the one with maximum SSE
# When performing kmeans, pick two random points the cluster with largest SSE as centers at every iteration. 
# terminating condition: when k clusters have been found


bisectingkmeans <- function(data.df, trails.max, k){
  # start your implementation here
  myClusters <- myBisectingKMeans(data.df, trails.max, k)
  clusterVec <- c()
  sseVec <- c()
  for (i in myClusters){
    frame <- as.data.frame(i)
    sseVec <- c(sseVec, frame$withinss[1])
    for (j in 1:length(frame$ID)){
      clusterVec[frame$ID[j]] <- frame$cluster[j]
    }
  }
  return (list(clusterVec, sseVec))
}

myBisectingKMeans <- function(data.df, trails.max, k){
  st <- 1
  data.df$cluster <- 1
  myClusters <- list()
  toSplit <- data.df[,1:3]
  curK <- 1
  curC <- 1
  while (curK <= k){
    ksplit <- kmeans(toSplit, 2)
    minsse <- ksplit$totss
    for(i in 1:trails.max){
      split <- kmeans(toSplit, 2)
      if (split$totss < minsse){
        ksplit <- split
        minsse <- split$totss
      }
    }
    clusters <- ksplit$cluster
    clusters <- replace(clusters, clusters == 2, curK+1)
    clusters <- replace(clusters, clusters == 1, curC)
    toSplit$cluster <- clusters
    split1 <- toSplit[toSplit$cluster==curC,]
    split1$withinss <- ksplit$withinss[1]
    split2 <- toSplit[toSplit$cluster==curK+1,]
    split2$withinss <- ksplit$withinss[2]
    myClusters[[curC]] <- split1
    myClusters[[curK+1]] <- split2
    temp <- 0
    ind <- 0
    for(i in 1:curK){
      if (!is.null(myClusters[[i]])){
        frame <- as.data.frame(myClusters[[i]])
        if (frame$withinss[1] > temp){
          temp <- frame$withinss[1]
          ind <- i
          curC <- frame$cluster[1]
          toSplit <- frame[1:3]
        }
      }
    }
    curK <- curK+1
    if (curK == k)return(myClusters)
  }
  return (myClusters)
}

# Code for comparing result from bisecting kmeans here
kmeans_comparison <- function(data.df, result, k){
  mycenters <- rbind(data.df[210,], data.df[247,], data.df[265,], data.df[278,], data.df[288,])
  kmeansClustering <- kmeans(data.df, centers = mycenters)
  print ("K means clusters with chosen centers - SSE of each cluster and total")
  print (kmeansClustering$withinss)
  t1 <- 0
  t2 <- 0
  for (i in kmeansClustering$withinss)t1 <- t1+i
  for (i in result[[2]])t2 <- t2+i
  print (t1)
  print ("Bisecting K means - SSE of each cluster and total")
  print (result[[2]])
  print (t2)
}

k=8
result <- bisectingkmeans(data.df, trails.max = 25 , k)
plot(data.df[, -1], col = brewer.pal(k, "Set3")[result[[1]]], pch = '.',
     cex = 3)
kmeans_comparison(data.df, result, k)
