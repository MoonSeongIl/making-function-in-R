rm(list=ls())
library(dplyr)
set.seed(1234)

# knn Algorithms

# 1. Y = X+e
x1 <- rnorm(100,m=0,sd=1)
head(x1)
e <- rnorm(100,m=0,sd=1)
y<- x1+e
head(y)
x1[order(x1)]

# 2. Y = X(1-X)+e
x2 <- rnorm(100,m=0,sd=1)
e <- rnorm(100,m=0,sd=1)
y2<- x2*(1-x2)+e

# knn function
knn_function <- function(x,y,k){
  
  #normalization function
  normalize <- function(x){
    num <- (x-mean(x)) / sd(x)
    return(num)
  }
  
  # eucliden_distance function
  eucliden_distance<- function(x,y){
    sqrt(sum((x-y)^2))
  }
  
  # distance matrix function
  distance_matrix<- function(x){
    if(is.matrix(x)==F){
      n<-length(x)
    }else {
      n<-dim(x)[1]
    }
    distance_result_matrix = matrix(rep(0,n^2),n,n)
    if(is.matrix(x)==F){
      for(i in 1:n){
        for(j in 1:n){
          distance_result_matrix[j,i] = eucliden_distance(x[j],x[i])
          }
        }
      }else {
          for(i in 1:n){
            for(j in 1:n){
              distance_result_matrix[j,i] = eucliden_distance(x[j,],x[i,])
            }
          }
        }
    distance_result_matrix = distance_result_matrix+t(distance_result_matrix)
    return(distance_result_matrix)
  }
  
  if(is.matrix(x)==F){
    x_bar = mean(x)
    x_sd = sd(x)
    x_normal = (x-x_bar)/x_sd
  }else {
    x_bar <- apply(x,2,mean)
    x_sd <- apply(x,2,sd)
    x_normal <- matrix(rep(0,dim(x)[1]*dim(x)[2]),nrow = dim(x)[1],ncol = dim(x)[2])
    
      for(i in 1:dim(x)[2]){
        x_normal[,i]<-(x[,i] - x_bar[i])/x_sd[i]
      }
  }
  dist_matrix <-distance_matrix(x_normal)
  
  matrix_order <- apply(dist_matrix,1,order)
  
  fitted_value <- c()
  for(q in 1:dim(matrix_order)[1]){
    fitted_value[q] <- mean(y[matrix_order[2:(k+1),q]])
    }
    return(fitted_value)
  }
knn_function(x1,y,k=5)
knn_function(x2,y2,k=5)

