rm(list=ls())
# knn Algorithms

# 1. Y = X+e
x1 <- rnorm(10000,m=0,sd=1)
e <- rnorm(10000,m=0,sd=1)
y<- x1+e


# Euclidean distance
knn_function <- function(x,y,k){
  normalize <- function(x){
    num <- x-min(x)
    denom <- max(x) - min(x)
    return (num/denom)
  }
  eucliden_distance<- function(x1,x2){
    sqrt(sum((x1-x2)^2))
  }
  
  # normalization
  x<-normalize(x)
  y<-normalize(y)
  
  # x,y 값 순서 order
  x<-x[(order(x))]
  y<-y[(order(y))]
  
  #length 설정
  num = length(x)
  
  #matrix 만들기
  data = matrix(y,x,nrow=num,ncol=2)
  
  #k 설정
  k=k
  
  #data_frame 만들기.
  data = as.data.frame(data)
  
  distance_result = matrix(rep(0,num),nrow=num,ncol=1)
  
  for(i in 1:num){
    distance_result[i] <- eucliden_distance(data[[2]][i],data[[2]][1])
  }
  
  data<-cbind(data,distance_result)
  colnames(data)<-c('y','x','distance_result')
  knn_fitted_value = mean(data[["y"]][1:k+1])
  print(knn_fitted_value)
}

knn_function(x1,y,k=10)


# # x1, y 값 순서 order 
# x1<-x1[(order(x1))]
# y<-y[(order(y))]
# 
# # matrix 만들기.
# data = matrix(y,x1,nrow=10000,ncol=2)
# 
# # columns 이름 변경
# colnames(data)<-c('y','x')
# # k값 설정
# k=3
# # length 설정
# num = dim(data)[1]
# 
# data<-as.data.frame(data)
# 
# # distance를 했을 때 가장 가까운 x값을 알 수 있음.
# distance_result<-matrix(rep(0,num),nrow=num,ncol=1)
# 
# for(i in 1:num){
#   distance_result[i] <- eucliden_distance(data[[2]][i],data[[2]][1])
# }
# data<-cbind(data,distance_result)
# colnames(data)<-c('y','x','distance_result')
# head(data)
# data[["y"]][1:k+1]
# knn_fitted_value = mean(data[["y"]][1:11])
# print(knn_fitted_value)
