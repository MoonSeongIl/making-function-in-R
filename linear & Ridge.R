data= read.csv('prostate.csv',header=T)
head(data)
dim(data)

data2 = data[-10]
head(data2)

y = data2[9]
x = data2[-9]
x = scale(x,T,T)
y = as.matrix(y)

library(dplyr)

mse = function(x,y){
  sum((x-y)^2)/nrow(x)
}

# linear Model
linear_function<-function(x,y){
  x = as.matrix(x)
  y = as.matrix(y)
  
  intercept = rep(1,dim(x)[1])
  
  x = cbind(intercept,x)
  
  Bhat = solve(t(x)%*%x)%*%(t(x)%*%y)
  fitted_value <- x%*%Bhat
  data = list(Bhat,fitted_value)
  return(data[[2]])
}

linear_function(x,y)

# Ridge Model
ridge_linear_function<-function(x,y,lambda){
  x = as.matrix(x)
  y = as.matrix(y)
  
  intercept = rep(1,dim(x)[1])
  x = cbind(intercept,x)
  I = diag(dim(x)[2])
  
  Bhat_ridge = solve((t(x)%*%x)+lambda*I)%*%(t(x)%*%y)
  fitted_value <- x%*%Bhat_ridge
  final = mse(fitted_value,y)
  return(final)
}

lambda = seq(0,10,by=0.1)
data=rep(0,100)

for(i in seq(0,10,by=0.1)){
  if(i==0){
    next
  }else{
    data[i*10] = ridge_linear_function(x,y,i)
  }
}

data<-as.data.frame(data)

data_final<-cbind(lambda[-1],data)
colnames(data_final)<-c('lambda','mse')

plot(data_final$lambda,data_final$mse)
head(data_final)
