# 1. y=x+e 문제.
set.seed(1234)
x = rnorm(100,0,1)
e = rnorm(100,0,1)
y = x+e

# linear Model
linear_function<-function(x,y){
  x = as.matrix(x)
  y = as.matrix(y)
  
  one_vector = rep(1,dim(x)[1])
  
  x = cbind(one_vector,x)
  
  Bhat = solve(t(x)%*%x)%*%(t(x)%*%y)
  fitted_value <- x%*%Bhat
  fitted_value<-as.vector(fitted_value)
  print(fitted_value)
}

linear_function(x,y)

# Y=X(1-X)
y2 = x1*(1-x1)
linear_function(x1,y2)