# 1. y=x+e 문제.
X = rnorm(10000,0,1)
e = rnorm(10000,0,1)
y = X+e

# linear Model
fit_package_1 = lm(y~X)
# lm 함수를 사용했을 때 b0 : 0.01903 / B1 : 1.01463

linear_function<-function(X,y){
  X = as.matrix(X)
  y = as.matrix(y)
  
  one_vector = rep(1,dim(X)[1])
  
  X = cbind(one_vector,X)
  
  Bhat = solve(t(X)%*%X)%*%(t(X)%*%y)
  Bhat = t(Bhat)
  colnames(Bhat)<-c('B0','B1')
  print(Bhat)
}

linear_function(X,y)
fit_package_1

# Y=X(1-X)
x2 = rnorm(10000,0,1)
e = rnorm(10000,0,1)
y2 = x2*(1-x2)
linear_function(x2,y2)