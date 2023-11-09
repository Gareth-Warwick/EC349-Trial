#This is for Week6 Lecture slides

#Load stuff
install.packages(tidyverse)
library(tidyverse)

#Slide10
##Creating Variables
set.seed(131)
x<-rnorm(50,mean=0,sd=1)
y<-50+x-x^2+0.5*x^3+rnorm(50,mean=0,sd=1.5)

##Testing
fit<-lm(y~x)
fit2<-lm(y~poly(x,2,raw=TRUE))
fit3<-lm(y~poly(x,3,raw=TRUE))
fit4<-smooth.spline(x,y,spar=0.2)

plot(fit2)
plot(fit3)

#Slide50

##Setting Ridge
install.packages("glmnet")
library(glmnet)
grid<-10^seq(10,-2,length=100)
ridge.mod<-glmnet(x,y,alpha=0,lambda=grid)

##Setting Training and Testing Data Sets

###Ensure that we fix the randomness
set.seed(1)

###Set training dataset
train<-sample(1:nrow(x),nrow(x)/2)

###Set test dataset
test<-(-train)

###Set y test set???
y.test<-y[test]

##Validation
ridge.mod<-glmnet(x[train, ],y[train],alpha=0,lambda=grid,thresh=1e-12)
ridge.pred<-predict(ridge.mod, s=4,newx=x[test, ])
mean((ridge.pred-y.test)^2)
