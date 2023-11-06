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
