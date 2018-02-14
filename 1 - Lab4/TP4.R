rm(list=objects())
library(magrittr)


#####AR(1)
n<-1000
eps<-rnorm(n,0,1)
acf(eps)

a<-0.7
puiss<-function(a,k){c(a^c(k:1),rep(0,n-k))}
# puiss(a,k=4)
# (a^4, a^3....)
M<-lapply(c(1:n),puiss,a=a)%>%unlist%>%matrix(nrow=n,ncol=n,byrow=T)
y<-M%*%eps
head(M)
par(mfrow=c(2,1))
plot(y,type='l')
plot(eps,type='l')

par(mfrow=c(1,1))
acf(y,lag.max=30)
lines(c(0:30),a^c(0:30),col='red')


a<-0.7
M<-lapply(c(1:n),puiss,a=a)%>%unlist%>%matrix(nrow=n,ncol=n,byrow=T)
y07<-M%*%eps
a<-0.1
M<-lapply(c(1:n),puiss,a=a)%>%unlist%>%matrix(nrow=n,ncol=n,byrow=T)
y01<-M%*%eps
a<--0.7
M<-lapply(c(1:n),puiss,a=a)%>%unlist%>%matrix(nrow=n,ncol=n,byrow=T)
y_07<-M%*%eps

plot(y01,type='l',ylim=range(y01,y07,y_07))
lines(y07,col='red')
lines(y_07,col='blue')



####cela éuivaut à :
M<-matrix( unlist(lapply(c(1:n),puiss,a=a)) ,nrow=n,ncol=n,byrow=T)


plot(y07,type='l')
par(mfrow=c(1,1))
acf(y)
pacf(y)


#####MA(4)
n<-200
eps<-rnorm(n,0,1)
b<-c(1:4)
y<-filter(eps, filter=b, method = c("convolution"),
          sides = 1, circular = FALSE)

par(mfrow=c(1,1))
plot(y,type='l')

par(mfrow=c(2,1))
acf(y,na.action = na.omit)
pacf(y,na.action = na.omit)

y<-y07
h<-1
y.lag1<-c(y[1:h],head(y,length(y)-h))
h<-2
y.lag2<-c(y[1:h],head(y,length(y)-h))

lm(y~y.lag1+y.lag2)




















