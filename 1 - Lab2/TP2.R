rm(list=objects())
library(zoo)
library(timeDate)
library(forecast)
library(xts)
library(mgcv)

date1<- strptime("01/01/1900", "%m/%d/%Y")
date2<- strptime("01/01/2000", "%m/%d/%Y")
Date<-seq.POSIXt(date1,date2,by = "year")
n<-length(Date)
t<-c(1:n)
t


#################################################exercice 1
T<-t/20+1
w=2*pi/50
S<-cos(w*t)+sin(w*t)
eps<-rnorm(n,0,1)

par(mfrow=c(1,1))
X<-T+S+eps
X<-xts(X,order.by=Date)
T<-xts(T,order.by=Date)
S<-xts(S,order.by=Date)
plot(X)
lines(T,col='red')
lines(T+S,col='blue')

###################################################estimation de la tendance
#################moyenne mobile
mb<-filter(X, filter=array(1/50,dim=50), method = c("convolution"),
             sides = 2, circular = FALSE)
mb<-xts(mb,order.by=Date)

plot(X,type='l')
lines(mb,col='red')


#################régression
reg<-lm(X~t)
ychap.lm<-xts(as.numeric(reg$fitted),order.by=Date)
plot(X,type='l')
lines(ychap.lm,col='red')


#################noyau Gaussien
h=1000
x<-seq(1,max(t),length=n)

# test<-lapply(x,function(x){x^2})
# test[[1]]
# test

W<-matrix(unlist(lapply(x,
  function(x){dnorm(x-t,0,sd=sqrt(h/2))/sum(dnorm(x-t,0,sd=sqrt(h/2)))})),ncol=n,nrow=n,byrow=F)
plot(W[,50])
ychap.kernel<-colSums(as.numeric(X)*W)
ychap.kernel<-xts(ychap.kernel,order.by=Date)
plot(X,type='l')
lines(ychap.kernel,col='red')


#################polynomes locaux
lo<-loess(X~t, degree=2,span=0.95)
ychap.lo<-xts(lo$fitted,order.by=Date)
plot(X,type='l')
lines(ychap.lo,col='red')
?loess

#################régression sur bases de splines
library(mgcv)
g<-gam(X~s(t,k=3))
ychap.gam<-xts(g$fitted,order.by=Date)
plot(X,type='l')
lines(ychap.gam,col='red')



###################################################estimation de la partie saisonnière
#################régression
X.detrend<-X-ychap.lm
plot(X.detrend)


w=2*pi/50
fourier<-cbind(cos(w*t), sin(w*t))
K<-1
for(i in c(2:K))
{
  fourier<-cbind(fourier,cos(i*w*t), sin(i*w*t))
}
matplot(fourier,type='l')


reg<-lm(X.detrend~fourier)
ychap.lm.season<-xts(as.numeric(reg$fitted),order.by=Date)
plot(X.detrend,type='l')
lines(ychap.lm.season,col='red')
lines(S,col='blue')

#################moyenne mobile
mb.season<-filter(X.detrend, filter=array(1/20,dim=20), method = c("convolution"),
             sides = 2, circular = FALSE)
mb.season<-xts(mb.season,order.by=Date)

plot(X.detrend,type='l')
lines(mb.season,col='red')
lines(S,col='blue')


#################noyau Gaussien
h=50
x<-seq(1,max(t),length=n)
W<-matrix(unlist(lapply(x,function(x){dnorm(x-t,0,sd=sqrt(h/2))/sum(dnorm(x-t,0,sd=sqrt(h/2)))})),ncol=n,nrow=n,byrow=F)
plot(W[,10])
ychap.kernel.season<-colSums(as.numeric(X.detrend)*W)
ychap.kernel.season<-xts(ychap.kernel.season,order.by=Date)

plot(X.detrend,type='l')
lines(ychap.kernel.season,col='red')
lines(S,col='blue')


#################polynomes locaux
lo<-loess(X.detrend~t, degree=2,span=0.3)
ychap.lo.season<-xts(lo$fitted,order.by=Date)
plot(X.detrend,type='l')
lines(ychap.lo.season,col='red')
lines(S,col='blue')

#################régression sur bases de splines cycliques
cycle<-c(rep(c(1:50),2),1)

g<-gam(X.detrend~s(cycle,k=5,bs='cc'))
ychap.gam.season<-xts(g$fitted,order.by=Date)
plot(X.detrend,type='l')
lines(ychap.gam.season,col='red')
lines(S,col='blue')



######################comparaison des méthodes
plot(X-eps,type='l',ylim=range(X))
lines(X-eps,lwd=2)
lines(X,col='grey')
lines(ychap.lm+ychap.lm.season,col='purple')
lines(ychap.lm+ychap.kernel.season,col='red')
lines(ychap.lm+ychap.lo.season,col='blue')
lines(ychap.lm+ychap.gam.season,col='turquoise2')
lines(ychap.lm+mb.season,col='violetred1')

acf(X-(ychap.lm+ychap.lm.season))
acf(X-(ychap.lm+ychap.kernel.season))
acf(X-(ychap.lm+ychap.lo.season))
acf(X-(ychap.lm+ychap.gam.season))
acf(X-(ychap.lm+mb.season),na.action = na.omit)





#################################################exercice 2
setwd("C:\\Enseignement\\2015-2016\\Serie_temp\\Datasets\\")
beer<-read.csv("beer2.csv",header=TRUE,skip=1)

#######creation de la date
date1<- strptime(c("01/01/91"), "%m/%d/%y")
date2<- strptime(c("08/01/95"), "%m/%d/%y")
Date<-seq(date1,date2,by = "1 month")
Time<-c(1:length(Date))
beer<-data.frame(Date,beer$BeerProd,Time)
names(beer)<-c("Date","BeerProd","Time")
plot(beer$Date,beer$BeerProd,type='l')

#################################################regression sur base de splines
Month<-as.numeric(format(Date,"%m"))
beer<-data.frame(beer,Month)

g<-gam(BeerProd~s(Time,k=10)+s(Month,k=4,bs='cc'),data=beer)
ychap.gam<-g$fitted

plot(beer$Date,beer$BeerProd,type='l')
lines(beer$Date,ychap.gam,col='red')

plot(g)
terms<-predict(g,type="terms")

plot(beer$Date,beer$BeerProd-mean(beer$BeerProd),type='l')
lines(beer$Date,terms[,1],col='blue')
lines(beer$Date,terms[,2],col='red')





































