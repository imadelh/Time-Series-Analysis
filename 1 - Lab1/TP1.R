rm(list=objects())
library(zoo)
library(timeDate)
library(forecast)
library(xts)
library(dygraphs)

################################################################Exercice 1: beer production
######Import des donn?es
setwd("C:\\Enseignement\\2015-2016\\Serie_temp\\Datasets\\")
beer<-read.csv("beer2.csv",header=TRUE,skip=1)
head(beer)
plot(beer$BeerProd,type='b',pch=20)





#######creation de la date
date1<- strptime(c("01/01/91"), "%m/%d/%y")
date2<- strptime(c("08/01/95"), "%m/%d/%y")
Date<-seq(date1,date2,by = "1 month")

beer<-data.frame(Date,beer$BeerProd)
names(beer)<-c("Date","BeerProd")
summary(beer)
plot(beer$Date,beer$BeerProd,type='l')


#########classe ts
beer.ts<-ts(beer$BeerProd,start=1,frequency=12)
plot(beer.ts)

#########classe zoo
beer.zoo<-zoo(beer$BeerProd,order.by=beer$Date)
plot(beer.zoo)


######################statistiques de base
mean(beer$BeerProd)
sd(beer$BeerProd)
summary(beer)
boxplot(beer$BeerProd)
hist(beer$BeerProd,breaks=20)

month<-format(beer$Date,"%m")
mean.month<-tapply(beer$BeerProd,as.factor(month),mean)
plot(mean.month,type='b')






###############################exercice 2
setwd("C:\\Enseignement\\2015-2016\\Serie_temp\\Datasets\\")
data<-read.table("conso_2015.csv",header=T,sep=';')

date1<- strptime("01/01/2015 00:00:00", "%m/%d/%Y %H:%M:%S")
date2<- strptime("11/30/2015 23:30:00", "%m/%d/%Y %H:%M:%S")
Date<-seq.POSIXt(date1,date2,by = "30 min")

X<-as.matrix(t(data[,-1]))
conso<-c(X)

plot(conso[1:(24*7)],type='l')
plot(Date,conso,type='l')

conso.xts<-xts(conso,order.by=Date)
plot(conso.xts)




#####################stat desc.
#####stats.
mean(conso.xts)


month<-as.factor(.indexmon(conso.xts))
mean.month<-tapply(conso.xts,month,mean)
plot(mean.month,type='b')


#####profils
dow<-as.factor(.indexwday(conso.xts))
tapply(conso.xts,dow,mean)
hour<-as.factor(.indexhour(conso.xts))
mean.dow.hour<-tapply(conso.xts,dow:hour,mean)
plot(mean.dow.hour,type='l')
abline(v=seq(1,24*7,by=24))

col.pal<-colorRampPalette(c("lightblue", "red"))( 12 )
sel<-which(.indexhour(conso.xts)==20)
boxplot(conso[sel]~month[sel],col=col.pal)

###############################################################autocorr?lations
####fonction lag
lag.test<-lag.xts(conso.xts,k=1,na.pad=T)
lag.test[1:3]
conso.xts[1:3]

####fonction calculant l'autocorr?lation d'ordre h
autoCorr<-function(x,h)
{
  x.lag<-lag.xts(x,k=h,na.pad=T)
  return(cor(x.lag,x,use="pairwise.complete.obs"))
}


autoCorr2<-function(x,h)
{
  x.lag<-c(x[1:h],head(x,length(x)-h))
  return(cor(x.lag,x))
}

a1<-sapply(c(1:336),autoCorr,x=conso.xts)
a2<-sapply(c(1:336),autoCorr2,x=conso.xts)
plot(a1,type='h',ylim=c(0,1))
lines(a2,col='red')

####2e m?thode avec la fonction acf
a3<-acf(conso.xts,lag.max=336,type="correlation")













###on constate des diff?rences:
plot(a3$acf[-1],type='h',ylim=c(0,1))
lines(a2,col='red')

plot(a3$acf-a2)
plot(a2-a1)


###############################################################autocorr?lations partielles

PartialAutoCorr<-function(x,h)
{
  x.lag<-lapply(c(1:h),lag.xts,x=x,na.pad=T)
  x.lag<-matrix(unlist(x.lag),ncol=length(x.lag))
  reg<-lm(x~x.lag-1)
  return(tail(reg$coef,1))
}

PartialAutoCorr(conso,h=1)
pa1<-sapply(c(1:50),PartialAutoCorr,x=conso.xts)

pa2<-pacf(conso,lag.max=50) 
points(pa2$acf)

pa1-pa2$acf



###############################exercice 3

#######simulation d'une s?rie p?riodique
t<-c(1:200)
w=2*pi/50

x<-cos(w*t)+rnorm(length(t),0,1)
plot(x,type='l')

acf(x,lag.max=50)


#######simulation d'une s?rie avec tendance
t<-c(1:200)
#lin?aire
x<-t/10+rnorm(length(t),0,1)
plot(x,type='l')
acf(x)
pacf(x)

#lin?aire avec variance croissante
x<-t/10+rnorm(length(t),0,sd=log(t/10+1))
plot(x,type='l')
acf(x)



#################exercice 3
#mars 1986 ? avril 2002
date1<- strptime("03/01/1986", "%m/%d/%Y")
date2<- strptime("04/01/2002", "%m/%d/%Y")
Date<-seq.POSIXt(date1,date2,by = "month")

n<-length(Date)
t<-c(1:n)

T<-log(t/10+1)

w=2*pi/12
S<-cos(w*t)
eps<-rnorm(n,0,1)


X<-T+S+eps

X<-xts(X,order.by=Date)
T<-xts(T,order.by=Date)
S<-xts(S,order.by=Date)
plot(X)
lines(T,col='red')
lines(T+S,col='blue')

indexFormat(X)


extractY<-function(y,X,Date)
{
  year<-format(Date,"%Y")
  return(as.numeric(X[which(year==y)]))
}

y<-c(1987,1990,1993)

plot(extractY(1987,X=X,Date=Date),type='l',ylim=range(X))
lines(extractY(1990,X=X,Date=Date),col='red')
lines(extractY(1993,X=X,Date=Date),col='blue')



###########mod?le multiplicatif
#mars 1986 a avril 2002
date1<- strptime("03/01/1986", "%m/%d/%Y")
date2<- strptime("04/01/2002", "%m/%d/%Y")
Date<-seq.POSIXt(date1,date2,by = "month")

n<-length(Date)
t<-c(1:n)

T<-log(t/10+1)

w=2*pi/12
S<-cos(w*t)
eps<-rnorm(n,0,1/2)


X<-T*S*eps
X<-xts(X,order.by=Date)
T<-xts(T,order.by=Date)
S<-xts(S,order.by=Date)
plot(X)
lines(T,col='red')
lines(T*S,col='blue')








date1<- strptime("01/01/1900", "%m/%d/%Y")
date2<- strptime("01/01/2000", "%m/%d/%Y")
Date<-seq.POSIXt(date1,date2,by = "year")

n<-length(Date)
t<-c(1:n)

T<-t/20+1

w=2*pi/5
S<-cos(w*t)
eps<-rnorm(n,0,1)


par(mfrow=c(2,1))
X<-T+S+eps
X<-xts(X,order.by=Date)
T<-xts(T,order.by=Date)
S<-xts(S,order.by=Date)
plot(X)
lines(T,col='red')
lines(T+S,col='blue')



X<-T*S*eps
X<-xts(X,order.by=Date)
T<-xts(T,order.by=Date)
S<-xts(S,order.by=Date)
plot(X)
lines(T,col='red')
lines(T*S,col='blue')




###################################exercice 4
setwd("C:\\Enseignement\\2015-2016\\Serie_temp\\TP\\TP1_data\\")
load("Solar1.RData")     
objects()
names(Data1)

saveRDS(Data1,"Solar1.RDS")
head(Data1)


###prod. moyenne par mois, heure
AvMonProd=tapply(Data1$Z1,as.factor(Data1$Mois),mean)
barplot(AvMonProd,col="palegoldenrod")

AvHourProd=tapply(Data1$Z1,as.factor(Data1$Heure),mean)
barplot(AvHourProd,col="palevioletred")

AvHourMonthProd=tapply(Data1$Z1,as.factor(Data1$Mois):as.factor(Data1$Heure),mean)
AvHourMonthProd=matrix(AvHourMonthProd,nrow=12,ncol=24,byrow=T)
matplot(t(AvHourMonthProd),type='l',col=rainbow(12))

###########transformation des variables à visualiser en objets xts
Z1=xts(Data1$Z1,order.by=Data1$date)
Ssrd=xts(Data1$Ssrd,order.by=Data1$date)
Tsr=xts(Data1$Tsr,order.by=Data1$date)
Tcc=xts(Data1$Tcc,order.by=Data1$date)
Ssrd.diff<-diff.xts(Ssrd)
Ssrd.diff[which(Data1$Heure==12)]<-Ssrd[which(Data1$Heure==12)]

#standardisation
Z1.sd=Z1/sd(Z1)
Ssrd.diff.sd=Ssrd.diff/sd(Ssrd.diff,na.rm=T)


###########standardisation des variables à visualiser
sd.time.series=cbind(Z1.sd,Ssrd.diff.sd)

names(sd.time.series)=c("Z1.sd","Ssrd.diff.sd")
##dygraph de base
dygraph(sd.time.series)

##avec fenêtre 
dygraph(sd.time.series)%>% dyRangeSelector()   ##equivalentà  dyRangeSelector(dygraph(sd.time.series))


