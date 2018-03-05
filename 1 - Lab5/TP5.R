rm(list=objects())
library(magrittr)

#######################################################################################################################
##################################exercice 1
#######################################################################################################################

sigma<-1/2
phi<-0.7
n<-500
eps<-rnorm(n,0,sd=sigma)
y<-arima.sim(n = n, list(ar = c(phi)),innov=eps)
par(mfrow=c(1,1))
plot(y,type='l')


var(y)
sigma^2/(1-phi^2)


##################autocorrélation empirique et théorique
rho_est<-acf(y,lag.max=10, plot = FALSE)
rho_theo<-phi^c(0:10)

rho_est
rho_theo

plot(rho_est$acf,type='b',pch=20)
lines(rho_theo,col='red')

##################autocovariance empirique et théorique
gamma_est<-rho_est$acf*var(y)
gamma_theo<-sigma^2/(1-phi^2)*phi^c(0:10)

gamma_est
gamma_theo

plot(gamma_est,type='b',pch=20)
lines(gamma_theo,col='red')



######estimateur de phi:
phi_hat<-rho_est$acf[2]

######intervalle de confiance
sigma<-1/2
phi<-0.7
n<-500

estim_phi<-function(n,sigma,phi)
{
  eps<-rnorm(n,0,sd=sigma)
  y<-arima.sim(n = n, list(ar = c(phi)),innov=eps)
  rho_est<-acf(y,lag.max=1, plot =FALSE)
  phi_hat<-rho_est$acf[2]
  return(phi_hat)
}


Nsimu<-500
phi_hat<-lapply(rep(n,Nsimu),estim_phi,sigma=sigma,phi=phi)
phi_hat<-unlist(phi_hat)

###histogramme des estimations de phi
hist(phi_hat,breaks=30)
abline(v=phi,col='red')

###hintervalle de confiances (2 approches)
alpha<-0.05

#solution1
a<-sort(phi_hat)[floor(alpha/2*Nsimu)]
b<-sort(phi_hat)[floor((1-alpha/2)*Nsimu)]
c(a,b)
#solution2
quantile(phi_hat,c(alpha/2,1-alpha/2))



Nsimu<-100
s_n<-seq(50,500,by=50)
IC<-NULL
for(n in s_n)
{
  phi_h<-lapply(rep(n,Nsimu),estim_phi,sigma=sigma,phi=phi)
  phi_h<-unlist(phi_h)
  IC<-rbind(IC,quantile(phi_h,c(alpha/2,1-alpha/2)))
  print(n)
}


plot(s_n,IC[,1],pch='-',ylim=range(IC),cex=3)
points(s_n,IC[,2],pch='-',cex=3)


l<-abs(IC[,2]-IC[,1])
plot(s_n,l,type='b',pch=20)
conv<-1/sqrt(s_n)
reg<-lm(l~conv-1)
lines(s_n,reg$coeff/sqrt(s_n),col='red')





#######################################################################################################################
##################################exercice 2
#######################################################################################################################

setwd("C:\\Enseignement\\2015-2016\\Serie_temp\\TP\\TP5_data\\")
data<-read.table("exercice2.txt",sep=';',header=T)

par(mfrow=c(1,1))
plot(data$y,type='l')

s<-spectrum(data$y)
plot(s$freq,s$spec,type='l')


############construction de la série de fourier

a1<-which.max(s$spec)
f1<-s$freq[a1]
a2<-which.max(s$spec[-a1])
f2<-s$freq[a2]
a3<-which.max(s$spec[-c(a1,a2)])
f3<-s$freq[a3]

1/c(f1,f2,f3)

X1cos<-cos(2*pi*f1*data$t)
X1sin<-sin(2*pi*f1*data$t)
X2cos<-cos(2*pi*f2*data$t)
X2sin<-sin(2*pi*f2*data$t)
X3cos<-cos(2*pi*f3*data$t)
X3sin<-sin(2*pi*f3*data$t)

model1<-lm(data$y~X1cos+X1sin+X2cos+X2sin+X3cos+X3sin-1)
summary(model1)
plot(data$y,type='l')




#######################################################################################################################
##################################exercice 3
#######################################################################################################################

setwd("C:\\Enseignement\\2015-2016\\Serie_temp\\TP\\TP5_data\\")
data<-read.table('exercice3.txt',header=T,sep=';')
data[1,]
attach(data)

###########x1
x1<-ts(x1)
par(mfrow=c(1,2))
acf(x1)
pacf(x1)
mean(x1)
x1.model<-arima(x1, order = c(2,0,0), method = c("ML"),
                SSinit = c("Rossignol2011"),
                optim.method = "BFGS",include.mean = F)
x1.model
horizon<-10
par(mfrow=c(1,1))
x1.forecast<-predict(x1.model,n.ahead = horizon,se.fit =F)
plot(x1,xlim=c(1,nrow(data)+horizon))
lines(nrow(data)*c(1:horizon),x1.forecast,col='red')

names(x1.model)

#-2*x1.model$loglik+2*(2+1)






###########x2
x2<-ts(x2)
par(mfrow=c(1,2))
acf(x2)
pacf(x2)
x2.model<-arima(x2, order = c(1,0,0), method = c("ML"),SSinit = c("Rossignol2011"),optim.method = "BFGS",include.mean = F)
x2.model
horizon<-10
x2.forecast<-predict(x2.model,n.ahead = horizon,se.fit =F)
plot(x2,xlim=c(1,nrow(data)+horizon))
lines(nrow(data)*c(1:horizon),x2.forecast,col='red')


phi<-ARMAtoMA(ar =  x2.model$coef, ma=0, 12) 
phi
0.7^c(1:12)
plot(phi,type='l')



###########x3
x3<-ts(x3)
par(mfrow=c(1,2))
acf(x3)
pacf(x3)
mean(x3)
sd(x3)
t.test(x3,alternative=c("two.sided"))

x3.model<-arima(x3, order = c(0,0,6), method = c("ML"),SSinit = c("Rossignol2011"),optim.method = "BFGS",include.mean = T)
x3.model
horizon<-10
x3.forecast<-predict(x3.model,n.ahead = horizon,se.fit =F)
par(mfrow=c(1,1))
plot(x3,xlim=c(1,nrow(data)+horizon))
lines(nrow(data)*c(1:horizon),x3.forecast,col='red')






###########x4
x4<-ts(x4)
acf(x4)
pacf(x4)
mean(x4)
x4.model<-arima(x4, order = c(0,0,20), method = c("ML"),SSinit = c("Rossignol2011"),optim.method = "BFGS",include.mean = F)
x4.model
horizon<-10
x4.forecast<-predict(x4.model,n.ahead = horizon,se.fit =F)
plot(x4,xlim=c(1,nrow(data)+horizon))
lines(nrow(data)*c(1:horizon),x4.forecast,col='red')






     
     
     






