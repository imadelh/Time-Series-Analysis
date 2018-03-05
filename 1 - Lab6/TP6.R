rm(list=objects())
library(polynom)
library(magrittr)
library(portes)

################################################################################################
#########################exercice 1
################################################################################################
set.seed(110)
n<-1000
sigma<-1
eps<-rnorm(n,0,sd=sigma)

ar=c(1,-1/4)
ma<-c(-1)
x1<-arima.sim(n = n, list(ar = ar,ma=ma), innov=eps)
par(mfrow=c(1,1))
plot(x1)

par(mfrow=c(1,2))
acf(x1)
pacf(x1)

####pmax=5
####qmax=7


ordre<-expand.grid(p = c(0:5), q = c(0:7))
ordre<-cbind(ordre[,1],0,ordre[,2])

head(ordre)
dim(ordre)


model<-apply(ordre,1,arima,x=x1,method = c("ML"),SSinit = c("Rossignol2011"),optim.method = "BFGS",include.mean = F)


aic<-lapply(model,function(x) x$aic)%>%unlist
bic<-lapply(model,function(x) -2*x$loglik+x$nobs*length(x$coef))%>%unlist
like<-lapply(model,function(x) -2*x$loglik)%>%unlist

##magrittr

par(mfrow=c(1,1))
o<-order(aic)
plot(aic[o],type='b',pch=20,axes=F)
axis(1,c(1:length(aic)),
     paste(ordre[o,1],ordre[o,3]),las=2)
axis(2)


par(mfrow=c(1,1))
o<-order(aic)
plot(aic[o[1:10]],type='b',pch=20,axes=F)
axis(1,c(1:10),paste(ordre[o[1:10],1],ordre[o[1:10],3]),las=2)
axis(2)


o<-order(bic)
plot(bic[o],type='b',pch=20,axes=F)
axis(1,c(1:length(aic)),paste(ordre[o,1],ordre[o,3]),las=2)
axis(2)

##########modèle choisi: 
ordre.opt<-ordre[which.min(aic),]
ordre.opt

model.opt<-arima(x=x1,order=ordre.opt,method = c("ML"),SSinit = c("Rossignol2011"),optim.method = "BFGS",include.mean = F)
model.opt
names(model.opt)

##########paramètres associés 
model.opt$coef
model.opt$sigma2
model.opt$var.coef

##########diagnostics des coefficients
abs(model.opt$coef)/sqrt(diag(model.opt$var.coef))<1.96

##########pvalue du test de student
pvalue<-function(model)
{
  (1-pnorm(abs(model$coef)/sqrt(diag(model$var.coef))))*2
}



pvalue(model.opt)


  
###########validation du modèle (ordre plus grand)
model.opt_p1<-arima(x=x1,order=ordre.opt+c(1,0,0),
                    method = c("ML"),SSinit = c("Rossignol2011"),
                    optim.method = "BFGS",include.mean = F)
pvalue(model.opt_p1)

model.opt_q1<-arima(x=x1,order=ordre.opt+c(0,0,1),method = c("ML"),SSinit = c("Rossignol2011"),optim.method = "BFGS",include.mean = F)
pvalue(model.opt_q1)


###########validation du modèle (ordre plus petit)
model.opt_pm1<-arima(x=x1,order=ordre.opt-c(1,0,0),method = c("ML"),SSinit = c("Rossignol2011"),optim.method = "BFGS",include.mean = F)
pvalue(model.opt_pm1)

model.opt_qm1<-arima(x=x1,order=ordre.opt-c(0,0,1),method = c("ML"),SSinit = c("Rossignol2011"),optim.method = "BFGS",include.mean = F)
pvalue(model.opt_qm1)

#ARMA(1,1); ARMA(2,0) plausibles


##########################etude des résidus
plot(model.opt$residuals,type='l')

par(mfrow=c(2,1))
acf(model.opt$residuals)
pacf(model.opt$residuals)

par(mfrow=c(1,1))
qqnorm(model.opt$residuals)
hist(model.opt$residuals,breaks=50,freq=F)
x<-seq(min(model.opt$residuals),
       max(model.opt$residuals),length=100)
lines(x,dnorm(x,0,model.opt$sigma2))

    
#######test de box pierce
pvalue_BP<-function(model,K)
{
  rho<-acf(model$residuals,lag.max=K,plot=F)$acf[-1]
  n<-model$nobs
  pval<-(1-pchisq(n*sum(rho^2),df=K-length(model$coef)))
  return(pval)
}

pvalue_BP(model.opt,K=10)
pvalue_BP(model.opt,K=20)


#BoxPierce(model.opt)


pvalue_BP(model.opt_pm1,K=10) ####on rejette l'hypothèse de non-corrélation à l'ordre au plus 10 des résidus
pvalue_BP(model.opt_qm1,K=10) #### idem

###################################################prévision
x1_a<-x1[1:900]
x1_b<-x1[-c(1:900)]

model.opt<-arima(x=x1_a,order=ordre.opt,method = c("ML"),SSinit = c("Rossignol2011"),optim.method = "BFGS",include.mean = F)
pvalue(model.opt_pm1)

forecast<-function(model,h)
{
  forecast<-array(0,dim=100)
  for(i in c(900:999))
  {
    mod<-arima(x=x1[1:(i-1)],order=ordre.opt,fixed=model$coef,include.mean = F)
    forecast[i-899]<-tail(predict(mod,n.ahead=h)$pred,1)
  }
  return(forecast)
}


prev<-forecast(model.opt,h=1)
  
par(mfrow=c(1,1))
plot(x1_b,type='l')
lines(prev,col='red')



prev<-lapply(c(1:10),forecast,model=model.opt)

x1_b-prev

erreur<-unlist(lapply(prev,function(x){mean((x-x1_b)^2)}))
plot(erreur,type='b')



################################################################################################
#########################exercice 2
################################################################################################
setwd("C:\\Enseignement\\2015-2016\\Serie_temp\\TP\\TP6_data\\")
data<-read.table(file='TP6_exercice2.txt',header=T,sep=';')
attach(data)

##############################################################model1
par(mfrow=c(1,1))
plot(x1)

par(mfrow=c(1,1))
acf(x1,lag.max=60) ####on constate une saisonnalité d'ordre 12
                   ####décroissance rapide vers 0 des ordres et des ordres k*12, pas de différenciation
s=12
par(mfrow=c(1,2))
acf(x1,lag.max=3*s)        ###qmax= 1, Qmax=2
pacf(x1,lag.max=3*s)       ###pmax= 2, Pmax=2


ordre<-expand.grid(p = c(0:2), q = c(0:1), P=c(0:2),Q=c(0:2))
ordre<-cbind(ordre[,1],0,ordre[,2],ordre[,3],0,ordre[,4])
dim(ordre)

sarima<-function(x,ordre,s)
{
  arima(x,order = ordre[1:3], seasonal = list(order = ordre[4:6], period = s),include.mean = F)
}

model.sarima<-apply(ordre,1,sarima,x=x1,s=12)
aic<-lapply(model.sarima,function(x) x$aic)%>%unlist
bic<-lapply(model.sarima,function(x) -2*x$loglik+x$nobs*length(x$coef))%>%unlist
like<-lapply(model.sarima,function(x) -2*x$loglik)%>%unlist

par(mfrow=c(1,1))
o<-order(aic)
plot(aic[o],type='b',pch=20,axes=F,xlab='')
axis(1,c(1:length(aic)),paste(ordre[o,1],ordre[o,3],ordre[o,4],ordre[o,6]),las=2)
axis(2)

ordre[which.min(aic),]
ordre[which.min(bic),]

model.sarima[[which.min(aic)]]$coef
pvalue(model.sarima[[which.min(aic)]])


model.sarima[[order(aic)[2]]]$coef
pvalue(model.sarima[[order(aic)[2]]])





##############################################################model2
plot(x2,type='l')

s=7
par(mfrow=c(1,2))
acf(x2,lag.max=3*s)        ###qmax= 3, Qmax=2
pacf(x2,lag.max=3*s)       ###pmax= 1, Pmax=1


ordre<-expand.grid(p = c(0:1), q = c(0:3), P=c(0:1),Q=c(0:2))
ordre<-cbind(ordre[,1],0,ordre[,2],ordre[,3],0,ordre[,4])
dim(ordre)



model.sarima<-apply(ordre,1,sarima,x=x2,s=7)
aic<-lapply(model.sarima,function(x) x$aic)%>%unlist
bic<-lapply(model.sarima,function(x) -2*x$loglik+x$nobs*length(x$coef))%>%unlist
like<-lapply(model.sarima,function(x) -2*x$loglik)%>%unlist

par(mfrow=c(1,1))
o<-order(aic)
plot(aic[o],type='b',pch=20,axes=F,xlab='')
axis(1,c(1:length(aic)),paste(ordre[o,1],ordre[o,3],ordre[o,4],ordre[o,6]),las=2)
axis(2)

ordre[which.min(aic),]

model.sarima[[which.min(aic)]]$coef
pvalue(model.sarima[[which.min(aic)]]) #####attention phi2 non signi. non-nulle à 5% on regarde le modèle suivant en AIC


model.sarima[[order(aic)[2]]]$coef
pvalue(model.sarima[[order(aic)[2]]])  ####ok

#etude des résidus

pvalue_BP(model.sarima[[order(aic)[2]]],K=10) #ok

par(mfrow=c(2,1))
acf(model.sarima[[order(aic)[2]]]$residuals)
pacf(model.sarima[[order(aic)[2]]]$residuals)


##############################################################model3

plot(x3,type='l')
par(mfrow=c(1,1))
acf(x3)            ####non stationnaire, différencier


x3.diff<-diff(x3,lag=1,differences = 1l)
acf(x3.diff)       #######décroissance rapide vers 0, ok
plot(x3.diff,type='l')


par(mfrow=c(1,2))
acf(x3.diff,lag.max=20)        ###qmax= 4
pacf(x3.diff,lag.max=20)       ###pmax= 5

ordre<-expand.grid(p = c(0:5), q = c(0:4))
dim(ordre)
ordre<-cbind(ordre[,1],1,ordre[,2])
             
model<-apply(ordre,1,arima,x=x3,method = c("ML"),SSinit = c("Rossignol2011"),optim.method = "BFGS",include.mean = F)
aic<-lapply(model,function(x) x$aic)%>%unlist

par(mfrow=c(1,1))
o<-order(aic)
plot(aic[o],type='b',pch=20,axes=F,xlab='')
axis(1,c(1:length(aic)),paste(ordre[o,1],ordre[o,3]),las=2)
axis(2)

ordre[which.min(aic),]

model[[which.min(aic)]]$coef
pvalue(model[[which.min(aic)]])

acf(model[[which.min(aic)]]$residuals)
qqnorm(model[[which.min(aic)]]$residuals)


####portemanteau pierce test

K<-10
pvalue_BP(model[[which.min(aic)]],K)







# ########################################################
# ##############préparation du jeux de données
# ########################################################
# set.seed(104)
# n<-200
# sigma<-1
# #sigma<-1/2
# eps<-rnorm(n,0,sd=sigma)
# x1<-arima.sim(n = n,list(order = c(0,0,12), ma = c(0.5,rep(0,10),0.9)),innov=eps)
# 
# 
# set.seed(104)
# n<-200
# sigma<-1
# #sigma<-1/2
# eps<-rnorm(n,0,sd=sigma)
# x2<-arima.sim(n = n, list(ar =c(.6,rep(0,5),.5,-.30),innov=eps))
# 
# 
# set.seed(104)
# n<-200
# sigma<-1
# #sigma<-1/2
# eps<-rnorm(n,0,sd=sigma)
# x3<-arima.sim(n = n, list(order=c(1,1,1), ar =c(.6),ma= 0.9),innov=eps)
# x3<-x3[-1]
# 
# 
# exercice2<-data.frame(x1,x2,x3)
# setwd("C:\\Enseignement\\2015-2016\\Serie_temp\\TP\\TP5_data\\")
# write.table(exercice2,file='TP6_exercice2.txt',col.names=T,row.names=F,quote=F,sep=';')
# # 




