##Exo1 

#library(lattice)
#library(MASS)
library(forecast)
library(dygraphs)
library(zoo)
library(timeDate)
library(xts)
library(stats)
library(mgcv)
##############
n = 100 
t <- c(1:n)
#Modeles  :

eps = rnorm(n,0,1)
X1 = eps
X2 = t/5 + eps
X3 = t/5 + 5 *cos(2*t*pi/10) + eps

plot(X1,type='l',ylim = range(X1,X2,X3))
lines(X2,lty = 2)
lines(X3, lty = 3)
#LissageSimple :


lissage_simple = function(alpha,x)
  { 
  xsmooth =x # Y_chap dans le cours
  for(i in c(2:length(x)))
  { 
    xsmooth[i] <- (1-alpha)*xsmooth[i-1]+alpha*x[i]
    } 
  return(xsmooth) 
}

#Prevoir un bruit blanc ==> par moyenne ==> pas de composante determ. donc on prend alpha --> 0 

X1.smooth <- lissage_simple(0.01,X1) ## Alpha ---> 1 ==> On s'attache le plus possible à la val prec. 
plot(X1,type = 'l')
lines(X1.smooth,col = 'blue')



#Composante deterministe ==> prend la valeur qui prec. une bonne stategie !!

X3.smooth <- lissage_simple(0.8,X3) 
plot(X3,type = 'l')
lines(X3.smooth,col = 'blue')

#######calculer prev. Pour determiner alpha qui minimise l'erreur  !!!


plot(tail(X1,n-1),type = 'l')
lines(head(X1.smooth,n-1),col = 'red')
mean(tail(X1,n-1)-head(X1.smooth,n-1)^2)


alpha <- seq(0.05,0.95,length=100)
forcast <- lapply(alpha,lissage_simple,x=X1)


erreur <- unlist(lapply(forcast, function(x){mean((tail(X1,n-1)-head(x,n-1))^2)}))

plot(alpha,erreur,type = 'l')

X1.smooth <- lissage_simple(alpha[which.min(erreur)],X1)
plot(X1,type ='l')
lines(X1.smooth,col='red')

####bor.

Mean = 0*length(X1)

for(i in 1:100)
{ 
  y = lissage_simple(alpha[i],X1)
  Mean[i] = mean((tail(X1,n-1)-head(y,n-1))^2)
  }

i <- which.min(Mean)

plot(Mean)

####################################
#beta pour la ponte dans Lissage de Holt-Winters


lissage_double = function (alpha,x)
  
{ 
  xsmooth =x # Y_chap dans le cours
 
  # l <- array(x(1, dim = length(x))
  # b = array(x(2)-x(1),dim = length(x)) à utliser avec ça !!!
  
  lt = 0*length(x)
  bt = 0*length(x)
  
  for(i in c(2:length(x)))
  { 
    lt[i] <- (1-(1-alpha)^2)*x[i]+(1-alpha)^2*(lt[i-1]+bt[i-1])
    bt[i] <- (alpha^2)*x[i]-alpha^2*lt[i]+2*alpha*(1-alpha)*bt[i-1]
      xsmooth[i] <- lt[i]+bt[i]
  } 
  
  res <- list()
  # res$smooth <- xsmooth
  # res$l <- l
  # res$b <- b
  # return (res)
  
  
  return(xsmooth) 
}

#######Test :

X2.double = lissage_double(0.2,X2)
plot(X2,type='l')
lines(X2.double,col = 'red')

####Erreur et Alpha qui minimise l'erreur : 


plot(tail(X2,n-1),type = 'l')
lines(head(X2.smooth,n-1),col = 'red')
mean(tail(X2,n-1)-head(X2.smooth,n-1)^2)


alpha <- seq(0.05,0.95,length=100)
forcast <- lapply(alpha,lissage_double,x=X2)
erreur <- unlist(lapply(forcast, function(x){mean((tail(X2,n-1)-head(x,n-1))^2)}))

plot(alpha,erreur,type = 'l')

X2.smooth <- lissage_double(alpha[which.min(erreur)],X2)
plot(X2,type ='l')
lines(X2.smooth,col='red')

###################

holt_winters = function (alpha,beta,x)
{
   xsmooth =x
   lt <- array(x[1], dim = length(x))
   bt <-  array(x[2]-x[1],dim = length(x)) 
 
  for(i in c(2:length(x)))
  { 
    lt[i] <- (alpha)*x[i]+(1-alpha)^2*(lt[i-1]+bt[i-1])
    bt[i] <- beta*(lt[i]-lt[i-1])+(1-beta)*bt[i-1]
    xsmooth[i] <- lt[i]+bt[i]
  } 
  
  res <- list()
  res$hw <- xsmooth
  res$l <- lt
  res$b <- bt
  return (res)
}

alpha =0.1
beta = 0.3

X1.hw <- holt_winters(alpha,beta,X1)
plot(X1,type ='l')
lines(unlist(X1.hw$hw),col='red') 


##### Optimisation de alpha et beta ( on peut rajouter par exemple l'hyp que alpha et beta sont lies !!!)




#### Predict : dans le cas de lissage double !!(quest d'avant)

predict.lissage <- function(Xsmooth,inst,horizon)
{ 
  
  n <- length(Xsmooth$hw)
  # n-horizon = inst à remplacer 
  
  prev <- c(Xsmooth$hw[1:inst],Xsmooth$l[inst]+Xsmooth$b[inst]*c(1:horizon))
  return (prev)
}

Y <- predict.lissage(X1.hw,80,20)

lines(Y,col='blue')

  # { } 
  #[ ]
  # ~


############ CAC40 #############
cac.ts=EuStockMarkets[,"FTSE"]


#### pREVISION AVEC Holt Winters 

Cac = HoltWinters(cac.ts,gamma= NULL)

cac.predict <- forecast.HoltWinters(Cac,200)

plot(cac.predict)

