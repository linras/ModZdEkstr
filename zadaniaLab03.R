
library(fitdistrplus)

X=rexp(100)
curve(dexp(x),col='blue',xlim=c(0,10))
points(X,rep(0,100),col=2,pch=19)

#####################################################################################################################

hist(X,breaks=7,prob=T)
curve(dexp(x),col=2)
points(X,rep(0,100),col=2,pch=19)

#####################################################################################################################

plot(ecdf(c(1,2,3)))

#####################################################################################################################

plot(ecdf(X))
curve(pexp(x),col=2,lwd=2,add=T)
points(X,rep(0,100),col=2,pch=19)

#####################################################################################################################

alpha=ppoints(30) # sprawdz co robi ppoints
qqplot(qexp(alpha),X)
qqline(distribution=qexp,X,col=2,lwd=2)

#####################################################################################################################

X <- rexp(100,1)
fw<- fitdist(X,"exp") 
lambda <- fw$estimate[[1]]
hist(X,prob=T)
curve(dexp(x,lambda), add=T, col=2)
points(X, rep(0,100))

alpha=ppoints(30)
#my.qweibull <- function(x) qweibull(x, 2, 5)
Q<-qexp(alpha,1)
qqplot(Q,X)
qqline(distribution=qexp, X) # lub qexp(x,1) -> 1 lub próba
#qqline(distribution=my.qweibull, X) # alternatywnie do tego wy¿ej

#####################################################################################################################
#    Zadanie 1 i 3
Z <- c(0.26, 0.83, 6.04, 1.74, 0.18, 3.37, 3.60, 0.12, 1.23, 2.08)

fw<- fitdist(Z,"exp") 
lambda <- fw$estimate[[1]]

hist(X,prob=T)
curve(dexp(x,lambda), add=T, col=2)
points(X, rep(0,100))

#####################################################################################################################
#    Zadanie 2 i 3
#qqplot(X,Z) X-dane rozk³adu teoretycznego Z-dane testowe
Z <- c(0.26, 0.83, 6.04, 1.74, 0.18, 3.37, 3.60, 0.12, 1.23, 2.08)

fw <- fitdist(Z, "weibull")
fe <- fitdist(Z, "exp")
lambda <- fw$estimate[[1]]
lambdaExp <- fe$estimate[[1]]
hist(Z,prob=T)
curve(dexp(x,lambda), add=T, col=2)
curve(dexp(x,lambdaExp), add=T, col="green")
points(X, rep(0,100))



#####################################################################################################################
#weibull.plot(Z, x, l.type = 1:3, l.col = c("blue", "red"))
D <- dweibull(Z, 1, scale = 1, log = FALSE)
hist(D,prob=T)
hist(Z,prob=T)

Q<-qexp(Z,1)
qqplot(Q,X)
qqline(distribution=qexp, X)

#####################################################################################################################
