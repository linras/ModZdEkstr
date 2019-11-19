#install.packages("fitdistrplus")
library(fitdistrplus)
#==========
#Przykład 1. (Centralne Twierdzenie Graniczne)
#==========
#Losujemy  n liczb z rozkladu wykladniczego, sumujemy je - 
#powtarzamy to wiele razy. Analizujemy otrzymane wyniki 
#pod  pod katem normalnosci rozkladu.

#proby 30-elementowe 
m <- 500  #liczba powtorzen
n <- 30

lambda <- 2 

S <- replicate(m, sum(rexp(n,rate=lambda)))

#estymujemy parametry rozkladu normalnego
fit <- fitdist(S,"norm")

#oceniamy dopasowanie na podstawie wykresow
par(mfrow=c(2,2))
plot.legend <- c("normal")
denscomp(fit, legendtext = plot.legend)
qqcomp(fit, legendtext = plot.legend)
cdfcomp(fit, legendtext = plot.legend)
ppcomp(fit, legendtext = plot.legend)

#Oceniamy na podstawie testu Shapiro-Wilka
#p-value > 0.05 nie ma podstaw do odrzucenia hipotezy zerowej
#H0: rozklad jest normalny (ten z ktorego pochodzi proba)

shapiro.test(S)
#============================================================
#Zadanie 1.
#============
#Powtorz Przyklad 1. dla prob o licznosci n=100 i n=200 
#(mozesz zwiekszyc liczbe powtorzen m).
#proby 100-elementowe 
m <- 500  #liczba powtorzen
n <- 100

lambda <- 2 

S <- replicate(m, sum(rexp(n,rate=lambda)))

#estymujemy parametry rozkladu normalnego
fit <- fitdist(S,"norm")

#oceniamy dopasowanie na podstawie wykresow
par(mfrow=c(2,2))
plot.legend <- c("normal")
denscomp(fit, legendtext = plot.legend)
qqcomp(fit, legendtext = plot.legend)
cdfcomp(fit, legendtext = plot.legend)
ppcomp(fit, legendtext = plot.legend)

##################################################################################################################
#proby 200-elementowe 
m <- 500  #liczba powtorzen
n <- 200

lambda <- 2 

S <- replicate(m, sum(rexp(n,rate=lambda)))

#estymujemy parametry rozkladu normalnego
fit <- fitdist(S,"norm")

#oceniamy dopasowanie na podstawie wykresow
par(mfrow=c(2,2))
plot.legend <- c("normal")
denscomp(fit, legendtext = plot.legend)
qqcomp(fit, legendtext = plot.legend)
cdfcomp(fit, legendtext = plot.legend)
ppcomp(fit, legendtext = plot.legend)


#Zadanie 2.
#==========
#Powtorz Przyklad 1. dla prob o licznosci n=30,100,200
#z rozkladow 
#a) U(0,1) - jednostajny na odcinku (0,1)
#b) LN(0,1) - lognormalny
#c) t(4) - t-Studenta
#d) wybierz jakis swoj ulubiony rozklad:) np fitW <- fitdist(S,"weibull")

#Narysujmy gestosci rozkladow z ktorych bedziemy generowali probki.
par(mfrow=c(2,2))
curve(dunif(x,0,1), col='blue')
curve(dlnorm(x,0,1), xlim=c(0,8), col='blue')
curve(dt(x,4), xlim=c(-5,5),  col='blue')
#dopisz Twoj ulubiony

#b)
#proby 30-elementowe 
m <- 500  #liczba powtorzen
n <- 30
lambda <- 2 
S <- replicate(m, sum(rexp(n,rate=lambda)))
fit <- fitdist(S,"lnorm")
#Narysujmy gestosci rozkladow z ktorych bedziemy generowali probki.
par(mfrow=c(2,2))
curve(dunif(x,0,1), col='blue')
curve(dlnorm(x,0,1), xlim=c(0,8), col='blue')
curve(dt(x,4), xlim=c(-5,5),  col='blue')
curve(dexp(x,2), xlim=c(0,8), col='blue')

#proby 100-elementowe 
m <- 500  #liczba powtorzen
n <- 100
lambda <- 2 
S <- replicate(m, sum(rexp(n,rate=lambda)))
fit <- fitdist(S,"lnorm")
#Narysujmy gestosci rozkladow z ktorych bedziemy generowali probki.
par(mfrow=c(2,2))
curve(dunif(x,0,1), col='blue')
curve(dlnorm(x,0,1), xlim=c(0,8), col='blue')
curve(dt(x,4), xlim=c(-5,5),  col='blue')
curve(dexp(x,2), xlim=c(0,8), col='blue')

#proby 200-elementowe 
m <- 500  #liczba powtorzen
n <- 30
lambda <- 2 
S <- replicate(m, sum(rexp(n,rate=lambda)))
fit <- fitdist(S,"lnorm")
#Narysujmy gestosci rozkladow z ktorych bedziemy generowali probki.
par(mfrow=c(2,2))
curve(dunif(x,0,1), col='blue')
curve(dlnorm(x,0,1), xlim=c(0,8), col='blue')
curve(dt(x,4), xlim=c(-5,5),  col='blue')
curve(dexp(x,2), xlim=c(0,8), col='blue')

##############################################################################################################

#c)
#proby 30-elementowe 
m <- 500  #liczba powtorzen
n <- 30
lambda <- 2 
S <- replicate(m, sum(rexp(n,rate=lambda)))
fit <- fitdist(S,"lnorm")
#Narysujmy gestosci rozkladow z ktorych bedziemy generowali probki.
par(mfrow=c(2,2))
curve(dunif(x,0,1), col='blue')
curve(dlnorm(x,0,1), xlim=c(0,8), col='blue')
curve(dt(x,4), xlim=c(-5,5),  col='blue')
curve(dexp(x,2), xlim=c(0,8), col='blue')

#proby 100-elementowe 
m <- 500  #liczba powtorzen
n <- 100
lambda <- 2 
S <- replicate(m, sum(rexp(n,rate=lambda)))
fit <- fitdist(S,"lnorm")
#Narysujmy gestosci rozkladow z ktorych bedziemy generowali probki.
par(mfrow=c(2,2))
curve(dunif(x,0,1), col='blue')
curve(dlnorm(x,0,1), xlim=c(0,8), col='blue')
curve(dt(x,4), xlim=c(-5,5),  col='blue')
curve(dexp(x,2), xlim=c(0,8), col='blue')

#proby 200-elementowe 
m <- 500  #liczba powtorzen
n <- 30
lambda <- 2 
S <- replicate(m, sum(rexp(n,rate=lambda)))
fit <- fitdist(S,"lnorm")
#Narysujmy gestosci rozkladow z ktorych bedziemy generowali probki.
par(mfrow=c(2,2))
curve(dunif(x,0,1), col='blue')
curve(dlnorm(x,0,1), xlim=c(0,8), col='blue')
curve(dt(x,4), xlim=c(-5,5),  col='blue')
curve(dexp(x,2), xlim=c(0,8), col='blue')

##############################################################################################################

#d)
#proby 30-elementowe 
m <- 500  #liczba powtorzen
n <- 30
lambda <- 2 
S <- replicate(m, sum(rexp(n,rate=lambda)))
fit <- fitdist(S,"weibull")
#Narysujmy gestosci rozkladow z ktorych bedziemy generowali probki.
par(mfrow=c(2,2))
curve(dunif(x,0,1), col='blue')
curve(dlnorm(x,0,1), xlim=c(0,8), col='blue')
curve(dt(x,4), xlim=c(-5,5),  col='blue')
curve(dexp(x,2), xlim=c(0,8), col='blue')

#proby 100-elementowe 
m <- 500  #liczba powtorzen
n <- 100
lambda <- 2 
S <- replicate(m, sum(rexp(n,rate=lambda)))
fit <- fitdist(S,"weibull")
#Narysujmy gestosci rozkladow z ktorych bedziemy generowali probki.
par(mfrow=c(2,2))
curve(dunif(x,0,1), col='blue')
curve(dlnorm(x,0,1), xlim=c(0,8), col='blue')
curve(dt(x,4), xlim=c(-5,5),  col='blue')
curve(dexp(x,2), xlim=c(0,8), col='blue')

#proby 200-elementowe 
m <- 500  #liczba powtorzen
n <- 30
lambda <- 2 
S <- replicate(m, sum(rexp(n,rate=lambda)))
fit <- fitdist(S,"weibull")
#Narysujmy gestosci rozkladow z ktorych bedziemy generowali probki.
par(mfrow=c(2,2))
curve(dunif(x,0,1), col='blue')
curve(dlnorm(x,0,1), xlim=c(0,8), col='blue')
curve(dt(x,4), xlim=c(-5,5),  col='blue')
curve(dexp(x,2), xlim=c(0,8), col='blue')

##############################################################################################################

#==========
#Przykład 2. (Centralne Twierdzenie Graniczne)
#==========
#Losujemy  n liczb z rozkladu wykladniczego, sumujemy je,
#nastepnie standaryzujemy - powtarzamy to wiele razy. 
#Analizujemy otrzymana probe - czy pochodzi z rozkladu N(0,1).

#proby 30-elementowe 
m <- 500  #liczba powtorzen
n <- 1000

lambda <- 2 

Y <- replicate(m, sum(rexp(n,rate=lambda)))

#standaryzujemy
m <- n*(1/lambda)
s <- sqrt(n *(1/lambda^2))

S <- (Y-m)/s

#wykresy
par(mfrow=c(2,2))
hist(S,prob=T)
curve(dnorm(x,0,1), col=2, add=T)

plot(ecdf(S))
curve(pnorm(x,0,1), col=2, add=T)

qqnorm(S)
qqline(S, distribution = qnorm, col=2)

#Oceniamy na podstawie testu Shapiro-Wilka
#p-value > 0.05 nie ma podstaw do odrzucenia hipotezy zerowej
#H0: rozklad jest normalny (ten z ktorego pochodzi proba)
shapiro.test(S)
#================================================================
#Zadanie 3
#=========
#Powtorz Przyklad 2 dla rozkladow z Zadania 2.
#Dla tych rozkladow wyszukaj teoretyczne wzory na
#u --  wartosc oczekiwana
#w -- wariancje
#Nastepnie zrob standaryzacje
#S=(Y-m)/s
#gdzie m=n*u, s^2= n*w
#i przeprowadz dalsza analize.

#c
#proby 30-elementowe 
m <- 500  #liczba powtorzen
n <- 30
lambda <- 2 

Y <- replicate(m, sum(rexp(n,rate=lambda)))

#standaryzujemy
m <- n*(1/lambda)
s <- sqrt(n *(1/lambda^2))

S <- (Y-m)/s
S <- replicate(m, sum(rexp(n,rate=lambda)))
fit <- fitdist(S,"lnorm")
#Narysujmy gestosci rozkladow z ktorych bedziemy generowali probki.
par(mfrow=c(2,2))
curve(dunif(x,0,1), col='blue')
curve(dlnorm(x,0,1), xlim=c(0,8), col='blue')
curve(dt(x,4), xlim=c(-5,5),  col='blue')
curve(dexp(x,2), xlim=c(0,8), col='blue')

#proby 100-elementowe 
m <- 500  #liczba powtorzen
n <- 100
lambda <- 2 
S <- replicate(m, sum(rexp(n,rate=lambda)))
fit <- fitdist(S,"lnorm")
#Narysujmy gestosci rozkladow z ktorych bedziemy generowali probki.
par(mfrow=c(2,2))
curve(dunif(x,0,1), col='blue')
curve(dlnorm(x,0,1), xlim=c(0,8), col='blue')
curve(dt(x,4), xlim=c(-5,5),  col='blue')
curve(dexp(x,2), xlim=c(0,8), col='blue')

#proby 200-elementowe 
m <- 500  #liczba powtorzen
n <- 30
lambda <- 2 
S <- replicate(m, sum(rexp(n,rate=lambda)))
fit <- fitdist(S,"lnorm")
#Narysujmy gestosci rozkladow z ktorych bedziemy generowali probki.
par(mfrow=c(2,2))
curve(dunif(x,0,1), col='blue')
curve(dlnorm(x,0,1), xlim=c(0,8), col='blue')
curve(dt(x,4), xlim=c(-5,5),  col='blue')
curve(dexp(x,2), xlim=c(0,8), col='blue')

##############################################################################################################
