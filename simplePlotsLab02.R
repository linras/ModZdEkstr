#Proste wykresy
#==============
#Na ponizszych przykladach, sprawdz  dzialanie funkcji plot(). 

#plot()
plot(1:5,c(1,-1,1,-1,1))
plot(1:5,c(1,-1,1,-1,1),ylim=c(-2,2))    #zmieniamy zakres osi oy
plot(1:5,c(1,-1,1,-1,1),ylim=c(-2,2),type='l',col='blue')
#sprawdz type: 'b' oraz 's'
x=c(1:4,rev(1:4))
y=c(3,2,2,3,3,4,4,3)
plot(x,y,type='l',lwd=4,col='red')   #lwd (line width)
plot(x,y,type='l',lwd=4,col='red',xlab=NA,ylab='')  #xlab(labels) - etykiety

x=seq(-2,2,by=0.01)
plot(x,x^2)
plot(x,x^2,type='l')
abline(1,-2)         #abline(a,b) dorysowuje do wykresu proste y=bx+a
abline(h=2,col='red') #h-horizontal, v-vertical

#Na ponizszych przykladach, sprawdz  dzialanie funkcji curve(). 
curve(x^2,xlim=c(-2,2))
curve(-x^2+3,col='red',add=T)  #add=TRUE pozwala dorysowac kolejny wykres
curve(sin(4*x)+1.5,lwd=3,lty=3,col='blue',add=T) 
#lty (line type), sprawdz 0,1,2,3,4

#w przypadkku gestosci, dystrybuant - zamiast wpisywania wzoru funkcji
#wygodniej jest uzywac gotowych funkcji np. dnorm, pnorm
curve(dnorm(x,0,2), xlim=c(-8,8), col='blue')


#==============================================================
#Zadania B    Zadania B    Zadania B    Zadania B    Zadania B
#==============================================================
#Zadanie 4.
#----------


#a) Narysuj wykresy g¦sto±ci rozkaadów N(1,1), N(4,1)  w jednym rz¦dzie i w
#drugim N(0,4), N(0,9). U»yj okna kilku wykresów par(mfrow=c(n,m)

par(mfrow=c(2,2))
#plot(x,x^2,type='l')
curve(dnorm(x,1,2), xlim=c(-8,8), col='blue')
curve(dnorm(x,4,1), xlim=c(-8,8), col='green')
curve(dnorm(x,0,4), xlim=c(-8,8), col='red')
curve(dnorm(x,0,9), xlim=c(-8,8), col='yellow')

#b)
par(mfrow=c(1,1))

curve(dnorm(x,1,2), xlim=c(-8,8), col='blue')
curve(dnorm(x,4,1), xlim=c(-8,8), col='green',add=T)
curve(dnorm(x,0,4), xlim=c(-8,8), col='red',add=T)
curve(dnorm(x,0,9), xlim=c(-8,8), col='yellow',add=T)

#-------------------------------------------------------------
#Jesli chcemy wygenerowac probe losowa z rozkladu, robimy to funkcja 
#rozpoczynajaca sie od "r" (random), a nastepnie  skrot nazwy rozkladu.

#Przyklad  (Rozklad log-normalny)
X=rnorm(1000)    #generowanie proby z rozkladu N(0,1)
Y=exp(X)         #proba z rozkladu LN(0,1)
hist(Y,100)          #histogram - na osi oy licznosci
hist(Y,100,prob=T)   #histogram - na osi oy czestosci
curve(dlnorm(x),add=T,col='blue')  #dorysowujemy gestosc 
#teoretyczna rozkladu LN(0,1)
#-------------------------------------------------------------
#Zadanie 5.
#----------

#Wygeneruj prób¦ 1000-elementow! X z rozkaadu N(0, 16). Narysuj na jednym
#rysunku histogram z próby Y = exp(X) oraz g¦sto± teoretyczn! rozkaadu lognormalnego LN(0, 16)

X=rnorm(1000, 16)
Y=exp(X)
hist(Y,100)
hist(Y,100,prob=T)
curve(dnorm(x,0,16),add=T,col='red')

#-------------------------------------------------------
#Przyklad. (Rozklad chi-kwadrat o n=10 stopniach swobody)
m <- 1000
n <- 10
Y <- c()             
for (i in 1:m) { 
  X <- rnorm(n)
  Y[i] <- sum(X^2)
}                 

hist(Y,prob=T)            #histogram Y
curve(dchisq(x,10),xlim=c(0,30), add=T,col='blue') #rzeczywista gestosc

#Mozemy porownac  srednia  z proby z rzeczywista wartosci¹ oczekiwana: Ex=10
#Dorysujemy je na wykresie.
points(mean(Y),0, pch=19,col=2); mean(Y)
#--------------------------------------------------------
#Zadanie 6
#--------

m <- 1000
n <- 100
Y <- c()             
for (i in 1:m) { 
  X <- rnorm(n)
  Y[i] <- sum(X^2)
}                 

hist(Y,prob=T)            #histogram Y
curve(dchisq(x,100), add=T,col='blue') #rzeczywista gestosc

points(mean(Y),0, pch=19,col=2); mean(Y)

#Zadanie 7
#----------
#Dla rozkladu t-Studenta mamy odpowiednio: dt( ,n), pt( ,n), rt( , ,n) gdzie n stopnie swobody
X <- rnorm(100)
ft <- fitdistrplus::fitdist(X, "norm"); ft
m<- ft$estimate[[1]]
s<- ft$estimate[[2]]

curve(dt(x, 4), col = "sky blue", from = -10, to = 10, add = F, lwd = 2) #stud
curve(dnorm(x, m, s), from = -10, to = 10, col='green', add=T)   # stud z wykladu
curve(dnorm(x), xlim=c(-8,8), from = -10, to = 10, col='blue', add=T)   # N(0,1

#----------------------------------------------
#Przyklad. (replicate() zamiast loop())

#Generujemy 100 dziesiêcio-elementowych  probek z rozkladu Exp(1/50)
#Mozec myslec, ze sa to 10-osobowe grupy z populacji 50-latkow 
D <- replicate(100,rexp(10,1/50))
#Co otrzymamy? Macierz o wymiarze 10x100.
head(D); class(D); dim(D)
#Mozemy obliczyc srednia, mediane dla kazdej z grup (z wierszy 1 zamiast 2)
m <- apply(D,2,mean)
tail(m); class(m)

Me <- apply(D,2,median)
tail(Me); class(Me)

#Wyniki na wykresie (histogram, gestosc).
par(mfrow=c(1,2))
hist(m,prob=T)
points(c(mean(m),50),c(0,0), pch=19, col=c("blue", "red"))
hist(Me,prob=T)
points(c(mean(Me),log(2)*50),c(0,0), pch=19, col=c("blue", "red"))

#Jesli chcemy tylko histogramy, mozemy zrobic to tak (zwiekszamy liczbe generowanych prob probek)
par(mfrow=c(1,2))
hist(replicate(10000, mean(rexp(100,1/50))), prob=T)
hist(replicate(10000, median(rexp(100,1/50))), prob=T)



#Zadanie 8
#---------


