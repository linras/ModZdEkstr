#Analiza maksimów

#==========
#Zadanie 1
#==========
#Narysujmy gestosci rozkladow z ktorych bedziemy generowali probki.
plot(function(x) dnorm(x),-4,4,
     ylim=c(0,0.41),
     type='l',lwd='2',col='blue',
     xlab=NA,ylab=NA)
curve(dunif(x,-2,2),-2,2,
      type='l',lwd='2',col='green', 
      add=T)
curve(dt(x,4,0),type='l',
      lwd='2',col='red', 
      add=T)

#generowanie prob
m <- 1000
n <- 300
a <- -2         #parametry rozkładu jednostajnego
b <- 2
v <- 4          #parametr rozkladu t-Studenta
Mu <- replicate(m, max(runif(n,-2,2)))
Mn <- replicate(m, max(rnorm(n)))
Mt <- replicate(m, max(rt(n,4,0)))

Mu=as.numeric(Mu)
Mn=as.numeric(Mn)
Mt=as.numeric(Mt)

#obejrzyjmy wyniki na histogramach
par(mfrow=c(3,1))
hist(Mu,prob=TRUE)
hist(Mn,prob=TRUE)
hist(Mt,prob=TRUE)

#Dopasujemy rozkład GEV do maksimow.
#Uzyjemy funkcji 'gev' z  library(evir).
library(evir)
fitu=gev(Mu) #xi=-1 (SColes p.52), dla tych rozkładow znamy wartosci wspołczynnika xi 
fitn=gev(Mn) #xi=0 (SColes p.52)
fitt=gev(Mt)  #xi=0.25

#Wydobycie dopasowanych parametrow (xi,sigma,mu).
paru=c(fitu$par.ests[[1]],fitu$par.ests[[2]],fitu$par.ests[[3]])
parn=c(fitn$par.ests[[1]],fitn$par.ests[[2]],fitn$par.ests[[3]])
part=c(fitt$par.ests[[1]],fitt$par.ests[[2]],fitt$par.ests[[3]])
paru; parn; part

#Histogramy i  dopasowane gestosci GEV 
#podajemy parametry w kolejnosci (xi,mu,sigma)!
par(mfrow=c(3,1))
hist(Mu,prob=TRUE)
curve(dgev(x,paru[1],paru[3],paru[2]),add=T)
hist(Mn,prob=TRUE)
curve(dgev(x,parn[1],parn[3],parn[2]),add=T)
hist(Mt,prob=TRUE)
curve(dgev(x,part[1],part[3],part[2]),add=T)

#Dystrybuanty empiryczne i dopasowane 
#podajemy parametry w kolejnosci (xi,mu,sigma)!
par(mfrow=c(3,1))
plot(ecdf(Mu))
curve(pgev(x,paru[1],paru[3],paru[2]),add=T,col=2)
plot(ecdf(Mn))
curve(pgev(x,parn[1],parn[3],parn[2]),add=T,col=2)
plot(ecdf(Mt))
curve(pgev(x,part[1],part[3],part[2]),add=T,col=2)

#==========
#Zadanie 2.  (patrz 04GEV_slajdy.pdf)
#==========

X <- rnorm(3000,40,10)
Y <- rt(3000,10)

#Narysujmy gestosci rozkladow z ktorych bedziemy generowali probki.

curve(dnorm(x,40,100), xlim=c(-40,100), col='blue')

curve(dt(x,10),type='l',
      lwd='2',col='red', 
      add=F)

#generowanie prob
m <- 1000
n <- 300
a <- -2         #parametry rozkładu jednostajnego
b <- 2
v <- 4          #parametr rozkladu t-Studenta
Mu <- replicate(m, max(runif(n,-2,2)))
Mn <- replicate(m, max(rnorm(n)))
Mt <- replicate(m, max(rt(n,4,0)))

Mu=as.numeric(Mu)
Mn=as.numeric(Mn)
Mt=as.numeric(Mt)

#obejrzyjmy wyniki na histogramach
par(mfrow=c(3,1))
hist(Mu,prob=TRUE)
hist(Mn,prob=TRUE)
hist(Mt,prob=TRUE)

#Dopasujemy rozkład GEV do maksimow.
#Uzyjemy funkcji 'gev' z  library(evir).
library(evir)
fitu=gev(Mu) #xi=-1 (SColes p.52), dla tych rozkładow znamy wartosci wspołczynnika xi 
fitn=gev(Mn) #xi=0 (SColes p.52)
fitt=gev(Mt)  #xi=0.25

#Wydobycie dopasowanych parametrow (xi,sigma,mu).
paru=c(fitu$par.ests[[1]],fitu$par.ests[[2]],fitu$par.ests[[3]])
parn=c(fitn$par.ests[[1]],fitn$par.ests[[2]],fitn$par.ests[[3]])
part=c(fitt$par.ests[[1]],fitt$par.ests[[2]],fitt$par.ests[[3]])
paru; parn; part

#Histogramy i  dopasowane gestosci GEV 
#podajemy parametry w kolejnosci (xi,mu,sigma)!
par(mfrow=c(3,1))
hist(Mu,prob=TRUE)
curve(dgev(x,paru[1],paru[3],paru[2]),add=T)
hist(Mn,prob=TRUE)
curve(dgev(x,parn[1],parn[3],parn[2]),add=T)
hist(Mt,prob=TRUE)
curve(dgev(x,part[1],part[3],part[2]),add=T)

#Dystrybuanty empiryczne i dopasowane 
#podajemy parametry w kolejnosci (xi,mu,sigma)!
par(mfrow=c(3,1))
plot(ecdf(Mu))
curve(pgev(x,paru[1],paru[3],paru[2]),add=T,col=2)
plot(ecdf(Mn))
curve(pgev(x,parn[1],parn[3],parn[2]),add=T,col=2)
plot(ecdf(Mt))
curve(pgev(x,part[1],part[3],part[2]),add=T,col=2)

#==========
#Zadanie 3.  
#==========
#Danae sa duze proby X i Y (ponizej), z rozkladow $N(40,10^2)$ i $t(10,37)$
X <- rnorm(3000,40,10)
Y <- rt(3000,10)

#a) Interesuje nas rozklad maksimow z 30-elementowych blokow.
#Zrob analize jak w Zadani 2 nie wyznaczajac wczesniej 
#maksimow - korzystając tylko z mozliwosci funkcji gev()
# z pakietu evir (?gev).

#b) Wyestymuj parametry GEV korzystajac tym razem z funkcji
#gev.fit() w pakiecie ismev. Przeanalizuj dobroc dopasowania
#korzystajac z wykresow diagnostycznych jakie 
#daje funkca gev.diag()w tym pakiecie.

#============
#Zadanie 4.
#============
#Dla danych z Zadania 2. wyznacz poziom zwrotu  20 i 50. 
#Skorzystaj z funkcji rlevel.gev() w pakiecie evir.
#Zinterpretuj uzyskane wyniki.

#============
#Zadanie 5. 
#============
#Dla wybranych przez siebie danych, przeprowadź powyzsze 
#analizy dla maksimów miesiecznych i rocznych.
#Wykorzystaj funkcie gevFit(), summary() i gevrlevelPlot()
#z pakietu fExtremes.



