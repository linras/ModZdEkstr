# set working directory
path_loc <- "C:/repo/ModZdEkstr/Analiza02"
setwd(path_loc)
# reading in the data
df <- read_csv("waves20months.csv") ; df

# taking a quick look
glimpse(df)

# retype to numeric
df$Hmax <- as.numeric(df$Hmax)

data <- df$Hmax

u=quantile(data,0.90)

#wykresy rozrzutu z zaznaczonym progiem u 
plot(data)
abline(h=u,lwd=3,col='red')   

#nadwyzki nad prog u
Y=data[data>u]-u
plot(Y,type='h')

#estymujemy parametry rozkladu GPD
#gpd(dane,u) - podajemy prog lub liczbe nadwyzek
fitGPD=ismev::gpd.fit(data,u)   #u=kwantyl 90%, 
#można podać liczbę nadwyżek, tutaj 0.10*length(data)

#wyestymowane parametry rozkladu GPD
xi=fitGPD$mle[[2]]
beta=fitGPD$mle[[1]]


#dobroc dopasowania  na wykresach 
hist(Y,prob=TRUE)                                    #histogram nadwyżek
curve(evir::dgpd(x,xi,0,beta),col='red',lwd=2,add=T) #gęstość rozkładu GPD

qqplot(Y,evir::qgpd(ppoints(1000),xi,0,beta))   #QQ-ploty
qqline(Y,distribution=function(x) evir::qgpd(x,xi,0,beta),
       prob=c(0.25,0.75), col=2)

#dobroc dopasowania  na wykresach 
ismev::gpd.diag(fitGPD)

Nu=length(Y)   #licznosc nadwyzek
N=length(data)    #licznosc probki

k=20
x20=u+((k*Nu/N)^xi-1)*beta/xi #jesli przyjmiemy, ze xi rozne od zera 
x20Gumbel=u+beta*log(k*Nu/N)  #jesli zdecydujemy,  ze to jednak Gumbel
x20; x20Gumbel

#lub w library(evir) mamy gotowa funkcje 
#riskmeasures(fitGPD,0.95)[2]


##################################################################  BLOKI

fitGEV=evir::gev(data,20); 
fitGEV$par.ests #- wyestymowane parametry xi sigma mu
xiGEV=fitGEV$par.ests[[1]]; xiGEV
sigmaGEV=fitGEV$par.ests[[2]]; sigmaGEV
muGEV=fitGEV$par.ests[[3]]; muGEV
#fitGEV

hist(data,prob=TRUE)
curve(dgev(x,xiGEV,sigmaGEV,muGEV),add=T)


