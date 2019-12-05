library(tidyverse)
library(fitdistrplus)
# set working directory
path_loc <- "C:/repo/ModZdEkstr/Analiza"
setwd(path_loc)
# reading in the data
df <- read_csv("weight-height.csv") ; df
df <- df[seq(1, nrow(df), 50), ]

# taking a quick look
glimpse(df)

# retype to numeric
df$Height <- as.numeric(df$Height)
df$Weight <- as.numeric(df$Weight)

#df$sepal.length[[1]]>=df$sepal.length[[2]]

#converting height to meters and weight to kg
df <- df %>% as_tibble() %>% mutate(
  Height = Height * 2.54 /100,
  Weight = Weight / 2.205
)

data <- df$Weight

# reading second dataset
#df <- read_csv("world-happiness-report-2019.csv") ; df
df <- read_csv("AirPassengers.csv") ; df

# retype to numeric
#df$Ladder <- as.numeric(df$Ladder)
data <- df$Ladder
data <- df$`#Passengers`

# średnia:
mean(data)
# odchylenie standardowe / standard duration
sd(data)
# skośność / skewness oraz kurtoza
library(moments)
skewness(data)
kurtosis(data)

# histogram 
#plotdist(data, histo = TRUE, demp = TRUE)

# wykres skośność- kurtoza
descdist(data, boot = 200)

# przygotowanie pod wykresy diagnostyczne
#ciągłe
fw <- fitdist(data, "weibull")
fg <- fitdist(data, "gamma")
fln <- fitdist(data, "lnorm")
#dyskretne
fitp <- fitdist(data, "pois")
fitnb <- fitdist(data, "nbinom")

# wykresy diagnostyczne
#ciągłe
par(mfrow = c(1,1))
plot.legend <- c("Weibull", "lognormal", "gamma")
denscomp(list(fw, fln, fg), legendtext = plot.legend)
qqcomp(list(fw, fln, fg), legendtext = plot.legend)
cdfcomp(list(fw, fln, fg), legendtext = plot.legend)
ppcomp(list(fw, fln, fg), legendtext = plot.legend)

#dyskretne
#par("mar")
par(mar=c(1,1,1,1))
par(mfrow = c(2,2))
plot.legend <- c("Poisson", "nbinomial")
denscomp(list(fitp, fitnb), legendtext = plot.legend)
qqcomp(list(fitp, fitnb), legendtext = plot.legend)
cdfcomp(list(fitp, fitnb), legendtext = plot.legend)
ppcomp(list(fitp, fitnb), legendtext = plot.legend)

# kryteria AIC BIC
summary(fw)
summary(fg)
summary(fln)

summary(fitp)
summary(fitnb)

# kwantyle
#median(data)
#quantile(data, probs = 0.5)

quantile(data, probs = 0.98)
quantile(data, probs = 0.95)

data(package = .packages(all.available = TRUE))
data(AirPassengers)
force(AirPassengers)

data2 <- AirPassengers[1][1]

