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
fw <- fitdist(data, "weibull")
fg <- fitdist(data, "gamma")
fln <- fitdist(data, "lnorm")
summary(fw)
summary(fg)
summary(fln)

# wykresy diagnostyczne
par(mfrow = c(1,1))
plot.legend <- c("Weibull", "lognormal", "gamma")
denscomp(list(fw, fln, fg), legendtext = plot.legend)
qqcomp(list(fw, fln, fg), legendtext = plot.legend)
cdfcomp(list(fw, fln, fg), legendtext = plot.legend)
ppcomp(list(fw, fln, fg), legendtext = plot.legend)
