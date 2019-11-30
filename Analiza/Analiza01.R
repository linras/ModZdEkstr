library(tidyverse)
# set working directory
path_loc <- "C:/repo/ModZdEkstr/Analiza"
setwd(path_loc)
# reading in the data
df <- read_csv("weight-height.csv") ; df

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

