library(tidyverse)
dat <- read.csv("hw1_data.csv")
names(dat)
head(dat)
new_dat <- dat %>%
  filter(Ozone > 31 & Temp > 90 & !is.na(Solar.R) & !is.na(Ozone) & !is.na(Temp))
mean(new_dat$Solar.R)
