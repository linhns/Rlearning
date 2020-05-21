library(tidyverse)
dat <- read.csv("hw1_data.csv")
names(dat)
head(dat)
new_dat <- dat %>%
  filter(Ozone > 31 & Temp > 90 & !is.na(Solar.R) & !is.na(Ozone) & !is.na(Temp))
mean(new_dat$Solar.R)

# Date and time
x <- as.Date("1970-01-01")
x
unclass(x)
unclass(as.Date("1970-01-02"))

current_time <- Sys.time()
current_time
p <- as.POSIXlt(current_time)
p$min

datestring <- c("January 10, 2012 10:40", "December 9, 2011")
x <- strptime(datestring, "%B %d, %Y %H:%M")
x

class(x)