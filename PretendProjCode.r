library(tidyverse)

data(cars)
glimpse(cars)
my_lm<- lm(speed~dist, data=cars) #I'm building our first model
