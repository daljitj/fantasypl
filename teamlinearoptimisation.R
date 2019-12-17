library(fplscrapR)
library(tidyverse)
library(lpSolve)
library(readr)

#
setwd("C:/Users/User/Documents/R/Fantasy football")
getwd()
#

## import predictions

predictions <- read_csv("10matchprediction.csv")