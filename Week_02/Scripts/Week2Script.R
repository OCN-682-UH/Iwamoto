### Week 2 Script - Data importing ###
## Created by: Mei Iwamoto ##
## Created on September 2nd, 2026 ##
#################################

### load libraries ###
library(tidyverse)
library(here)

#read in data
weight <- read.csv(here("Week_02","Data","weightdata.csv"))


#data analysis
head(weight)
tail(weight)
view(weight)
