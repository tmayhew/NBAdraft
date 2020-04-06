library(dplyr)
dat = read.csv("allstardf.csv")
dat =  dat %>% filter(Yr > 2007 & Yr < 2015)
