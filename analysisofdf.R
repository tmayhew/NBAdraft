# https://zoom.us/j/97428874837?pwd=b1czTEEwS1lJSGJvMmdPeXdTTTZndz09

library(rvest)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(nbastatR)
library(stats)
library(gam)

df = read.csv("finaldf.csv")[,-1]
dat1 = df %>% filter(Yr < 2008); summary(dat1$Yr)
dat2 = df %>% filter(Yr > 2007 & Yr < 2015); summary(dat2$Yr)
dat3 = df %>% filter(Yr > 2014); summary(dat3$Yr)

