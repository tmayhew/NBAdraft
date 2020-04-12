library(rvest)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(nbastatR)
'%!in%' <- function(x,y)!('%in%'(x,y))

dat = read.csv("statsdf.csv")[,-c(1)]
primedf = read.csv("primedf.csv")[,-c(1)]
dat$Player = as.character(dat$Player)
primedf$Player = as.character(primedf$Player)
primedfPl = sort(primedf$Player[which(primedf$Player %!in% dat$Player)])
datPl = sort(dat$Player[which(dat$Player %!in% primedf$Player)])

for (i in 1:length(datPl)){
  dat$Player[which(dat$Player == datPl[i])] = primedfPl[i]
}

df <- merge(x=dat, y=primedf, by="Player", all=TRUE)
write.csv(df, "finaldf.csv")
