library(rvest)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(nbastatR)

'%!in%' <- function(x,y)!('%in%'(x,y))

draftpool1 = read.csv("drafts20152019.csv")[,-1]
draftpool2 = read.csv("drafts20082014.csv")[,-1]
draftpool3 = read.csv("drafts19952007.csv")[,-1]
sch.df = read.csv("schools.csv")[,-1]
sch.df$school = as.character(sch.df$school)
sch.df$conf = as.character(sch.df$conf)

df = rbind.data.frame(draftpool1, draftpool2, draftpool3) %>% select(pk, Player, Yr, Yrs, allstar, PC1, College)

for (i in 1:nrow(df)){
  if (df$pk[i] < 4){
    df$rank[i] = "Top 3"
  } else if (df$pk[i] < 6){
    df$rank[i] = "Top 5"
  } else if (df$pk[i] < 11){
    df$rank[i] = "Top 10"
  } else if (df$pk[i] < 15){
    df$rank[i] = "Lottery"
  } else{
    df$rank[i] = "Non-Lottery"
  }
}
for (i in 1:nrow(df)){
  if (df$College[i] == ""){
    df$cbb[i] = 0
  } else{
    df$cbb[i] = 1
  }
}

dfc = df %>% filter(College != "" & College != "University of West Florida" & College != "Butler County Community College" & College != "Augsburg College" & College != "Northeast Mississippi Community College" & College != "Shaw")
# 618/750 (82.4%) of players in the entire dataset played D1 basketball.

for (i in 1:nrow(dfc)){
  dfc$conf[i] = sch.df$conf[which(as.character(sch.df$school) == dfc$College[i])]
}

dfc = dfc %>% select(-cbb)

for (i in 1:nrow(dfc)){
  if (dfc$Yr[i] <= 2012){
    if (dfc$College[i] == "UConn"){
      dfc$conf[i] = "big-east"
    } else if (dfc$College[i] == "Memphis"){
      dfc$conf[i] = "cusa"
    }
  }
  if (dfc$Yr[i] <= 2011){
    if (dfc$College[i] == "Temple"){
      dfc$conf[i] = "atlantic-10"
    } 
    if (dfc$College[i] == "Fresno State"){
      dfc$conf[i] = "wac"
    } 
  }
  if (dfc$Yr[i] <= 2010){
    if (dfc$College[i] == "Cincinnati"){
      dfc$conf[i] = "big-east"
    }
  }
  if (dfc$Yr[i] <= 2006){
    if (dfc$College[i] == "South Florida"){
      dfc$conf[i] = "big-east"
    }
  }
  if (dfc$Yr[i] < 2012){
    if (dfc$conf[i] == "pac-12"){
      dfc$conf[i] = "pac-10"
    }
  }
  if (dfc$Yr[i] <= 1999){
    if (dfc$College[i] == "Tulsa"){
      dfc$conf[i] = "wac"
    }
  }
  if (dfc$Yr[i] < 2000){
    if (dfc$conf[i] == "mwc"){
      dfc$conf[i] = "wac"
    }
  }
  if (dfc$Yr[i] <= 1996){
    if (dfc$conf[i] == "big-12"){
      dfc$conf[i] = "big-8"
    }
  }
  if (dfc$Yr[i] <= 1996){
    if (dfc$College[i] == "Wright State University"){
      dfc$conf[i] = "midwestern-collegiate"
    }
  }
}

confyear = as.data.frame(table(dfc$conf, dfc$Yr))
names(confyear) = c("conf", "Yr", "count")
confyear = confyear %>% filter(count != 0)
confyear = confyear %>% mutate(
  link = paste0("https://www.sports-reference.com/cbb/conferences/",conf,"/",as.character(Yr),"-stats.html")
)
confyear$Yr = as.numeric(as.character(confyear$Yr))

df = read.csv("dfstarter.csv")[,-1]
df$Year = 2019
for (i in 1:nrow(confyear)){
  link = confyear$link[i]
  page <- read_html(link)
  data.raw <- html_table(page, fill=TRUE)
  players <- data.raw[[1]]
  players <- players[,-1]
  players <- players[-1,]
  rownames(players) = NULL
  names(players) = c("Player", "Class", "Pos", "School", "G", "MP", "TRB", "AST", "STL", "BLK", "TOV", "PF", "PTS", "FG.", "X2P.", 
                     "X3P.", "FT.", "PPG", "RPG", "APG", "SPG", "BPG", "PER", "WS", "BPM")
  for (row in 1:nrow(players)){
    for (col in 5:ncol(players)){
      if (players[row,col] == ""){
        players[row,col] = 0
      }
      players[row,col] = as.double(players[row,col])
    }
  }
  players = na.omit(players)
  players$Year = confyear$Yr[i]
  players = players %>% filter(Player %in% dfc$Player)
  df = rbind.data.frame(df, players)
}

df = df[-1,]
rownames(df) = NULL
df

confyear = confyear %>% filter(Yr == 1996)
for (i in 1:nrow(confyear)){
  link = confyear$link[i]
  page <- read_html(link)
  print(confyear$link[i])
}