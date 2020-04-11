library(rvest)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(nbastatR)
'%!in%' <- function(x,y)!('%in%'(x,y))

dat = read.csv("allstardf.csv") %>% filter(Yr < 2008) %>% filter(WS > 0.1 & VORP > 0.1 | allstar == 1)
summary(dat$Yr)

stats2007 = dat[,-1] %>% filter(Yr == 2007) %>% arrange(desc(WS))
stats2007$Player = as.character(stats2007$Player)
stats2006 = dat[,-1] %>% filter(Yr == 2006) %>% arrange(desc(WS))
stats2006$Player = as.character(stats2006$Player)
stats2005 = dat[,-1] %>% filter(Yr == 2005) %>% arrange(desc(WS))
stats2005$Player = as.character(stats2005$Player)
stats2004 = dat[,-1] %>% filter(Yr == 2004) %>% arrange(desc(WS))
stats2004$Player = as.character(stats2004$Player)
stats2003 = dat[,-1] %>% filter(Yr == 2003) %>% arrange(desc(WS))
stats2003$Player = as.character(stats2003$Player)
stats2002 = dat[,-1] %>% filter(Yr == 2002) %>% arrange(desc(WS))
stats2002$Player = as.character(stats2002$Player)
stats2001 = dat[,-1] %>% filter(Yr == 2001) %>% arrange(desc(WS))
stats2001$Player = as.character(stats2001$Player)
stats2000 = dat[,-1] %>% filter(Yr == 2000) %>% arrange(desc(WS))
stats2000$Player = as.character(stats2000$Player)
stats1999 = dat[,-1] %>% filter(Yr == 1999) %>% arrange(desc(WS))
stats1999$Player = as.character(stats1999$Player)
stats1998 = dat[,-1] %>% filter(Yr == 1998) %>% arrange(desc(WS))
stats1998$Player = as.character(stats1998$Player)
stats1997 = dat[,-1] %>% filter(Yr == 1997) %>% arrange(desc(WS))
stats1997$Player = as.character(stats1997$Player)
stats1996 = dat[,-1] %>% filter(Yr == 1996) %>% arrange(desc(WS))
stats1996$Player = as.character(stats1996$Player)
stats1995 = dat[,-1] %>% filter(Yr == 1995) %>% arrange(desc(WS))
stats1995$Player = as.character(stats1995$Player)

scrape_nbapl = function(df){
  new.df = data.frame()
  for (i in 1:nrow(df)){
    link = paste(df[i, 2])
    page <- read_html(link)
    data.raw <- html_table(page, fill=TRUE)
    data = data.raw[[1]]
    data$Player = df$Player[i]
    data = data %>% select(Player, everything())
    
    for (row in 1:nrow(data)){
      for (j in c(12, 15, 18, 19, 22)){
        if (is.na(data[row, j])){
          data[row, j] = 0
        }
      }
    }
    
    for (row in 1:nrow(data)){
      for (col in 7:ncol(data)){
        data[row, col] = as.double((data[row, col]))
      }
    }
    
    data = na.omit(data)
    data[,7:ncol(data)] = data.frame(lapply(data[,7:ncol(data)],as.numeric))
    data$GmSc = data$PTS + (0.4*data$FG) - (0.7*data$FGA) - (0.4*(data$FTA - data$FT)) + (0.7*data$ORB) + (0.3*data$DRB) + data$STL + (0.7*data$AST) + (0.7*data$BLK) - (0.4*data$PF) - data$TOV
    
    for (row in 1:nrow(data)){
      if (row < 5){
        data$cumGmSc[row] = 0
      } else{
        data$cumGmSc[row] = data$GmSc[row] + data$GmSc[row-1] + data$GmSc[row-2] + data$GmSc[row-3] + data$GmSc[row-4]
      }
    }
    
    if (nrow(data) >= 5){
      p = which((data$cumGmSc) == max(data$cumGmSc))[1]
      p.df = data[(p-4):p,]
    } else{
      p = nrow(data)
      p.df = data[(1:p),]
    }
    
    p.df = 
      p.df %>% select(-Season, -Age, -Tm, -Lg, -Pos, -GmSc, -cumGmSc)
    sumG = sum(p.df$G)
    
    for (row in 1:nrow(p.df)){
      for (j in 4:ncol(p.df)){
        p.df[row, j] = (p.df[row, j]*p.df[row, 2])/sumG
      }
    }
    f.df = cbind.data.frame(Player = p.df$Player[1], G = sumG, MP = sum(p.df$MP), FG = sum(p.df$FG), FGA = sum(p.df$FGA),FG. = sum(p.df$`FG%`),X3P = sum(p.df$`3P`),X3PA = sum(p.df$`3PA`),X3P. = sum(p.df$`3P%`),X2P = sum(p.df$`2P`),X2PA = sum(p.df$`2PA`),X2P. = sum(p.df$`2P%`),eFG. = sum(p.df$`eFG%`),FT = sum(p.df$FT),  FTA = sum(p.df$FTA),  FT. = sum(p.df$`FT%`),ORB = sum(p.df$ORB),  DRB = sum(p.df$DRB),  TRB = sum(p.df$TRB),  AST = sum(p.df$AST),  STL = sum(p.df$STL),  BLK = sum(p.df$BLK),  TOV = sum(p.df$TOV),  PF = sum(p.df$PF),  PTS = sum(p.df$PTS))
    new.df[i,1:27] = cbind.data.frame(df[i,1:2], f.df)
  }
  return(new.df %>% select(-Player.1))
}

# 1995 #################################################################################################################
df1995 = read.csv("sc.1995df.csv")[,-1]
head(df1995 %>% select(Player, G, PTS, TRB, AST, STL, BLK, TOV) %>% arrange(desc(PTS)), 5)

# 1996 #################################################################################################################
df1996 = read.csv("sc.1996df.csv")[,-1]
head(df1996 %>% select(Player, G, PTS, TRB, AST, STL, BLK, TOV) %>% arrange(desc(PTS)), 5)

# 1997 #################################################################################################################
df1997 = read.csv("sc.1997df.csv")[,-1]
head(df1997 %>% select(Player, G, PTS, TRB, AST, STL, BLK, TOV) %>% arrange(desc(PTS)), 5)

# 1998 #################################################################################################################
df1998 = read.csv("sc.1998df.csv")
head(df1998 %>% select(Player, G, PTS, TRB, AST, STL, BLK, TOV) %>% arrange(desc(PTS)), 5)

# 1999 #################################################################################################################
df1999 = read.csv("sc.1999df.csv")
head(df1999 %>% select(Player, G, PTS, TRB, AST, STL, BLK, TOV) %>% arrange(desc(PTS)), 5)

# 2000 #################################################################################################################
df2000 = read.csv("sc.2000df.csv")
head(df2000 %>% select(Player, G, PTS, TRB, AST, STL, BLK, TOV) %>% arrange(desc(PTS)), 5)




