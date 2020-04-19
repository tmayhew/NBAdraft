library(rvest)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(nbastatR)
'%!in%' <- function(x,y)!('%in%'(x,y))

dat = read.csv("data/allstardf.csv")
summary(dat$Yr)
stats2019 = dat[,-1] %>% filter(Yr == 2019) %>% arrange(desc(MP))
stats2019 = stats2019[1:30,]
stats2019$Player = as.character(stats2019$Player)
stats2018 = dat[,-1] %>% filter(Yr == 2018) %>% arrange(desc(MP))
stats2018 = stats2018[1:30,]
stats2018$Player = as.character(stats2018$Player)
stats2017 = dat[,-1] %>% filter(Yr == 2017) %>% arrange(desc(MP))
stats2017 = stats2017[1:30,]
stats2017$Player = as.character(stats2017$Player)
stats2016 = dat[,-1] %>% filter(Yr == 2016) %>% arrange(desc(MP))
stats2016 = stats2016[1:30,]
stats2016$Player = as.character(stats2016$Player)
stats2015 = dat[,-1] %>% filter(Yr == 2015) %>% arrange(desc(MP))
stats2015 = stats2015[1:30,]
stats2015$Player = as.character(stats2015$Player)
stats2014 = dat[,-1] %>% filter(Yr == 2014) %>% arrange(desc(MP))
stats2014 = stats2014[1:30,]
stats2014$Player = as.character(stats2014$Player)
stats2013 = dat[,-1] %>% filter(Yr == 2013) %>% arrange(desc(MP))
stats2013 = stats2013[1:30,]
stats2013$Player = as.character(stats2013$Player)
stats2012 = dat[,-1] %>% filter(Yr == 2012) %>% arrange(desc(MP))
stats2012 = stats2012[1:30,]
stats2012$Player = as.character(stats2012$Player)
stats2011 = dat[,-1] %>% filter(Yr == 2011) %>% arrange(desc(MP))
stats2011 = stats2011[1:30,]
stats2011$Player = as.character(stats2011$Player)
stats2010 = dat[,-1] %>% filter(Yr == 2010) %>% arrange(desc(MP))
stats2010 = stats2010[1:30,]
stats2010$Player = as.character(stats2010$Player)
stats2009 = dat[,-1] %>% filter(Yr == 2009) %>% arrange(desc(MP))
stats2009 = stats2009[1:30,]
stats2009$Player = as.character(stats2009$Player)
stats2008 = dat[,-1] %>% filter(Yr == 2008) %>% arrange(desc(MP))
stats2008 = stats2008[1:30,]
stats2008$Player = as.character(stats2008$Player)
stats2007 = dat[,-1] %>% filter(Yr == 2007) %>% arrange(desc(MP))
stats2007 = stats2007[1:30,]
stats2007$Player = as.character(stats2007$Player)
stats2006 = dat[,-1] %>% filter(Yr == 2006) %>% arrange(desc(MP))
stats2006 = stats2006[1:30,]
stats2006$Player = as.character(stats2006$Player)
stats2005 = dat[,-1] %>% filter(Yr == 2005) %>% arrange(desc(MP))
stats2005 = stats2005[1:30,]
stats2005$Player = as.character(stats2005$Player)
stats2004 = dat[,-1] %>% filter(Yr == 2004) %>% arrange(desc(MP))
stats2004 = stats2004[1:30,]
stats2004$Player = as.character(stats2004$Player)
stats2003 = dat[,-1] %>% filter(Yr == 2003) %>% arrange(desc(MP))
stats2003 = stats2003[1:30,]
stats2003$Player = as.character(stats2003$Player)
stats2002 = dat[,-1] %>% filter(Yr == 2002) %>% arrange(desc(MP))
stats2002 = stats2002[1:30,]
stats2002$Player = as.character(stats2002$Player)
stats2001 = dat[,-1] %>% filter(Yr == 2001) %>% arrange(desc(MP))
stats2001 = stats2001[1:30,]
stats2001$Player = as.character(stats2001$Player)
stats2000 = dat[,-1] %>% filter(Yr == 2000) %>% arrange(desc(MP))
stats2000 = stats2000[1:30,]
stats2000$Player = as.character(stats2000$Player)
stats1999 = dat[,-1] %>% filter(Yr == 1999) %>% arrange(desc(MP))
stats1999 = stats1999[1:30,]
stats1999$Player = as.character(stats1999$Player)
stats1998 = dat[,-1] %>% filter(Yr == 1998) %>% arrange(desc(MP))
stats1998 = stats1998[1:30,]
stats1998$Player = as.character(stats1998$Player)
stats1997 = dat[,-1] %>% filter(Yr == 1997) %>% arrange(desc(MP))
stats1997 = stats1997[1:30,]
stats1997$Player = as.character(stats1997$Player)
stats1996 = dat[,-1] %>% filter(Yr == 1996) %>% arrange(desc(MP))
stats1996 = stats1996[1:30,]
stats1996$Player = as.character(stats1996$Player)
stats1995 = dat[,-1] %>% filter(Yr == 1995) %>% arrange(desc(MP))
stats1995 = stats1995[1:30,]
stats1995$Player = as.character(stats1995$Player)

statsdf = rbind.data.frame(stats2019, stats2018, stats2017, stats2016, stats2015, 
                           stats2014, stats2013, stats2012, stats2011, stats2010,
                           stats2009, stats2008, stats2007, stats2006, stats2005, 
                           stats2004, stats2003, stats2002, stats2001, stats2000,
                           stats1999, stats1998, stats1997, stats1996, stats1995)
#write.csv(statsdf, "statsdf.csv")
                           

"scrape_nbapl = function(df){
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
    
    for (row in 1:nrow(data)){
      for (j in c(12, 15, 18, 19, 22)){
        if (is.na(data[row, j])){
          data[row, j] = 0
        }
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
}"

# 1995 #################################################################################################################
df1995 = read.csv("sc.1995df.csv")
df1995 = df1995 %>% mutate(PER = ((FG*85.910) + (STL*53.897) + (X3P*51.757) + (FT*46.845) + (BLK*39.190) + (ORB*39.190) + (AST*34.677) + (DRB*14.707) - (PF*17.174) - ((FTA-FT)*20.091) - ((FGA-FG)*39.190) - (TOV*53.897)) * (1/MP))
df1995 %>% select(Player, G, PTS, TRB, AST, STL, BLK, TOV, PER) %>% arrange(desc(PER))

# 1996 #################################################################################################################
df1996 = read.csv("sc.1996df.csv")
df1996 = df1996 %>% mutate(PER = ((FG*85.910) + (STL*53.897) + (X3P*51.757) + (FT*46.845) + (BLK*39.190) + (ORB*39.190) + (AST*34.677) + (DRB*14.707) - (PF*17.174) - ((FTA-FT)*20.091) - ((FGA-FG)*39.190) - (TOV*53.897)) * (1/MP))
df1996 %>% select(Player, G, PTS, TRB, AST, STL, BLK, TOV, PER) %>% arrange(desc(PER))

# 1997 #################################################################################################################
df1997 = read.csv("sc.1997df.csv")
df1997 = df1997 %>% mutate(PER = ((FG*85.910) + (STL*53.897) + (X3P*51.757) + (FT*46.845) + (BLK*39.190) + (ORB*39.190) + (AST*34.677) + (DRB*14.707) - (PF*17.174) - ((FTA-FT)*20.091) - ((FGA-FG)*39.190) - (TOV*53.897)) * (1/MP))
df1997 %>% select(Player, G, PTS, TRB, AST, STL, BLK, TOV, PER) %>% arrange(desc(PER))

# 1998 #################################################################################################################
df1998 = read.csv("sc.1998df.csv")
df1998 = df1998 %>% mutate(PER = ((FG*85.910) + (STL*53.897) + (X3P*51.757) + (FT*46.845) + (BLK*39.190) + (ORB*39.190) + (AST*34.677) + (DRB*14.707) - (PF*17.174) - ((FTA-FT)*20.091) - ((FGA-FG)*39.190) - (TOV*53.897)) * (1/MP))
df1998 %>% select(Player, G, PTS, TRB, AST, STL, BLK, TOV, PER) %>% arrange(desc(PER))

# 1999 #################################################################################################################
df1999 = read.csv("sc.1999df.csv")
df1999 = df1999 %>% mutate(PER = ((FG*85.910) + (STL*53.897) + (X3P*51.757) + (FT*46.845) + (BLK*39.190) + (ORB*39.190) + (AST*34.677) + (DRB*14.707) - (PF*17.174) - ((FTA-FT)*20.091) - ((FGA-FG)*39.190) - (TOV*53.897)) * (1/MP))
df1999 %>% select(Player, G, PTS, TRB, AST, STL, BLK, TOV, PER) %>% arrange(desc(PER))

# 2000 #################################################################################################################
df2000 = read.csv("sc.2000df.csv")
df2000 = df2000 %>% mutate(PER = ((FG*85.910) + (STL*53.897) + (X3P*51.757) + (FT*46.845) + (BLK*39.190) + (ORB*39.190) + (AST*34.677) + (DRB*14.707) - (PF*17.174) - ((FTA-FT)*20.091) - ((FGA-FG)*39.190) - (TOV*53.897)) * (1/MP))
df2000 %>% select(Player, G, PTS, TRB, AST, STL, BLK, TOV, PER) %>% arrange(desc(PER))

# 2001 #################################################################################################################
df2001 = read.csv("sc.2001df.csv")
df2001 = df2001 %>% mutate(PER = ((FG*85.910) + (STL*53.897) + (X3P*51.757) + (FT*46.845) + (BLK*39.190) + (ORB*39.190) + (AST*34.677) + (DRB*14.707) - (PF*17.174) - ((FTA-FT)*20.091) - ((FGA-FG)*39.190) - (TOV*53.897)) * (1/MP))
df2001 %>% select(Player, G, PTS, TRB, AST, STL, BLK, TOV, PER) %>% arrange(desc(PER))

# 2002 #################################################################################################################
df2002 = read.csv("sc.2002df.csv")
df2002 = df2002 %>% mutate(PER = ((FG*85.910) + (STL*53.897) + (X3P*51.757) + (FT*46.845) + (BLK*39.190) + (ORB*39.190) + (AST*34.677) + (DRB*14.707) - (PF*17.174) - ((FTA-FT)*20.091) - ((FGA-FG)*39.190) - (TOV*53.897)) * (1/MP))
df2002 %>% select(Player, G, PTS, TRB, AST, STL, BLK, TOV, PER) %>% arrange(desc(PER))

# 2003 #################################################################################################################
df2003 = read.csv("sc.2003df.csv")
df2003 = df2003 %>% mutate(PER = ((FG*85.910) + (STL*53.897) + (X3P*51.757) + (FT*46.845) + (BLK*39.190) + (ORB*39.190) + (AST*34.677) + (DRB*14.707) - (PF*17.174) - ((FTA-FT)*20.091) - ((FGA-FG)*39.190) - (TOV*53.897)) * (1/MP))
df2003 %>% select(Player, G, PTS, TRB, AST, STL, BLK, TOV, PER) %>% arrange(desc(PER))

# 2004 #################################################################################################################
df2004 = read.csv("sc.2004df.csv")
df2004 = df2004 %>% mutate(PER = ((FG*85.910) + (STL*53.897) + (X3P*51.757) + (FT*46.845) + (BLK*39.190) + (ORB*39.190) + (AST*34.677) + (DRB*14.707) - (PF*17.174) - ((FTA-FT)*20.091) - ((FGA-FG)*39.190) - (TOV*53.897)) * (1/MP))
df2004 %>% select(Player, G, PTS, TRB, AST, STL, BLK, TOV, PER) %>% arrange(desc(PER))

# 2005 #################################################################################################################
df2005 = read.csv("sc.2005df.csv")
df2005 = df2005 %>% mutate(PER = ((FG*85.910) + (STL*53.897) + (X3P*51.757) + (FT*46.845) + (BLK*39.190) + (ORB*39.190) + (AST*34.677) + (DRB*14.707) - (PF*17.174) - ((FTA-FT)*20.091) - ((FGA-FG)*39.190) - (TOV*53.897)) * (1/MP))
df2005 %>% select(Player, G, PTS, TRB, AST, STL, BLK, TOV, PER) %>% arrange(desc(PER))

# 2006 #################################################################################################################
df2006 = read.csv("sc.2006df.csv")
df2006 = df2006 %>% mutate(PER = ((FG*85.910) + (STL*53.897) + (X3P*51.757) + (FT*46.845) + (BLK*39.190) + (ORB*39.190) + (AST*34.677) + (DRB*14.707) - (PF*17.174) - ((FTA-FT)*20.091) - ((FGA-FG)*39.190) - (TOV*53.897)) * (1/MP))
df2006 %>% select(Player, G, PTS, TRB, AST, STL, BLK, TOV, PER) %>% arrange(desc(PER))

# 2007 #################################################################################################################
df2007 = read.csv("sc.2007df.csv")
df2007 = df2007 %>% mutate(PER = ((FG*85.910) + (STL*53.897) + (X3P*51.757) + (FT*46.845) + (BLK*39.190) + (ORB*39.190) + (AST*34.677) + (DRB*14.707) - (PF*17.174) - ((FTA-FT)*20.091) - ((FGA-FG)*39.190) - (TOV*53.897)) * (1/MP))
df2007 %>% select(Player, G, PTS, TRB, AST, STL, BLK, TOV, PER) %>% arrange(desc(PER))

# 2008 #################################################################################################################
df2008 = read.csv("sc.2008df.csv")
df2008 = df2008 %>% mutate(PER = ((FG*85.910) + (STL*53.897) + (X3P*51.757) + (FT*46.845) + (BLK*39.190) + (ORB*39.190) + (AST*34.677) + (DRB*14.707) - (PF*17.174) - ((FTA-FT)*20.091) - ((FGA-FG)*39.190) - (TOV*53.897)) * (1/MP))
df2008 %>% select(Player, G, PTS, TRB, AST, STL, BLK, TOV, PER) %>% arrange(desc(PER))

# 2009 #################################################################################################################
df2009 = read.csv("sc.2009df.csv")
df2009 = df2009 %>% mutate(PER = ((FG*85.910) + (STL*53.897) + (X3P*51.757) + (FT*46.845) + (BLK*39.190) + (ORB*39.190) + (AST*34.677) + (DRB*14.707) - (PF*17.174) - ((FTA-FT)*20.091) - ((FGA-FG)*39.190) - (TOV*53.897)) * (1/MP))
df2009 %>% select(Player, G, PTS, TRB, AST, STL, BLK, TOV, PER) %>% arrange(desc(PER))

# 2010 #################################################################################################################
df2010 = read.csv("sc.2010df.csv")
df2010 = df2010 %>% mutate(PER = ((FG*85.910) + (STL*53.897) + (X3P*51.757) + (FT*46.845) + (BLK*39.190) + (ORB*39.190) + (AST*34.677) + (DRB*14.707) - (PF*17.174) - ((FTA-FT)*20.091) - ((FGA-FG)*39.190) - (TOV*53.897)) * (1/MP))
df2010 %>% select(Player, G, PTS, TRB, AST, STL, BLK, TOV, PER) %>% arrange(desc(PER))

# 2011 #################################################################################################################
df2011 = read.csv("sc.2011df.csv")
df2011 = df2011 %>% mutate(PER = ((FG*85.910) + (STL*53.897) + (X3P*51.757) + (FT*46.845) + (BLK*39.190) + (ORB*39.190) + (AST*34.677) + (DRB*14.707) - (PF*17.174) - ((FTA-FT)*20.091) - ((FGA-FG)*39.190) - (TOV*53.897)) * (1/MP))
df2011 %>% select(Player, G, PTS, TRB, AST, STL, BLK, TOV, PER) %>% arrange(desc(PER))

# 2012 #################################################################################################################
df2012 = read.csv("sc.2012df.csv")
df2012 = df2012 %>% mutate(PER = ((FG*85.910) + (STL*53.897) + (X3P*51.757) + (FT*46.845) + (BLK*39.190) + (ORB*39.190) + (AST*34.677) + (DRB*14.707) - (PF*17.174) - ((FTA-FT)*20.091) - ((FGA-FG)*39.190) - (TOV*53.897)) * (1/MP))
df2012 %>% select(Player, G, PTS, TRB, AST, STL, BLK, TOV, PER) %>% arrange(desc(PER))

# 2013 #################################################################################################################
df2013 = read.csv("sc.2013df.csv")
df2013 = df2013 %>% mutate(PER = ((FG*85.910) + (STL*53.897) + (X3P*51.757) + (FT*46.845) + (BLK*39.190) + (ORB*39.190) + (AST*34.677) + (DRB*14.707) - (PF*17.174) - ((FTA-FT)*20.091) - ((FGA-FG)*39.190) - (TOV*53.897)) * (1/MP))
df2013 %>% select(Player, G, PTS, TRB, AST, STL, BLK, TOV, PER) %>% arrange(desc(PER))

# 2014 #################################################################################################################
df2014 = read.csv("sc.2014df.csv")
df2014 = df2014 %>% mutate(PER = ((FG*85.910) + (STL*53.897) + (X3P*51.757) + (FT*46.845) + (BLK*39.190) + (ORB*39.190) + (AST*34.677) + (DRB*14.707) - (PF*17.174) - ((FTA-FT)*20.091) - ((FGA-FG)*39.190) - (TOV*53.897)) * (1/MP))
df2014 %>% select(Player, G, PTS, TRB, AST, STL, BLK, TOV, PER) %>% arrange(desc(PER))

# 2015 #################################################################################################################
df2015 = read.csv("sc.2015df.csv")
df2015 = df2015 %>% mutate(PER = ((FG*85.910) + (STL*53.897) + (X3P*51.757) + (FT*46.845) + (BLK*39.190) + (ORB*39.190) + (AST*34.677) + (DRB*14.707) - (PF*17.174) - ((FTA-FT)*20.091) - ((FGA-FG)*39.190) - (TOV*53.897)) * (1/MP))
df2015 %>% select(Player, G, PTS, TRB, AST, STL, BLK, TOV, PER) %>% arrange(desc(PER))

# 2016 #################################################################################################################
df2016 = read.csv("sc.2016df.csv")
df2016 = df2016 %>% mutate(PER = ((FG*85.910) + (STL*53.897) + (X3P*51.757) + (FT*46.845) + (BLK*39.190) + (ORB*39.190) + (AST*34.677) + (DRB*14.707) - (PF*17.174) - ((FTA-FT)*20.091) - ((FGA-FG)*39.190) - (TOV*53.897)) * (1/MP))
df2016 %>% select(Player, G, PTS, TRB, AST, STL, BLK, TOV, PER) %>% arrange(desc(PER))

# 2017 #################################################################################################################
df2017 = read.csv("sc.2017df.csv")
df2017 = df2017 %>% mutate(PER = ((FG*85.910) + (STL*53.897) + (X3P*51.757) + (FT*46.845) + (BLK*39.190) + (ORB*39.190) + (AST*34.677) + (DRB*14.707) - (PF*17.174) - ((FTA-FT)*20.091) - ((FGA-FG)*39.190) - (TOV*53.897)) * (1/MP))
df2017 %>% select(Player, G, PTS, TRB, AST, STL, BLK, TOV, PER) %>% arrange(desc(PER))

# 2018 #################################################################################################################
df2018 = read.csv("sc.2018df.csv")
df2018 = df2018 %>% mutate(PER = ((FG*85.910) + (STL*53.897) + (X3P*51.757) + (FT*46.845) + (BLK*39.190) + (ORB*39.190) + (AST*34.677) + (DRB*14.707) - (PF*17.174) - ((FTA-FT)*20.091) - ((FGA-FG)*39.190) - (TOV*53.897)) * (1/MP))
df2018 %>% select(Player, G, PTS, TRB, AST, STL, BLK, TOV, PER) %>% arrange(desc(PER))

# 2019 #################################################################################################################
df2019 = read.csv("sc.2019df.csv")
df2019 = df2019 %>% mutate(PER = ((FG*85.910) + (STL*53.897) + (X3P*51.757) + (FT*46.845) + (BLK*39.190) + (ORB*39.190) + (AST*34.677) + (DRB*14.707) - (PF*17.174) - ((FTA-FT)*20.091) - ((FGA-FG)*39.190) - (TOV*53.897)) * (1/MP))
df2019 %>% select(Player, G, PTS, TRB, AST, STL, BLK, TOV, PER) %>% arrange(desc(PER))

primedf = rbind.data.frame(df1995, df1996, df1997, df1998, df1999,
                            df2000, df2001, df2002, df2003, df2004,
                            df2005, df2006, df2007, df2008, df2009,
                            df2010, df2011, df2012, df2013, df2014,
                            df2015, df2016, df2017, df2018, df2019)




