library(rvest)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(nbastatR)
library(stats)
library(gam)
library(rvest)
library(caTools)
library(caret)
library(mboost)
library(pROC)
library(plyr)
library(ggpubr)

df = read.csv("data/prelimcbb.csv")[,-1] %>% select(-PER, -WS, -BPM)
dfc = read.csv("data/collegedf.csv")[,-1] %>% select(-conf)
conf2008 = read.csv("data/confyear.csv")[,-1]

df2019 = read.csv("data/2019c.csv")[,-1]
df2018 = read.csv("data/2018c.csv")[,-1]
df2017 = read.csv("data/2017c.csv")[,-1]
df2016 = read.csv("data/2016c.csv")[,-1]
df2015 = read.csv("data/2015c.csv")[,-1]
df2014 = read.csv("data/2014c.csv")[,-1]
df2013 = read.csv("data/2013c.csv")[,-1]
df2012 = read.csv("data/2012c.csv")[,-1]
df2011 = read.csv("data/2011c.csv")[,-1]
df2010 = read.csv("data/2010c.csv")[,-1]
df2009 = read.csv("data/2009c.csv")[,-1]
df2008 = read.csv("data/2008c.csv")[,-1]
df2007 = read.csv("data/2007c.csv")[,-1]
df2006 = read.csv("data/2006c.csv")[,-1]
df2005 = read.csv("data/2005c.csv")[,-1]
df2004 = read.csv("data/2004c.csv")[,-1]
df2003 = read.csv("data/2003c.csv")[,-1]
df2002 = read.csv("data/2002c.csv")[,-1]
df2001 = read.csv("data/2001c.csv")[,-1]
df2000 = read.csv("data/2000c.csv")[,-1]
df1999 = read.csv("data/1999c.csv")[,-1]
df1998 = read.csv("data/1998c.csv")[,-1]
df1997 = read.csv("data/1997c.csv")[,-1]
df1996 = read.csv("data/1996c.csv")[,-1]
df1995 = read.csv("data/1995c.csv")[,-1]
data = rbind.data.frame(df2019, df2018, df2017, df2016, df2015, 
                        df2014, df2013, df2012, df2011, df2010, 
                        df2009, df2008, df2007, df2006, df2005, 
                        df2004, df2003, df2002, df2001, df2000,
                        df1999, df1998, df1997, df1996, df1995) 
data = data %>% select(-Year, -School, -Class, -Pos, -Yrs, -PC1, -allstar)
data %>% filter(MP == 0) #Note: In 1998 they didn't record minutes played for some reason
data %>% filter(TOV == 0) # In 1998 they also didn't record turnovers?
data %>% filter(PF == 0) # Or fouls
data = na.omit(data)
data = data %>% filter(Yr < 2019)

names = data %>% select(pk, Player, Yr, College, rank)
dat1 = data %>% select(-pk, -Player, -Yr, -College, -rank, -MP, -TOV, -PF)

pr.out = prcomp(dat1, scale = T)
pr.out$center
pr.out$scale
pr.out$rotation[,1:3]
pr.out$x
pr.var1 = pr.out$sdev^2
pr.var1[length(pr.var1)]
pr.var = pr.var1[1:10]
perc.df = cbind.data.frame(pc.index = 1:10, tot.ex = pr.var)
for (i in 1:nrow(perc.df)){
  if (i == 1){
    perc.df$cum.p[i] = perc.df$tot.ex[i]
  } else{
    perc.df$cum.p[i] = (perc.df$tot.ex[i] + perc.df$cum.p[i-1])
  }
}
perc.df$pc.ex = perc.df$tot.ex/(sum(pr.var1))
perc.df$cum.pc.ex = perc.df$cum.p/(sum(pr.var1))
perc.df %>% ggplot(aes(x = pc.index, y = cum.pc.ex)) + geom_bar(stat = "identity", width = I(1/12)) + theme_clean() + scale_x_continuous(name = "Principal Component", breaks = 1:25) + scale_y_continuous("Cumulative Percent Explained") + geom_point()

dat = cbind.data.frame(names, dat1, pr.out$x)
dat$rank = factor(dat$rank, levels = c("Non-Lottery", "Lottery", "Top 10", "Top 5", "Top 3"))
dat %>% ggplot(aes(x = rank, y = PC1)) + geom_boxplot()

dat$Top3 = ifelse(dat$rank == "Top 3", 1, 0)
dat$Top5 = ifelse(dat$rank == "Top 3" | dat$rank == "Top 5", 1, 0)
dat$Top10 = ifelse(dat$rank == "Top 10" | dat$rank == "Top 5" | dat$rank == "Top 3", 1, 0)
dat$Lottery = ifelse(dat$rank == "Lottery" | dat$rank == "Top 10" | dat$rank == "Top 5" | dat$rank == "Top 3", 1, 0)
dat$NonLottery = ifelse(dat$rank == "Non-Lottery", 1, 0)
dat$NonLottery <- as.factor(dat$NonLottery);dat$Lottery <- as.factor(dat$Lottery);dat$Top10 <- as.factor(dat$Top10);dat$Top5 <- as.factor(dat$Top5);dat$Top3 <- as.factor(dat$Top3)

# Classification ########################################################################################################
## Top 3 ################################################################################################################
df2 <- dat %>% select(Top3, PPG, RPG, APG, SPG, BPG, FG.)
df2$Top3 <- ifelse(df2$Top3 == 1, "Rk. 1-3", "Rk. 4-30")
df2$Top3 <- factor(df2$Top3, levels = c("Rk. 4-30", "Rk. 1-3"))
mu <- ddply(df2, "Top3", summarise, grp.mean=mean(PPG))
pl <- ggplot(df2, aes(x=PPG, color = Top3)) + geom_density(aes(fill=Top3, alpha = I(1/4))) + 
  geom_vline(data = mu, aes(xintercept=grp.mean, color=as.factor(Top3)), linetype="dashed") + theme_clean() +
  scale_color_manual(values=c("#999999", "#002db3")) + scale_y_continuous("") + theme(legend.position = "none") + scale_fill_manual(values=c("#999999", "#002db3"))
mu <- ddply(df2, "Top3", summarise, grp.mean=mean(RPG))
pl2 <- ggplot(df2, aes(x=RPG, color = Top3)) + geom_density(aes(fill=Top3, alpha = I(1/4))) + 
  geom_vline(data = mu, aes(xintercept=grp.mean, color=as.factor(Top3)), linetype="dashed") + theme_clean() +
  scale_color_manual(values=c("#999999", "#002db3")) + scale_y_continuous("") + theme(legend.position = "none") + scale_fill_manual(values=c("#999999", "#002db3"))
mu <- ddply(df2, "Top3", summarise, grp.mean=mean(APG))
pl3 <- ggplot(df2, aes(x=APG, color = Top3)) + geom_density(aes(fill=Top3, alpha = I(1/4))) + 
  geom_vline(data = mu, aes(xintercept=grp.mean, color=as.factor(Top3)), linetype="dashed") + theme_clean() +
  scale_color_manual(values=c("#999999", "#002db3")) + scale_y_continuous("") + theme(legend.position = "none") + scale_fill_manual(values=c("#999999", "#002db3"))
mu <- ddply(df2, "Top3", summarise, grp.mean=mean(SPG))
pl4 <- ggplot(df2, aes(x=SPG, color = Top3)) + geom_density(aes(fill=Top3, alpha = I(1/4))) + 
  geom_vline(data = mu, aes(xintercept=grp.mean, color=as.factor(Top3)), linetype="dashed") + theme_clean() +
  scale_color_manual(values=c("#999999", "#002db3")) + scale_y_continuous("") + theme(legend.position = "none") + scale_fill_manual(values=c("#999999", "#002db3"))
mu <- ddply(df2, "Top3", summarise, grp.mean=mean(BPG))
pl5 <- ggplot(df2, aes(x=BPG, color = Top3)) + geom_density(aes(fill=Top3, alpha = I(1/4))) + 
  geom_vline(data = mu, aes(xintercept=grp.mean, color=as.factor(Top3)), linetype="dashed") + theme_clean() +
  scale_color_manual(values=c("#999999", "#002db3")) + scale_y_continuous("") + theme(legend.position = "none") + scale_fill_manual(values=c("#999999", "#002db3"))
mu <- ddply(df2, "Top3", summarise, grp.mean=mean(FG.))
pl6 <- ggplot(df2, aes(x=FG., color = Top3)) + geom_density(aes(fill=Top3, alpha = I(1/4))) + 
  geom_vline(data = mu, aes(xintercept=grp.mean, color=as.factor(Top3)), linetype="dashed") + theme_clean() +
  scale_color_manual(values=c("#999999", "#002db3")) + scale_y_continuous("") + theme(legend.position = "none") + scale_fill_manual(values=c("#999999", "#002db3"))
figure <- ggarrange(pl, pl2, pl3, pl4, pl5, pl6, ncol = 3, nrow = 2, common.legend = T, legend = "bottom")
annotate_figure(figure, top = text_grob("Basic Counting Statistics in College: Top 3 Picks vs Picks 4-30", color = "black", face = "bold", size = 15))

# Classification ########################################################################################################
## Top 3 ################################################################################################################
df2 <- dat %>% select(Top3, PPG, RPG, APG, SPG, BPG, FG.)
df2$Top3 <- ifelse(df2$Top3 == 1, "Rk. 1-3", "Rk. 4-30")
df2$Top3 <- factor(df2$Top3, levels = c("Rk. 4-30", "Rk. 1-3"))
mu <- ddply(df2, "Top3", summarise, grp.mean=mean(PPG))
pl <- ggplot(df2, aes(x=PPG, color = Top3)) + geom_density(aes(fill=Top3, alpha = I(1/4))) + 
  geom_vline(data = mu, aes(xintercept=grp.mean, color=as.factor(Top3)), linetype="dashed") + theme_clean() +
  scale_color_manual(values=c("#999999", "#002db3")) + scale_y_continuous("") + theme(legend.position = "none") + scale_fill_manual(values=c("#999999", "#002db3"))
mu <- ddply(df2, "Top3", summarise, grp.mean=mean(RPG))
pl2 <- ggplot(df2, aes(x=RPG, color = Top3)) + geom_density(aes(fill=Top3, alpha = I(1/4))) + 
  geom_vline(data = mu, aes(xintercept=grp.mean, color=as.factor(Top3)), linetype="dashed") + theme_clean() +
  scale_color_manual(values=c("#999999", "#002db3")) + scale_y_continuous("") + theme(legend.position = "none") + scale_fill_manual(values=c("#999999", "#002db3"))
mu <- ddply(df2, "Top3", summarise, grp.mean=mean(APG))
pl3 <- ggplot(df2, aes(x=APG, color = Top3)) + geom_density(aes(fill=Top3, alpha = I(1/4))) + 
  geom_vline(data = mu, aes(xintercept=grp.mean, color=as.factor(Top3)), linetype="dashed") + theme_clean() +
  scale_color_manual(values=c("#999999", "#002db3")) + scale_y_continuous("") + theme(legend.position = "none") + scale_fill_manual(values=c("#999999", "#002db3"))
mu <- ddply(df2, "Top3", summarise, grp.mean=mean(SPG))
pl4 <- ggplot(df2, aes(x=SPG, color = Top3)) + geom_density(aes(fill=Top3, alpha = I(1/4))) + 
  geom_vline(data = mu, aes(xintercept=grp.mean, color=as.factor(Top3)), linetype="dashed") + theme_clean() +
  scale_color_manual(values=c("#999999", "#002db3")) + scale_y_continuous("") + theme(legend.position = "none") + scale_fill_manual(values=c("#999999", "#002db3"))
mu <- ddply(df2, "Top3", summarise, grp.mean=mean(BPG))
pl5 <- ggplot(df2, aes(x=BPG, color = Top3)) + geom_density(aes(fill=Top3, alpha = I(1/4))) + 
  geom_vline(data = mu, aes(xintercept=grp.mean, color=as.factor(Top3)), linetype="dashed") + theme_clean() +
  scale_color_manual(values=c("#999999", "#002db3")) + scale_y_continuous("") + theme(legend.position = "none") + scale_fill_manual(values=c("#999999", "#002db3"))
mu <- ddply(df2, "Top3", summarise, grp.mean=mean(FG.))
pl6 <- ggplot(df2, aes(x=FG., color = Top3)) + geom_density(aes(fill=Top3, alpha = I(1/4))) + 
  geom_vline(data = mu, aes(xintercept=grp.mean, color=as.factor(Top3)), linetype="dashed") + theme_clean() +
  scale_color_manual(values=c("#999999", "#002db3")) + scale_y_continuous("") + theme(legend.position = "none") + scale_fill_manual(values=c("#999999", "#002db3"))
figure <- ggarrange(pl, pl2, pl3, pl4, pl5, pl6, ncol = 3, nrow = 2, common.legend = T, legend = "bottom")
annotate_figure(figure, top = text_grob("Basic Counting Statistics in College: Top 3 Picks vs Picks 4-30", color = "black", face = "bold", size = 15))

## Top 5 ################################################################################################################
df2 <- dat %>% select(Top5, PPG, RPG, APG, SPG, BPG, FG.)
df2$Top5 <- ifelse(df2$Top5 == 1, "Rk. 1-5", "Rk. 6-30")
df2$Top5 <- factor(df2$Top5, levels = c("Rk. 6-30", "Rk. 1-5"))
mu <- ddply(df2, "Top5", summarise, grp.mean=mean(PPG))
pl <- ggplot(df2, aes(x=PPG, color = Top5)) + geom_density(aes(fill=Top5, alpha = I(1/4))) + 
  geom_vline(data = mu, aes(xintercept=grp.mean, color=as.factor(Top5)), linetype="dashed") + theme_clean() +
  scale_color_manual(values=c("#999999", "#002db3")) + scale_y_continuous("") + theme(legend.position = "none") + scale_fill_manual(values=c("#999999", "#002db3"))
mu <- ddply(df2, "Top5", summarise, grp.mean=mean(RPG))
pl2 <- ggplot(df2, aes(x=RPG, color = Top5)) + geom_density(aes(fill=Top5, alpha = I(1/4))) + 
  geom_vline(data = mu, aes(xintercept=grp.mean, color=as.factor(Top5)), linetype="dashed") + theme_clean() +
  scale_color_manual(values=c("#999999", "#002db3")) + scale_y_continuous("") + theme(legend.position = "none") + scale_fill_manual(values=c("#999999", "#002db3"))
mu <- ddply(df2, "Top5", summarise, grp.mean=mean(APG))
pl3 <- ggplot(df2, aes(x=APG, color = Top5)) + geom_density(aes(fill=Top5, alpha = I(1/4))) + 
  geom_vline(data = mu, aes(xintercept=grp.mean, color=as.factor(Top5)), linetype="dashed") + theme_clean() +
  scale_color_manual(values=c("#999999", "#002db3")) + scale_y_continuous("") + theme(legend.position = "none") + scale_fill_manual(values=c("#999999", "#002db3"))
mu <- ddply(df2, "Top5", summarise, grp.mean=mean(SPG))
pl4 <- ggplot(df2, aes(x=SPG, color = Top5)) + geom_density(aes(fill=Top5, alpha = I(1/4))) + 
  geom_vline(data = mu, aes(xintercept=grp.mean, color=as.factor(Top5)), linetype="dashed") + theme_clean() +
  scale_color_manual(values=c("#999999", "#002db3")) + scale_y_continuous("") + theme(legend.position = "none") + scale_fill_manual(values=c("#999999", "#002db3"))
mu <- ddply(df2, "Top5", summarise, grp.mean=mean(BPG))
pl5 <- ggplot(df2, aes(x=BPG, color = Top5)) + geom_density(aes(fill=Top5, alpha = I(1/4))) + 
  geom_vline(data = mu, aes(xintercept=grp.mean, color=as.factor(Top5)), linetype="dashed") + theme_clean() +
  scale_color_manual(values=c("#999999", "#002db3")) + scale_y_continuous("") + theme(legend.position = "none") + scale_fill_manual(values=c("#999999", "#002db3"))
mu <- ddply(df2, "Top5", summarise, grp.mean=mean(FG.))
pl6 <- ggplot(df2, aes(x=FG., color = Top5)) + geom_density(aes(fill=Top5, alpha = I(1/4))) + 
  geom_vline(data = mu, aes(xintercept=grp.mean, color=as.factor(Top5)), linetype="dashed") + theme_clean() +
  scale_color_manual(values=c("#999999", "#002db3")) + scale_y_continuous("") + theme(legend.position = "none") + scale_fill_manual(values=c("#999999", "#002db3"))
figure <- ggarrange(pl, pl2, pl3, pl4, pl5, pl6, ncol = 3, nrow = 2, common.legend = T, legend = "bottom")
annotate_figure(figure, top = text_grob("Basic Counting Statistics in College: Top 5 Picks vs Picks 6-30", color = "black", face = "bold", size = 15))

## Top 10 ################################################################################################################
df2 <- dat %>% select(Top10, PPG, RPG, APG, SPG, BPG, FG.)
df2$Top10 <- ifelse(df2$Top10 == 1, "Rk. 1-10", "Rk. 11-30")
df2$Top10 <- factor(df2$Top10, levels = c("Rk. 11-30", "Rk. 1-10"))
mu <- ddply(df2, "Top10", summarise, grp.mean=mean(PPG))
pl <- ggplot(df2, aes(x=PPG, color = Top10)) + geom_density(aes(fill=Top10, alpha = I(1/4))) + 
  geom_vline(data = mu, aes(xintercept=grp.mean, color=as.factor(Top10)), linetype="dashed") + theme_clean() +
  scale_color_manual(values=c("#999999", "#002db3")) + scale_y_continuous("") + theme(legend.position = "none") + scale_fill_manual(values=c("#999999", "#002db3"))
mu <- ddply(df2, "Top10", summarise, grp.mean=mean(RPG))
pl2 <- ggplot(df2, aes(x=RPG, color = Top10)) + geom_density(aes(fill=Top10, alpha = I(1/4))) + 
  geom_vline(data = mu, aes(xintercept=grp.mean, color=as.factor(Top10)), linetype="dashed") + theme_clean() +
  scale_color_manual(values=c("#999999", "#002db3")) + scale_y_continuous("") + theme(legend.position = "none") + scale_fill_manual(values=c("#999999", "#002db3"))
mu <- ddply(df2, "Top10", summarise, grp.mean=mean(APG))
pl3 <- ggplot(df2, aes(x=APG, color = Top10)) + geom_density(aes(fill=Top10, alpha = I(1/4))) + 
  geom_vline(data = mu, aes(xintercept=grp.mean, color=as.factor(Top10)), linetype="dashed") + theme_clean() +
  scale_color_manual(values=c("#999999", "#002db3")) + scale_y_continuous("") + theme(legend.position = "none") + scale_fill_manual(values=c("#999999", "#002db3"))
mu <- ddply(df2, "Top10", summarise, grp.mean=mean(SPG))
pl4 <- ggplot(df2, aes(x=SPG, color = Top10)) + geom_density(aes(fill=Top10, alpha = I(1/4))) + 
  geom_vline(data = mu, aes(xintercept=grp.mean, color=as.factor(Top10)), linetype="dashed") + theme_clean() +
  scale_color_manual(values=c("#999999", "#002db3")) + scale_y_continuous("") + theme(legend.position = "none") + scale_fill_manual(values=c("#999999", "#002db3"))
mu <- ddply(df2, "Top10", summarise, grp.mean=mean(BPG))
pl5 <- ggplot(df2, aes(x=BPG, color = Top10)) + geom_density(aes(fill=Top10, alpha = I(1/4))) + 
  geom_vline(data = mu, aes(xintercept=grp.mean, color=as.factor(Top10)), linetype="dashed") + theme_clean() +
  scale_color_manual(values=c("#999999", "#002db3")) + scale_y_continuous("") + theme(legend.position = "none") + scale_fill_manual(values=c("#999999", "#002db3"))
mu <- ddply(df2, "Top10", summarise, grp.mean=mean(FG.))
pl6 <- ggplot(df2, aes(x=FG., color = Top10)) + geom_density(aes(fill=Top10, alpha = I(1/4))) + 
  geom_vline(data = mu, aes(xintercept=grp.mean, color=as.factor(Top10)), linetype="dashed") + theme_clean() +
  scale_color_manual(values=c("#999999", "#002db3")) + scale_y_continuous("") + theme(legend.position = "none") + scale_fill_manual(values=c("#999999", "#002db3"))
figure <- ggarrange(pl, pl2, pl3, pl4, pl5, pl6, ncol = 3, nrow = 2, common.legend = T, legend = "bottom")
annotate_figure(figure, top = text_grob("Basic Counting Statistics in College: Top 10 Picks vs Picks 11-30", color = "black", face = "bold", size = 15))

## Lottery ################################################################################################################
df2 <- dat %>% select(Lottery, PPG, RPG, APG, SPG, BPG, FG.)
df2$Lottery <- ifelse(df2$Lottery == 1, "Rk. 1-14", "Rk. 15-30")
df2$Lottery <- factor(df2$Lottery, levels = c("Rk. 15-30", "Rk. 1-14"))
mu <- ddply(df2, "Lottery", summarise, grp.mean=mean(PPG))
pl <- ggplot(df2, aes(x=PPG, color = Lottery)) + geom_density(aes(fill=Lottery, alpha = I(1/4))) + 
  geom_vline(data = mu, aes(xintercept=grp.mean, color=as.factor(Lottery)), linetype="dashed") + theme_clean() +
  scale_color_manual(values=c("#999999", "#002db3")) + scale_y_continuous("") + theme(legend.position = "none") + scale_fill_manual(values=c("#999999", "#002db3"))
mu <- ddply(df2, "Lottery", summarise, grp.mean=mean(RPG))
pl2 <- ggplot(df2, aes(x=RPG, color = Lottery)) + geom_density(aes(fill=Lottery, alpha = I(1/4))) + 
  geom_vline(data = mu, aes(xintercept=grp.mean, color=as.factor(Lottery)), linetype="dashed") + theme_clean() +
  scale_color_manual(values=c("#999999", "#002db3")) + scale_y_continuous("") + theme(legend.position = "none") + scale_fill_manual(values=c("#999999", "#002db3"))
mu <- ddply(df2, "Lottery", summarise, grp.mean=mean(APG))
pl3 <- ggplot(df2, aes(x=APG, color = Lottery)) + geom_density(aes(fill=Lottery, alpha = I(1/4))) + 
  geom_vline(data = mu, aes(xintercept=grp.mean, color=as.factor(Lottery)), linetype="dashed") + theme_clean() +
  scale_color_manual(values=c("#999999", "#002db3")) + scale_y_continuous("") + theme(legend.position = "none") + scale_fill_manual(values=c("#999999", "#002db3"))
mu <- ddply(df2, "Lottery", summarise, grp.mean=mean(SPG))
pl4 <- ggplot(df2, aes(x=SPG, color = Lottery)) + geom_density(aes(fill=Lottery, alpha = I(1/4))) + 
  geom_vline(data = mu, aes(xintercept=grp.mean, color=as.factor(Lottery)), linetype="dashed") + theme_clean() +
  scale_color_manual(values=c("#999999", "#002db3")) + scale_y_continuous("") + theme(legend.position = "none") + scale_fill_manual(values=c("#999999", "#002db3"))
mu <- ddply(df2, "Lottery", summarise, grp.mean=mean(BPG))
pl5 <- ggplot(df2, aes(x=BPG, color = Lottery)) + geom_density(aes(fill=Lottery, alpha = I(1/4))) + 
  geom_vline(data = mu, aes(xintercept=grp.mean, color=as.factor(Lottery)), linetype="dashed") + theme_clean() +
  scale_color_manual(values=c("#999999", "#002db3")) + scale_y_continuous("") + theme(legend.position = "none") + scale_fill_manual(values=c("#999999", "#002db3"))
mu <- ddply(df2, "Lottery", summarise, grp.mean=mean(FG.))
pl6 <- ggplot(df2, aes(x=FG., color = Lottery)) + geom_density(aes(fill=Lottery, alpha = I(1/4))) + 
  geom_vline(data = mu, aes(xintercept=grp.mean, color=as.factor(Lottery)), linetype="dashed") + theme_clean() +
  scale_color_manual(values=c("#999999", "#002db3")) + scale_y_continuous("") + theme(legend.position = "none") + scale_fill_manual(values=c("#999999", "#002db3"))
figure <- ggarrange(pl, pl2, pl3, pl4, pl5, pl6, ncol = 3, nrow = 2, common.legend = T, legend = "bottom")
annotate_figure(figure, top = text_grob("Basic Counting Statistics in College: Lottery Picks vs Picks 15-30", color = "black", face = "bold", size = 15))

control <- trainControl(method="cv", number=10)
metric <- "Accuracy"
sample <- sample.split(dat$Lottery, SplitRatio = 0.75)
train <- subset(dat, sample == TRUE)
test <- subset(dat, sample == FALSE)
fit.lda <- train(Lottery ~ PC1 + PC2 + PC3 + PC4 + PC5, data = train, method="lda", metric=metric, trControl=control)
fit.knn <- train(Lottery ~ PC1 + PC2 + PC3 + PC4 + PC5, data=train, method="knn", metric=metric, trControl=control)
fit.svm <- train(Lottery ~ PC1 + PC2 + PC3 + PC4 + PC5, data=train, method="svmRadial", metric=metric, trControl=control)
fit.rf <- train(Lottery ~ PC1 + PC2 + PC3 + PC4 + PC5, data=train, method="rf", metric=metric, trControl=control)
fit.glm <- train(Lottery ~ PC1 + PC2 + PC3 + PC4 + PC5, data=train, method="glm", metric=metric, trControl=control, family = "binomial")
results <- resamples(list(lda=fit.lda, knn=fit.knn, svm=fit.svm, rf=fit.rf, glm = fit.glm))
summary(results);dotplot(results)

predictions.glm = predict(fit.glm, test, type = "prob")[,'1']
pred.df = cbind.data.frame(test, glm.p = predictions.glm) %>% select(Player, Yr, rank, Lottery, glm.p)
pred.df %>% ggplot(aes(rank, glm.p)) + geom_hline(yintercept = 0.50, linetype = "dashed") + geom_boxplot(width = I(1/4)) + theme_clean() + scale_y_continuous("Lottery Prediction", limits = c(0,1)) + scale_x_discrete("") + ggtitle("Probability of Lottery Classification vs. Actual Rank in Re-Draft", subtitle = "Test Observations")
