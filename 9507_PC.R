library(rvest)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(nbastatR)
library(stats)
library(gam)

df = read.csv("data/finaldf.csv")[,-1]
dat1 = df %>% filter(Yr < 2008); summary(dat1$Yr)
dat2 = df %>% filter(Yr > 2007 & Yr < 2015); summary(dat2$Yr)
dat3 = df %>% filter(Yr > 2014); summary(dat3$Yr)

names = dat1 %>% select(Player, Yr, Tm, College, Yrs, allstar)
dat1 = dat1 %>% select(-Player, -Yr, -Tm, -College, -Yrs, -allstar)

pr.out = prcomp(dat1, scale = T)
pr.out$center
pr.out$scale
pr.out$rotation[,1:3]
pr.out$x

PC1weights = data.frame(pr.out$rotation[,1])
PC1weights[,2] = rownames(PC1weights)
colnames(PC1weights) = c("weight", "stat")
rownames(PC1weights) = NULL
PC1weights = PC1weights %>% select(stat, weight)
PC1weights$weight = abs(as.numeric(as.character(PC1weights$weight)))
PC1weights = PC1weights %>% arrange((weight))
PC1weights$stat = factor(as.character(PC1weights$stat), levels = PC1weights$stat)
PC1weights %>% ggplot(aes(x = stat, y = weight)) + geom_bar(stat = "identity", width = I(1/4)) + theme_bw() + coord_flip() + ggtitle("Weights for PC1", subtitle = "1995-2007 Drafts") + scale_x_discrete("") + scale_y_continuous("")

pr.var = pr.out$sdev^2
pr.var = pr.var[1:25]
perc.df = cbind.data.frame(pc.index = 1:25, pc.ex = pr.var)
for (i in 1:nrow(perc.df)){
  if (i == 1){
    perc.df$cum.p[i] = perc.df$pc.ex[i]
  } else{
    perc.df$cum.p[i] = perc.df$pc.ex[i] + perc.df$cum.p[i-1]
  }
}
perc.df %>% ggplot(aes(x = pc.index, y = cum.p)) + geom_bar(stat = "identity", width = I(1/12)) + theme_clean() + scale_x_continuous(name = "Principal Component", breaks = 1:25) + scale_y_continuous("Cumulative Percent Explained")

new.var = pr.out$x[,1:8]
new.df = cbind.data.frame(names, new.var)

# Observing Spline Fit: PC1
gam.model1 = gam(allstar ~ s(PC1, 5), data = new.df)

# PC1
df1 = cbind.data.frame(X = new.df$PC1, Y = new.df$allstar, Type = "Real")
tobind = cbind.data.frame(X = new.df$PC1, Y = gam.model1$fitted.values, Type = "Predicted")
df2 = rbind.data.frame(df1, tobind)
ggplot(data = subset(df2, Type == "Real"), aes(x = X, y = Y)) + geom_point(alpha = I(2/5)) + 
  theme_bw() + geom_smooth(data = subset(df2, Type = "Predicted"), method = "loess", 
                           linetype = "dashed", formula = "y~x", se = F) + 
  scale_x_continuous("PC1") + scale_y_continuous("All Stars")

# Final GAM model
gam.model = gam(allstar ~ s(PC1, 4), data = new.df)
predictions = predict(gam.model, data = new.df)
new.df$allstar.pred = predictions
final.df = new.df %>% select(Player, College, Yr, Yrs, allstar, allstar.pred, PC1, PC2)

draft2007 = final.df %>% filter(Yr == 2007) %>% arrange(desc(PC1));print(draft2007)
draft2007 = cbind.data.frame(pk = 1:30, draft2007)
draft2006 = final.df %>% filter(Yr == 2006) %>% arrange(desc(PC1));print(draft2006)
draft2006 = cbind.data.frame(pk = 1:30, draft2006)
draft2005 = final.df %>% filter(Yr == 2005) %>% arrange(desc(PC1));print(draft2005)
draft2005 = cbind.data.frame(pk = 1:30, draft2005)
draft2004 = final.df %>% filter(Yr == 2004) %>% arrange(desc(PC1));print(draft2004)
draft2004 = cbind.data.frame(pk = 1:30, draft2004)
draft2003 = final.df %>% filter(Yr == 2003) %>% arrange(desc(PC1));print(draft2003)
draft2003 = cbind.data.frame(pk = 1:30, draft2003)
draft2002 = final.df %>% filter(Yr == 2002) %>% arrange(desc(PC1));print(draft2002)
draft2002 = cbind.data.frame(pk = 1:30, draft2002)
draft2001 = final.df %>% filter(Yr == 2001) %>% arrange(desc(PC1));print(draft2001)
draft2001 = cbind.data.frame(pk = 1:30, draft2001)
draft2000 = final.df %>% filter(Yr == 2000) %>% arrange(desc(PC1));print(draft2000)
draft2000 = cbind.data.frame(pk = 1:30, draft2000)
draft1999 = final.df %>% filter(Yr == 1999) %>% arrange(desc(PC1));print(draft1999)
draft1999 = cbind.data.frame(pk = 1:30, draft1999)
draft1998 = final.df %>% filter(Yr == 1998) %>% arrange(desc(PC1));print(draft1998)
draft1998 = cbind.data.frame(pk = 1:30, draft1998)
draft1997 = final.df %>% filter(Yr == 1997) %>% arrange(desc(PC1));print(draft1997)
draft1997 = cbind.data.frame(pk = 1:30, draft1997)
draft1996 = final.df %>% filter(Yr == 1996) %>% arrange(desc(PC1));print(draft1996)
draft1996 = cbind.data.frame(pk = 1:30, draft1996)
draft1995 = final.df %>% filter(Yr == 1995) %>% arrange(desc(PC1));print(draft1995)
draft1995 = cbind.data.frame(pk = 1:30, draft1995)

df = rbind.data.frame(draft1995, draft1996, draft1997, draft1998, draft1999,
                 draft2000, draft2001, draft2002, draft2003, draft2004,
                 draft2005, draft2006, draft2007)


