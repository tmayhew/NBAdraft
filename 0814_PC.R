library(rvest)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(nbastatR)
library(stats)
library(gam)

df = read.csv("data/finaldf.csv")[,-1]
dat1 = df %>% filter(Yr > 2007 & Yr < 2015); summary(dat2$Yr)
# dat3 = df %>% filter(Yr > 2014); summary(dat3$Yr)

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
PC1weights %>% ggplot(aes(x = stat, y = weight)) + geom_bar(stat = "identity", width = I(1/4)) + theme_bw() + coord_flip() + ggtitle("Weights for PC1", subtitle = "2008-2014 Drafts") + scale_x_discrete("") + scale_y_continuous("")

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

draft2014 = final.df %>% filter(Yr == 2014) %>% arrange(desc(PC1));print(draft2014)
draft2014 = cbind.data.frame(pk = 1:30, draft2014)
draft2013 = final.df %>% filter(Yr == 2013) %>% arrange(desc(PC1));print(draft2013)
draft2013 = cbind.data.frame(pk = 1:30, draft2013)
draft2012 = final.df %>% filter(Yr == 2012) %>% arrange(desc(PC1));print(draft2012)
draft2012 = cbind.data.frame(pk = 1:30, draft2012)
draft2011 = final.df %>% filter(Yr == 2011) %>% arrange(desc(PC1));print(draft2011)
draft2011 = cbind.data.frame(pk = 1:30, draft2011)
draft2010 = final.df %>% filter(Yr == 2010) %>% arrange(desc(PC1));print(draft2010)
draft2010 = cbind.data.frame(pk = 1:30, draft2010)
draft2009 = final.df %>% filter(Yr == 2009) %>% arrange(desc(PC1));print(draft2009)
draft2009 = cbind.data.frame(pk = 1:30, draft2009)
draft2008 = final.df %>% filter(Yr == 2008) %>% arrange(desc(PC1));print(draft2008)
draft2008 = cbind.data.frame(pk = 1:30, draft2008)

df = rbind.data.frame(draft2014, draft2013, draft2012, draft2011, 
                      draft2010, draft2009, draft2008)


