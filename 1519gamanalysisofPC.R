library(rvest)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(nbastatR)
library(stats)
library(gam)

df = read.csv("finaldf.csv")[,-1]
dat1 = df %>% filter(Yr > 2014); summary(dat3$Yr)

names = dat1 %>% select(Player, Yr, Tm, College, Yrs, allstar)
dat1 = dat1 %>% select(-Player, -Yr, -Tm, -College, -Yrs, -allstar)

pr.out = prcomp(dat1, scale = T)
pr.out$center
pr.out$scale
pr.out$rotation[,1:3]
pr.out$x

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
final.df = new.df %>% select(Player, Yr, Yrs, allstar, allstar.pred, PC1, PC2)

draft2019 = final.df %>% filter(Yr == 2019) %>% arrange(desc(PC1));print(draft2019)
draft2018 = final.df %>% filter(Yr == 2018) %>% arrange(desc(PC1));print(draft2018)
draft2017 = final.df %>% filter(Yr == 2017) %>% arrange(desc(PC1));print(draft2017)
draft2016 = final.df %>% filter(Yr == 2016) %>% arrange(desc(PC1));print(draft2016)
draft2015 = final.df %>% filter(Yr == 2015) %>% arrange(desc(PC1));print(draft2015)
