library(rvest)
library(ggplot2)
library(ggthemes)
library(gam)
library(tidyr)
library(formattable)
library(dplyr)

df = read.csv("finaldf.csv")[,-1]
dat = df %>% filter(Yr > 2007 & Yr < 2015); summary(dat$Yr)



# Correlation Plot
dat.numeric <- select(dat, -Tm, -Yrs, -College, -Player, -Yr)
draft.cor1 <- data.frame(abs(cor(dat.numeric)[,"allstar"]))
colnames(draft.cor1) <- "cor"
draft.cor1$var <- rownames(draft.cor1)
mm.cor1 <- select(arrange(draft.cor1, desc(cor)), var, cor)
mm.cor1 <- mm.cor1[2:nrow(mm.cor1),]
pl1 = ggplot(data=mm.cor1, aes(x=reorder(var, cor), y=cor)) + geom_bar(stat = "identity", width = I(1/8), position = position_dodge(width=0.2)) + coord_flip() + scale_x_discrete("Variable") + theme_clean() + scale_y_continuous("Absolute Value of Correlation", breaks = seq(0, 0.60, by = 0.10)) + ggtitle("Variable Correlation", subtitle = "Correlation with All Star Appearences")

aic <- vector()

fit1 <- lm(allstar ~ VORP, data = dat)
aic[1] <- AIC(fit1)

fit2 <- lm(allstar ~ VORP + p.FT, data = dat)
aic[2] <- AIC(fit2)

fit3 <- lm(allstar ~ VORP + p.FT + PTS, data = dat)
aic[3] <- AIC(fit3)

# PTS increased AIC

fit4 <- lm(allstar ~ VORP + p.FT + WS, data = dat)
aic[4] <- AIC(fit4)

fit5 <- lm(allstar ~ VORP + p.FT + WS + PPG, data = dat)
aic[5] <- AIC(fit5)

# PPG increased AIC

fit6 <- lm(allstar ~ VORP + p.FT + WS + p.FTA, data = dat)
aic[6] <- AIC(fit6)

# p.FTA increased AIC

fit7 <- lm(allstar ~ VORP + p.FT + WS + p.PTS, data = dat)
aic[7] <- AIC(fit7)

fit8 <- lm(allstar ~ VORP + p.FT + WS + p.PTS + BPM, data = dat)
aic[8] <- AIC(fit8)

fit9 <- lm(allstar ~ VORP + p.FT + WS + p.PTS + BPM + p.PER, data = dat)
aic[9] <- AIC(fit9)

fit10 <- lm(allstar ~ VORP + p.FT + WS + p.PTS + BPM + p.PER + p.FG, data = dat)
aic[10] <- AIC(fit10)

# p.FG increased AIC

fit11 <- lm(allstar ~ VORP + p.FT + WS + p.PTS + BPM + p.PER + p.FGA, data = dat)
aic[11] <- AIC(fit11)

# p.FGA increased AIC

fit12 <- lm(allstar ~ VORP + p.FT + WS + p.PTS + BPM + p.PER + p.TOV, data = dat)
aic[12] <- AIC(fit12)

fit13 <- lm(allstar ~ VORP + p.FT + WS + p.PTS + BPM + p.PER + p.TOV + AST, data = dat)
aic[13] <- AIC(fit13)

# AST increased AIC

fit14 <- lm(allstar ~ VORP + p.FT + WS + p.PTS + BPM + p.PER + p.TOV + p.X2PA, data = dat)
aic[14] <- AIC(fit14)

fit15 <- lm(allstar ~ VORP + p.FT + WS + p.PTS + BPM + p.PER + p.TOV + p.X2PA + p.X2P, data = dat)
aic[15] <- AIC(fit15)

# p.X2P increased AIC

fit16 <- lm(allstar ~ VORP + p.FT + WS + p.PTS + BPM + p.PER + p.TOV + p.X2PA + MPG, data = dat)
aic[16] <- AIC(fit16)

# MPG increased AIC

fit17 <- lm(allstar ~ VORP + p.FT + WS + p.PTS + BPM + p.PER + p.TOV + p.X2PA + p.AST, data = dat)
aic[17] <- AIC(fit17)

fit18 <- lm(allstar ~ VORP + p.FT + WS + p.PTS + BPM + p.PER + p.TOV + p.X2PA + p.AST + MP, data = dat)
aic[18] <- AIC(fit18)

fit19 <- lm(allstar ~ VORP + p.FT + WS + p.PTS + BPM + p.PER + p.TOV + p.X2PA + p.AST + MP + APG, data = dat)
aic[19] <- AIC(fit19)

# APG increased AIC

fit20 <- lm(allstar ~ VORP + p.FT + WS + p.PTS + BPM + p.PER + p.TOV + p.X2PA + p.AST + MP + WS.48, data = dat)
aic[20] <- AIC(fit20)

fit21 <- lm(allstar ~ VORP + p.FT + WS + p.PTS + BPM + p.PER + p.TOV + p.X2PA + p.AST + MP + WS.48 + p.STL, data = dat)
aic[21] <- AIC(fit21)

# p.STL increased AIC

fit22 <- lm(allstar ~ VORP + p.FT + WS + p.PTS + BPM + p.PER + p.TOV + p.X2PA + p.AST + MP + WS.48 + p.MP, data = dat)
aic[22] <- AIC(fit22)

fit23 <- lm(allstar ~ VORP + p.FT + WS + p.PTS + BPM + p.PER + p.TOV + p.X2PA + p.AST + MP + WS.48 + p.MP + TRB, data = dat)
aic[23] <- AIC(fit23)

fit24 <- lm(allstar ~ VORP + p.FT + WS + p.PTS + BPM + p.PER + p.TOV + p.X2PA + p.AST + MP + WS.48 + p.MP + TRB + p.DRB, data = dat)
aic[24] <- AIC(fit24)

fit25 <- lm(allstar ~ VORP + p.FT + WS + p.PTS + BPM + p.PER + p.TOV + p.X2PA + p.AST + MP + WS.48 + p.MP + TRB + p.DRB + p.X3PA, data = dat)
aic[25] <- AIC(fit25)

# p.X3PA increased AIC

fit26 <- lm(allstar ~ VORP + p.FT + WS + p.PTS + BPM + p.PER + p.TOV + p.X2PA + p.AST + MP + WS.48 + p.MP + TRB + p.DRB + p.X3P, data = dat)
aic[26] <- AIC(fit26)

# p.X3P increased AIC

fit27 <- lm(allstar ~ VORP + p.FT + WS + p.PTS + BPM + p.PER + p.TOV + p.X2PA + p.AST + MP + WS.48 + p.MP + TRB + p.DRB + p.TRB, data = dat)
aic[27] <- AIC(fit27)

fit28 <- lm(allstar ~ VORP + p.FT + WS + p.PTS + BPM + p.PER + p.TOV + p.X2PA + p.AST + MP + WS.48 + p.MP + TRB + p.DRB + p.TRB + RPG, data = dat)
aic[28] <- AIC(fit28)

# RPG increased AIC

fit29 <- lm(allstar ~ VORP + p.FT + WS + p.PTS + BPM + p.PER + p.TOV + p.X2PA + p.AST + MP + WS.48 + p.MP + TRB + p.DRB + p.TRB + G, data = dat)
aic[29] <- AIC(fit29)

fit30 <- lm(allstar ~ VORP + p.FT + WS + p.PTS + BPM + p.PER + p.TOV + p.X2PA + p.AST + MP + WS.48 + p.MP + TRB + p.DRB + p.TRB + G + p.FT., data = dat)
aic[30] <- AIC(fit30)

# p.FT increased AIC

fit31 <- lm(allstar ~ VORP + p.FT + WS + p.PTS + BPM + p.PER + p.TOV + p.X2PA + p.AST + MP + WS.48 + p.MP + TRB + p.DRB + p.TRB + G + p.PF, data = dat)
aic[31] <- AIC(fit31)

fit32 <- lm(allstar ~ VORP + p.FT + WS + p.PTS + BPM + p.PER + p.TOV + p.X2PA + p.AST + MP + WS.48 + p.MP + TRB + p.DRB + p.TRB + G + p.PF + p.G, data = dat)
aic[32] <- AIC(fit32)

# p.G increaed AIC 

fit33 <- lm(allstar ~ VORP + p.FT + WS + p.PTS + BPM + p.PER + p.TOV + p.X2PA + p.AST + MP + WS.48 + p.MP + TRB + p.DRB + p.TRB + G + p.PF + p.BLK, data = dat)
aic[33] <- AIC(fit33)

fit34 <- lm(allstar ~ VORP + p.FT + WS + p.PTS + BPM + p.PER + p.TOV + p.X2PA + p.AST + MP + WS.48 + p.MP + TRB + p.DRB + p.TRB + G + p.PF + p.BLK + p.X3P., data = dat)
aic[34] <- AIC(fit34)

# p.X3P. increased AIC

fit35 <- lm(allstar ~ VORP + p.FT + WS + p.PTS + BPM + p.PER + p.TOV + p.X2PA + p.AST + MP + WS.48 + p.MP + TRB + p.DRB + p.TRB + G + p.PF + p.BLK + X3P., data = dat)
aic[35] <- AIC(fit35)

# X3P. increased AIC

fit36 <- lm(allstar ~ VORP + p.FT + WS + p.PTS + BPM + p.PER + p.TOV + p.X2PA + p.AST + MP + WS.48 + p.MP + TRB + p.DRB + p.TRB + G + p.PF + p.BLK + p.ORB, data = dat)
aic[36] <- AIC(fit36)

fit37 <- lm(allstar ~ VORP + p.FT + WS + p.PTS + BPM + p.PER + p.TOV + p.X2PA + p.AST + MP + WS.48 + p.MP + TRB + p.DRB + p.TRB + G + p.PF + p.BLK + p.ORB + p.eFG., data = dat)
aic[37] <- AIC(fit37)

# p.eFG. increased AIC

fit38 <- lm(allstar ~ VORP + p.FT + WS + p.PTS + BPM + p.PER + p.TOV + p.X2PA + p.AST + MP + WS.48 + p.MP + TRB + p.DRB + p.TRB + G + p.PF + p.BLK + p.ORB + p.X2P., data = dat)
aic[38] <- AIC(fit38)

# p.X2P. increased AIC

fit39 <- lm(allstar ~ VORP + p.FT + WS + p.PTS + BPM + p.PER + p.TOV + p.X2PA + p.AST + MP + WS.48 + p.MP + TRB + p.DRB + p.TRB + G + p.PF + p.BLK + p.ORB + FG., data = dat)
aic[39] <- AIC(fit39)

# FG. increased AIC

fit40 <- lm(allstar ~ VORP + p.FT + WS + p.PTS + BPM + p.PER + p.TOV + p.X2PA + p.AST + MP + WS.48 + p.MP + TRB + p.DRB + p.TRB + G + p.PF + p.BLK + p.ORB + p.FG., data = dat)
aic[40] <- AIC(fit40)

aic.val <- data.frame(aic)
aic.val$model <- seq(1, nrow(aic.val))
aic.val$optimal <- ifelse(aic.val$aic == min(aic.val$aic), "Optimal Point", "")

print(aic.val)
ggplot(data=aic.val, aes(x=model, y=aic)) + geom_line(linetype = "dashed") + geom_point(aes(color = optimal)) + 
  theme_bw() + scale_x_continuous("Model", breaks = seq(1,length(aic.val$model))) + 
  scale_y_continuous("AIC", breaks = seq(2600, 3100, by=25)) + theme(legend.position = "none") + 
  scale_color_manual(values = c("black", "red3")) + ggtitle("AIC: Measuring Goodness of Fit for each Model")

# MODEL 24:
lm.model <-lm(allstar ~ VORP + p.FT + WS + p.PTS + BPM + p.PER + p.TOV + p.X2PA + p.AST + MP + WS.48 + p.MP + TRB + p.DRB, data = dat)
print(anova(lm.model))
summary(lm.model)$coefficients[,"Estimate"]

# Final GAM Model 
gam.model = gam(allstar ~ s(VORP, 3) + s(p.FT, 3) + s(WS, 3) + s(p.PTS, 3) + s(BPM, 3) + s(p.PER, 3) + s(p.TOV, 3) + s(p.X2PA, 3) + 
                  s(p.AST, 3) + s(MP, 3) + s(WS.48, 3) + s(p.MP, 3) + s(TRB, 3) + s(p.DRB, 3), data = dat)
lm.model <-lm(allstar ~ VORP + p.FT + WS + p.PTS + BPM + p.PER + p.TOV + p.X2PA + p.AST + MP + WS.48 + p.MP + TRB + p.DRB, data = dat)

sse.gam = sum((gam.model$residuals)^2)
sse.lm = sum((lm.model$residuals)^2)
df.sse = c(sse.gam, sse.lm)
df.sse = data.frame(df.sse)
df.sse$Model = c("GAM", "LM")
ggplot(data = df.sse, aes(x = Model, y = df.sse))

# Preliminary Assessment of Predicted All Star Appearances
allstar.pred = predict(lm.model, dat)
as.dat = cbind.data.frame(dat, allstar.pred)
head(as.dat %>% filter(Yr == 2014) %>% arrange(desc(allstar.pred)) %>% select(Tm, Player, College, WS, VORP, allstar, allstar.pred), 30)
head(as.dat %>% filter(Yr == 2013) %>% arrange(desc(allstar.pred)) %>% select(Tm, Player, College, WS, VORP, allstar, allstar.pred), 30)
head(as.dat %>% filter(Yr == 2012) %>% arrange(desc(allstar.pred)) %>% select(Tm, Player, College, WS, VORP, allstar, allstar.pred), 30)
head(as.dat %>% filter(Yr == 2011) %>% arrange(desc(allstar.pred)) %>% select(Tm, Player, College, WS, VORP, allstar, allstar.pred), 30)
head(as.dat %>% filter(Yr == 2010) %>% arrange(desc(allstar.pred)) %>% select(Tm, Player, College, WS, VORP, allstar, allstar.pred), 30)
head(as.dat %>% filter(Yr == 2009) %>% arrange(desc(allstar.pred)) %>% select(Tm, Player, College, WS, VORP, allstar, allstar.pred), 30)
head(as.dat %>% filter(Yr == 2008) %>% arrange(desc(allstar.pred)) %>% select(Tm, Player, College, WS, VORP, allstar, allstar.pred), 30)
