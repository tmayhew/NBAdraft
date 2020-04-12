library(rvest)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(formattable)
library(gam)
library(tidyr)

df = read.csv("finaldf.csv")[,-1]
dat = df %>% filter(Yr < 2008); summary(dat1$Yr)


# Correlation Plot
  dat.numeric <- select(dat, -Tm, -Yrs, -College, -Player, -Yr)
  draft.cor1 <- data.frame(abs(cor(dat.numeric)[,"allstar"]))
  colnames(draft.cor1) <- "cor"
  draft.cor1$var <- rownames(draft.cor1)
  mm.cor1 <- select(arrange(draft.cor1, desc(cor)), var, cor)
  mm.cor1 <- mm.cor1[2:nrow(mm.cor1),]
  pl1 = ggplot(data=mm.cor1, aes(x=reorder(var, cor), y=cor)) + geom_bar(stat = "identity", width = I(1/8), position = position_dodge(width=0.2)) + coord_flip() + scale_x_discrete("Variable") + theme_clean() + scale_y_continuous("Absolute Value of Correlation", breaks = seq(0, 1, by = 0.10)) + ggtitle("Variable Correlation", subtitle = "Correlation with All Star Appearences"); pl1

# AIC Plot
  aic <- vector()
  fit1 <- lm(allstar ~ VORP, data = dat)
  aic[1] <- AIC(fit1)
  
  fit2 <- lm(allstar ~ VORP + WS, data = dat)
  aic[2] <- AIC(fit2)
  
  fit3 <- lm(allstar ~ VORP + PTS, data = dat)
  aic[3] <- AIC(fit3)
  
  fit4 <- lm(allstar ~ VORP + PTS + p.PER, data = dat)
  aic[4] <- AIC(fit4)
  
  fit5 <- lm(allstar ~ VORP + PTS + PPG, data = dat)
  aic[5] <- AIC(fit5)
  
  fit6 <- lm(allstar ~ VORP + PTS + p.FT, data = dat)
  aic[6] <- AIC(fit6)
  
  fit7 <- lm(allstar ~ VORP + PTS + p.FT + p.FTA, data = dat)
  aic[7] <- AIC(fit7)
  
  fit8 <- lm(allstar ~ VORP + PTS + p.FT + p.PTS, data = dat)
  aic[8] <- AIC(fit8)
  
  fit9 <- lm(allstar ~ VORP + PTS + p.FT + p.PTS + BPM, data = dat)
  aic[9] <- AIC(fit9)
  
  fit10 <- lm(allstar ~ VORP + PTS + p.FT + p.PTS + BPM + MP, data = dat)
  aic[10] <- AIC(fit10)
  
  fit11 <- lm(allstar ~ VORP + PTS + p.FT + p.PTS + BPM + MP + p.FG, data = dat)
  aic[11] <- AIC(fit11)
  
  fit12 <- lm(allstar ~ VORP + PTS + p.FT + p.PTS + BPM + MP + p.FG + p.X2P, data = dat)
  aic[12] <- AIC(fit12)
  
  fit13 <- lm(allstar ~ VORP + PTS + p.FT + p.PTS + BPM + MP + p.FG + TRB, data = dat)
  aic[13] <- AIC(fit13)
  
  fit14 <- lm(allstar ~ VORP + PTS + p.FT + p.PTS + BPM + MP + p.FG + p.X2PA, data = dat)
  aic[14] <- AIC(fit14)
  
  fit15 <- lm(allstar ~ VORP + PTS + p.FT + p.PTS + BPM + MP + p.FG + p.FGA, data = dat)
  aic[15] <- AIC(fit15)
  
  fit16 <- lm(allstar ~ VORP + PTS + p.FT + p.PTS + BPM + MP + p.FG + AST, data = dat)
  aic[16] <- AIC(fit16)
  
  fit17 <- lm(allstar ~ VORP + PTS + p.FT + p.PTS + BPM + MP + p.FG + WS.48, data = dat)
  aic[17] <- AIC(fit17)
  
  fit18 <- lm(allstar ~ VORP + PTS + p.FT + p.PTS + BPM + MP + p.FG + WS.48 + p.TOV, data = dat)
  aic[18] <- AIC(fit18)
  
  fit19 <- lm(allstar ~ VORP + PTS + p.FT + p.PTS + BPM + MP + p.FG + WS.48 + p.TOV + MPG, data = dat)
  aic[19] <- AIC(fit19)
  
  fit20 <- lm(allstar ~ VORP + PTS + p.FT + p.PTS + BPM + MP + p.FG + WS.48 + p.TOV + MPG + G, data = dat)
  aic[20] <- AIC(fit20)
  
  fit21 <- lm(allstar ~ VORP + PTS + p.FT + p.PTS + BPM + MP + p.FG + WS.48 + p.TOV + MPG + p.MP, data = dat)
  aic[21] <- AIC(fit21)
  
  fit22 <- lm(allstar ~ VORP + PTS + p.FT + p.PTS + BPM + MP + p.FG + WS.48 + p.TOV + MPG + p.DRB, data = dat)
  aic[22] <- AIC(fit22)
  
  fit23 <- lm(allstar ~ VORP + PTS + p.FT + p.PTS + BPM + MP + p.FG + WS.48 + p.TOV + MPG + APG, data = dat)
  aic[23] <- AIC(fit23)
  
  fit24 <- lm(allstar ~ VORP + PTS + p.FT + p.PTS + BPM + MP + p.FG + WS.48 + p.TOV + MPG + p.AST, data = dat)
  aic[24] <- AIC(fit24)
  
  fit25 <- lm(allstar ~ VORP + PTS + p.FT + p.PTS + BPM + MP + p.FG + WS.48 + p.TOV + MPG + RPG, data = dat)
  aic[25] <- AIC(fit25)
  
  fit26 <- lm(allstar ~ VORP + PTS + p.FT + p.PTS + BPM + MP + p.FG + WS.48 + p.TOV + MPG + p.TRB, data = dat)
  aic[26] <- AIC(fit26)
  
  fit27 <- lm(allstar ~ VORP + PTS + p.FT + p.PTS + BPM + MP + p.FG + WS.48 + p.TOV + MPG + p.STL, data = dat)
  aic[27] <- AIC(fit27)
  
  fit28 <- lm(allstar ~ VORP + PTS + p.FT + p.PTS + BPM + MP + p.FG + WS.48 + p.TOV + MPG + p.STL + p.G, data = dat)
  aic[28] <- AIC(fit28)
  
  fit29 <- lm(allstar ~ VORP + PTS + p.FT + p.PTS + BPM + MP + p.FG + WS.48 + p.TOV + MPG + p.STL + p.G + p.BLK, data = dat)
  aic[29] <- AIC(fit29)
  
  fit30 <- lm(allstar ~ VORP + PTS + p.FT + p.PTS + BPM + MP + p.FG + WS.48 + p.TOV + MPG + p.STL + p.G + p.X2P., data = dat)
  aic[30] <- AIC(fit30)
  
  fit31 <- lm(allstar ~ VORP + PTS + p.FT + p.PTS + BPM + MP + p.FG + WS.48 + p.TOV + MPG + p.STL + p.G + p.FT., data = dat)
  aic[31] <- AIC(fit31)
  
  fit32 <- lm(allstar ~ VORP + PTS + p.FT + p.PTS + BPM + MP + p.FG + WS.48 + p.TOV + MPG + p.STL + p.G + FT., data = dat)
  aic[32] <- AIC(fit32)
  
  fit33 <- lm(allstar ~ VORP + PTS + p.FT + p.PTS + BPM + MP + p.FG + WS.48 + p.TOV + MPG + p.STL + p.G + p.X3PA, data = dat)
  aic[33] <- AIC(fit33)
  
  fit34 <- lm(allstar ~ VORP + PTS + p.FT + p.PTS + BPM + MP + p.FG + WS.48 + p.TOV + MPG + p.STL + p.G + p.FG., data = dat)
  aic[34] <- AIC(fit34)
  
  fit35 <- lm(allstar ~ VORP + PTS + p.FT + p.PTS + BPM + MP + p.FG + WS.48 + p.TOV + MPG + p.STL + p.G + FG., data = dat)
  aic[35] <- AIC(fit35)
  
  fit36 <- lm(allstar ~ VORP + PTS + p.FT + p.PTS + BPM + MP + p.FG + WS.48 + p.TOV + MPG + p.STL + p.G + FG. + p.X3P, data = dat)
  aic[36] <- AIC(fit36)
  
  fit37 <- lm(allstar ~ VORP + PTS + p.FT + p.PTS + BPM + MP + p.FG + WS.48 + p.TOV + MPG + p.STL + p.G + FG. + p.eFG., data = dat)
  aic[37] <- AIC(fit37)
  
  fit38 <- lm(allstar ~ VORP + PTS + p.FT + p.PTS + BPM + MP + p.FG + WS.48 + p.TOV + MPG + p.STL + p.G + FG. + p.eFG. + p.ORB, data = dat)
  aic[38] <- AIC(fit38)
  
  fit39 <- lm(allstar ~ VORP + PTS + p.FT + p.PTS + BPM + MP + p.FG + WS.48 + p.TOV + MPG + p.STL + p.G + FG. + p.eFG. + p.X3P., data = dat)
  aic[39] <- AIC(fit39)
  
  fit40 <- lm(allstar ~ VORP + PTS + p.FT + p.PTS + BPM + MP + p.FG + WS.48 + p.TOV + MPG + p.STL + p.G + FG. + p.eFG. + p.PF, data = dat)
  aic[40] <- AIC(fit40)
  
  fit41 <- lm(allstar ~ VORP + PTS + p.FT + p.PTS + BPM + MP + p.FG + WS.48 + p.TOV + MPG + p.STL + p.G + FG. + p.eFG. + X3P., data = dat)
  aic[41] <- AIC(fit41)
  
  aic.val <- data.frame(aic)
  aic.val$model <- seq(1, nrow(aic.val))
  aic.val$optimal <- ifelse(aic.val$aic == min(aic.val$aic), "Optimal Point", "")
  
  print(aic.val)
  pl = ggplot(data=aic.val, aes(x=model, y=aic)) + geom_line(linetype = "dashed") + geom_point(aes(color = optimal)) + theme_bw() + scale_x_continuous("Model", breaks = seq(1,length(aic.val$model))) + scale_y_continuous("AIC") + theme(legend.position = "none") + scale_color_manual(values = c("black", "red3")) + ggtitle("AIC: Measuring Goodness of Fit for each Model");pl
  
# Model Selected: Model 37
  lm.model = lm(allstar ~ VORP + PTS + p.FT + p.PTS + BPM + MP + p.FG + WS.48 + p.TOV + MPG + p.STL + p.G + FG. + p.eFG., data = dat)

# Observing Spline Fit: VORP
  #gam.model1 = gam(allstar ~ s(VORP, 10), data = dat)
      # VORP
  #df1 = cbind.data.frame(X = dat$VORP, Y = dat$allstar, Type = "Real")
  #tobind = cbind.data.frame(X = dat$VORP, Y = gam.model1$fitted.values, Type = "Predicted")
  #df2 = rbind.data.frame(df1, tobind)
  #ggplot(data = subset(df2, Type == "Real"), aes(x = X, y = Y)) + geom_point(alpha = I(2/5)) + 
    #theme_bw() + geom_smooth(data = subset(df2, Type = "Predicted"), method = "loess", 
    #                         linetype = "dashed", formula = "y~x", se = F) + 
    #scale_x_continuous("VORP") + scale_y_continuous("All Stars")

# Final GAM Model
gam.model = gam(allstar ~ s(VORP, 3) + s(PTS, 3) + s(p.FT, 3) + s(p.PTS, 3) + s(BPM, 3) + s(MP, 3) + s(p.FG, 3) + s(WS.48, 3) + s(p.TOV, 3) + s(MPG, 3) + s(p.STL, 3) + s(p.G, 3) + s(FG., 3) + s(p.eFG.), data = dat)
lm.model = lm(allstar ~ VORP + PTS + p.FT + p.PTS + BPM + MP + p.FG + WS.48 + p.TOV + MPG + p.STL + p.G + FG. + p.eFG., data = dat)

## SSE:
  sse.gam = sum((gam.model$residuals)^2)
  sse.lm = sum((lm.model$residuals)^2)
  df.sse = c(sse.gam, sse.lm)
  df.sse = data.frame(df.sse)
  df.sse$Model = c("GAM", "LM")
  ggplot(data = df.sse, aes(x = Model, y = df.sse)) + geom_bar(stat = "identity", width = I(1/8), color = "black", fill = "steelblue", alpha = I(3/4)) + theme_clean() + scale_y_continuous("SSE")
  

# Apply Model to Predictions
allstar.pred = predict(gam.model, dat)
as.df = cbind.data.frame(dat, allstar.pred)
  
##################################################################
as.df %>% filter(Yr == 2007) %>% arrange(desc(allstar.pred)) %>% mutate(redraft = row_number(-allstar.pred)) %>% select(redraft, Tm, Player, College, WS, VORP, allstar, allstar.pred)
as.df %>% filter(Yr == 2006) %>% arrange(desc(allstar.pred)) %>% mutate(redraft = row_number(-allstar.pred)) %>% select(redraft, Tm, Player, College, WS, VORP, allstar, allstar.pred)
as.df %>% filter(Yr == 2005) %>% arrange(desc(allstar.pred)) %>% mutate(redraft = row_number(-allstar.pred)) %>% select(redraft, Tm, Player, College, WS, VORP, allstar, allstar.pred)
as.df %>% filter(Yr == 2004) %>% arrange(desc(allstar.pred)) %>% mutate(redraft = row_number(-allstar.pred)) %>% select(redraft, Tm, Player, College, WS, VORP, allstar, allstar.pred)
as.df %>% filter(Yr == 2003) %>% arrange(desc(allstar.pred)) %>% mutate(redraft = row_number(-allstar.pred)) %>% select(redraft, Tm, Player, College, WS, VORP, allstar, allstar.pred)
as.df %>% filter(Yr == 2002) %>% arrange(desc(allstar.pred)) %>% mutate(redraft = row_number(-allstar.pred)) %>% select(redraft, Tm, Player, College, WS, VORP, allstar, allstar.pred)
as.df %>% filter(Yr == 2001) %>% arrange(desc(allstar.pred)) %>% mutate(redraft = row_number(-allstar.pred)) %>% select(redraft, Tm, Player, College, WS, VORP, allstar, allstar.pred)
as.df %>% filter(Yr == 2000) %>% arrange(desc(allstar.pred)) %>% mutate(redraft = row_number(-allstar.pred)) %>% select(redraft, Tm, Player, College, WS, VORP, allstar, allstar.pred)
as.df %>% filter(Yr == 1999) %>% arrange(desc(allstar.pred)) %>% mutate(redraft = row_number(-allstar.pred)) %>% select(redraft, Tm, Player, College, WS, VORP, allstar, allstar.pred)
as.df %>% filter(Yr == 1998) %>% arrange(desc(allstar.pred)) %>% mutate(redraft = row_number(-allstar.pred)) %>% select(redraft, Tm, Player, College, WS, VORP, allstar, allstar.pred)
as.df %>% filter(Yr == 1997) %>% arrange(desc(allstar.pred)) %>% mutate(redraft = row_number(-allstar.pred)) %>% select(redraft, Tm, Player, College, WS, VORP, allstar, allstar.pred)
as.df %>% filter(Yr == 1996) %>% arrange(desc(allstar.pred)) %>% mutate(redraft = row_number(-allstar.pred)) %>% select(redraft, Tm, Player, College, WS, VORP, allstar, allstar.pred)
as.df %>% filter(Yr == 1995) %>% arrange(desc(allstar.pred)) %>% mutate(redraft = row_number(-allstar.pred)) %>% select(redraft, Tm, Player, College, WS, VORP, allstar, allstar.pred)
  
  
  


"as.draftyear = 
  as.df %>% 
    group_by(Yr) %>% 
    summarise(
      allstar = sum(allstar),
      pred.allstar = sum(allstar.pred)
    )"
#ggplot(data = as.draftyear2, aes(x = Yr, y = Value)) + geom_bar(stat = "identity", width = I(1/4), alpha = I(3/4), color = "black", aes(fill = Type), position = "dodge") + scale_x_continuous("Draft Year", breaks = seq(1995, 2007, by=1)) + scale_y_continuous("Total All Star Appearances") + theme_clean() + ggtitle("Draft Success: Number of All Star Appearances by Draft Year")
  

"as.draftyear2 = 
  as.draftyear %>% gather(allstar, pred.allstar, -Yr)
colnames(as.draftyear2) = c("Yr", "Type", "Value")"

