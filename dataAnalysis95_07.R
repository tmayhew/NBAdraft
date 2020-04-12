library(rvest)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(formattable)
library(gam)
library(tidyr)

dat = read.csv("allstardf.csv") %>% filter(Yr < 2008) %>% filter(WS > 0.1 & VORP > 0.1 | allstar == 1)
summary(dat$Yr)
dat[,6:21] = scale(dat[,6:21])

# Correlation Plot
  dat.numeric <- select(dat, -Tm, -Yrs, -College, -Player, -Yr)
  draft.cor1 <- data.frame(abs(cor(dat.numeric)[,"allstar"]))
  colnames(draft.cor1) <- "cor"
  draft.cor1$var <- rownames(draft.cor1)
  mm.cor1 <- select(arrange(draft.cor1, desc(cor)), var, cor)
  mm.cor1 <- mm.cor1[2:nrow(mm.cor1),]
  pl1 = ggplot(data=mm.cor1, aes(x=reorder(var, cor), y=cor)) + geom_bar(stat = "identity", width = I(1/8), position = position_dodge(width=0.2)) + coord_flip() + scale_x_discrete("Variable") + theme_clean() + scale_y_continuous("Absolute Value of Correlation", breaks = seq(0, 0.60, by = 0.10)) + ggtitle("Variable Correlation", subtitle = "Correlation with All Star Appearences")

# AIC Plot
  aic <- vector()
  fit1 <- glm(allstar ~ VORP, data = dat)
  aic[1] <- fit1$aic
  
  fit2 <- glm(allstar ~ VORP + WS, data = dat)
  aic[2] <- fit2$aic
  
  fit3 <- glm(allstar ~ VORP + PTS, data = dat)
  aic[3] <- fit3$aic
  
  fit4 <- glm(allstar ~ VORP + PTS + BPM, data = dat)
  aic[4] <- fit4$aic
  
  fit5 <- glm(allstar ~ VORP + PTS + BPM + PPG, data = dat)
  aic[5] <- fit5$aic
  
  fit6 <- glm(allstar ~ VORP + PTS + BPM + PPG + MP, data = dat)
  aic[6] <- fit6$aic
  
  fit7 <- glm(allstar ~ VORP + PTS + BPM + PPG + MP + WS.48, data = dat)
  aic[7] <- fit7$aic
  
  fit8 <- glm(allstar ~ VORP + PTS + BPM + PPG + MP + WS.48 + TRB, data = dat)
  aic[8] <- fit8$aic
  
  fit9 <- glm(allstar ~ VORP + PTS + BPM + PPG + MP + WS.48 + TRB + AST, data = dat)
  aic[9] <- fit9$aic
  
  fit10 <- glm(allstar ~ VORP + PTS + BPM + PPG + MP + WS.48 + TRB + AST + MPG, data = dat)
  aic[10] <- fit10$aic
  
  fit11 <- glm(allstar ~ VORP + PTS + BPM + PPG + MP + WS.48 + TRB + AST + MPG + G, data = dat)
  aic[11] <- fit11$aic
  
  fit12 <- glm(allstar ~ VORP + PTS + BPM + PPG + MP + WS.48 + TRB + AST + MPG + RPG, data = dat)
  aic[12] <- fit12$aic
  
  fit13 <- glm(allstar ~ VORP + PTS + BPM + PPG + MP + WS.48 + TRB + AST + MPG + APG, data = dat)
  aic[13] <- fit13$aic
  
  fit14 <- glm(allstar ~ VORP + PTS + BPM + PPG + MP + WS.48 + TRB + AST + MPG + FG., data = dat)
  aic[14] <- fit14$aic
  
  fit15 <- glm(allstar ~ VORP + PTS + BPM + PPG + MP + WS.48 + TRB + AST + MPG + FT., data = dat)
  aic[15] <- fit15$aic
  
  fit16 <- glm(allstar ~ VORP + PTS + BPM + PPG + MP + WS.48 + TRB + AST + MPG + X3P., data = dat)
  aic[16] <- fit16$aic
  
  
  
  aic.val <- data.frame(aic)
  aic.val$model <- seq(1, nrow(aic.val))
  aic.val$optimal <- ifelse(aic.val$aic == min(aic.val$aic), "Optimal Point", "")
  
  print(aic.val)
  ggplot(data=aic.val, aes(x=model, y=aic)) + geom_line(linetype = "dashed") + geom_point(aes(color = optimal)) + 
    theme_bw() + scale_x_continuous("Model", breaks = seq(1,length(aic.val$model))) + 
    scale_y_continuous("AIC") + theme(legend.position = "none") + scale_color_manual(values = c("black", "red3")) + ggtitle("AIC: Measuring Goodness of Fit for each Model")


# Model Selected: Model 10
  lm.model = lm(allstar ~ VORP + PTS + BPM + PPG + MP + WS.48 + TRB + AST + MPG, data = dat)
  anova(lm.model)
  summary(lm.model)$coefficients[,"Estimate"]

# Observing Spline Fit: VORP
  gam.model1 = gam(allstar ~ s(VORP, 3), data = dat)
  # VORP
  df1 = cbind.data.frame(X = dat$VORP, Y = dat$allstar, Type = "Real")
  tobind = cbind.data.frame(X = dat$VORP, Y = gam.model1$fitted.values, Type = "Predicted")
  df2 = rbind.data.frame(df1, tobind)
  ggplot(data = subset(df2, Type == "Real"), aes(x = X, y = Y)) + geom_point(alpha = I(2/5)) + 
    theme_bw() + geom_smooth(data = subset(df2, Type = "Predicted"), method = "loess", 
                             linetype = "dashed", formula = "y~x", se = F) + 
    scale_x_continuous("VORP") + scale_y_continuous("All Stars")


# Final GAM Model
gam.model = gam(allstar ~ s(VORP, 3) + s(WS, 3) + s(PTS, 3) + s(MP, 4) + s(TRB, 3) + s(AST, 3), data = dat)
lm.model = lm(allstar ~ VORP + PTS + BPM + PPG + MP + WS.48 + TRB + AST + MPG, data = dat)

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

as.draftyear = 
  as.df %>% 
    group_by(Yr) %>% 
    summarise(
      allstar = sum(allstar),
      pred.allstar = sum(allstar.pred)
    )

ggplot(data = as.draftyear2, aes(x = Yr, y = Value)) + geom_bar(stat = "identity", width = I(1/4), alpha = I(3/4), color = "black", aes(fill = Type), position = "dodge") + scale_x_continuous("Draft Year", breaks = seq(1995, 2007, by=1)) + scale_y_continuous("Total All Star Appearances") + theme_clean() + ggtitle("Draft Success: Number of All Star Appearances by Draft Year")

as.draftyear2 = 
  as.draftyear %>% gather(allstar, pred.allstar, -Yr)
colnames(as.draftyear2) = c("Yr", "Type", "Value")







<<<<<<< HEAD
# Model Selected: Model 10
lm.model = lm(allstar ~ VORP + PTS + BPM + PPG + MP + WS.48 + TRB + AST + MPG, data = dat)
anova(lm.model)
summary(lm.model)$coefficients[,"Estimate"]

allstar.pred = predict(lm.model, dat)
as.df = cbind.data.frame(dat, allstar.pred)

as.df %>% filter(Yr == 2007) %>% arrange(desc(allstar.pred)) %>% select(Tm, Player, College, WS, VORP, allstar, allstar.pred)
as.df %>% filter(Yr == 2006) %>% arrange(desc(allstar.pred)) %>% select(Tm, Player, College, WS, VORP, allstar, allstar.pred)
as.df %>% filter(Yr == 2005) %>% arrange(desc(allstar.pred)) %>% select(Tm, Player, College, WS, VORP, allstar, allstar.pred)
as.df %>% filter(Yr == 2004) %>% arrange(desc(allstar.pred)) %>% select(Tm, Player, College, WS, VORP, allstar, allstar.pred)
as.df %>% filter(Yr == 2003) %>% arrange(desc(allstar.pred)) %>% select(Tm, Player, College, WS, VORP, allstar, allstar.pred)
as.df %>% filter(Yr == 2002) %>% arrange(desc(allstar.pred)) %>% select(Tm, Player, College, WS, VORP, allstar, allstar.pred)
as.df %>% filter(Yr == 2001) %>% arrange(desc(allstar.pred)) %>% select(Tm, Player, College, WS, VORP, allstar, allstar.pred)
as.df %>% filter(Yr == 2000) %>% arrange(desc(allstar.pred)) %>% select(Tm, Player, College, WS, VORP, allstar, allstar.pred)
as.df %>% filter(Yr == 1999) %>% arrange(desc(allstar.pred)) %>% select(Tm, Player, College, WS, VORP, allstar, allstar.pred)
as.df %>% filter(Yr == 1998) %>% arrange(desc(allstar.pred)) %>% select(Tm, Player, College, WS, VORP, allstar, allstar.pred)
as.df %>% filter(Yr == 1997) %>% arrange(desc(allstar.pred)) %>% select(Tm, Player, College, WS, VORP, allstar, allstar.pred)
as.df %>% filter(Yr == 1996) %>% arrange(desc(allstar.pred)) %>% select(Tm, Player, College, WS, VORP, allstar, allstar.pred)
as.df %>% filter(Yr == 1995) %>% arrange(desc(allstar.pred)) %>% select(Tm, Player, College, WS, VORP, allstar, allstar.pred)
=======
>>>>>>> f0852f240be40f00a49a1f00bf30f640211ca053

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
  
