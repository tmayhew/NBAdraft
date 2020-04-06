dat = read.csv("allstardf.csv") %>% filter(Yr < 2008) %>% filter(WS > 0.01 & VORP > 0.01)
summary(dat$Yr)

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

allstar.pred = predict(lm.model, dat)
brnn.pred = 
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

