# AIC Criterion
aic <- vector()

fit1 <- glm(marmad ~ conf + W, data = df)
aic[1] <- fit1$aic

fit2 <- glm(marmad ~ conf + W + SRS, data=df)
aic[2] <- fit2$aic

fit3 <- glm(marmad ~ conf + W + SRS + W.L., data = df)
aic[3] <- fit3$aic

fit4 <- glm(marmad ~ conf + W + SRS + L, data = df)
aic[4] <- fit4$aic

fit5 <- glm(marmad ~ conf + W + SRS + L + point.diff, data = df)
aic[5] <- fit5$aic

fit6 <- glm(marmad ~ conf + W + SRS + L + point.diff + Away.L, data = df)
aic[6] <- fit6$aic

fit7 <- glm(marmad ~ conf + W + SRS + L + point.diff + G, data = df)
aic[7] <- fit7$aic

fit8 <- glm(marmad ~ conf + W + SRS + L + point.diff + MP, data = df)
aic[8] <- fit8$aic

fit9 <- glm(marmad ~ conf + W + SRS + L + point.diff + Home.L, data = df)
aic[9] <- fit9$aic

fit10 <- glm(marmad ~ conf + W + SRS + L + point.diff + Home.L + Conf.W, data = df)
aic[10] <- fit10$aic

fit11 <- glm(marmad ~ conf + W + SRS + L + point.diff + Home.L + Conf.W + Points.S, data = df)
aic[11] <- fit11$aic

fit12 <- glm(marmad ~ conf + W + SRS + L + point.diff + Home.L + Conf.W + Home.W, data = df)
aic[12] <- fit12$aic

fit13 <- glm(marmad ~ conf + W + SRS + L + point.diff + Home.L + Conf.W + Home.W + FG, data = df)
aic[13] <- fit13$aic

fit14 <- glm(marmad ~ conf + W + SRS + L + point.diff + Home.L + Conf.W + Home.W + Conf.L, data = df)
aic[14] <- fit14$aic

fit15 <- glm(marmad ~ conf + W + SRS + L + point.diff + Home.L + Conf.W + Home.W + AST, data = df)
aic[15] <- fit15$aic

fit16 <- glm(marmad ~ conf + W + SRS + L + point.diff + Home.L + Conf.W + Home.W + TRB, data = df)
aic[16] <- fit16$aic

fit17 <- glm(marmad ~ conf + W + SRS + L + point.diff + Home.L + Conf.W + Home.W + SOS, data = df)
aic[17] <- fit17$aic

fit18 <- glm(marmad ~ conf + W + SRS + L + point.diff + Home.L + Conf.W + Home.W + SOS + FGA, data = df)
aic[18] <- fit18$aic

fit19 <- glm(marmad ~ conf + W + SRS + L + point.diff + Home.L + Conf.W + Home.W + SOS + FT, data = df)
aic[19] <- fit19$aic

fit20 <- glm(marmad ~ conf + W + SRS + L + point.diff + Home.L + Conf.W + Home.W + SOS + FT + FG., data = df)
aic[20] <- fit20$aic

fit21 <- glm(marmad ~ conf + W + SRS + L + point.diff + Home.L + Conf.W + Home.W + SOS + FT + Away.W, data = df)
aic[21] <- fit21$aic

fit22 <- glm(marmad ~ conf + W + SRS + L + point.diff + Home.L + Conf.W + Home.W + SOS + FT + Away.W + FTA, data = df)
aic[22] <- fit22$aic

fit23 <- glm(marmad ~ conf + W + SRS + L + point.diff + Home.L + Conf.W + Home.W + SOS + FT + Away.W + BLK, data = df)
aic[23] <- fit23$aic

fit24 <- glm(marmad ~ conf + W + SRS + L + point.diff + Home.L + Conf.W + Home.W + SOS + FT + Away.W + BLK + STL, data = df)
aic[24] <- fit24$aic

fit25 <- glm(marmad ~ conf + W + SRS + L + point.diff + Home.L + Conf.W + Home.W + SOS + FT + Away.W + BLK + STL + ORB, data = df)
aic[25] <- fit25$aic

fit26 <- glm(marmad ~ conf + W + SRS + L + point.diff + Home.L + Conf.W + Home.W + SOS + FT + Away.W + BLK + STL + X3P., data = df)
aic[26] <- fit26$aic

fit27 <- glm(marmad ~ conf + W + SRS + L + point.diff + Home.L + Conf.W + Home.W + SOS + FT + Away.W + BLK + STL + X3P, data = df)
aic[27] <- fit27$aic

fit28 <- glm(marmad ~ conf + W + SRS + L + point.diff + Home.L + Conf.W + Home.W + SOS + FT + Away.W + BLK + STL + X3PA, data = df)
aic[28] <- fit28$aic

fit29 <- glm(marmad ~ conf + W + SRS + L + point.diff + Home.L + Conf.W + Home.W + SOS + FT + Away.W + BLK + STL + FT., data = df)
aic[29] <- fit29$aic

fit30 <- glm(marmad ~ conf + W + SRS + L + point.diff + Home.L + Conf.W + Home.W + SOS + FT + Away.W + BLK + STL + PF, data = df)
aic[30] <- fit30$aic

fit31 <- glm(marmad ~ conf + W + SRS + L + point.diff + Home.L + Conf.W + Home.W + SOS + FT + Away.W + BLK + STL + Points.A, data = df)
aic[31] <- fit31$aic

fit32 <- glm(marmad ~ conf + W + SRS + L + point.diff + Home.L + Conf.W + Home.W + SOS + FT + Away.W + BLK + STL + TOV, data = df)
aic[32] <- fit32$aic

aic.val <- data.frame(aic)
aic.val$model <- seq(1, nrow(aic.val))
aic.val$optimal <- ifelse(aic.val$aic == min(aic.val$aic), "Optimal Point", "")

print(aic.val)
ggplot(data=aic.val, aes(x=model, y=aic)) + geom_line(linetype = "dashed") + geom_point(aes(color = optimal)) + 
  theme_bw() + scale_x_continuous("Model", breaks = seq(1,length(aic.val$model))) + 
  scale_y_continuous("AIC", breaks = seq(600, 1600, by=100)) + theme(legend.position = "none") + scale_color_manual(values = c("black", "red3")) + ggtitle("AIC: Measuring Goodness of Fit for each Model")
