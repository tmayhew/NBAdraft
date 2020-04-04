#cor-plot
df1.numeric <- select(df1, -Tm, -Yr, -conf)
marmad.cor1 <- data.frame(abs(cor(df1.numeric)[,"marmad"]))
colnames(marmad.cor1) <- "cor"
marmad.cor1$var <- rownames(marmad.cor1)
mm.cor1 <- select(arrange(marmad.cor1, desc(cor)), var, cor)
mm.cor1 <- mm.cor1[2:nrow(mm.cor1),]
pl1 = ggplot(data=mm.cor1, aes(x=reorder(var, cor), y=cor)) + geom_bar(stat = "identity", width = I(1/8), position = position_dodge(width=0.2)) + coord_flip() + scale_x_discrete("Variable") + theme_clean() + scale_y_continuous("Absolute Value of Correlation", breaks = seq(0, 0.60, by = 0.10)) + ggtitle("Variable Correlation", subtitle = "Mid-Major Teams")