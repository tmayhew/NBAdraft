dfYEAR = df %>% filter(Year == YEAR)
dfcYEAR = dfc %>% filter(Yr == YEAR)
fYEAR = dfcYEAR %>% left_join(dfYEAR, key = "Player")
write.csv(fYEAR, "YEARc.csv")
dfYEAR = read.csv("YEARc.csv")[,-1]
