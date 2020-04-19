# YEAR #################################################################################################################
df = bref_bios(players = c(statsYEAR$Player))
df = df %>% filter(nameTable == "Transactions")
df = df %>% select(namePlayerBREF, urlPlayerBioBREF)
write.csv(df, "YEARplayers.csv")

dfYEAR = read.csv("YEARplayers.csv") %>% select(namePlayerBREF, urlPlayerBioBREF)
names(dfYEAR) = c("Player", "BREF")
dfYEAR$Player = as.character(dfYEAR$Player)
dfYEAR$BREF = as.character(dfYEAR$BREF)

statsYEAR$Player[which(statsYEAR$Player %!in% dfYEAR$Player)]
dfYEAR$Player[which(dfYEAR$Player %!in% statsYEAR$Player)]

rownames(dfYEAR) <- NULL

sc.YEARdf = scrape_nbapl2(dfYEAR)
write.csv(sc.YEARdf, "sc.YEARdf.csv")

dfYEAR = read.csv("sc.YEARdf.csv")
dfYEAR %>% select(Player, G, PTS, TRB, AST, STL, BLK, TOV) %>% arrange(desc(PTS))

