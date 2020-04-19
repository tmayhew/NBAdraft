library(rvest)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(nbastatR)

'%!in%' <- function(x,y)!('%in%'(x,y))

draftpool1 = read.csv("data/drafts20152019.csv")[,-1]
draftpool2 = read.csv("data/drafts20082014.csv")[,-1]
draftpool3 = read.csv("data/drafts19952007.csv")[,-1]
sch.df = read.csv("data/schools.csv")[,-1]
sch.df$school = as.character(sch.df$school)
sch.df$conf = as.character(sch.df$conf)

df = rbind.data.frame(draftpool1, draftpool2, draftpool3) %>% select(pk, Player, Yr, Yrs, allstar, PC1, College)

for (i in 1:nrow(df)){
  if (df$pk[i] < 4){
    df$rank[i] = "Top 3"
  } else if (df$pk[i] < 6){
    df$rank[i] = "Top 5"
  } else if (df$pk[i] < 11){
    df$rank[i] = "Top 10"
  } else if (df$pk[i] < 15){
    df$rank[i] = "Lottery"
  } else{
    df$rank[i] = "Non-Lottery"
  }
}
for (i in 1:nrow(df)){
  if (df$College[i] == ""){
    df$cbb[i] = 0
  } else{
    df$cbb[i] = 1
  }
}

dfc = df %>% filter(College != "" & College != "University of West Florida" & College != "Butler County Community College" & College != "Augsburg College" & College != "Northeast Mississippi Community College" & College != "Shaw")
# 618/750 (82.4%) of players in the entire dataset played D1 basketball.

for (i in 1:nrow(dfc)){
  dfc$conf[i] = sch.df$conf[which(as.character(sch.df$school) == dfc$College[i])]
}

#############################################################################
dfc$conf = factor(dfc$conf)
newtab = data.frame(count = summary(dfc$conf))
newtab[,2] = rownames(newtab)
names(newtab) = c("count", "conf")
rownames(newtab) = NULL

newtab1 = newtab %>% arrange(desc(count)) 
newtab1$conf = factor(as.character(newtab1$conf), levels = newtab1$conf)
newtab1 %>% ggplot(aes(x = conf, y = count)) + 
  geom_bar(stat = "identity", width = I(1/2)) + coord_flip() + 
  scale_y_continuous("Players") + theme_clean() + scale_x_discrete("")


dfc = dfc %>% arrange(desc(Yr))
dfc$Player = factor(as.character(dfc$Player), levels = dfc$Player)
plot = c("Ja Morant", "Pascal Siakam", "Karl-Anthony Towns", "Elfrid Payton", "CJ McCollum", "Kyle O'Quinn", "James Harden", "George Hill", "Jason Thompson", "Paul Millsap", "Kevin Martin", "Wally Szczerbiak", "Shawn Marion", "Anthony Parker", "Steve Nash")
xnudge = c(0, 0, 0.5, 0, 0, 0, 0.5, 0, 0, 0, 0, 0, 0, 0, 0)
ynudge = c(2.93, 3.9, -0.2, 3.35, 4, 3.45, -.01, 3.15, 4.85, 3.60, 3.35, 3.7, 4.5, 4, 3.2)
nudges = cbind.data.frame(plot, xnudge, ynudge);nudges

dfc %>% filter(Player %in% plot) %>% arrange(desc(Yr))
dfc %>% ggplot(aes(x = conf, y = PC1)) + geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") + geom_boxplot() + coord_flip() + geom_text(aes(label = Player), data = dfc[dfc$Player %in% plot,], position = position_nudge(x = xnudge, y = ynudge)) + theme_bw() + 
  scale_y_continuous("Relative Career Productivity (PC1)", breaks = seq(-12,20,2), limits = c(-12,20)) + scale_x_discrete("Conference") + ggtitle("NBA Productivity by Conference, 1995-2019")



