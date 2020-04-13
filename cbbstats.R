draftpool1 = read.csv("drafts20152019.csv")[,-1]
draftpool2 = read.csv("drafts20082014.csv")[,-1]
draftpool3 = read.csv("drafts19952007.csv")[,-1]
df = rbind.data.frame(draftpool1, draftpool2, draftpool3)

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
head(df, 30)








