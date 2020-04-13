draftpool1 = read.csv("drafts20152019.csv")[,-1]

link = "https://www.sports-reference.com/cbb/conferences/big-12/2020-stats.html"
page <- read_html(link)
data.raw <- html_table(page, fill=TRUE)








