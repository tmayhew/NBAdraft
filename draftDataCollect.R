library(rvest)
library(ggplot2)
library(ggthemes)
library(dplyr)

scrape_cbbplayers = function(given_year = 2019){
  year = paste(given_year)
  link <- paste0("https://www.basketball-reference.com/draft/NBA_",year,".html")
  page <- read_html(link)
  data.raw <- html_table(page, fill=TRUE)
  
  players <- data.raw[[1]]
  players <- players[,-1]
  players <- players[,-1]
  players <- players[-1,]
  
  names(players) = c("Tm", "Player", "College", "Yrs", "G", "MP", "PTS", "TRB", "AST", "FG.", "X3P.", "FT.", "MPG", "PTS", "TRB", "AST", "WS", "WS.48", "BPM", "VORP")
  players = players[,-c(7:9)]
  
  for (i in 4:ncol(players)){
    for (j in 7:9){
      if (players[j,i] == ""){
        players[j,i] = "0"
      }
    }
    players[,i] <- as.double(players[,i])
  }
  players <- na.omit(players)
  players$Yr = given_year
  return(players)
}

stats2019 = arrange(scrape_cbbplayers(2019), -WS)
stats2018 = arrange(scrape_cbbplayers(2018), -WS)
stats2017 = arrange(scrape_cbbplayers(2017), -WS)
stats2016 = arrange(scrape_cbbplayers(2016), -WS)
stats2015 = arrange(scrape_cbbplayers(2015), -WS)
stats2014 = arrange(scrape_cbbplayers(2014), -WS)
stats2013 = arrange(scrape_cbbplayers(2013), -WS)
stats2012 = arrange(scrape_cbbplayers(2012), -WS)
stats2011 = arrange(scrape_cbbplayers(2011), -WS)
stats2010 = arrange(scrape_cbbplayers(2010), -WS)
stats2009 = arrange(scrape_cbbplayers(2009), -WS)
stats2008 = arrange(scrape_cbbplayers(2008), -WS)
stats2007 = arrange(scrape_cbbplayers(2007), -WS)
stats2006 = arrange(scrape_cbbplayers(2006), -WS)
stats2005 = arrange(scrape_cbbplayers(2005), -WS)
stats2004 = arrange(scrape_cbbplayers(2004), -WS)
stats2003 = arrange(scrape_cbbplayers(2003), -WS)
stats2002 = arrange(scrape_cbbplayers(2002), -WS)
stats2001 = arrange(scrape_cbbplayers(2001), -WS)
stats2000 = arrange(scrape_cbbplayers(2000), -WS)
stats1999 = arrange(scrape_cbbplayers(1999), -WS)
stats1998 = arrange(scrape_cbbplayers(1998), -WS)
stats1997 = arrange(scrape_cbbplayers(1997), -WS)
stats1996 = arrange(scrape_cbbplayers(1996), -WS)
stats1995 = arrange(scrape_cbbplayers(1995), -WS)




# Next Step: Using the NBA Draft Class dataframe, pair with all-star/all-nba appearances to rank the players
# Next Issue: find a way to deal with players who didn't play CBB

as.2020 <- as.list <- c("Giannis Antetokounmpo", "LeBron James", "Anthony Davis", "James Harden", 
                        "Luka Doncic", "Joel Embiid", "Kawhi Leonard", "Pascal Siakam", "Kemba Walker",
                        "Trae Young", "Bam Adebayo", "Brandon Ingram", "Chris Paul", "Rudy Gobert", 
                        "Damian Lillard", "Donovan Mitchell", "Nikola Jokic", "Ben Simmons", "Domantas Sabonis",
                        "Jayson Tatum", "Jimmy Butler", "Kyle Lowry", "Khris Middleton", "Russell Westbrook")
as.2019 <- c("LeBron James", "James Harden", "Kevin Durant", "Kyrie Irving", "Kawhi Leonard", 
             "Bradley Beal", "Anthony Davis", "Ben Simmons", "Damian Lillard", 
             "Karl-Anthony Towns", "Klay Thompson", "LaMarcus Aldridge", "Giannis Antetokounmpo", 
             "Stephen Curry", "Joel Embiid", "Paul George", "Kemba Walker", "Blake Griffin", 
             "D'Angelo Russell", "Khris Middleton", "Kyle Lowry", "Nikola Jokic", 
             "Nikola Vucevic", "Victor Oladipo", "Russell Westbrook")
as.2018 <- c("LeBron James", "James Harden", "Kevin Durant", "Kyrie Irving", "DeMarcus Cousins", "Bradley Beal", 
             "Anthony Davis", "Andre Drummond", "Goran Dragic", "Kemba Walker", "Paul George", "John Wall", 
             "Kevin Love", "Kristaps Porzingis", "LaMarcus Aldridge", "Victor Oladipo", "Russell Westbrook", 
             "Stephen Curry", "DeMar DeRozan", "Giannis Antetokounmpo", "Joel Embiid", "Al Horford", 
             "Draymond Green", "Jimmy Butler", "Damian Lillard", "Kyle Lowry", "Karl-Anthony Towns", "Klay Thompson")
as.2017 <- c("Kyrie Irving", "DeMar DeRozan", "LeBron James", "Jimmy Butler", "Giannis Antetokounmpo", "Isaiah Thomas", 
             "John Wall", "Kevin Love", "Carmelo Anthony", "Kyle Lowry", "Paul George", "Kemba Walker", "Paul Millsap", 
             "Stephen Curry", "James Harden", "Kevin Durant", "Kawhi Leonard", "Anthony Davis", "Russell Westbrook", 
             "Klay Thompson", "Draymond Green", "DeMarcus Cousins", "Marc Gasol", "DeAndre Jordan", "Gordon Hayward")
as.2016 <- c("Dwyane Wade", "Kyle Lowry", "LeBron James", "Paul George", "Carmelo Anthony", "Jimmy Butler", 
             "Chris Bosh", "John Wall", "Paul Millsap", "DeMar DeRozan", "Andre Drummond", "Isaiah Thomas", 
             "Pau Gasol", "Al Horford", "Stephen Curry", "Russell Westbrook", "Kobe Bryant", "Kevin Durant", 
             "Kawhi Leonard", "Chris Paul", "LaMarcus Aldridge", "James Harden", "Anthony Davis", 
             "DeMarcus Cousins", "Klay Thompson", "Draymond Green")
as.2015 <- c("John Wall", "Kyle Lowry", "LeBron James", "Pau Gasol", "Carmelo Anthony", "Al Horford", 
             "Chris Bosh", "Paul Millsap", "Jimmy Butler", "Dwyane Wade", "Jeff Teague", "Kyrie Irving", 
             "Kyle Korver", "Stephen Curry", "Kobe Bryant", "Anthony Davis", "Marc Gasol", "Blake Griffin", 
             "LaMarcus Aldridge", "Tim Duncan", "Kevin Durant", "Klay Thompson", "Russell Westbrook", 
             "James Harden", "Chris Paul", "DeMarcus Cousins", "Damian Lillard", "Dirk Nowitzki")
as.2014 <- c("Dwyane Wade", "Kyrie Irving", "LeBron James", "Paul George", "Carmelo Anthony", 
             "Joakim Noah", "Roy Hibbert", "Chris Bosh", "Paul Millsap", "John Wall", "Joe Johnson", 
             "DeMar DeRozan", "Stephen Curry", "Kobe Bryant", "Kevin Durant", "Blake Griffin", 
             "Kevin Love", "Dwight Howard", "LaMarcus Aldridge", "Dirk Nowitzki", "Chris Paul", 
             "James Harden", "Tony Parker", "Damian Lillard", "Anthony Davis")
as.2013 <- c("Rajon Rondo", "Dwyane Wade", "LeBron James", "Carmelo Anthony", "Kevin Garnett", 
             "Chris Bosh", "Tyson Chandler", "Luol Deng", "Paul George", "Jrue Holiday", 
             "Kyrie Irving", "Joakim Noah", "Brook Lopez", "Chris Paul", "Kobe Bryant", 
             "Kevin Durant", "Blake Griffin", "Dwight Howard", "LaMarcus Aldridge", 
             "Tim Duncan", "James Harden", "David Lee", "Tony Parker", "Zach Randolph", 
             "Russell Westbrook")
as.2012 <- c("Chris Paul", "Kobe Bryant", "Kevin Durant", "Blake Griffin", "Andrew Bynum", 
             "LaMarcus Aldridge", "Marc Gasol", "Kevin Love", "Steve Nash*", "Dirk Nowitzki", 
             "Tony Parker", "Russell Westbrook", "Derrick Rose", "Dwyane Wade", "LeBron James", 
             "Carmelo Anthony", "Dwight Howard", "Chris Bosh", "Luol Deng", "Roy Hibbert", 
             "Andre Iguodala", "Joe Johnson", "Paul Pierce", "Rajon Rondo", "Deron Williams")
as.2011 <- c("Derrick Rose", "Dwyane Wade", "LeBron James", "Amar'e Stoudemire", "Dwight Howard", 
             "Ray Allen*", "Chris Bosh", "Kevin Garnett", "Al Horford", "Joe Johnson", 
             "Paul Pierce", "Rajon Rondo", "Chris Paul", "Kobe Bryant", "Kevin Durant", 
             "Carmelo Anthony", "Yao Ming*", "Tim Duncan", "Pau Gasol", "Manu Ginobili", 
             "Blake Griffin", "Kevin Love", "Dirk Nowitzki", "Russell Westbrook", "Deron Williams")
as.2010 <- c("Allen Iverson*", "Dwyane Wade", "LeBron James", "Kevin Garnett", "Dwight Howard", 
             "Joe Johnson", "Rajon Rondo", "Derrick Rose", "Paul Pierce", "Gerald Wallace", 
             "Chris Bosh", "Al Horford", "David Lee", "Steve Nash*", "Kobe Bryant", "Carmelo Anthony", 
             "Tim Duncan", "Amar'e Stoudemire", "Chauncey Billups", "Jason Kidd*", "Chris Paul", 
             "Brandon Roy", "Deron Williams", "Kevin Durant", "Dirk Nowitzki", "Zach Randolph", 
             "Pau Gasol", "Chris Kaman")
as.2009 <- c("Allen Iverson*", "Dwyane Wade", "LeBron James", "Kevin Garnett", "Dwight Howard", 
             "Ray Allen*", "Devin Harris", "Joe Johnson", "Jameer Nelson", "Mo Williams", "Danny Granger", 
             "Rashard Lewis", "Paul Pierce", "Chris Bosh", "Chris Paul", "Kobe Bryant", "Amar'e Stoudemire", 
             "Tim Duncan", "Yao Ming*", "Chauncey Billups", "Tony Parker", "Brandon Roy", "Pau Gasol", 
             "Dirk Nowitzki", "David West", "Shaquille O'Neal*")
as.2008 <- c("Ray Allen*", "Chauncey Billups", "Chris Bosh", "Caron Butler", "Kevin Garnett", 
             "Richard Hamilton", "Dwight Howard", "LeBron James", "Antawn Jamison", "Joe Johnson", 
             "Jason Kidd*", "Paul Pierce", "Dwyane Wade", "Rasheed Wallace", "Carmelo Anthony", "Carlos Boozer", 
             "Kobe Bryant", "Tim Duncan", "Allen Iverson*", "Yao Ming*", "Steve Nash*", "Dirk Nowitzki", 
             "Chris Paul", "Brandon Roy", "Amar'e Stoudemire", "David West")
as.2007 <- c("Gilbert Arenas", "Chauncey Billups", "Chris Bosh", "Caron Butler", "Vince Carter", "Richard Hamilton", 
             "Dwight Howard", "LeBron James", "Joe Johnson", "Jason Kidd*", "Jermaine O'Neal", "Shaquille O'Neal*", 
             "Dwyane Wade", "Ray Allen*", "Carmelo Anthony", "Carlos Boozer", "Kobe Bryant", "Tim Duncan",
             "Kevin Garnett", "Josh Howard", "Allen Iverson*", "Shawn Marion", "Tracy McGrady*", "Yao Ming*", 
             "Steve Nash*", "Dirk Nowitzki", "Mehmet Okur","Tony Parker", "Amar'e Stoudemire")
as.2006 <- c("Gilbert Arenas", "Chauncey Billups", "Chris Bosh", "Vince Carter", "Richard Hamilton",
             "Allen Iverson*","LeBron James","Jermaine O'Neal","Shaquille O'Neal*","Paul Pierce",
             "Dwyane Wade","Ben Wallace","Rasheed Wallace","Ray Allen*", "Elton Brand","Kobe Bryant",
             "Tim Duncan", "Kevin Garnett","Pau Gasol", "Shawn Marion","Tracy McGrady*","Yao Ming*", 
             "Steve Nash*","Dirk Nowitzki","Tony Parker")
as.2005 <- c("Gilbert Arenas", "Vince Carter", "Grant Hill*", "Zydrunas Ilgauskas", "Allen Iverson*",
             "LeBron James", "Antawn Jamison", "Jermaine O'Neal", "Shaquille O'Neal*", "Paul Pierce", 
             "Dwyane Wade", "Ben Wallace", "Ray Allen*", "Kobe Bryant", "Tim Duncan", "Kevin Garnett", 
             "Manu Ginobili", "Rashard Lewis", "Shawn Marion", "Tracy McGrady*", "Yao Ming*", "Steve Nash*",
             "Dirk Nowitzki", "Amar'e Stoudemire")
as.2004 <- c("Metta World Peace", "Vince Carter", "Baron Davis", "Allen Iverson*", "Jason Kidd*",
             "Jamaal Magloire", "Kenyon Martin", "Tracy McGrady*", "Jermaine O'Neal", "Paul Pierce", 
             "Michael Redd", "Ben Wallace", "Ray Allen*", "Kobe Bryant", "Sam Cassell", "Tim Duncan",
             "Steve Francis", "Kevin Garnett", "Andrei Kirilenko", "Brad Miller", "Yao Ming*", 
             "Dirk Nowitzki", "Shaquille O'Neal*", "Peja Stojakovic")

as.2003 <- c("Vince Carter", "Zydrunas Ilgauskas", "Allen Iverson*", "Jason Kidd*", "Michael Jordan*",
             "Jamal Mashburn ", "Tracy McGrady*", "Brad Miller", "Jermaine O'Neal", "Paul Pierce",
             "Antoine Walker", "Ben Wallace", "Kobe Bryant", "Tim Duncan", "Steve Francis",
             "Kevin Garnett", "Stephon Marbury", "Shawn Marion", "Yao Ming*", "Steve Nash*",
             "Dirk Nowitzki", "Shaquille O'Neal*", "Gary Payton*", "Peja Stojakovic", "Chris Webber")

as.2002 <- c("Shareef Abdur-Rahim","Ray Allen*", "Vince Carter", "Baron Davis", "Allen Iverson*",
             "Michael Jordan*", "Jason Kidd*", "Tracy McGrady*", "Alonzo Mourning*", "Dikembe Mutombo*",
             "Jermaine O'Neal", "Paul Pierce", "Antoine Walker", "Elton Brand", "Kobe Bryant",
             "Tim Duncan", "Steve Francis", "Kevin Garnett", "Karl Malone*", "Steve Nash*",
             "Dirk Nowitzki", "Shaquille O'Neal*", "Gary Payton*", "Peja Stojakovic", "Wally Szczerbiak",
             "Chris Webber")

as.2001 <- c("Ray Allen*", "Vince Carter", "Antonio Davis", "Grant Hill*", "Allan Houston",
             "Allen Iverson*", "Stephon Marbury", "Anthony Mason", "Tracy McGrady*", "Alonzo Mourning*",
             "Dikembe Mutombo*", "Theo Ratliff", "Glenn Robinson", "Latrell Sprewell", "Jerry Stackhouse",
             "Kobe Bryant", "Vlade Divac*", "Tim Duncan", "Michael Finley", "Kevin Garnett",
             "Jason Kidd*", "Karl Malone*", "Antonio McDyess", "Shaquille O'Neal*", "Gary Payton*",
             "David Robinson*", "Rasheed Wallace", "Chris Webber")

as.2000 <- c("Ray Allen*", "Vince Carter", "Dale Davis", "Grant Hill*", "Allan Houston", "Allen Iverson*",
             "Eddie Jones", "Reggie Miller*", "Alonzo Mourning*", "Dikembe Mutombo*", "Glenn Robinson",
             "Jerry Stackhouse", "Kobe Bryant", "Tim Duncan", "Michael Finley", "Kevin Garnett",
             "Jason Kidd*", "Karl Malone*", "Shaquille O'Neal*", "Gary Payton*", "David Robinson*",
             "John Stockton*", "Rasheed Wallace", "Chris Webber")
#as.1999 <- c("https://theundefeated.com/features/the-1999-nba-all-star-weekend-that-never-was/")
as.1999 <- c()
as.1998 <- c("Anfernee Hardaway", "Tim Hardaway", "Grant Hill*", "Michael Jordan*", "Shawn Kemp",
             "Reggie Miller*", "Dikembe Mutombo*", "Glen Rice", "Steve Smith", "Rik Smits", 
             "Antoine Walker", "Jayson Williams", "Vin Baker", "Kobe Bryant", "Tim Duncan", 
             "Kevin Garnett", "Eddie Jones", "Jason Kidd*", "Karl Malone*", "Shaquille O'Neal*",
             "Gary Payton*", "Mitch Richmond*", "David Robinson*", "Nick Van Exel")
as.1997 <- c()

as.names = c(as.2020, as.2019, as.2018, as.2017, as.2016, 
             as.2015, as.2014, as.2013, as.2012, as.2011,
             as.2010, as.2009, as.2008, as.2007, as.2006,
             as.2005, as.2004, as.2003, as.2002, as.2001, 
             as.2000, as.1999, as.1998) #as.1997, as.1996, #as.1995)

data.frame(table(as.names))





