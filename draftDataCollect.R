library(rvest)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(formattable)
#'%!in%' <- function(x,y)!('%in%'(x,y))

scrape_cbbplayers = function(given_year = 2019){
  year = paste(given_year)
  link <- paste0("https://www.basketball-reference.com/draft/NBA_",year,".html")
  page <- read_html(link)
  data.raw <- html_table(page, fill=TRUE)
  
  players <- data.raw[[1]]
  players <- players[,-1]
  players <- players[,-1]
  players <- players[-1,]
  
  names(players) = c("Tm", "Player", "College", "Yrs", "G", "MP", "PTS", "TRB", "AST", "FG.", "X3P.", "FT.", "MPG", "PPG", "RPG", "APG", "WS", "WS.48", "BPM", "VORP")
  
  
  for (i in 4:ncol(players)){
    for (j in 10:12){
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
stats2018$Player[1] = "Luka Doncic"
stats2017 = arrange(scrape_cbbplayers(2017), -WS)
stats2016 = arrange(scrape_cbbplayers(2016), -WS)
stats2015 = arrange(scrape_cbbplayers(2015), -WS)
stats2015$Player[8] = "Kristaps Porzingis"
stats2014 = arrange(scrape_cbbplayers(2014), -WS)
stats2014$Player[1] = "Nikola Jokic"
stats2014$Player[which(stats2014$Player == "Glenn Robinson")] = "Glenn Robinson**"
stats2013 = arrange(scrape_cbbplayers(2013), -WS)
stats2013$Player[which(stats2013$Player == "Glen Rice")] = "Glen Rice**"
stats2013$Player[which(stats2013$Player == "Tim Hardaway")] = "Tim Hardaway**"
stats2012 = arrange(scrape_cbbplayers(2012), -WS)
stats2011 = arrange(scrape_cbbplayers(2011), -WS)
stats2011$Player[6] = "Nikola Vucevic"
stats2010 = arrange(scrape_cbbplayers(2010), -WS)
stats2009 = arrange(scrape_cbbplayers(2009), -WS)
stats2008 = arrange(scrape_cbbplayers(2008), -WS)
stats2008$Player[9] = "Goran Dragic"
stats2007 = arrange(scrape_cbbplayers(2007), -WS)
stats2006 = arrange(scrape_cbbplayers(2006), -WS)
stats2005 = arrange(scrape_cbbplayers(2005), -WS)
stats2004 = arrange(scrape_cbbplayers(2004), -WS)
stats2003 = arrange(scrape_cbbplayers(2003), -WS)
stats2002 = arrange(scrape_cbbplayers(2002), -WS)
stats2001 = arrange(scrape_cbbplayers(2001), -WS)
stats2000 = arrange(scrape_cbbplayers(2000), -WS)
stats1999 = arrange(scrape_cbbplayers(1999), -WS)
stats1999$Player[3] = "Manu Ginobili"
stats1998 = arrange(scrape_cbbplayers(1998), -WS)
stats1997 = arrange(scrape_cbbplayers(1997), -WS)
stats1996 = arrange(scrape_cbbplayers(1996), -WS)
stats1996$Player[5] = "Peja Stojakovic"
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
             "LaMarcus Aldridge", "Marc Gasol", "Kevin Love", "Steve Nash", "Dirk Nowitzki", 
             "Tony Parker", "Russell Westbrook", "Derrick Rose", "Dwyane Wade", "LeBron James", 
             "Carmelo Anthony", "Dwight Howard", "Chris Bosh", "Luol Deng", "Roy Hibbert", 
             "Andre Iguodala", "Joe Johnson", "Paul Pierce", "Rajon Rondo", "Deron Williams")
as.2011 <- c("Derrick Rose", "Dwyane Wade", "LeBron James", "Amar'e Stoudemire", "Dwight Howard", 
             "Ray Allen", "Chris Bosh", "Kevin Garnett", "Al Horford", "Joe Johnson", 
             "Paul Pierce", "Rajon Rondo", "Chris Paul", "Kobe Bryant", "Kevin Durant", 
             "Carmelo Anthony", "Yao Ming", "Tim Duncan", "Pau Gasol", "Manu Ginobili", 
             "Blake Griffin", "Kevin Love", "Dirk Nowitzki", "Russell Westbrook", "Deron Williams")
as.2010 <- c("Allen Iverson", "Dwyane Wade", "LeBron James", "Kevin Garnett", "Dwight Howard", 
             "Joe Johnson", "Rajon Rondo", "Derrick Rose", "Paul Pierce", "Gerald Wallace", 
             "Chris Bosh", "Al Horford", "David Lee", "Steve Nash", "Kobe Bryant", "Carmelo Anthony", 
             "Tim Duncan", "Amar'e Stoudemire", "Chauncey Billups", "Jason Kidd", "Chris Paul", 
             "Brandon Roy", "Deron Williams", "Kevin Durant", "Dirk Nowitzki", "Zach Randolph", 
             "Pau Gasol", "Chris Kaman")
as.2009 <- c("Allen Iverson", "Dwyane Wade", "LeBron James", "Kevin Garnett", "Dwight Howard", 
             "Ray Allen", "Devin Harris", "Joe Johnson", "Jameer Nelson", "Mo Williams", "Danny Granger", 
             "Rashard Lewis", "Paul Pierce", "Chris Bosh", "Chris Paul", "Kobe Bryant", "Amar'e Stoudemire", 
             "Tim Duncan", "Yao Ming", "Chauncey Billups", "Tony Parker", "Brandon Roy", "Pau Gasol", 
             "Dirk Nowitzki", "David West", "Shaquille O'Neal")
as.2008 <- c("Ray Allen", "Chauncey Billups", "Chris Bosh", "Caron Butler", "Kevin Garnett", 
             "Richard Hamilton", "Dwight Howard", "LeBron James", "Antawn Jamison", "Joe Johnson", 
             "Jason Kidd", "Paul Pierce", "Dwyane Wade", "Rasheed Wallace", "Carmelo Anthony", "Carlos Boozer", 
             "Kobe Bryant", "Tim Duncan", "Allen Iverson", "Yao Ming", "Steve Nash", "Dirk Nowitzki", 
             "Chris Paul", "Brandon Roy", "Amar'e Stoudemire", "David West")
as.2007 <- c("Gilbert Arenas", "Chauncey Billups", "Chris Bosh", "Caron Butler", "Vince Carter", "Richard Hamilton", 
             "Dwight Howard", "LeBron James", "Joe Johnson", "Jason Kidd", "Jermaine O'Neal", "Shaquille O'Neal", 
             "Dwyane Wade", "Ray Allen", "Carmelo Anthony", "Carlos Boozer", "Kobe Bryant", "Tim Duncan",
             "Kevin Garnett", "Josh Howard", "Allen Iverson", "Shawn Marion", "Tracy McGrady", "Yao Ming", 
             "Steve Nash", "Dirk Nowitzki", "Mehmet Okur","Tony Parker", "Amar'e Stoudemire")
as.2006 <- c("Gilbert Arenas", "Chauncey Billups", "Chris Bosh", "Vince Carter", "Richard Hamilton",
             "Allen Iverson","LeBron James","Jermaine O'Neal","Shaquille O'Neal","Paul Pierce",
             "Dwyane Wade","Ben Wallace","Rasheed Wallace","Ray Allen", "Elton Brand","Kobe Bryant",
             "Tim Duncan", "Kevin Garnett","Pau Gasol", "Shawn Marion","Tracy McGrady","Yao Ming", 
             "Steve Nash","Dirk Nowitzki","Tony Parker")
as.2005 <- c("Gilbert Arenas", "Vince Carter", "Grant Hill", "Zydrunas Ilgauskas", "Allen Iverson",
             "LeBron James", "Antawn Jamison", "Jermaine O'Neal", "Shaquille O'Neal", "Paul Pierce", 
             "Dwyane Wade", "Ben Wallace", "Ray Allen", "Kobe Bryant", "Tim Duncan", "Kevin Garnett", 
             "Manu Ginobili", "Rashard Lewis", "Shawn Marion", "Tracy McGrady", "Yao Ming", "Steve Nash",
             "Dirk Nowitzki", "Amar'e Stoudemire")
as.2004 <- c("Metta World Peace", "Vince Carter", "Baron Davis", "Allen Iverson", "Jason Kidd",
             "Jamaal Magloire", "Kenyon Martin", "Tracy McGrady", "Jermaine O'Neal", "Paul Pierce", 
             "Michael Redd", "Ben Wallace", "Ray Allen", "Kobe Bryant", "Sam Cassell", "Tim Duncan",
             "Steve Francis", "Kevin Garnett", "Andrei Kirilenko", "Brad Miller", "Yao Ming", 
             "Dirk Nowitzki", "Shaquille O'Neal", "Peja Stojakovic")
as.2003 <- c("Vince Carter", "Zydrunas Ilgauskas", "Allen Iverson", "Jason Kidd", "Michael Jordan",
             "Jamal Mashburn ", "Tracy McGrady", "Brad Miller", "Jermaine O'Neal", "Paul Pierce",
             "Antoine Walker", "Ben Wallace", "Kobe Bryant", "Tim Duncan", "Steve Francis",
             "Kevin Garnett", "Stephon Marbury", "Shawn Marion", "Yao Ming", "Steve Nash",
             "Dirk Nowitzki", "Shaquille O'Neal", "Gary Payton", "Peja Stojakovic", "Chris Webber")

as.2002 <- c("Shareef Abdur-Rahim","Ray Allen", "Vince Carter", "Baron Davis", "Allen Iverson",
             "Michael Jordan", "Jason Kidd", "Tracy McGrady", "Alonzo Mourning", "Dikembe Mutombo",
             "Jermaine O'Neal", "Paul Pierce", "Antoine Walker", "Elton Brand", "Kobe Bryant",
             "Tim Duncan", "Steve Francis", "Kevin Garnett", "Karl Malone", "Steve Nash",
             "Dirk Nowitzki", "Shaquille O'Neal", "Gary Payton", "Peja Stojakovic", "Wally Szczerbiak",
             "Chris Webber")

as.2001 <- c("Ray Allen", "Vince Carter", "Antonio Davis", "Grant Hill", "Allan Houston",
             "Allen Iverson", "Stephon Marbury", "Anthony Mason", "Tracy McGrady", "Alonzo Mourning",
             "Dikembe Mutombo", "Theo Ratliff", "Glenn Robinson", "Latrell Sprewell", "Jerry Stackhouse",
             "Kobe Bryant", "Vlade Divac", "Tim Duncan", "Michael Finley", "Kevin Garnett",
             "Jason Kidd", "Karl Malone", "Antonio McDyess", "Shaquille O'Neal", "Gary Payton",
             "David Robinson", "Rasheed Wallace", "Chris Webber")

as.2000 <- c("Ray Allen", "Vince Carter", "Dale Davis", "Grant Hill", "Allan Houston", "Allen Iverson",
             "Eddie Jones", "Reggie Miller", "Alonzo Mourning", "Dikembe Mutombo", "Glenn Robinson",
             "Jerry Stackhouse", "Kobe Bryant", "Tim Duncan", "Michael Finley", "Kevin Garnett",
             "Jason Kidd", "Karl Malone", "Shaquille O'Neal", "Gary Payton", "David Robinson",
             "John Stockton", "Rasheed Wallace", "Chris Webber")
#as.1999 <- c("https://theundefeated.com/features/the-1999-nba-all-star-weekend-that-never-was/")
as.1999 <- c()
as.1998 <- c("Anfernee Hardaway", "Tim Hardaway", "Grant Hill", "Michael Jordan", "Shawn Kemp",
             "Reggie Miller", "Dikembe Mutombo", "Glen Rice", "Steve Smith", "Rik Smits", 
             "Antoine Walker", "Jayson Williams", "Vin Baker", "Kobe Bryant", "Tim Duncan", 
             "Kevin Garnett", "Eddie Jones", "Jason Kidd", "Karl Malone", "Shaquille O'Neal",
             "Gary Payton", "Mitch Richmond", "David Robinson", "Nick Van Exel")
as.1997 = c("Vin Baker", "Terrell Brandon", "Joe Dumars", "Patrick Ewing", "Anfernee Hardaway", "Tim Hardaway", 
            "Grant Hill", "Michael Jordan", "Christian Laettner", "Alonzo Mourning", "Dikembe Mutombo", 
            "Scottie Pippen", "Glen Rice", "Chris Webber", "Charles Barkley", "Clyde Drexler", "Kevin Garnett", 
            "Chris Gatling", "Tom Gugliotta", "Eddie Jones", "Shawn Kemp", "Karl Malone", "Hakeem Olajuwon", 
            "Shaquille O'Neal", "Gary Payton", "Mitch Richmond", "Detlef Schrempf", "Latrell Sprewell", "John Stockton")
as.1996 = c("Vin Baker", "Terrell Brandon", "Patrick Ewing", "Anfernee Hardaway", "Grant Hill", "Juwan Howard", 
            "Michael Jordan", "Reggie Miller", "Alonzo Mourning", "Shaquille O'Neal", "Scottie Pippen", "Glen Rice", 
            "Charles Barkley", "Clyde Drexler", "Sean Elliott", "Shawn Kemp", "Jason Kidd", "Karl Malone", 
            "Dikembe Mutombo", "Hakeem Olajuwon", "Gary Payton", "Mitch Richmond", "David Robinson", "John Stockton")
as.1995 <- c("Vin Baker", "Dana Barros", "Joe Dumars", "Patrick Ewing", "Anfernee Hardaway",
             "Grant Hill", "Tyrone Hill", "Larry Johnson", "Reggie Miller", "Alonzo Mourning",
             "Shaquille O'Neal", "Scottie Pippen", "Charles Barkley", "Cedric Ceballos", "Shawn Kemp",
             "Dan Majerle", "Karl Malone", "Dikembe Mutombo", "Hakeem Olajuwon", "Gary Payton",
             "Mitch Richmond", "David Robinson", "Detlef Schrempf", "Latrell Sprewell", "John Stockton")


as.names = c(as.2020, as.2019, as.2018, as.2017, as.2016, 
             as.2015, as.2014, as.2013, as.2012, as.2011,
             as.2010, as.2009, as.2008, as.2007, as.2006,
             as.2005, as.2004, as.2003, as.2002, as.2001, 
             as.2000, as.1999, as.1998, as.1997, as.1996,
             as.1995)

as.app.df = data.frame(table(as.names))

# Every row (next 25 rows) should be all-stars
stats2019$Player[which(stats2019$Player %in% as.app.df$as.names)]
stats2018$Player[which(stats2018$Player %in% as.app.df$as.names)]
stats2017$Player[which(stats2017$Player %in% as.app.df$as.names)]
stats2016$Player[which(stats2016$Player %in% as.app.df$as.names)]
stats2015$Player[which(stats2015$Player %in% as.app.df$as.names)]
stats2014$Player[which(stats2014$Player %in% as.app.df$as.names)]
stats2013$Player[which(stats2013$Player %in% as.app.df$as.names)]
stats2012$Player[which(stats2012$Player %in% as.app.df$as.names)]
stats2011$Player[which(stats2011$Player %in% as.app.df$as.names)]
stats2010$Player[which(stats2010$Player %in% as.app.df$as.names)]
stats2009$Player[which(stats2009$Player %in% as.app.df$as.names)]
stats2008$Player[which(stats2008$Player %in% as.app.df$as.names)]
stats2007$Player[which(stats2007$Player %in% as.app.df$as.names)]
stats2006$Player[which(stats2006$Player %in% as.app.df$as.names)]
stats2005$Player[which(stats2005$Player %in% as.app.df$as.names)]
stats2004$Player[which(stats2004$Player %in% as.app.df$as.names)]
stats2003$Player[which(stats2003$Player %in% as.app.df$as.names)]
stats2002$Player[which(stats2002$Player %in% as.app.df$as.names)]
stats2001$Player[which(stats2001$Player %in% as.app.df$as.names)]
stats2000$Player[which(stats2000$Player %in% as.app.df$as.names)]
stats1999$Player[which(stats1999$Player %in% as.app.df$as.names)]
stats1998$Player[which(stats1998$Player %in% as.app.df$as.names)]
stats1997$Player[which(stats1997$Player %in% as.app.df$as.names)]
stats1996$Player[which(stats1996$Player %in% as.app.df$as.names)]
stats1995$Player[which(stats1995$Player %in% as.app.df$as.names)]

consistency_list = c(stats2019$Player[which(stats2019$Player %in% as.app.df$as.names)],
                    stats2018$Player[which(stats2018$Player %in% as.app.df$as.names)],
                    stats2017$Player[which(stats2017$Player %in% as.app.df$as.names)],
                    stats2016$Player[which(stats2016$Player %in% as.app.df$as.names)],
                    stats2015$Player[which(stats2015$Player %in% as.app.df$as.names)],
                    stats2014$Player[which(stats2014$Player %in% as.app.df$as.names)],
                    stats2013$Player[which(stats2013$Player %in% as.app.df$as.names)],
                    stats2012$Player[which(stats2012$Player %in% as.app.df$as.names)],
                    stats2011$Player[which(stats2011$Player %in% as.app.df$as.names)],
                    stats2010$Player[which(stats2010$Player %in% as.app.df$as.names)],
                    stats2009$Player[which(stats2009$Player %in% as.app.df$as.names)],
                    stats2008$Player[which(stats2008$Player %in% as.app.df$as.names)],
                    stats2007$Player[which(stats2007$Player %in% as.app.df$as.names)],
                    stats2006$Player[which(stats2006$Player %in% as.app.df$as.names)],
                    stats2005$Player[which(stats2005$Player %in% as.app.df$as.names)],
                    stats2004$Player[which(stats2004$Player %in% as.app.df$as.names)],
                    stats2003$Player[which(stats2003$Player %in% as.app.df$as.names)],
                    stats2002$Player[which(stats2002$Player %in% as.app.df$as.names)],
                    stats2001$Player[which(stats2001$Player %in% as.app.df$as.names)],
                    stats2000$Player[which(stats2000$Player %in% as.app.df$as.names)],
                    stats1999$Player[which(stats1999$Player %in% as.app.df$as.names)],
                    stats1998$Player[which(stats1998$Player %in% as.app.df$as.names)],
                    stats1997$Player[which(stats1997$Player %in% as.app.df$as.names)],
                    stats1996$Player[which(stats1996$Player %in% as.app.df$as.names)],
                    stats1995$Player[which(stats1995$Player %in% as.app.df$as.names)])

unique(as.names[which(as.names %!in% consistency_list)])

### Names that have AS appearances but aren't in the draft class dataframes ###
#### these guys should have draft years prior to 1995.

p = c("Jason Kidd", "Shaquille O'Neal", "Ben Wallace", "Grant Hill", 
      "Sam Cassell", "Brad Miller", "Michael Jordan", 
      "Jamal Mashburn", "Gary Payton", "Chris Webber", "Alonzo Mourning", 
      "Dikembe Mutombo", "Karl Malone", "Antonio Davis", "Allan Houston", 
      "Anthony Mason", "Glenn Robinson", "Latrell Sprewell", "Vlade Divac", 
      "David Robinson", "Dale Davis", "Eddie Jones", "Reggie Miller", 
      "John Stockton", "Anfernee Hardaway", "Tim Hardaway", "Shawn Kemp", 
      "Glen Rice", "Steve Smith", "Rik Smits", "Jayson Williams", 
      "Vin Baker", "Mitch Richmond", "Nick Van Exel")
c = c(1994, 1992, 1996, 1994, 1993, 1998, 1984, 1993, 1990, 1993, 1992, 
      1991, 1985, 1990, 1993, 1988, 1994, 1992, 1989, 1987, 1991, 1994,
      1987, 1984, 1993, 1989, 1989, 1989, 1991, 1988, 1990, 1993, 1988, 
      1993)
d = cbind.data.frame(p[1:length(c)], c)
names(d) = c("Player", "Draft Year")
d %>% arrange(desc(`Draft Year`))

add_allstar <- function(df = stats2019){
  for (i in 1:nrow(df)){
    if (df$Player[i] %in% consistency_list){
      for (j in 1:nrow(as.app.df)){
        if (df$Player[i] == as.app.df$as.names[j]){
          df$allstar[i] = as.app.df$Freq[j]
        }
      }
    } else{
      df$allstar[i] = 0
    }
  }
  return(df)
}

stats2019 = add_allstar(stats2019) %>% arrange(desc(allstar))
stats2018 = add_allstar(stats2018) %>% arrange(desc(allstar))
stats2017 = add_allstar(stats2017) %>% arrange(desc(allstar))
stats2016 = add_allstar(stats2016) %>% arrange(desc(allstar))
stats2015 = add_allstar(stats2015) %>% arrange(desc(allstar))
stats2014 = add_allstar(stats2014) %>% arrange(desc(allstar))
stats2013 = add_allstar(stats2013) %>% arrange(desc(allstar))
stats2012 = add_allstar(stats2012) %>% arrange(desc(allstar))
stats2011 = add_allstar(stats2011) %>% arrange(desc(allstar))
stats2010 = add_allstar(stats2010) %>% arrange(desc(allstar))
stats2009 = add_allstar(stats2009) %>% arrange(desc(allstar))
stats2008 = add_allstar(stats2008) %>% arrange(desc(allstar))
stats2007 = add_allstar(stats2007) %>% arrange(desc(allstar))
stats2006 = add_allstar(stats2006) %>% arrange(desc(allstar))
stats2005 = add_allstar(stats2005) %>% arrange(desc(allstar))
stats2004 = add_allstar(stats2004) %>% arrange(desc(allstar))
stats2003 = add_allstar(stats2003) %>% arrange(desc(allstar))
stats2002 = add_allstar(stats2002) %>% arrange(desc(allstar))
stats2001 = add_allstar(stats2001) %>% arrange(desc(allstar))
stats2000 = add_allstar(stats2000) %>% arrange(desc(allstar))
stats1999 = add_allstar(stats1999) %>% arrange(desc(allstar))
stats1998 = add_allstar(stats1998) %>% arrange(desc(allstar))
stats1997 = add_allstar(stats1997) %>% arrange(desc(allstar))
stats1996 = add_allstar(stats1996) %>% arrange(desc(allstar))
stats1995 = add_allstar(stats1995) %>% arrange(desc(allstar))

df = rbind.data.frame(stats2019, stats2018, stats2017, stats2016, stats2015, 
                      stats2014, stats2013, stats2012, stats2011, stats2010,
                      stats2009, stats2008, stats2007, stats2006, stats2005,
                      stats2004, stats2003, stats2002, stats2001, stats2000,
                      stats1999, stats1998, stats1997, stats1996, stats1995)

# Correlation Plot
df.numeric <- select(df, -Tm, -Yrs, -College, -Player, -Yr)
draft.cor1 <- data.frame(abs(cor(df.numeric)[,"allstar"]))
colnames(draft.cor1) <- "cor"
draft.cor1$var <- rownames(draft.cor1)
mm.cor1 <- select(arrange(draft.cor1, desc(cor)), var, cor)
mm.cor1 <- mm.cor1[2:nrow(mm.cor1),]
pl1 = ggplot(data=mm.cor1, aes(x=reorder(var, cor), y=cor)) + geom_bar(stat = "identity", width = I(1/8), position = position_dodge(width=0.2)) + coord_flip() + scale_x_discrete("Variable") + theme_clean() + scale_y_continuous("Absolute Value of Correlation", breaks = seq(0, 0.60, by = 0.10)) + ggtitle("Variable Correlation", subtitle = "Correlation with All Star Appearences")

# AIC Plot
aic <- vector()

fit1 <- glm(allstar ~ VORP, data = df)
aic[1] <- fit1$aic

fit2 <- glm(allstar ~ VORP + WS, data = df)
aic[2] <- fit2$aic

fit3 <- glm(allstar ~ VORP + WS + PTS, data = df)
aic[3] <- fit3$aic

fit4 <- glm(allstar ~ VORP + WS + PTS + AST, data = df)
aic[4] <- fit4$aic

fit5 <- glm(allstar ~ VORP + WS + PTS + AST + MP, data = df)
aic[5] <- fit5$aic

fit6 <- glm(allstar ~ VORP + WS + PTS + AST + MP + TRB, data = df)
aic[6] <- fit6$aic

fit7 <- glm(allstar ~ VORP + WS + PTS + AST + MP + TRB + PPG, data = df)
aic[7] <- fit7$aic

fit8 <- glm(allstar ~ VORP + WS + PTS + AST + MP + TRB + PPG + G, data = df)
aic[8] <- fit8$aic

# Games variable brought AIC back up

fit9 <- glm(allstar ~ VORP + WS + PTS + AST + MP + TRB + PPG + MPG, data = df)
aic[9] <- fit9$aic

fit10 <- glm(allstar ~ VORP + WS + PTS + AST + MP + TRB + PPG + MPG + APG, data = df)
aic[10] <- fit10$aic

# Assists per game variable brought AIC up

fit11 <- glm(allstar ~ VORP + WS + PTS + AST + MP + TRB + PPG + MPG + BPM, data = df)
aic[11] <- fit11$aic

fit12 <- glm(allstar ~ VORP + WS + PTS + AST + MP + TRB + PPG + MPG + BPM + RPG, data = df)
aic[12] <- fit12$aic

fit13 <- glm(allstar ~ VORP + WS + PTS + AST + MP + TRB + PPG + MPG + BPM + RPG + WS.48, data = df)
aic[13] <- fit13$aic

fit14 <- glm(allstar ~ VORP + WS + PTS + AST + MP + TRB + PPG + MPG + BPM + RPG + FT., data = df)
aic[14] <- fit14$aic

fit15 <- glm(allstar ~ VORP + WS + PTS + AST + MP + TRB + PPG + MPG + BPM + RPG + FG., data = df)
aic[15] <- fit15$aic

fit16 <- glm(allstar ~ VORP + WS + PTS + AST + MP + TRB + PPG + MPG + BPM + RPG + X3P., data = df)
aic[16] <- fit16$aic


aic.val <- data.frame(aic)
aic.val$model <- seq(1, nrow(aic.val))
aic.val$optimal <- ifelse(aic.val$aic == min(aic.val$aic), "Optimal Point", "")


print(aic.val)
ggplot(data=aic.val, aes(x=model, y=aic)) + geom_line(linetype = "dashed") + geom_point(aes(color = optimal)) + 
  theme_bw() + scale_x_continuous("Model", breaks = seq(1,length(aic.val$model))) + 
  scale_y_continuous("AIC", breaks = seq(2600, 3100, by=25)) + theme(legend.position = "none") + scale_color_manual(values = c("black", "red3")) + ggtitle("AIC: Measuring Goodness of Fit for each Model")

# MODEL 12:
lm.model <- lm(allstar ~ VORP + WS + PTS + AST + MP + TRB + PPG + MPG + BPM + RPG, data = df)
print(anova(lm.model))
summary(lm.model)$coefficients[,"Estimate"]

# Preliminary Assessment of Predicted All Star Appearances
allstar.pred = predict(lm.model, df)
as.df = cbind.data.frame(df, allstar.pred)
write.csv(as.df, file = "allstardf.csv")


as.stats2019 = as.df %>% filter(Yr == 2019) %>% arrange(desc(allstar.pred))
as.stats2018 = as.df %>% filter(Yr == 2018) %>% arrange(desc(allstar.pred))
as.stats2017 = as.df %>% filter(Yr == 2017) %>% arrange(desc(allstar.pred))
as.stats2016 = as.df %>% filter(Yr == 2016) %>% arrange(desc(allstar.pred))
as.stats2015 = as.df %>% filter(Yr == 2015) %>% arrange(desc(allstar.pred))
as.stats2014 = as.df %>% filter(Yr == 2014) %>% arrange(desc(allstar.pred))
as.stats2013 = as.df %>% filter(Yr == 2013) %>% arrange(desc(allstar.pred))
as.stats2012 = as.df %>% filter(Yr == 2012) %>% arrange(desc(allstar.pred))
as.stats2011 = as.df %>% filter(Yr == 2011) %>% arrange(desc(allstar.pred))
as.stats2010 = as.df %>% filter(Yr == 2010) %>% arrange(desc(allstar.pred))

as.stats2009 = as.df %>% filter(Yr == 2009) %>% arrange(desc(allstar.pred))
as.stats2008 = as.df %>% filter(Yr == 2008) %>% arrange(desc(allstar.pred))
as.stats2007 = as.df %>% filter(Yr == 2007) %>% arrange(desc(allstar.pred))
as.stats2006 = as.df %>% filter(Yr == 2006) %>% arrange(desc(allstar.pred))
as.stats2005 = as.df %>% filter(Yr == 2005) %>% arrange(desc(allstar.pred))
as.stats2004 = as.df %>% filter(Yr == 2004) %>% arrange(desc(allstar.pred))
as.stats2003 = as.df %>% filter(Yr == 2003) %>% arrange(desc(allstar.pred))
as.stats2002 = as.df %>% filter(Yr == 2002) %>% arrange(desc(allstar.pred))
as.stats2001 = as.df %>% filter(Yr == 2001) %>% arrange(desc(allstar.pred))
as.stats2000 = as.df %>% filter(Yr == 2000) %>% arrange(desc(allstar.pred))







