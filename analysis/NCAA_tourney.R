library(dplyr)
library(rvest)
library(ggplot2)

wapo_raw <- "http://apps.washingtonpost.com/sports/apps/live-updating-mens-ncaa-basketball-bracket/search/?from=1985&game_type=7&opp_bid_type=&opp_coach=&opp_conference=&opp_power_conference=&opp_school_id=&opp_seed_from=1&opp_seed_to=16&pri_bid_type=&pri_coach=&pri_conference=&pri_power_conference=&pri_school_id=&pri_seed_from=1&pri_seed_to=16&submit=&to=2016"

wapo_scores <- html_table(html_nodes(read_html(wapo_raw), "table")[[1]])
names(wapo_scores) = c("Year", "Round", "Seed1", "Team1", "Score1", "Seed2", "Team2", "Score2")
wapo_scores <- wapo_scores %>%
  tidyr::separate(Team1, into = c("Team1", "Team1_dupe"), sep = "\n") %>%
  tidyr::separate(Team2, into = c("Team2", "Team2_dupe"), sep = "\n")

wikipedia_2016 <- "https://en.wikipedia.org/wiki/2016_NCAA_Division_I_Men's_Basketball_Tournament"

get_2016 <- function(region, index){
  return(html_table(html_nodes(read_html(wikipedia_2016), "table"), fill = T)[[index]] %>%
    select(Seed, School) %>%
    mutate(Region = region, Year = 2016))
}

S2016 <- get_2016("South", 4)
W2016 <- get_2016("West", 5)
E2016 <- get_2016("East", 6)
MW2016 <- get_2016("Midwest", 7)
final_2016 <- rbind.data.frame(S2016, W2016, E2016, MW2016)

wikipedia_2015 <- "https://en.wikipedia.org/wiki/2015_NCAA_Division_I_Men's_Basketball_Tournament"

get_2015 <- function(region, index){
  return(html_table(html_nodes(read_html(wikipedia_2015), "table"), fill = T)[[index]] %>%
           select(Seed, School) %>%
           mutate(Region = region, Year = 2015))
}

MW2015 <- get_2015("Midwest", 5)
W2015 <- get_2015("West", 6)
E2015 <- get_2015("East", 7)
S2015 <- get_2015("South", 8)
final_2015 <- rbind.data.frame(S2015, W2015, E2015, MW2015)

wikipedia_2014 <- "https://en.wikipedia.org/wiki/2014_NCAA_Division_I_Men's_Basketball_Tournament"

get_2014 <- function(region, index){
  return(html_table(html_nodes(read_html(wikipedia_2014), "table"), fill = T)[[index]] %>%
           select(Seed, School) %>%
           mutate(Region = region, Year = 2014))
}

S2014 <- get_2014("South", 5)
W2014 <- get_2014("West", 6)
E2014 <- get_2014("East", 7)
MW2014 <- get_2014("Midwest", 8)
final_2014 <- rbind.data.frame(S2014, W2014, E2014, MW2014)

wikipedia_2013 <- "https://en.wikipedia.org/wiki/2013_NCAA_Division_I_Men's_Basketball_Tournament"

get_2013 <- function(region, index){
  temp2013 <- html_table(html_nodes(read_html(wikipedia_2013), "table"), fill = T)[[index]]
  temp2013 <- temp2013[2:nrow(temp2013), 1:2]
  names(temp2013) = c("Seed", "School")
  temp2013 <- temp2013 %>% mutate(Region = region, Year = 2013)
}

S2013 <- get_2013("South", 7)
W2013 <- get_2013("West", 8)
E2013 <- get_2013("East", 9)
MW2013 <- get_2013("Midwest", 10)
final_2013 <- rbind.data.frame(S2013, W2013, E2013, MW2013)

wikipedia_2012 <- "https://en.wikipedia.org/wiki/2012_NCAA_Division_I_Men's_Basketball_Tournament"

get_2012 <- function(region, index){
  temp2012 <- html_table(html_nodes(read_html(wikipedia_2012), "table"), fill = T)[[index]]
  temp2012 <- temp2012[2:nrow(temp2012), 1:2]
  names(temp2012) = c("Seed", "School")
  temp2012 <- temp2012 %>% mutate(Region = region, Year = 2012)
}

E2012 <- get_2012("East", 7)
MW2012 <- get_2012("Midwest", 8)
W2012 <- get_2012("West", 6)
S2012 <- get_2012("South", 5)
final_2012 <- rbind.data.frame(S2012, W2012, E2012, MW2012)

wikipedia_2011 <- "https://en.wikipedia.org/wiki/2011_NCAA_Division_I_Men's_Basketball_Tournament"

get_2011 <- function(region, index){
  temp2011 <- html_table(html_nodes(read_html(wikipedia_2011), "table"), fill = T)[[index]]
  temp2011 <- temp2011[2:nrow(temp2011), 1:2]
  names(temp2011) = c("Seed", "School")
  temp2011 <- temp2011 %>% mutate(Region = region, Year = 2011)
}

#South is "Southeast", Midwest is "Southwest"
S2011 <- get_2011("South", 7)
MW2011 <- get_2011("Midwest", 6)
W2011 <- get_2011("West", 5)
E2011 <- get_2011("East", 4)
final_2011 <- rbind.data.frame(S2011, W2011, E2011, MW2011)

wikipedia_2010 <- "https://en.wikipedia.org/wiki/2010_NCAA_Division_I_Men's_Basketball_Tournament"

get_2010 <- function(region, index){
  temp2010 <- html_table(html_nodes(read_html(wikipedia_2010), "table"), fill = T)[[index]]
  temp2010 <- temp2010[2:nrow(temp2010), 1:2]
  names(temp2010) = c("Seed", "School")
  temp2010 <- temp2010 %>% mutate(Region = region, Year = 2010)
}
E2010 <- get_2010("East", 4)
W2010 <- get_2010("West", 5)
MW2010 <- get_2010("Midwest", 6)
S2010 <- get_2010("South", 7)
final_2010 <- rbind.data.frame(S2010, W2010, E2010, MW2010)

wikipedia_2009 <- "https://en.wikipedia.org/wiki/2009_NCAA_Division_I_Men's_Basketball_Tournament"

get_2009 <- function(region, index){
  temp2009 <- html_table(html_nodes(read_html(wikipedia_2009), "table"), fill = T)[[index]]
  temp2009 <- temp2009[2:nrow(temp2009), 1:2]
  names(temp2009) = c("Seed", "School")
  temp2009 <- temp2009 %>% mutate(Region = region, Year = 2009)
}
E2009 <- get_2009("East", 4)
W2009 <- get_2009("West", 5)
MW2009 <- get_2009("Midwest", 6)
S2009 <- get_2009("South", 7)
final_2009 <- rbind.data.frame(S2009, W2009, E2009, MW2009)

wikipedia_2008 <- "https://en.wikipedia.org/wiki/2008_NCAA_Division_I_Men's_Basketball_Tournament"

get_2008 <- function(region, index){
  temp2008 <- html_table(html_nodes(read_html(wikipedia_2008), "table"), fill = T)[[index]]
  temp2008 <- temp2008[2:nrow(temp2008), 1:2]
  names(temp2008) = c("Seed", "School")
  temp2008 <- temp2008 %>% mutate(Region = region, Year = 2008)
}
E2008 <- get_2008("East", 4)
W2008 <- get_2008("West", 5)
MW2008 <- get_2008("Midwest", 6)
S2008 <- get_2008("South", 7)
final_2008 <- rbind.data.frame(S2008, W2008, E2008, MW2008)

wikipedia_2007 <- "https://en.wikipedia.org/wiki/2007_NCAA_Division_I_Men's_Basketball_Tournament"

get_2007 <- function(region, index){
  temp2007 <- html_table(html_nodes(read_html(wikipedia_2007), "table"), fill = T)[[index]]
  temp2007 <- temp2007[2:nrow(temp2007), 1:2]
  names(temp2007) = c("Seed", "School")
  temp2007 <- temp2007 %>% mutate(Region = region, Year = 2007)
}
E2007 <- get_2007("East", 4)
W2007 <- get_2007("West", 5)
MW2007 <- get_2007("Midwest", 6)
S2007 <- get_2007("South", 7)
final_2007 <- rbind.data.frame(S2007, W2007, E2007, MW2007)

wikipedia_2006 <- "https://en.wikipedia.org/wiki/2006_NCAA_Division_I_Men's_Basketball_Tournament"

get_2006 <- function(region, index){
  temp2006 <- html_table(html_nodes(read_html(wikipedia_2006), "table"), fill = T)[[index]]
  temp2006 <- temp2006[2:nrow(temp2006), 1:2]
  names(temp2006) = c("Seed", "School")
  temp2006 <- temp2006 %>% mutate(Region = region, Year = 2006)
}
E2006 <- get_2006("East", 4)
W2006 <- get_2006("West", 5)
MW2006 <- get_2006("Midwest", 6)
S2006 <- get_2006("South", 7)
final_2006 <- rbind.data.frame(S2006, W2006, E2006, MW2006)

last_ten <- rbind.data.frame(final_2016, final_2015, final_2014, final_2013, final_2012, final_2011, final_2010,
                             final_2009, final_2008, final_2007, final_2006) %>%
  mutate(Seed = gsub("[[:punct:]]", "", Seed))

last_ten$School <- plyr::mapvalues(last_ten$School, from=c("UAB", "SMU", "VCU", "Little Rock", "Saint Joseph's", "USC",
                                                           "Memphis (Vacated)", "Southern California (Vacated)", "Miami (Florida)", "BYU", "Saint Mary's",
                                                           "Saint Mary's (CA)", "Murray State", "Saint Louis"),
                  to = c("Alabama-Birmingham", "Southern Methodist", "Virginia Commonwealth",
                         "Arkansas-Little Rock", "St. Joseph's", "Southern California", "Memphis", "Southern California", "Miami (Fla.)",
                         "Brigham Young", "St. Mary's (Cal.)", "St. Mary's (Cal.)", "Murray St.", "St. Louis"))
last_ten$School <- ifelse(last_ten$School == "Miami" & last_ten$Year == 2007, "Miami (Ohio)", last_ten$School)
last_ten$School <- ifelse(last_ten$School == "Miami" & last_ten$Year != 2007, "Miami (Fla.)", last_ten$School)

#if else for Miami

#test with South 2016
excluded_games <- c("Play-InPlay-In", "Final FourFinal Four", "National ChampionshipNational Championship")

check_region <- function(year, region){
  temp <- subset(last_ten, Year == year & Region == region)
  print(table(subset(wapo_scores, Year == year & Team1 %in% temp$School &
                        Round %notin% excluded_games)$Round))
}

get_score <- function(year, region){
  temp <- subset(last_ten, Year == year & Region == region)
  temp2 <- subset(wapo_scores, Year == year & Team1 %in% temp$School &
                    Round %notin% excluded_games) %>%
  mutate(winning_seed = ifelse(Score1 > Score2, Seed1, Seed2))
  res <- data.frame(Year = year, Region = region, Score = sum(temp2$winning_seed))
  return(res)
}
  

all_scores <- c()
for(x in 2007:2016){
  for(y in c("West", "East", "South", "Midwest")){
    temp3 <- get_score(x, y)
    all_scores <- rbind(all_scores, temp3)
  }
}

all_scores$Score = all_scores$Score/50
ggplot(all_scores, aes(x = Year, y = Score, color = Region)) + geom_line(size=2) +
  ggtitle("NCAA Tournament Upsets by Region by Year") +
  theme(plot.title = element_text(hjust = 0.5))

plot_all_wapo_data <-
  wapo_scores %>%
  subset(Round %notin% excluded_games) %>%
  mutate(winning_seed = ifelse(Score1 > Score2, Seed1, Seed2)) %>%
  group_by(Year) %>%
  do(data.frame(Score = sum(.$winning_seed))) %>%
  mutate(Score = Score / 200)

ggplot(plot_all_wapo_data, aes(x = Year, y = Score)) + geom_line() +
  ggtitle("Aggregate NCAA Tournament Upsets by Year") +
  theme(plot.title = element_text(hjust = 0.5))
