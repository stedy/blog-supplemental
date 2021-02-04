library(dplyr)

#start with baseball
MLB_teams_html <- xml2::read_html("https://en.wikipedia.org/wiki/List_of_World_Series_champions")
MLB_teams_html_tables <- rvest:: html_nodes(MLB_teams_html, "table")
MLB_teams_html_table <- rvest::html_table(MLB_teams_html_tables[[2]], fill = T) %>%
  select(Year, "Winning team", "Losing team") %>%
    rename("winner" = "Winning team", "loser" = "Losing team")

cities <- c("Anaheim", "Atlanta", "Baltimore", "Boston", "Brooklyn", "Chicago", "Cincinnati", "Cleveland",
            "Detroit", "Houston", "Kansas City", "Los Angeles", "Milwaukee", "Minnesota",
            "New York", "Oakland", "Philadelphia", "Pittsburgh", "San Diego", "San Francisco",
            "St. Louis","Tampa Bay", "Toronto","Washington", "Arizona", "Colorado","Florida", "Texas")
cities.string <- paste(unlist(cities), collapse = "|")

MLB_teams_html_table$winner_city <- as.character(stringr::str_extract_all(MLB_teams_html_table$winner, cities.string))
MLB_teams_html_table$loser_city <- as.character(stringr::str_extract_all(MLB_teams_html_table$loser, cities.string))

#then do basketball
NBA_teams_html <- xml2::read_html("https://en.wikipedia.org/wiki/List_of_NBA_champions")
NBA_teams_html_tables <- rvest:: html_nodes(NBA_teams_html, "table")
NBA_teams_html_table <- rvest::html_table(NBA_teams_html_tables[[6]], fill = T) %>%
  select(Year, "Western finalist", "Eastern finalist")

NBA_cities <- c("Dallas", "Fort Wayne","Golden State", "Miami", "Minneapolis", "Oklahoma City",
                "Orlando", "Phoenix", "Portland","Rochester", "San Antonio", "Seattle", "Syracuse",
                "Indiana", "New Jersey", "Utah")
all_cities <- paste(unlist(c(cities, NBA_cities)), collapse = "|")
NBA_teams_html_table$Year <- gsub("\\[[a-z]\\]", "", NBA_teams_html_table$Year)
NBA_teams_html_table$western_city <- as.character(stringr::str_extract_all(NBA_teams_html_table$`Western finalist`,
                                                             all_cities))
NBA_teams_html_table$eastern_city <- as.character(stringr::str_extract_all(NBA_teams_html_table$`Eastern finalist`,
                                                              all_cities))

#NFL
NFL_teams_html <- xml2::read_html("https://en.wikipedia.org/wiki/List_of_Super_Bowl_champions")
NFL_teams_html_tables <- rvest:: html_nodes(NFL_teams_html, "table")
NFL_teams_html_table <- rvest::html_table(NFL_teams_html_tables[[2]], fill = T)
NFL_teams_html_table$Year <- 1:nrow(NFL_teams_html_table) + 1966

NFL_cities <- c("Buffalo","Denver", "Green Bay", "Kansas City", "New England",
                "New Orleans", "Carolina", "Tennessee")
all_cities <- paste(unlist(c(cities, NBA_cities, NFL_cities)), collapse = "|")

NFL_teams_html_table$winning_city <- as.character(stringr::str_extract_all(NFL_teams_html_table$`Winning team`,
                                                              all_cities))
NFL_teams_html_table$losing_city <- as.character(stringr::str_extract_all(NFL_teams_html_table$`Losing team`,
                                                              all_cities))

#finally do NHL
NHL_teams_html <- xml2::read_html("https://en.wikipedia.org/wiki/List_of_Stanley_Cup_champions")
NHL_teams_html_tables <- rvest:: html_nodes(NHL_teams_html, "table")
NHL_teams_html_table <- rvest::html_table(NHL_teams_html_tables[[3]], fill = T)
names(NHL_teams_html_table)[3] <- "Wincoach"

NHL_cities <- c("Calgary", "Edmonton","Montreal", "Nashville" ,"Ottawa","San Jose", "Vancouver", "Vegas")
all_cities <- paste(unlist(c(cities, NBA_cities, NFL_cities, NHL_cities)), collapse = "|")

NHL_teams_html_table$winning_city <- as.character(stringr::str_extract_all(NHL_teams_html_table$`Winning team`,
                                                              all_cities))
NHL_teams_html_table$losing_city <- as.character(stringr::str_extract_all(NHL_teams_html_table$`Losing team`,
                                                              all_cities))

#figuring out the states
all_cities_states <- data.frame(city = c(cities, NBA_cities, NFL_cities, NHL_cities),
                                state = c("California", "Georgia", "Maryland", "Massachusetts",
                                          "New York", "Illinois", "Ohio", "Ohio", "Michigan",
                                          "Texas", "Missouri", "California", "Wisconsin", "Minnesota",
                                          "New York", "California", "Pennsylvania", "Pennsylvania",
                                          "California", "California", "Missouri", "Florida", "Canada",
                                          "Washington D.C.", "Arizona", "Colorado", "Florida", "Texas",
                                          "Texas", "Indiana", "California", "Florida", "Minnesota", "Oklahoma",
                                          "Florida", "Arizona", "Oregon", "New York", "Texas", "Washington",
                                          "New York", "Indiana", "New Jersey", "Utah", "New York", "Colorado",
                                          "Wisconsin", "Missouri", "Massachusetts", "Louisiana", "North Carolina",
                                          "Tennessee", "Canada", "Canada", "Canada", "Tennessee",
                                          "Canada", "California", "Canada", "Nevada"))

#then merge in all the states
MLB_final <- merge(MLB_teams_html_table, all_cities_states, by.x="winner_city", by.y="city") %>%
  rename("MLBstateA" = "state")
MLB_final <- merge(MLB_final, all_cities_states, by.x="loser_city", by.y="city") %>%
  rename("MLBstateB" = "state") %>%
  select(Year, MLBstateA, MLBstateB)

NBA_final <- merge(NBA_teams_html_table, all_cities_states, by.x="western_city", by.y="city") %>%
  rename("NBAstateA" = "state")
NBA_final <- merge(NBA_final, all_cities_states, by.x="eastern_city", by.y="city") %>%
  rename("NBAstateB" = "state") %>%
  select(Year, NBAstateA, NBAstateB)

NFL_final <- merge(NFL_teams_html_table, all_cities_states, by.x="winning_city", by.y="city") %>%
  rename("NFLstateA" = "state")
NFL_final <- merge(NFL_final, all_cities_states, by.x="losing_city", by.y="city") %>%
  rename("NFLstateB" = "state") %>%
  select(Year, NFLstateA, NFLstateB)

NHL_final <- merge(NHL_teams_html_table, all_cities_states, by.x="winning_city", by.y="city") %>%
  rename("NHLstateA" = "state")
NHL_final <- merge(NHL_final, all_cities_states, by.x="losing_city", by.y="city") %>%
  rename("NHLstateB" = "state") %>%
  select(Year, NHLstateA, NHLstateB)

final <- merge(MLB_final, NBA_final, all.x=T)
final <- merge(final, NFL_final, all.x=T)
final <- merge(final, NHL_final, all.x=T)

final <- final %>% mutate_all(as.character) %>%
  subset(complete.cases(.)) %>%
  distinct

summary_table <- c()
for(x in 1:nrow(final)){
  temp <- data.frame(table(unlist(final[x, 2:9])))
  temp <- arrange(temp, desc(temp$Freq))
  temp$year <- final[x, 1]
  temp <- temp[1, ]
  summary_table <- rbind(summary_table, temp)
}

summary_4 <- subset(summary_table, summary_table$Freq == 4)
summary_3 <- subset(summary_table, summary_table$Freq == 3)

#figure out state playoff droughts
final$Year <- as.numeric(final$Year)
sn <- grep("state", names(final), value = T)
state_last_df <- c()
for(y in unique(as.character(all_cities_states$state))){
  temp <- final[apply(final[sn], 1, function(x) any(x %in% c(y))),]
  temp_df <- data.frame(year = max(temp$Year), state = y)
  state_last_df <- rbind(state_last_df, temp_df)
}
