library(dplyr)
library(rvest)
library(ggplot2)

pac12_teams <- c("USC", "UCLA", "Arizona", "Arizona State", "Stanford", "California",
           "Utah", "Colorado", "Washington State", "Oregon State", "Oregon")

season2017 <-
  read_html("https://en.wikipedia.org/wiki/2017_Washington_Huskies_football_team") %>%
  html_nodes("table") %>%
  .[[7]] %>%
  html_table(trim=T, fill = T) %>%
  data.frame(.) %>%
  subset(grepl("Husky", Site)) %>%
  mutate(opponent = stringr::str_match(`Opponent.`, paste(pac12_teams, collapse="|")),
         date = as.Date(paste0(Date, " 2017"), "B %d %Y"),
         season = "2017",
         game_order = 1:nrow(.)) %>%
  select(date, Attendance, season, game_order, opponent, TV)

season2016 <-
  read_html("https://en.wikipedia.org/wiki/2016_Washington_Huskies_football_team") %>%
  html_nodes("table") %>%
  .[[8]] %>%
  html_table(trim=T, fill = T) %>%
  data.frame(.) %>%
  subset(grepl("Husky", Site)) %>%
  mutate(opponent = stringr::str_match(`Opponent.`, paste(pac12_teams, collapse="|")),
         date = as.Date(paste0(Date, " 2016"), "%B %d %Y"),
         season = "2016",
         game_order = 1:nrow(.)) %>%
  select(date, Attendance, season, game_order, opponent, TV)

season2015 <-
  read_html("https://en.wikipedia.org/wiki/2015_Washington_Huskies_football_team") %>%
  html_nodes("table") %>%
  .[[9]] %>%
  html_table(trim = T, fill = T) %>%
  data.frame(.) %>%
  subset(grepl("Husky", Site)) %>%
  mutate(opponent = stringr::str_match(`Opponent.`, paste(pac12_teams, collapse="|")),
         date = as.Date(paste0(Date, " 2015"), "%B %d %Y"),
         season = "2015",
         game_order = 1:nrow(.)) %>%
  select(date, Attendance, season, game_order, opponent, TV)

season2014 <-
  read_html("https://en.wikipedia.org/wiki/2014_Washington_Huskies_football_team") %>%
  html_nodes("table") %>%
  .[[9]] %>%
  html_table(trim = T, fill = T) %>%
  data.frame(.) %>%
  subset(grepl("Husky", Site)) %>%
  mutate(opponent = stringr::str_match(`Opponent.`, paste(pac12_teams, collapse="|")),
         date = as.Date(paste0(Date, " 2014"), "%B %d %Y"),
         season = "2014",
         game_order = 1:nrow(.)) %>%
  select(date, Attendance, season, game_order, opponent, TV)

season2013 <-
  read_html("https://en.wikipedia.org/wiki/2013_Washington_Huskies_football_team") %>%
  html_nodes("table") %>%
  .[[8]] %>%
  html_table(trim =T, fill = T) %>%
  data.frame(.) %>%
  subset(grepl("Husky", Site)) %>%
  mutate(opponent = stringr::str_match(`Opponent.`, paste(pac12_teams, collapse="|")),
         date = as.Date(paste0(Date, " 2013"), "%B %d %Y"),
         season = "2013",
         game_order = 1:nrow(.)) %>%
  select(date, Attendance, season, game_order, opponent, TV)

season2012 <-
  read_html("https://en.wikipedia.org/wiki/2012_Washington_Huskies_football_team") %>%
  html_nodes("table") %>%
  .[[3]] %>%
  html_table(trim = T, fill = T) %>%
  data.frame(.) %>%
  subset(grepl("CenturyLink Field", Site)) %>%
  mutate(opponent = stringr::str_match(`Opponent.`, paste(pac12_teams, collapse="|")),
         date = as.Date(paste0(Date, " 2012"), "%B %d %Y"),
         season = "2012",
         game_order = 1:nrow(.)) %>%
  select(date, Attendance, season, game_order, opponent, TV)

season2011 <-
  read_html("https://en.wikipedia.org/wiki/2011_Washington_Huskies_football_team") %>%
  html_nodes("table") %>%
  .[[3]] %>%
  html_table(trim = T, fill = T) %>%
  data.frame(.) %>%
  subset(grepl(paste(c("CenturyLink Field", "Husky Stadium"), collapse="|"), Site)) %>%
  mutate(opponent = stringr::str_match(`Opponent.`, paste(pac12_teams, collapse="|")),
          date = as.Date(paste0(Date, " 2011"), "%B %d %Y"),
          season = "2011",
          game_order = 1:nrow(.)) %>%
  select(date, Attendance, season, game_order, opponent, TV)
season2011$Attendance <- sapply(strsplit(season2011$Attendance, "\\["), "[", 1)

season2010 <-
  read_html("https://en.wikipedia.org/wiki/2010_Washington_Huskies_football_team") %>%
  html_nodes("table") %>%
  .[[3]] %>%
  html_table(trim = T, fill = T) %>%
  data.frame(.) %>%
  subset(grepl("Husky Stadium", Site)) %>%
  mutate(opponent = stringr::str_match(`Opponent.`, paste(pac12_teams, collapse="|")),
        date = as.Date(paste0(Date, " 2010"), "%B %d %Y"),
        season = "2010",
        game_order = 1:nrow(.)) %>%
  select(date, Attendance, season, game_order, opponent, TV)
season2010$Attendance <- sapply(strsplit(season2010$Attendance, "\\["), "[", 1)

season2009 <-
  read_html("https://en.wikipedia.org/wiki/2009_Washington_Huskies_football_team") %>%
  html_nodes("table") %>%
  .[[3]] %>%
  html_table(trim = T, fill = T) %>%
  data.frame(.) %>%
  subset(grepl("Husky Stadium", Site)) %>%
  mutate(opponent = stringr::str_match(`Opponent.`, paste(pac12_teams, collapse="|")),
         date = as.Date(paste0(Date, " 2009"), "%B %d %Y"),
         season = "2009",
         game_order = 1:nrow(.)) %>%
  select(date, Attendance, season, game_order, opponent, TV)
season2009$Attendance <- sapply(strsplit(season2009$Attendance, "\\["), "[", 1)

season2008 <-
  read_html("https://en.wikipedia.org/wiki/2008_Washington_Huskies_football_team") %>%
  html_nodes("table") %>%
  .[[3]] %>%
  html_table(trim =T, fill = T) %>%
  data.frame(.) %>%
  subset(grepl("Husky Stadium", Site)) %>%
  mutate(opponent = stringr::str_match(Opponent, paste(pac12_teams, collapse="|")),
         date = as.Date(paste0(Date, " 2008"), "%B %d %Y"),
         season = "2008",
         game_order = 1:nrow(.)) %>%
  select(date, Attendance, season, game_order, opponent, TV)
season2008$Attendance <- sapply(strsplit(season2008$Attendance, "\\["), "[", 1)

forplot <- rbind.data.frame(season2017, season2016, season2015, season2014,
                            season2013, season2012, season2011,
                            season2010, season2009, season2008)
forplot$opponent <- as.character(forplot$opponent)
forplot$Attendance = as.numeric(gsub(",", "", forplot$Attendance))
forplot$TV <- ifelse(forplot$TV == "P12N", "P12N", "other")

p <- ggplot(forplot, aes(x=date, y=Attendance, group=TV))
p + geom_point(aes(color=TV, size=TV)) +
  theme(legend.key.size= unit(20, "point"),
        legend.text=element_text(size=12)) +
  labs(x="Game date", y = "Total Attendance") +
  ggtitle("University of Washington home football game attendance stratified by TV channel") + 
  theme(plot.title = element_text(hjust = 0.5))
ggsave("UW_football_attendance_by_TV_2008-17.png", width=7, height=5, units="in")

#Pac10/Pac12 only

forplot_pac <- forplot %>%
  subset(complete.cases(.))
p <- ggplot(forplot_pac, aes(x=date, y=Attendance, group=TV))
p + geom_point(aes(color=TV, size=TV)) +
  theme(legend.key.size= unit(20, "point"),
        legend.text=element_text(size=12)) +
  labs(x="Game date", y = "Total Attendance",
  title = "University of Washington home football game attendance stratified by TV channel",
  subtitle = "Pac-10/Pac-12 Teams only") + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle=element_text(hjust=0.5))
ggsave("UW_football_attendance_by_TV_2008-17_Pac12_only.png", width=7, height=5, units="in")
