library(dplyr)
library(rvest)
library(ggplot2)

pac12_teams <- c("USC", "UCLA", "Arizona", "Arizona State", "Stanford", "California",
           "Utah", "Colorado", "Washington State", "Oregon State", "Oregon")

season2015 <- 
  read_html("https://en.wikipedia.org/wiki/2015_Washington_Huskies_football_team") %>%
  html_nodes(xpath="/html/body/div[3]/div[3]/div[4]/table[6]") %>%
  html_table(fill = T) %>%
  data.frame(.) %>%
  subset(grepl("Husky", Site)) %>%
  mutate(opponent = stringr::str_match(`Opponent.`, paste(pac12_teams, collapse="|")),
         date = as.Date(paste0(Date, " 2015"), "%B %d %Y"),
         season = "2015",
         game_order = 1:nrow(.)) %>%
  select(date, Attendance, season, game_order, opponent, TV)

season2014 <-
  read_html("https://en.wikipedia.org/wiki/2014_Washington_Huskies_football_team") %>%
  html_nodes(xpath="/html/body/div[3]/div[3]/div[4]/table[6]") %>%
  html_table(fill = T) %>%
  data.frame(.) %>%
  subset(grepl("Husky", Site)) %>%
  mutate(opponent = stringr::str_match(`Opponent.`, paste(pac12_teams, collapse="|")),
         date = as.Date(paste0(Date, " 2014"), "%B %d %Y"),
         season = "2014",
         game_order = 1:nrow(.)) %>%
  select(date, Attendance, season, game_order, opponent, TV)

season2013 <-
  read_html("https://en.wikipedia.org/wiki/2013_Washington_Huskies_football_team") %>%
  html_nodes(xpath="/html/body/div[3]/div[3]/div[4]/table[5]") %>%
  html_table(fill = T) %>%
  data.frame(.) %>%
  subset(grepl("Husky", Site)) %>%
  mutate(opponent = stringr::str_match(`Opponent.`, paste(pac12_teams, collapse="|")),
         date = as.Date(paste0(Date, " 2013"), "%B %d %Y"),
         season = "2013",
         game_order = 1:nrow(.)) %>%
  select(date, Attendance, season, game_order, opponent, TV)

season2012 <-
  read_html("https://en.wikipedia.org/wiki/2012_Washington_Huskies_football_team") %>%
  html_nodes(xpath="/html/body/div[3]/div[3]/div[4]/table[3]") %>%
  html_table(fill = T) %>%
  data.frame(.) %>%
  subset(grepl("CenturyLink Field", Site)) %>%
  mutate(opponent = stringr::str_match(`Opponent.`, paste(pac12_teams, collapse="|")),
         date = as.Date(paste0(Date, " 2012"), "%B %d %Y"),
         season = "2012",
         game_order = 1:nrow(.)) %>%
  select(date, Attendance, season, game_order, opponent, TV)

season2011 <-
  read_html("https://en.wikipedia.org/wiki/2011_Washington_Huskies_football_team") %>%
  html_nodes(xpath="/html/body/div[3]/div[3]/div[4]/table[3]") %>%
  html_table(fill = T) %>%
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
  html_nodes(xpath="/html/body/div[3]/div[3]/div[4]/table[3]") %>%
  html_table(fill = T) %>%
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
  html_nodes(xpath="/html/body/div[3]/div[3]/div[4]/table[3]") %>%
  html_table(fill = T) %>%
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
  html_nodes(xpath="/html/body/div[3]/div[3]/div[4]/table[3]") %>%
  html_table(fill = T) %>%
  data.frame(.) %>%
  subset(grepl("Husky Stadium", Site)) %>%
  mutate(opponent = stringr::str_match(Opponent, paste(pac12_teams, collapse="|")),
         date = as.Date(paste0(Date, " 2008"), "%B %d %Y"),
         season = "2008",
         game_order = 1:nrow(.)) %>%
  select(date, Attendance, season, game_order, opponent, TV)
season2008$Attendance <- sapply(strsplit(season2008$Attendance, "\\["), "[", 1)

forplot <- rbind.data.frame(season2015, season2014, season2013, season2012, season2011,
                            season2010, season2009, season2008)
forplot$opponent <- as.character(forplot$opponent)
forplot$Attendance = as.numeric(gsub(",", "", forplot$Attendance))

#first look at TV
p <- ggplot(forplot, aes(x=date, y=Attendance, group=TV))
p + geom_point(aes(color=TV, size=TV)) +
  scale_size_manual(values = c(rep(2,9), 3, 2, 2), guide="none") + 
  theme(legend.key.size= unit(20, "point"),
        legend.text=element_text(size=12)) +
  labs(x="Game date", y = "Total Attendance") +
  ggtitle("University of Washington home football game attendance by TV channel")
ggsave("UW_football_attendance_by_TV.png", width=7, height=5, units="in")

#Then Pac-12 only

forplot$opponent <- ifelse(forplot$date == "2015-09-19", NA, forplot$opponent)
forplot <- forplot %>% subset(!is.na(opponent))

p <- ggplot(forplot, aes(x=game_order, y=Attendance, group=season))
p + geom_smooth(aes(color=season)) +
  labs(x="Home game number", y = "Total Attendance") +
  ggtitle("University of Washington home football game attendance")
ggsave("UW_football_total_attendance.png", width=7, height=5, units="in")
 
forplot$capacity <- ifelse(forplot$date < as.Date("2011-11-26"), 72500, ifelse(forplot$date >= as.Date("2013-08-31"), 70138, 67000))

p <- ggplot(forplot, aes(x=game_order, y=Attendance/capacity, group=season))
p + stat_smooth(aes(color=season)) +
  labs(x="Home game number", y = "Percentage of capacity") +
  ggtitle("University of Washington home football game attendance")
ggsave("UW_football_percentage_capacity.png", width=7, height=5, units="in")

counts <- forplot %>%
  group_by(opponent) %>%
  do(data.frame(mean_attendance = mean(.$Attendance))) %>%
  arrange(desc(mean_attendance))