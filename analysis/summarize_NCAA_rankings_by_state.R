library(dplyr)
library(rvest)

urls <- read.csv("NCAA_rank_urls.txt", header=F)[, 1]
lookup <- read.csv("team_state_lookup.csv")

all_years <- c()

for(url in urls){
  t1 <- read_html(url) %>% html_nodes("table") %>%
    .[[3]] %>%
    html_table(trim=T)

  long_table1 <- c()
  for(x in 1:ncol(t1)){
    temp <- t1[, x]
    temp <- data.frame("raw_team" = t1[, x],
                     "week" = names(t1)[x])
    long_table1 <- rbind(long_table1, temp[1:10, ])
  }

  long_table1$raw_team <- gsub("т", "", long_table1$raw_team)
  long_table1$raw_team <- gsub("([0-9])", "", long_table1$raw_team)
  long_table1$raw_team <- gsub("\\(\\)", "", long_table1$raw_team)
  long_table1$team <- trimws(gsub("\\(–\\)|\\(-\\)|\\(––\\)|\\(--\\)", "", long_table1$raw_team), which = "right")

  counts <- long_table1 %>% merge(lookup, all.x=T)

  summary_total <- counts %>%
    group_by(week) %>%
    do(data.frame(multiple = table(.$state))) %>%
    subset(multiple.Freq > 1) %>%
    mutate(weekclean = stringr::str_extract(week, "Week [0-9]*|Preseason")) %>%
    mutate(year = stringr::str_extract(url, '[0-9]{4}'))
  all_years <- rbind(all_years, summary_total)
}

final <-
  all_years %>%
  ungroup() %>%
  rename(state = `multiple.Var1`, count = `multiple.Freq`) %>%
  select(state, count, weekclean, year)

wa_only <- subset(final, state == "Washington")

most_recent <-
  final %>%
  mutate(year = as.numeric(year)) %>%
  group_by(state) %>%
  slice(which.max(year))

most_recent3 <-
  final %>%
  subset(count == 3) %>%
  mutate(year = as.numeric(year)) %>%
  group_by(state) %>%
  slice(which.max(year))
