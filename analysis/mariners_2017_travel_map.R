library(dplyr)
library(rvest)

raw <- read_html("https://www.baseball-reference.com/teams/SEA/2017-schedule-scores.shtml") %>%
  html_nodes("table") %>%
  .[[1]] %>%
  html_table()
names(raw)[c(3,5)] <- c("boxscore", "at")

schedule <-
  raw %>%
  subset(Tm == "SEA") %>%
  mutate(Date = gsub("\\([0-9]\\)", "", Date)) %>%
  mutate(Date_noday = trimws(stringr::str_split_fixed(Date, ",", 2)[, 2])) %>%
  mutate(Date = as.Date(paste0(Date_noday, " 2017"), format = "%b %d %Y")) %>%
  select(Date, Tm, at, Opp) %>%
  mutate(location = ifelse(at == "@", Opp, "SEA"))

team_info <- jsonlite::read_json("https://gist.githubusercontent.com/the55/2155142/raw/30a251395cd3c04771f29f2a6295fc8849b73d11/mlb_stadium.json",
                                 simplifyVector = T)
team_info$abbr <- c("LAA", "ARI", "ATL", "BAL", "BOS", "CHC", "CHW", "CIN", "CLE",
                    "COL", "DET", "FLO", "HOU", "KCR", "LAD", "MIL", "MIN", "WSN",
                    "NYM", "NYY", "OAK", "PHI", "PIT", "STL", "SDP", "SFG", "SEA",
                    "TBR", "TEX", "TOR")

schedule_ll <-
  schedule %>%
  rename(abbr = location) %>%
  merge(team_info) %>%
  arrange(Date)

SEA_to_HOU <- geosphere::distHaversine(
  c(team_info[team_info$abbr == "SEA", ]$lng,
    team_info[team_info$abbr == "SEA", ]$lat),
  c(team_info[team_info$abbr == "HOU", ]$lng,
    team_info[team_info$abbr == "HOU", ]$lat))

LAA_to_SEA <- geosphere::distHaversine(
  c(team_info[team_info$abbr == "LAA", ]$lng,
    team_info[team_info$abbr == "LAA", ]$lat),
  c(team_info[team_info$abbr == "SEA", ]$lng,
    team_info[team_info$abbr == "SEA", ]$lat))

dist_travelled <- c(SEA_to_HOU)
day_start <- c("SEA")
day_end <- c()

for(x in 2:nrow(schedule_ll) - 1){
  start <- schedule_ll[x, ]
  end <- schedule_ll[x + 1, ]
  day_start <- c(day_start, start$abbr)
  day_end <- c(day_end, end$abbr)
  dist_travelled <- c(dist_travelled, geosphere::distHaversine(c(start$lng, start$lat), c(end$lng, end$lat)))
}
dist_travelled[162] <- LAA_to_SEA
day_end <- c(day_end, "SEA")

schedule_ll$dist_travelled <- dist_travelled
schedule_ll$day_start <- day_start
schedule_ll$day_end <- day_end

final <-
  schedule_ll %>%
  select(Date, dist_travelled, day_start, day_end) %>%
  merge(team_info, by.x= "day_start", by.y="abbr") %>%
  rename(day_start_lng = lng, day_start_lat = lat) %>%
  select(Date, dist_travelled, day_start, day_end, day_start_lng, day_start_lat) %>%
  merge(team_info, by.x= "day_end", by.y="abbr") %>%
  rename(day_end_lng = lng, day_end_lat = lat) %>%
  select(Date, dist_travelled, day_start, day_end, day_start_lng, day_start_lat,
         day_end_lng, day_end_lat) %>%
  arrange(Date) %>%
  subset(dist_travelled > 0)

#then make map

maps::map("usa", col="#f2f2f2", fill=TRUE, bg="white", lwd=0.05)
for(y in 1:nrow(final)){
  seg <- geosphere::gcIntermediate(c(final[y,]$day_start_lng, final[y,]$day_start_lat),
                                   c(final[y,]$day_end_lng, final[y,]$day_end_lat),
                                   addStartEnd = T)
  lines(seg)
}

#then repeat with white lines on black
maps::map("usa", col="#191919", fill =T, bg="#000000")
for(y in 1:nrow(final)){
  seg <- geosphere::gcIntermediate(c(final[y,]$day_start_lng, final[y,]$day_start_lat),
                                   c(final[y,]$day_end_lng, final[y,]$day_end_lat),
                                   addStartEnd = T)
  lines(seg, col= "#FFFFFF")
}

#jitter
final_jitter <- rnorm(nrow(final), sd=0.2)

maps::map("usa", col="#191919", fill =T, bg="#000000")
for(y in 1:nrow(final)){
  seg <- geosphere::gcIntermediate(c(final[y,]$day_start_lng + final_jitter[y],
                                     final[y,]$day_start_lat + final_jitter[y]),
                                   c(final[y,]$day_end_lng + final_jitter[y],
                                     final[y,]$day_end_lat + final_jitter[y]),
                                   addStartEnd = T)
  lines(seg, col= "#FFFFFF")
}
