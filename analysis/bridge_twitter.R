#code used for http://zachstednick.name/blog/2015/09/02/fremont-bridge-opening-times/

library(twitteR)
library(RCurl)
library(dplyr)

setup_twitter_oauth('Consumer Key (API Key)',
                    'Consumer Secret (API Secret)',
                    'Access Token',
                    'Access Token Secret')

bridges.raw <- userTimeline('sdotbridges', n = 10000)
bridges.raw.df <- do.call("rbind", lapply(bridges.raw, as.data.frame))
bridges.raw.df$time <- sapply(strsplit(bridges.raw.df$text, "-"), "[", 2)

fremont <- 
  bridges.raw.df %>%
  subset(grepl("Fremont", text)) %>%
  mutate(event.time = as.POSIXct(created, "%Y-%m-%d %H:%M:%S", tz="US/Pacific") - (7*3600),
         open = grepl("reopened", text),
         weekday = lubridate::wday(event.time),
         post.morning.rush = event.time - as.POSIXct(paste(as.character(as.Date(fremont$event.time)),
                                                           "09:00 AM"), format="%Y-%m-%d %I:%M %p", tz="UTC"),
         post.evening.rush = event.time - as.POSIXct(paste(as.character(as.Date(fremont$event.time)),
                                                           "06:00 PM"), format="%Y-%m-%d %I:%M %p", tz="UTC")) %>%
  select(text, event.time, created, open, weekday, post.morning.rush, post.evening.rush)


bridge.morning <- 
  fremont %>%
  subset(post.morning.rush >= 0 & weekday %in% 2:6 & open == TRUE)

bridge.evening <- 
  fremont %>%
  subset(post.evening.rush >= 0 & weekday %in% 2:6 & open == TRUE)

bridge.morning.first <-
  bridge.morning %>%
  group_by(as.Date(event.time)) %>%
  do(data.frame(first.opening=min(.$post.morning.rush)/60)) %>%
  mutate(event = "morning")

bridge.evening.first <-
  bridge.evening %>%
  group_by(as.Date(event.time)) %>%
  do(data.frame(first.opening=min(.$post.evening.rush))) %>%
  mutate(event = "evening")

forplot = rbind(bridge.morning.first, bridge.evening.first)
forplot$event <- factor(forplot$event, levels = c('morning', 'evening'))

png("Fremont_bridge_openings.png")
boxplot(forplot$first.opening ~ forplot$event,
        col=c("blue", "darkorange3"), ylab="Minutes difference",
        main="Inital post weekday rush hour Fremont bridge opening times")
dev.off()
