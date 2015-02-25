devtools::install_github("corynissen/fitbitScraper")
library("fitbitScraper")
library("ggplot2")

cookie <- login(email="email", password="password")
df <- get_15_min_data(cookie, what="steps", date=as.character(Sys.Date() - 1))

ggplot(df) + geom_bar(aes(x=time, y=data, fill=data), stat="identity") +
  xlab("Time of day") +ylab("steps") +
  theme(axis.ticks.x=element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background=element_blank(),
        panel.grid.major.y=element_line(colour="gray", size=.1),
        legend.position="none") +
  ggtitle("Single day of steps counted with Fitbit")

df <- get_daily_data(cookie, what="steps", start_date="2014-02-26", end_date=as.character(Sys.Date()))
ggplot(df, aes(x=time, y=data)) + geom_point() + stat_smooth() + labs(y = "Steps taken", x="Date")
print(mean(df$data))

df <- get_daily_data(cookie, what="floors", start_date="2014-02-26", end_date=as.character(Sys.Date()))
ggplot(df, aes(x=time, y=data)) + geom_point() + stat_smooth() + labs(y = "Floors climbed", x="Date")
print(mean(df$data))