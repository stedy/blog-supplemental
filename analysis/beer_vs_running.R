library(ggplot2)
library(plyr)
library(tidyr)
library(lubridate)

beer <- read.csv("data/bars_2014.csv", na.strings="")
run <- read.csv("data/run_2014.csv")

beerm <- ddply(beer, "date", function(x) data.frame(count=length(unique(na.omit(x$name)))))
final <- merge(beerm, run)
final[is.na(final)] <- 0
final$cal.in <- final$count * 2 * 180
final$cal.out <- final$distance * 145
final$residual <- final$cal.out - final$cal.in

forplot <- final[c('date', 'cal.in', 'cal.out', 'residual')]
forplot$week <- week(forplot$date)
forplot$date <- NULL
forplot.m <- gather(forplot, type, amount, -week)

qplot(week, amount,data=forplot.m, color=type) + geom_smooth() + ggtitle("Beers consumed at bars vs. running in 2014")
