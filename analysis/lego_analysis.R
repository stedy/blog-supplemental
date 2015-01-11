library(plyr)
library(ggplot2)

raw <- read.csv("data/LEGO.csv", header=T)
raw$estimated.price <- raw$pieces / 10
raw$estimated.price.adjusted <- raw$pieces / 19

data2014 <- raw[c('price', 'estimated.price')]
data2014$year <- "2014"

data1989 <- raw[c('price', 'estimated.price.adjusted')]
data1989$year <- "1989"
names(data1989)[2] <- 'estimated.price'

forplot <- rbind(data1989, data2014)

qplot(price, estimated.price, data = forplot, color = year) + 
  geom_smooth() +
  xlab("Price") + ylab("Estimated Price") +
  theme(axis.text.y = element_text(face='bold', size=14),
        axis.text.x = element_text(face='bold', size=14),
        axis.title.y = element_text(face='bold', size=14),
        axis.title.x = element_text(face='bold', size=14))

print(cor(raw$price, raw$estimated.price))

group.means <- ddply(raw, "collection", function(x) data.frame(mg = mean(x$price)))
group.cor <- ddply(raw, "collection", function(x) data.frame(gc = cor(x$price, x$estimated.price)))
