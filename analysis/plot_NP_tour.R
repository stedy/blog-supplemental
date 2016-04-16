library(ggmap)
rawpoints <- feather::read_feather("frompython.feather")

names(rawpoints) <- c("lat", "long")
parks <- read.csv("NPS_tour.csv")
US <- get_map("United States of America", zoom=3)

p <- ggmap(US)
p + geom_path(data=rawpoints, aes(x=lat, y=long), color="red", size=0.3) +
  geom_point(data=parks, aes(x=long, y=lat), color="blue", size=3, alpha=0.5)

ggsave("NP_tour.png")
