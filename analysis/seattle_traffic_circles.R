library(sp)
library(dplyr)

trim <- function (x) 
  gsub("^\\s+|\\s+$", "", x)

streets <- rgdal::readOGR("Street_Network_Database/WGS84/Street_Network_Database.shp", layer="Street_Network_Database")
traffic_circles <- rgdal::readOGR("Traffic Circles/WGS84/TrafficCircles.shp", layer="TrafficCircles")

#get street names
traffic_circles$NSstreet <- trim(sapply(strsplit(traffic_circles$UNITDESC, "AND"), "[", 1))
traffic_circles$EWstreet <- trim(sapply(strsplit(traffic_circles$UNITDESC, "AND"), "[", 2))

traffic_circles.df <- data.frame(table(c(traffic_circles$NSstreet, traffic_circles$EWstreet)))
traffic_circles.df <-
      traffic_circles.df %>%
      rename(street = Var1,
             count = Freq) %>%
      subset(count > 2) %>%
      arrange(desc(count))
  
streets_traffic_circles <- subset(streets, ORD_STNAME %in% traffic_circles.df$street)

#then color appropriately
lookup.df <- data.frame(
color = RColorBrewer::brewer.pal(9, "PuBu")[2:9],
binned = levels(cut(traffic_circles.df$count, 8))
)

traffic_circles.df <-
  traffic_circles.df %>%
  mutate(binned = cut(traffic_circles.df$count, 8)) %>%
  merge(lookup.df) %>%
  select(street, color)

colored_traffic_circles <- merge(streets_traffic_circles, traffic_circles.df, by.x="ORD_STNAME", by.y="street")

rgdal::writeOGR(colored_traffic_circles, dsn = "colored_traffic_circles.geojson", layer ="Street_Network_Database",
                driver="GeoJSON", check_exists=FALSE)
