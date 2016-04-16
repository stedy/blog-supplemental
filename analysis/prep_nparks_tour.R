library(dplyr)

raw <- read.csv("NPS_tour.csv", colClasses = "character")

#add in home
home <- c("Seattle", "home", "47.6147628","-122.4759907", NA)

raw <- rbind(raw, home) %>%
  mutate(lat = as.numeric(lat),
         long = as.numeric(long))

calculateDistance <- function(park1, park2){
  lat1 <- subset(raw, name == park1)$lat
  lat2 <- subset(raw, name == park2)$lat
  long1 <- subset(raw, name == park1)$long
  long2 <- subset(raw, name == park2)$long
  degRad <- pi/180
  phi1 <- (90 - lat1) * degRad
  phi2 <- (90 - lat2) * degRad

  theta1 <- long1 * degRad
  theta2 <- long2 * degRad

  cosTotal <- sin(phi1) * sin(phi2) * cos(theta1-theta2) + cos(phi1) * cos(phi2)
  arc <- acos(cosTotal)
  return(arc * 3960)
}

ans <- c()
for(x in raw$name){
  for(y in raw$name){
    ans <- c(ans, calculateDistance(x,y))
  }
}

distmatrix <- matrix(ans, nrow=nrow(raw),
                     dimnames=list(raw$name, raw$name))
diag(distmatrix) <- 0

#then convert to TSP

distmatrix.TSP <- TSP::as.TSP(distmatrix)
tour <- TSP::solve_TSP(distmatrix.TSP, start=as.integer(45))
route <- tour[1:45]

writeout <- raw[match(names(route), raw$name), ] %>%
  dplyr::select(name, lat, long)
writeout <- rbind(writeout, writeout[1, ])

feather::write_feather(writeout, "TSP_route.feather")
