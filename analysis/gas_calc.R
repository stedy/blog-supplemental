library(plyr)
library(ggplot2)
raw <- read.csv("../data/gas2014.csv")
avediff <- diff(raw$Mileage)
raw$totalmiles <- c(NA, avediff)
raw$Avgcost <- raw$totalmiles / raw$TotalCost
raw$AvgMPG <- raw$totalmiles / raw$Amount

costsummary <- ddply(raw, "Brand", function(x) data.frame(meancost = mean(x$Avgcost, na.rm=T)))
mpgsummary <- ddply(raw, "Brand", function(x) data.frame(meanMPG = mean(x$AvgMPG, na.rm=T)))
final <- merge(costsummary, mpgsummary)

ggplot(final, aes(x= meancost, y= meanMPG, label= Brand)) +
  geom_point(size=4) + 
  geom_text(aes(label= Brand), hjust= -0.2, vjust= -0.2, size=6) +
  scale_x_continuous(limits = c(6,15)) +
  ylab("mean MPG") + xlab("Mean cost") +
  theme(axis.text.x = element_text(face='bold', size=20),
        axis.text.y = element_text(face='bold', size=20),
        axis.title.y = element_text(face="bold", size=20, angle=90),
        axis.title.x = element_text(face="bold", size=20)
  )
