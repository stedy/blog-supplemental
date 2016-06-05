library(dplyr)

#data from https://irma.nps.gov/Stats/Reports/National

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

raw <- read.csv("raw.csv", colClasses = "character") %>%
  rename(name = Rowelement1, year = Colhead2, visitation = Rowelement2, mean_visit = Rowelement3) %>%
  mutate(visitation = as.numeric(gsub(',', '', visitation)),
         mean_visit = as.numeric(gsub(',', '', mean_visit)),
         delta = visitation - mean_visit)

raw[] <- sapply(raw, trim)

#then calculate NP above or below mean

NP_only <-
  raw %>%
  mutate(classification = stringr::str_match(name, "N[A-Z]*$")) %>%
  subset(classification == 'NP') %>%
  select(name, year, delta)

outfile <- t(reshape(NP_only, idvar="name", timevar="year", direction="wide"))
outfile <- data.frame(outfile)
outfile$X <- c("date", 2006:2015)
outfile[is.na(outfile)] <- ""

write.table(outfile, "deltaNPfor_d3plot.tsv", row.names=F, col.names=F, quote=F, sep="\t")

NM_only <-
  raw %>%
  mutate(classification = stringr::str_match(name, "N[A-Z]*$")) %>%
  subset(classification == 'NM') %>%
  select(name, year, delta)

outfile <- t(reshape(NM_only, idvar="name", timevar="year", direction="wide"))
outfile <- data.frame(outfile)
outfile$X <- c("date", 2006:2015)
outfile[is.na(outfile)] <- ""

write.table(outfile, "deltaNMfor_d3plot.tsv", row.names=F, col.names=F, quote=F, sep="\t")
