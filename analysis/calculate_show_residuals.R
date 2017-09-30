library(dplyr)
library(RSQLite)
library(ggplot2)

conn <- dbConnect(SQLite(), dbname = "bwtv.db")
base <- dbReadTable(conn, "base")
completed <- dbReadTable(conn, "completed")

dbDisconnect(conn)

final <-
  base %>%
  mutate(Episode = as.numeric(Episode)) %>%
  subset(complete.cases(.)) %>%
  group_by(Title, Season) %>%
  mutate(resid = lm(Value ~ Episode)$residuals) %>%
  ungroup() %>%
  group_by(Title) %>%
  mutate(count = 1:n()) %>%
  mutate(appearance = count/n())

final_min <-
  final %>%
  group_by(Title) %>%
  slice(which.min(resid)) %>%
  select(Title, resid, appearance)

final_max <-
  final %>%
  group_by(Title) %>%
  slice(which.max(resid)) %>%
  select(Title, resid, appearance)

writeout <- rbind.data.frame(final_min, final_max) %>%
  ungroup() %>%
  rename(residual = resid) %>%
  mutate(residual = round(residual, 2)) %>%
  mutate(appearance = round(appearance, 2))

ggplot(writeout, aes(x=appearance, y=residual, color=Title)) + geom_point() +
  theme(legend.position="none")

ggplot(writeout, aes(x=appearance, y=residual, group=Title)) + geom_line() +
  theme(legend.position="none")

write.csv(writeout, "residuals.csv", row.names=F)
