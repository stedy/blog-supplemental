library(dplyr)
library(XML)
library(ggplot2)

`%notin%` <- function(x,y) !(x %in% y)

allLeg <- xmlParse("http://wslwebservices.leg.wa.gov/LegislationService.asmx/GetLegislationInfoIntroducedSince?sinceDate=2016-11-08") %>%
  xmlToDataFrame() %>%
  subset(ShortLegislationType == "BBill") %>%
  select(BillNumber, OriginalAgency) %>%
  distinct

SignedHouse <- xmlParse("http://wslwebservices.leg.wa.gov/LegislationService.asmx/GetLegislationGovernorSigned?biennium=2017-18&agency=House") %>%
    xmlToDataFrame()

SignedSenate <- xmlParse("http://wslwebservices.leg.wa.gov/LegislationService.asmx/GetLegislationGovernorSigned?biennium=2017-18&agency=Senate") %>%
    xmlToDataFrame()


passed_bills = c(SignedHouse$BillNumber, SignedSenate$BillNumber)

passed_bills_df <- c()
for (x in passed_bills){
  temp = xmlParse(paste0("http://wslwebservices.leg.wa.gov/LegislationService.asmx/GetSponsors?biennium=2017-18&BillId=", x)) %>%
  xmlToDataFrame() %>%
  select(Name) %>%
  mutate(BillNumber = x)
  passed_bills_df <- rbind(passed_bills_df, temp)
}

origin_df <- rbind.data.frame(SignedHouse, SignedSenate)

passed_bills_df_summary <-
  passed_bills_df %>%
  group_by(BillNumber) %>%
  do(data.frame(count = length(.$Name))) %>%
  merge(origin_df)

ggplot(passed_bills_df_summary, aes(x=count, fill = OriginalAgency)) +
  geom_histogram() +
  ggtitle('Bills passed as counted by number of sponsors') +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab('Sponsor Count')
ggsave('all_sponsor_counts.png')

#bills that did not pass

did_not_pass <-
  allLeg %>%
  subset(BillNumber %notin% passed_bills)

unpassed_bills_df <- c()
for (x in did_not_pass$BillNumber){
  temp = xmlParse(paste0("http://wslwebservices.leg.wa.gov/LegislationService.asmx/GetSponsors?biennium=2017-18&BillId=", x)) %>%
  xmlToDataFrame() %>%
  select(Name) %>%
  mutate(BillNumber = x)
  unpassed_bills_df <- rbind(unpassed_bills_df, temp)
}

unpassed_bills_df_summary <-
  unpassed_bills_df %>%
  distinct() %>%
  group_by(BillNumber) %>%
  do(data.frame(count = length(.$Name))) %>%
  merge(did_not_pass)

ggplot(unpassed_bills_df_summary, aes(x=count, fill = OriginalAgency)) +
  geom_histogram() +
  ggtitle("Bills not passed as counted by number of sponsors") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab('Sponsor Count')
ggsave('all_sponsor_counts_not_passed.png')

#look at roll calls per bill

bill_vote_counts <- c()
for (x in passed_bills){
  temp <- xmlParse(paste0("http://wslwebservices.leg.wa.gov/LegislationService.asmx/GetRollCalls?biennium=2017-18&BillNumber=", x)) %>%
      xmlToDataFrame() %>%
      subset(grepl("Final Passage", Motion)) %>%
      mutate(Yeas = stringr::str_extract(YeaVotes, "[0-9]{2}"),
         Nays = stringr::str_extract(NayVotes, "[0-9]*"),
         Absent = stringr::str_extract(AbsentVotes, "[0-9]*"),
         Excused = stringr::str_extract(ExcusedVotes, "[0-9]*")) %>%
  select(BillId, Agency, Yeas, Nays, Absent, Excused, VoteDate)
  bill_vote_counts <- rbind(bill_vote_counts, temp)
}

bill_vote_counts_cleaned <-
  bill_vote_counts %>%
  mutate(pct = ifelse(Agency == "Senate", as.numeric(Yeas) / 49, as.numeric(Yeas)/98),
         VoteDate = as.Date(VoteDate, "%Y-%m-%d")) %>%
  group_by(BillId, Agency) %>%
  slice(which.max(VoteDate))
ggplot(bill_vote_counts_cleaned, aes(x=pct, fill=Agency)) +
  geom_histogram() +
  ggtitle("Percentage of Yea votes by bill") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab('Percentage of Yea votes')
ggsave('percentage_yea_votes.png')

#post Manka Dhingra

bill_vote_counts_cleaned_2017 <-
  bill_vote_counts_cleaned %>%
  subset(VoteDate <= as.Date("2017-11-01"))
ggplot(bill_vote_counts_cleaned_2017, aes(x=pct, fill=Agency)) + geom_histogram()

bill_vote_counts_cleaned_2018 <-
  bill_vote_counts_cleaned %>%
  subset(VoteDate >= as.Date("2018-01-01"))
ggplot(bill_vote_counts_cleaned_2018, aes(x=pct, fill=Agency)) + geom_histogram()
