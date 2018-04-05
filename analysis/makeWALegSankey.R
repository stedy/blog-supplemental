library(XML)
library(dplyr)

allLeg <- xmlParse("http://wslwebservices.leg.wa.gov/LegislationService.asmx/GetLegislationInfoIntroducedSince?sinceDate=2016-11-08") %>%
  xmlToDataFrame()

IntroducedHousePassedHouse <- xmlParse("http://wslwebservices.leg.wa.gov/LegislationService.asmx/GetHouseLegislationPassedHouse?biennium=2017-18") %>%
  xmlToDataFrame()

IntroducedSenatePassedHouse <- xmlParse("http://wslwebservices.leg.wa.gov/LegislationService.asmx/GetSenateLegislationPassedHouse?biennium=2017-18") %>%
  xmlToDataFrame()

#then repeat with Senate
IntroducedSenatePassedSenate <- xmlParse("http://wslwebservices.leg.wa.gov/LegislationService.asmx/GetSenateLegislationPassedSenate?biennium=2017-18") %>%
  xmlToDataFrame()

IntroducedHousePassedSenate <- xmlParse("http://wslwebservices.leg.wa.gov/LegislationService.asmx/GetHouseLegislationPassedSenate?biennium=2017-18") %>%
  xmlToDataFrame()

#In Governor office
PartialVetoHouse <- xmlParse("http://wslwebservices.leg.wa.gov/LegislationService.asmx/GetLegislationGovernorPartialVeto?biennium=2017-18&agency=House") %>%
  xmlToDataFrame()

FullVetoHouse <- xmlParse("http://wslwebservices.leg.wa.gov/LegislationService.asmx/GetLegislationGovernorVeto?biennium=2017-18&agency=House") %>%
  xmlToDataFrame()

SignedHouse <- xmlParse("http://wslwebservices.leg.wa.gov/LegislationService.asmx/GetLegislationGovernorSigned?biennium=2017-18&agency=House") %>%
  xmlToDataFrame()

PartialVetoSenate <- xmlParse("http://wslwebservices.leg.wa.gov/LegislationService.asmx/GetLegislationGovernorPartialVeto?biennium=2017-18&agency=Senate") %>%
  xmlToDataFrame()

FullVetoSenate <- xmlParse("http://wslwebservices.leg.wa.gov/LegislationService.asmx/GetLegislationGovernorVeto?biennium=2017-18&agency=Senate") %>%
  xmlToDataFrame()

SignedSenate <- xmlParse("http://wslwebservices.leg.wa.gov/LegislationService.asmx/GetLegislationGovernorSigned?biennium=2017-18&agency=Senate") %>%
  xmlToDataFrame()

bills_only <- allLeg %>%
  subset(ShortLegislationType == "BBill") %>%
  select(BillNumber, OriginalAgency) %>%
  distinct()

introduced_house_count = length(which(bills_only$OriginalAgency == "House"))
introduced_senate_count = length(which(bills_only$OriginalAgency == "Senate"))

introduced_df <- data.frame(origin = c("Introduced House", "Introduced Senate"), target = c("Passed House", "Passed Senate"),
                            value = c(introduced_house_count, introduced_senate_count))

house_df <- data.frame(origin = rep("Introduced House", 3),
                      target = c("Passed House", "Died", "Passed Senate"),
                      value = c(nrow(IntroducedHousePassedHouse),
                                introduced_house_count - nrow(IntroducedHousePassedHouse),
                                nrow(IntroducedHousePassedSenate)))

senate_df <- data.frame(origin = rep("Introduced Senate", 3),
                        target = c("Passed Senate", "Died", "Passed House"),
                        value = c(nrow(IntroducedSenatePassedSenate),
                                  introduced_senate_count - nrow(IntroducedSenatePassedSenate),
                                  nrow(IntroducedSenatePassedHouse)))

governor_df <- data.frame(origin = rep(c("Passed House", "Passed Senate"), 3),
                          target = rep(c("Partial Veto", "Full Veto", "Signed"), each = 2),
                          value = c(nrow(PartialVetoHouse), nrow(PartialVetoSenate),
                                    nrow(FullVetoHouse), nrow(FullVetoSenate),
                                    nrow(SignedHouse), nrow(SignedSenate)))

governor_missing <- governor_df %>%
  group_by(origin) %>%
  do(data.frame(action = sum(.$value)))

gov_missing_house <- nrow(IntroducedSenatePassedHouse) - subset(governor_missing, origin == "Passed Senate")$action
gov_missing_senate <- nrow(IntroducedHousePassedSenate) - subset(governor_missing, origin == "Passed House")$action

gov_missing_df <- data.frame(origin = c("Passed Senate", "Passed House"),
                             target = c("Died", "Died"),
                             value = c(gov_missing_house, gov_missing_senate))

writeout <- rbind.data.frame(house_df, senate_df, governor_df, gov_missing_df) %>%
  rename(source = origin)
write.csv(writeout, "wa_leg_sankey.csv", row.names=F, quote=F)
