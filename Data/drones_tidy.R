library(knitr)
library(tidyverse)
library(lubridate)
library(stringr)

makerange <- function(column, word) {
  new_col = gsub("Between ", "", column)
  new_col = gsub(paste0(" ", word, " killed."), "", new_col)
  new_col = gsub(" and ", ",", new_col)
  return(new_col)
}

#############################
## Yemen                    #
#############################

#NAF

naf_strikesY <- read.csv("Strikes Data/NAFYemenStrikes.csv", 
                        col.names = c("Date", "Location", "TotalKilled", 
                                      "MilitantsKilled", "CiviliansKilled", "UnknownKilled", "Target"),
                        na.strings = c("")) %>%
  transform(Date = gsub(",", " ", Date)) %>% 
  transform(Date = as.Date(Date, "%a %b %d %Y")) %>%
  arrange(Date) %>%
  transform(Location = gsub("^in ", " in ", Location)) %>%
  transform(Location = gsub("'", "", Location))

naf_strikesY$TotalKilled <- makerange(naf_strikesY$TotalKilled, "total")
naf_strikesY$MilitantsKilled <- makerange(naf_strikesY$MilitantsKilled, "militants")
naf_strikesY$MilitantsKilled <- gsub(" and", "", naf_strikesY$MilitantsKilled)
naf_strikesY$CiviliansKilled <- makerange(naf_strikesY$CiviliansKilled, "civilians")
naf_strikesY$UnknownKilled <- makerange(naf_strikesY$UnknownKilled, "unknown")

naf_strikesY <- naf_strikesY %>%
  separate(TotalKilled, into = c("TotalKilledMin", "TotalKilledMax"), sep = ",") %>%
  separate(MilitantsKilled, into = c("MilitantsKilledMin", "MilitantsKilledMax"), sep = ",") %>%
  separate(CiviliansKilled, into = c("CiviliansKilledMin", "CiviliansKilledMax"), sep = ",") %>%
  separate(UnknownKilled, into = c("UnknownKilledMin", "UnknownKilledMax"), sep = ",") %>%
  separate(Location, into =  c("Location", "Province"), sep = " in ") %>%
  transform(Location = gsub(" $", "", Location)) %>%
  transform(Location = ifelse(Location == "", NA, Location))

naf_leadersY <- read.csv("Strikes Data/NAFYemenStrikesLeaders.csv", 
                        col.names=c("Date", "LeadersKilled", "Description"),
                        na.strings = c("")) %>%
  transform(Date = gsub(",", " ", Date)) %>% 
  transform(Date = as.Date(Date, "%a %b %d %Y")) %>%
  arrange(Date) %>%
  transform(LeadersKilled = gsub("<p>", "", LeadersKilled)) %>%
  transform(LeadersKilled = gsub("<\\/p>", "", LeadersKilled)) %>%
  transform(LeadersKilled = gsub(";", ",", LeadersKilled)) %>%
  transform(LeadersKilled = gsub("and ", "", LeadersKilled)) %>%
  transform(LeadersKilled = gsub(", possibly two unnamed", "", LeadersKilled)) %>%
  transform(LeadersKilled = gsub(", Nasr Ibn Ali al-Ansi \\(unconfirmed\\)", "", LeadersKilled)) %>%
  transform(LeadersKilled = gsub(", alias ", "/", LeadersKilled)) %>%
  mutate(NumLeadersKilled = str_count(LeadersKilled, ",") + 1)

naf_strikesY <- left_join(naf_strikesY, naf_leadersY, by = "Date")
naf_strikesY$NumLeadersKilled[is.na(naf_strikesY$NumLeadersKilled)] <- 0

write_csv(naf_strikesY, "Strikes Data/Tidy data/NAFYemenTidy.csv")

#TBIJ

bij_strikesY <- read.csv("Strikes Data/TBIJYemenStrikes.csv", na.strings = c("")) %>%
  filter(!is.na(Date)) %>%
  select(-Strike.ID, -Strike.link, -X, -Index) %>%
  transform(Date = as.Date(Date, "%d/%m/%Y")) %>%
  arrange(Date) %>%
  rename(AttackType = Type.of.attack) %>%
  rename(ConfirmedUSAttack = Confirmed..possible.US.attack.) %>%
  rename(DroneStrike = Drone.strike) %>%
  rename(StrikesMin = Minimum.number.of.strikes) %>%
  rename(StrikesMax = Maximum.number.of.strikes) %>%
  rename(TotalKilledMin = Minimum.people.killed) %>%
  rename(TotalKilledMax = Maximum.people.killed) %>%
  rename(CiviliansKilledMin = Minimum.civilians.reported.killed) %>%
  rename(CiviliansKilledMax = Maximum.civilians.reported.killed) %>%
  rename(ChildrenKilledMin = Minimum.children.reported.killed) %>%
  rename(ChildrenKilledMax = Maximum.children.reported.killed) %>%
  rename(TotalInjuredMin = Minimum.people.injured) %>%
  rename(TotalInjuredMax = Maximum.people.injured)
  
write_csv(bij_strikesY, "Strikes Data/Tidy data/TBIJYemenTidy.csv")

#Combined

naf_strikesY$SourceConfirmed <- "NAF"
bij_strikesY$SourceConfirmed <- "TBIJ (Confirmed, Possible)"

bij_confirmedY <- bij_strikesY %>%
  filter(ConfirmedUSAttack == "Confirmed") %>%
  mutate(SourceConfirmed = "TBIJ (Confirmed)")

strikesYcombined <- rbind(select(naf_strikesY, c(Date, Location, Province, TotalKilledMin, TotalKilledMax,
                                                 CiviliansKilledMin, CiviliansKilledMax, SourceConfirmed)),
                          select(bij_strikesY, c(Date, Location, Province, TotalKilledMin, TotalKilledMax,
                                                 CiviliansKilledMin, CiviliansKilledMax, SourceConfirmed)),
                          select(bij_confirmedY, c(Date, Location, Province, TotalKilledMin, TotalKilledMax,
                                                   CiviliansKilledMin, CiviliansKilledMax, SourceConfirmed)))

write_csv(strikesYcombined, "Strikes Data/Tidy data/CombinedYemenTidy.csv")

#############################
## Somalia                  #
#############################

#NAF

naf_strikesS <- read.csv("Strikes Data/NAFSomaliaStrikes.csv", 
                         col.names = c("Date", "Location", "TotalKilled", 
                                       "MilitantsKilled", "CiviliansKilled", "UnknownKilled", "Target"),
                         na.strings = c("")) %>%
  transform(Date = gsub(",", " ", Date)) %>% 
  transform(Date = as.Date(Date, "%a %b %d %Y")) %>%
  arrange(Date)

naf_strikesS$TotalKilled <- makerange(naf_strikesS$TotalKilled, "total")
naf_strikesS$MilitantsKilled <- makerange(naf_strikesS$MilitantsKilled, "militants")
naf_strikesS$MilitantsKilled <- gsub(" and", "", naf_strikesS$MilitantsKilled)
naf_strikesS$CiviliansKilled <- makerange(naf_strikesS$CiviliansKilled, "civilians")
naf_strikesS$UnknownKilled <- makerange(naf_strikesS$UnknownKilled, "unknown")

naf_strikesS <- naf_strikesS %>%
  separate(TotalKilled, into = c("TotalKilledMin", "TotalKilledMax"), sep = ",") %>%
  separate(MilitantsKilled, into = c("MilitantsKilledMin", "MilitantsKilledMax"), sep = ",") %>%
  separate(CiviliansKilled, into = c("CiviliansKilledMin", "CiviliansKilledMax"), sep = ",") %>%
  separate(UnknownKilled, into = c("UnknownKilledMin", "UnknownKilledMax"), sep = ",")

naf_leadersS <- read.csv("Strikes Data/NAFSomaliaStrikesLeaders.csv", 
                         col.names=c("Date", "LeadersKilled", "Description"),
                         na.strings = c("N/A")) %>%
  transform(Date = gsub(",", " ", Date)) %>% 
  transform(Date = as.Date(Date, "%a %b %d %Y")) %>%
  arrange(Date) %>%
  transform(LeadersKilled = gsub(", ", ",", LeadersKilled)) %>%
  transform(LeadersKilled = gsub(",and ",",", LeadersKilled)) %>%
  transform(LeadersKilled = gsub(" and ", ",", LeadersKilled)) %>%
  transform(LeadersKilled = gsub(" aka Abu Ubaidah\\(UNCONFIMED\\)", "", LeadersKilled)) %>%
  mutate(NumLeadersKilled = str_count(LeadersKilled, ",") + 1)

naf_strikesS <- left_join(naf_strikesS, naf_leadersS, by = "Date")
naf_strikesS$NumLeadersKilled[is.na(naf_strikesS$NumLeadersKilled)] <- 0

write_csv(naf_strikesS, "Strikes Data/Tidy data/NAFSomaliaTidy.csv")

#TBIJ

bij_strikesS <- read.csv("Strikes Data/TBIJSomaliaStrikes.csv", na.strings = c("")) %>%
  filter(!is.na(Date)) %>%
  select(-Strike.ID, -Strike.link, -X, -Index) %>%
  transform(Date = as.Date(Date, "%m/%d/%Y")) %>%
  arrange(Date) %>%
  rename(AttackType = Strike.type) %>%
  rename(ConfirmedUSAttack = Confirmed..possible.US.strike) %>%
  rename(DroneStrike = Drone.strike) %>%
  rename(StrikesMin = Minimum.strikes) %>%
  rename(StrikesMax = Maximum.strikes) %>%
  rename(TotalKilledMin = Minimum.people.killed) %>%
  rename(TotalKilledMax = Maximum.people.killed) %>%
  rename(CiviliansKilledMin = Minimum.civilians.killed) %>%
  rename(CiviliansKilledMax = Maximum.civilians.killed) %>%
  rename(ChildrenKilledMin = Minimum.children.killed) %>%
  rename(ChildrenKilledMax = Maximum.children.killed) %>%
  rename(TotalInjuredMin = Minimum.people.injured) %>%
  rename(TotalInjuredMax = Maximum.people.injured)

write_csv(bij_strikesS, "Strikes Data/Tidy data/TBIJSomaliaTidy.csv")

#Combined

naf_strikesS$SourceConfirmed <- "NAF"
bij_strikesS$SourceConfirmed <- "TBIJ (Confirmed, Possible)"

bij_confirmedS <- bij_strikesS %>%
  filter(ConfirmedUSAttack == "Confirmed") %>%
  mutate(SourceConfirmed = "TBIJ (Confirmed)")

strikesScombined <- rbind(select(naf_strikesS, c(Date, Location, TotalKilledMin, TotalKilledMax,
                                                 CiviliansKilledMin, CiviliansKilledMax, SourceConfirmed)),
                          select(bij_strikesS, c(Date, Location, TotalKilledMin, TotalKilledMax,
                                                 CiviliansKilledMin, CiviliansKilledMax, SourceConfirmed)),
                          select(bij_confirmedS, c(Date, Location, TotalKilledMin, TotalKilledMax,
                                                   CiviliansKilledMin, CiviliansKilledMax, SourceConfirmed)))

write_csv(strikesScombined, "Strikes Data/Tidy data/CombinedSomaliaTidy.csv")

#############################
## Afghanistan              #
#############################

#TBIJ
bij_strikesA <- read.csv("Strikes Data/TBIJAfghanistanStrikes.csv", na.strings = c("-", "")) %>%
  filter(!is.na(Date)) %>%
  select(-ï..Strike, -Strike.link, -Timeline.URL, -Index) %>%
  transform(Date = as.Date(Date, "%d/%m/%Y")) %>%
  arrange(Date) %>%
  rename(AttackType = Type.of.attack) %>%
  rename(ConfirmedUSAttack = US.confirmed.) %>%
  rename(DroneStrike = Reportedly.drone.) %>%
  rename(StrikesMin = Minimum.strikes) %>%
  rename(StrikesMax = Maximum.strikes) %>%
  rename(TotalKilledMin = Minimum.total.people.killed) %>%
  rename(TotalKilledMax = Maximum.total.people.killed) %>%
  rename(CiviliansKilledMin = Minimum.civilians.reported.killed) %>%
  rename(CiviliansKilledMax = Maximum.civilians.reported.killed) %>%
  rename(ChildrenKilledMin = Minimum.children.reported.killed) %>%
  rename(ChildrenKilledMax = Maximum.children.reported.killed) %>%
  rename(TotalInjuredMin = Minimum.reported.injured) %>%
  rename(TotalInjuredMax = Maximum.reported.injured)

write_csv(bij_strikesA, "Strikes Data/Tidy data/TBIJAfghanistanTidy.csv")

#############################
## Pakistan                 #
#############################

#TBIJ
bij_strikesP <- read.csv("Strikes Data/TBIJPakistanStrikes.csv", na.strings = c("")) %>%
  filter(!is.na(Date)) %>%
  select(-Strike.ID, -Strike.link, -X, -Index) %>%
  transform(Date = as.Date(Date, "%d/%m/%Y")) %>%
  arrange(Date) %>%
  rename(TotalKilledMin = Minimum.total.people.killed) %>%
  rename(TotalKilledMax = Maximum.total.people.killed) %>%
  rename(CiviliansKilledMin = Minimum.civilians.reported.killed) %>%
  rename(CiviliansKilledMax = Maximum.civilians.reported.killed) %>%
  rename(ChildrenKilledMin = Minimum.children.reported.killed) %>%
  rename(ChildrenKilledMax = Maximum.children.reported.killed) %>%
  rename(TotalInjuredMin = Minimum.reported.injured) %>%
  rename(TotalInjuredMax = Maximum.reported.injured)

write_csv(bij_strikesP, "Strikes Data/Tidy data/TBIJPakistanTidy.csv")

#############################
## TBIJ Combine             #
#############################



