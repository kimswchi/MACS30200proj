library(knitr)
library(tidyverse)
library(rvest)
library(RJSONIO)
library(RCurl)
library(lubridate)
library(stringr)
library(scales)

makerange <- function(column, word) {
  new_col = gsub("Between ", "", column)
  new_col = gsub(paste0(" ", word, " killed."), "", new_col)
  new_col = gsub(" and ", ",", new_col)
  return(new_col)
}

naf_strikes <- read.csv("Strikes Data/NAFYemenStrikes.csv", 
                        col.names = c("Date", "Location", "TotalKilled", 
                                      "MilitantsKilled", "CiviliansKilled", "UnknownKilled", "Target"),
                        na.strings = c("")) %>%
  transform(Date = gsub(",", " ", Date)) %>% 
  transform(Date = as.Date(Date, "%a %b %d %Y")) %>%
  arrange(Date) %>%
  transform(Location = gsub(" in ", ", ", Location)) %>%
  transform(Location = gsub("in ", "", Location))

naf_strikes$TotalKilled <- makerange(naf_strikes$TotalKilled, "total")
naf_strikes$MilitantsKilled <- makerange(naf_strikes$MilitantsKilled, "militants")
naf_strikes$MilitantsKilled <- gsub(" and", "", naf_strikes$MilitantsKilled)
naf_strikes$CiviliansKilled <- makerange(naf_strikes$CiviliansKilled, "civilians")
naf_strikes$UnknownKilled <- makerange(naf_strikes$UnknownKilled, "unknown")

naf_strikes <- naf_strikes %>%
  separate(TotalKilled, c("TotalKilledMin", "TotalKilledMax", ",")) %>%
  separate(MilitantsKilled, c("MilitantsKilledMin", "MilitantsKilledMax", ",")) %>%
  separate(CiviliansKilled, c("CiviliansKilledMin", "CiviliansKilledMax", ",")) %>%
  separate(UnknownKilled, c("UnknownKilledMin", "UnknownKilledMax", ","))

naf_strikes <- cbind(naf_strikes[1:4], naf_strikes[6:7], naf_strikes[9:10], naf_strikes[12:13])

naf_leaders <- read.csv("Strikes Data/NAFYemenStrikesLeaders.csv", 
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

naf_strikes <- left_join(naf_strikes, naf_leaders, by = "Date")
naf_strikes$NumLeadersKilled[is.na(naf_strikes$NumLeadersKilled)] <- 0

naf_filt <- naf_strikes[-1,] %>%
  mutate(YearMon = paste0(substr(as.character(Date), 1, 8), "1")) %>%
  transform(YearMon = as.Date(YearMon, format = "%Y-%m-%d"))

bij_strikes <- read.csv("Strikes Data/BIJDroneStrikes.csv", na.strings = c("")) %>%
  filter(!is.na(Date) & Confirmed..possible.US.attack. == "Confirmed")

ggplot(data = naf_filt, mapping = aes(YearMon)) +
  geom_histogram(binwidth = 5) + 
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("year"))

ggplot(data = filter(naf_filt, !is.na(TotalKilledMin))) + 
  geom_bar(mapping = aes(x = YearMon, y = TotalKilledMin), stat = "identity") + 
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("year"))