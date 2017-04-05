library(knitr)
library(tidyverse)
library(rvest)
library(RJSONIO)
library(RCurl)
library(lubridate)
library(stringr)
library(scales)
library(Cairo)

#yemen

yemen <- read.csv("Strikes Data/Tidy data/CombinedYemenTidy.csv") %>%
  mutate(YearMon = paste0(substr(as.character(Date), 1, 8), "1")) %>%
  transform(YearMon = as.Date(YearMon, format = "%Y-%m-%d"))

yemen_strikes <- ggplot(data = yemen, mapping = aes(x = YearMon)) +
  geom_freqpoly(mapping = aes(color = SourceConfirmed), size = 1.2) + 
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("year")) +
  labs(title = "US Covert Operations in Yemen, 2002 - present",
       caption = "Source: New America Foundation (NAF), The Bureau of Investigative Journalism (TBIJ)",
       color = "",
       x = "Year",
       y = "Number of Strikes") + 
  theme_bw() + 
  theme(legend.position = "bottom")

Cairo(file="yemen_strikes.png", 
      type="png",
      bg = "transparent",
      units="px", 
      width=550, 
      height=300, 
      pointsize=12, 
      dpi="auto")
yemen_strikes
dev.off()

#somalia

somalia <- read.csv("Strikes Data/Tidy data/CombinedSomaliaTidy.csv") %>%
  mutate(YearMon = paste0(substr(as.character(Date), 1, 8), "1")) %>%
  transform(YearMon = as.Date(YearMon, format = "%Y-%m-%d"))

somalia_strikes <- ggplot(data = somalia, mapping = aes(x = YearMon)) +
  geom_freqpoly(mapping = aes(color = SourceConfirmed), size = 1.2) + 
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("year")) +
  labs(title = "US Covert Operations in Somalia, 2003 - present",
       caption = "Source: New America Foundation (NAF), The Bureau of Investigative Journalism (TBIJ)",
       color = "",
       x = "Year",
       y = "Number of Strikes") + 
  theme_bw() + 
  theme(legend.position = "bottom")

Cairo(file="somalia_strikes.png", 
      type="png",
      bg = "transparent",
      units="px", 
      width=550, 
      height=300, 
      pointsize=12, 
      dpi="auto")
somalia_strikes
dev.off()

#all

bij_strikesA <- read.csv("Strikes Data/Tidy data/TBIJAfghanistanTidy.csv")
bij_strikesP <- read.csv("Strikes Data/Tidy data/TBIJPakistanTidy.csv")
bij_strikesS <- read.csv("Strikes Data/Tidy data/TBIJSomaliaTidy.csv")
bij_strikesY <- read.csv("Strikes Data/Tidy data/TBIJYemenTidy.csv")

bij_confirmedA <- filter(bij_strikesA, ConfirmedUSAttack == 1) %>%
  mutate(Country = "Afghanistan")
bij_confirmedY <- filter(bij_strikesY, ConfirmedUSAttack == "Confirmed") %>%
  mutate(Country = "Yemen")
bij_confirmedS <- filter(bij_strikesS, ConfirmedUSAttack == "Confirmed") %>%
  mutate(Country = "Somalia")
bij_strikesP <- bij_strikesP %>%
  mutate(Country = "Pakistan")

bij_strikesCombined <- rbind(select(bij_confirmedA , c(Date, Country)),
                             select(bij_strikesP, c(Date, Country)),
                             select(bij_confirmedS, c(Date, Country)),
                             select(bij_confirmedY, c(Date, Country)))


ggplot(data = somalia, mapping = aes(x = YearMon)) +
  geom_freqpoly(mapping = aes(color = SourceConfirmed), size = 1.2) + 
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("year")) +
  labs(title = "US Covert Operations in Somalia, 2003 - present",
       caption = "Source: New America Foundation (NAF), The Bureau of Investigative Journalism (TBIJ)",
       color = "",
       x = "Year",
       y = "Number of Strikes") + 
  theme_bw() + 
  theme(legend.position = "bottom")