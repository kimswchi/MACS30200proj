library(knitr)
library(tidyverse)
library(lubridate)
library(stringr)
library(scales)
library(Cairo)

suicide <- read.csv("Terrorism Data/cpost_summaryList (1).csv") %>%
  transform(attack_date = as.Date(attack_date, "%Y-%m-%d")) %>%
  mutate(YearMon = paste0(substr(as.character(attack_date), 1, 8), "1")) %>%
  transform(YearMon = as.Date(YearMon, format = "%Y-%m-%d"))

suicide_attacks <- ggplot(data = suicide, mapping = aes(x = YearMon)) +
  geom_freqpoly(mapping = aes(color = location_names), size = 1.2) + 
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("year")) +
  labs(title = "Suicide Terrorist Attacks, 2002 - 2016",
       caption = "Source: Suicide Attack Database",
       color = "",
       x = "Year",
       y = "Number of Attacks") + 
  theme_bw() + 
  theme(legend.position = "bottom")

Cairo(file="suicide_attacks.png", 
      type="png",
      bg = "transparent",
      units="px", 
      width=550, 
      height=300, 
      pointsize=12, 
      dpi="auto")
suicide_attacks
dev.off()