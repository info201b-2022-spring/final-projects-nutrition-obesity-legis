library(tidyverse)
library(dplyr)

dataset <- read.csv("Nutrition__Physical_Activity__and_Obesity_-_Behavioral_Risk_Factor_Surveillance_System.csv")

US_counts_by_year <- dataset %>%
  filter(LocationAbbr == "US") %>%
  count(ï..YearStart, LocationAbbr)


scatterplot <- ggplot(US_counts_by_year, aes(x=ï..YearStart, y=n)) +
  geom_point(aes(x=ï..YearStart, y=n)) +
  geom_line(aes(x=ï..YearStart, y=n)) +
  scale_x_continuous(breaks = seq(2011, 2020)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title="Year vs Total Surveys",
       x="Year",
       y="Total Surveys")