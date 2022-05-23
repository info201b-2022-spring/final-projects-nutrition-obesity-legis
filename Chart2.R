library(tidyverse)
library(dplyr)

dataset <- read.csv("Nutrition__Physical_Activity__and_Obesity_-_Behavioral_Risk_Factor_Surveillance_System.csv")

US_counts_by_year <- dataset %>%
  filter(LocationAbbr == "US") %>%
  count(YearStart, LocationAbbr)


scatterplot <- ggplot(US_counts_by_year, aes(x=YearStart, y=n)) +
  geom_point(aes(x=YearStart, y=n)) +
  geom_line(aes(x=YearStart, y=n)) +
  labs(title="Year vs Total Surveys",
       x="Year",
       y="Total Surveys")