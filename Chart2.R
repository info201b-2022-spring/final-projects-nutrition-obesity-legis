library(tidyverse)
library(dplyr)

dataset <- read.csv("CDC_Nutrition.csv")

US_counts_by_year <- dataset %>%
  filter(LocationAbbr == "US") %>%
  count(YearStart, LocationAbbr)


scatterplot <- ggplot(US_counts_by_year, aes(x=YearStart, y=n)) +
  geom_point(aes(x=YearStart, y=n)) +
  geom_line(aes(x=YearStart, y=n)) +
  labs(title="Year vs Total Surveys",
       x="Year",
       y="Total Surveys")