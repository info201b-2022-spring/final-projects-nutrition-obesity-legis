library(tidyverse)
library(dplyr)

dataset <- read.csv("Nutrition__Physical_Activity__and_Obesity_-_Behavioral_Risk_Factor_Surveillance_System.csv")

US_year_question <- dataset %>%
  filter(Education != "",LocationAbbr == "US") %>%
  select(ï..YearStart, Education, Data_Value) %>%
  na.omit() %>%
  group_by(Education, ï..YearStart) %>%
  summarise(avg_value = mean(Data_Value))

linechart <- ggplot(US_year_question, aes(ï..YearStart, avg_value)) +
  geom_line(aes(color = Education)) +
  labs(title = "Data Values by Education",
       subtitle = "2011-2020") +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    text = element_text(size = 11)) +
  xlab("Year") +
  ylab("Average % Overweight Classification by Education") +
  scale_x_continuous(breaks = seq(2011, 2020)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))