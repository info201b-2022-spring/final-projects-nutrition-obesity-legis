library(tidyverse)
library(dplyr)

dataset <- read.csv("Nutrition__Physical_Activity__and_Obesity_-_Behavioral_Risk_Factor_Surveillance_System.csv")

races_overweight <- dataset %>% 
  filter(Question == "Percent of adults aged 18 years and older who have an overweight classification", Race.Ethnicity != "", ï..YearStart == 2020) %>%
  select(Data_Value, Race.Ethnicity) %>%
  na.omit(Race.Ethnicity) %>%
  group_by(Race.Ethnicity) %>%
  summarize(avg_value = mean(Data_Value))

# http://www.cookbook-r.com/Graphs/Legends_(ggplot2)/ used for help with some of the formatting
# https://stackoverflow.com/questions/35090883/remove-all-of-x-axis-labels-in-ggplot used to figure out how to remove x-axis labels
race_plot <- ggplot(races_overweight, aes(x=Race.Ethnicity, y=avg_value, fill = Race.Ethnicity)) + geom_bar(stat="identity") +
  scale_x_discrete(guide = guide_axis(n.dodge=3))  +
  xlab("Race & Ethnicity") + 
  ylab("Average % Overweight Classification") +
  ggtitle("Average % Overweight Classification by Race & Ethnicity") +
  guides(fill=guide_legend(title="Race & Ethnicity")) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

age <- dataset %>% 
  filter(Question == "Percent of adults aged 18 years and older who have an overweight classification", Age.years. != "", ï..YearStart == 2020) %>%
  select(Data_Value, Age.years.) %>%
  na.omit(Age.years.) %>%
  group_by(Age.years.) %>%
  summarize(avg_value = mean(Data_Value))

# http://www.cookbook-r.com/Graphs/Legends_(ggplot2)/ used for help with some of the formatting
# https://stackoverflow.com/questions/35090883/remove-all-of-x-axis-labels-in-ggplot used to figure out how to remove x-axis labels
age_plot <- ggplot(age, aes(x=Age.years., y=avg_value, fill = Age.years.)) + geom_bar(stat="identity") +
  scale_x_discrete(guide = guide_axis(n.dodge=3))  +
  xlab("Age") + 
  ylab("Average % Overweight Classification") +
  ggtitle("Average % Overweight Classification by Income") +
  guides(fill=guide_legend(title="Age")) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())