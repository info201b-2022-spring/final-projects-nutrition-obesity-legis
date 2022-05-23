library(tidyverse)
library(dplyr)

dataset <- read.csv("CDC_Nutrition.csv")

races_overweight <- dataset %>% 
  filter(Question == "Percent of adults aged 18 years and older who have an overweight classification", Race.Ethnicity != "") %>%
  select(Data_Value, Race.Ethnicity) %>%
  na.omit(Race.Ethnicity)

barplot <- ggplot(races_overweight, aes(x=Race.Ethnicity, y=Data_Value)) + geom_bar(stat="identity")