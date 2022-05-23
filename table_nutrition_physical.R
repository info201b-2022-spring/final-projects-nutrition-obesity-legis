library(dplyr)
library(stringr)
library(ggplot2)

nutrition_act <- read.csv("Nutrition__Physical_Activity__and_Obesity_-_Behavioral_Risk_Factor_Surveillance_System.csv")

physical <- nutrition_act %>%
  group_by(Question) %>%
  summarize(average_percentage = round(mean(na.omit(Data_Value)), digits = 2))

colnames(physical) <- c("Question", "% of US citizens")
