library(dplyr)
library(stringr)
library(ggplot2)

nutrition_act <- read.csv("Nutrition__Physical_Activity__and_Obesity_-_Behavioral_Risk_Factor_Surveillance_System.csv")

select_nutrition = nutrition_act %>% select(YearStart, LocationAbbr, Topic, Question, Sample_Size, Race.Ethnicity)

physical <- group_by(select_nutrition, LocationAbbr)
summarize(physical, YearStart = max(YearStart), Sample_Size = max(Sample_Size), Topic, Question, RaceEthnicity = Race.Ethnicity)

colnames(physical) <- c("Year", "Location", "Topic", "Question", "Sample Size", "Race/identity" )

View(physical)
