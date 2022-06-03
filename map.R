# libraries
library(tidyverse)
library(usmap)
library(dplyr)

# ^Again, in your report you must describe why you included the chart 
# (e.g., what it attempts to seeks to express), and what information it reveals.
# Shows which states have what percentages of obesity in adults over 18

data <- read.csv("Nutrition__Physical_Activity__and_Obesity_-_Behavioral_Risk_Factor_Surveillance_System.csv")

# filtering to get the rows of interest
only_obesity_perc <- data %>%
  filter(data$LocationDesc != "National", data$ï..YearStart == "2020") %>%
  filter(Question=="Percent of adults aged 18 years and older who have obesity") %>%
  select(LocationDesc,Data_Value) %>%
  rename(state = LocationDesc) %>%
  group_by(state) %>%
  summarize('mean_percentage' = mean(na.omit(Data_Value)))

map <- plot_usmap(data = only_obesity_perc, values = "mean_percentage") + 
  scale_fill_continuous(low = "white", high = "red", name = "% obesity") +
  ggtitle("Percentage of Adults 18+ Who Have Obesity Per State")

