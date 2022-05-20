library("dplyr")
my_dataframe <- read.csv("Nutrition__Physical_Activity__and_Obesity_-_Behavioral_Risk_Factor_Surveillance_System.csv")
summary_info <- list()

# Value 1: Number of studies
summary_info$num_studies <- nrow(my_dataframe)

# Value 2: Types of data collection methods
summary_info$datasource_types <- unique(my_dataframe$Datasource)

# Value 3: Study with the largest sample size (state)
my_dataframe$Sample_Size <- as.numeric(gsub(",", "", my_dataframe$Sample_Size))
states_only <- filter(my_dataframe, my_dataframe$LocationDesc != "National")

summary_info$max_state_sample_size <- states_only %>%
  filter(states_only$Sample_Size == max(states_only$Sample_Size, na.rm = TRUE)) %>%
  select(YearEnd, LocationDesc, Topic, Question, Sample_Size)

#  gsub help: https://www.codegrepper.com/code-examples/whatever/R+remove+commas

# Value 4: Most common physical health topic of study:
summary_info$most_common_physical_topic <- names(which.max(table(my_dataframe$Topic)))

# most common value help <- https://stackoverflow.com/questions/12187187/how-to-retrieve-the-most-repeated-value-in-a-column-present-in-a-data-frame

# Value 5: Most common stratification category (group the study is focusing on)
summary_info$most_common_strategy <- names(which.max(table(my_dataframe$StratificationCategory1)))
