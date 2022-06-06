library(shiny)
library(ggplot2)
library(usmap)
library(dplyr)
library(stringr)
# Define UI ----

data <- read.csv("Nutrition__Physical_Activity__and_Obesity_-_Behavioral_Risk_Factor_Surveillance_System.csv")


introduction <-
  tabPanel(
    "Introduction",
    fluidPage(
      titlePanel("Introduction"),
      "Throughout the COVID-19 pandemic, our local gyms, recreational centers, 
      and other indoor establishments where we engage in physical activity became
      a risk to our health. While there were many who adapted by doing their 
      exercise by themselves, there were also many of us who lost the motivation 
      and routine to get active, or just felt as if there was a lack of facilities
      to do so. While our project does not focus solely on physical health during
      the pandemic, its effects on how we stay healthy piqued our curiosity. Below
      are our questions that lead our project:",

      h4("What is the general state of American health related to physical activity/inactivity?"),
      h4("What are the methods of collecting data about health and physical activity?"),
      h4("Are there any regional trends for where physical activity, nutrition, and obesity are better or worse?"),

      "We will be utilizing a dataset from the CDC about Nutrition, Physical
      Activity, and Obesity through their Behavioral Risk Factor Surveillance
      System. The dataset addresses topics such as diets, physical activity,
      weight, and more, and can be found at this link:",
      
      h4(tags$a(href="https://chronicdata.cdc.gov/Nutrition-Physical-Activity-and-Obesity/Nutrition-Physical-Activity-and-Obesity-Behavioral/hn4x-zwk7",
                "Nutrition, Physical Activity, and Obesity - Behavioral Risk Factor Surveillance System")),
      
      "A Shiny App Project by Nghi Huynh, Tyler Tran, Luke VanHouten, and Ayman Yousuf"
      )
    )

pageOne <-                   
  tabPanel(                 
    "Health by Group",               
    fluidPage(
      titlePanel("Stratification"),
      h4("What is the general state of American health related to physical activity/inactivity?"),
      sidebarLayout(
        sidebarPanel(
          radioButtons(
            inputId = "category",
            label = "Choose specific group of stratification to focus on",
            choices = list("Race & Ethnicity" = "race", "Highest Level of Education" = "education",
                           "Income" = "income", "Age" = "age")
          )
        ),
        mainPanel(
          plotOutput(outputId ="categories")
        )
      )
    )
  ) 

pageTwo <- 
  tabPanel(
    "Data Collection", 
    fluidPage(
      titlePanel("Data Collection"),
      h4("What specific questions measure data about health and physical activity?"),
      plotOutput(outputId = "lollipop", click = "question_click"),
      p("Click on the dots to see the individual questions!"),
      tableOutput(outputId = "question")
    )
  )

pageThree <-
  tabPanel(
    "US Map Visual",
    fluidPage(
      titlePanel("Map of % of Adults 18+ Who Have Obesity"),
      h4("Are there any regional trends for where physical activity, nutrition, and obesity are better or worse?"),
      sidebarLayout(
        sidebarPanel(
          sliderInput(
            inputId = "year",
            label = "Year being displayed",
            sep = "",
            min = 2011,
            max = 2020,
            value = 2020
          )
        ),
        mainPanel(
          plotOutput(outputId = "map")
          #tableOutput(outputId = "states")
        )
      )
    )
  )

summary <-
  tabPanel(
    "Summary",
    fluidPage(
      titlePanel("Summary"),
      h3("Takeaway 1"),
      "These takeaways are about the demographics of the study, which can be found
      on the Health by Group page of the app. It appears as if obesity is rather 
      constant between races and ethnicities, with white and Hispanic people being
      slightly more obese than the rest of the population. Obesity is negatively
      correlated with education, and the trend has been moving upwards in recent
      years, meaning that those who are least educated are more likely to have
      problems with their weight. Surprisingly, the data showed that obesity
      increases with income. This may be due to greater access to food. And lastly,
      the data shows that obesity increases with age. This makes sense, as most
      people are born at a healthy weight.",
      h3("Takeaway 2"),
      "These takeaways are about the questions used for data collection for our dataset,
      utilizing the Behavioral Risk Factor Surveillance System. The chart for this 
      can be found on the Data Collection page of the app. The first 
      takeaway is that health issues have a pretty clear correlatoin to nutrition.
      41% of adults eat fruit less than daily, and 22% of adults eat vegetables less
      than daily. That means a lot of adults are experiencing imbalanced diets and not
      getting enough of the 5 food groups. Going off of this, another takeaway is that
      physical activity is not the sole contributor. While 28% of adults don't engage in
      leisure-time physical activity, that is less than the % of adults that qualify as obese
      or overweight. The questions help us understand more specifics about the health issues
      in America since we can see trends on nutrition and physical activity.",
      h3("Takeaway 3"),
      "These takeaways are about the regions with the most obesity throughout the
      2010s, and can be found on the US Map Visual page of the app. The map chart
      shows that obesity has been consistently increasing from the low 30%s to
      around 40% over the course of the decade, with the largest increases seen
      in rural states such as those in the south. In the middle of the decade,
      the largest gaps could be seen between the states with the highest and lowest
      obesity rates, reaching almost 15% increases between states like California
      and Alabama. The only state with missing data is Colorado."
    )
  )

ui <- (                         
  fluidPage(                    
    navbarPage (                
      
      "Health and Nutrition",           
      introduction,
      pageOne,               
      pageTwo,
      pageThree,
      summary
      
    )
  )
)



# Define server logic ----
server <- function(input, output) {
  
  # START OF PAGE 1 (CATEGORY BARS)
  output$categories <- renderPlot({
    if(input$category == "race") { # RACE RADIO BUTTON OPTION
      races_overweight <- data %>% 
        filter(Question == "Percent of adults aged 18 years and older who have an overweight classification", Race.Ethnicity != "") %>%
        select(Data_Value, Race.Ethnicity) %>%
        na.omit(Race.Ethnicity) %>%
        group_by(Race.Ethnicity) %>%
        summarize(avg_value = mean(Data_Value))
      
      # http://www.cookbook-r.com/Graphs/Legends_(ggplot2)/ used for help with some of the formatting
      # https://stackoverflow.com/questions/35090883/remove-all-of-x-axis-labels-in-ggplot used to figure out how to remove x-axis labels
      ggplot(races_overweight, aes(x=Race.Ethnicity, y=avg_value, fill = Race.Ethnicity)) + geom_bar(stat="identity") +
        scale_x_discrete(guide = guide_axis(n.dodge=3))  +
        xlab("Race & Ethnicity") + 
        ylab("Average % Overweight Classification") +
        ggtitle("Average % Overweight Classification by Race & Ethnicity") +
        guides(fill=guide_legend(title="Race & Ethnicity")) +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              plot.title = element_text(size = 14, face = "bold"))
    } else if(input$category == "education") { # EDUCATION RADIO BUTTON OPTION
      US_year_question <- data %>%
        filter(Education != "",LocationAbbr == "US") %>%
        select(ï..YearStart, Education, Data_Value) %>%
        na.omit() %>%
        group_by(Education, ï..YearStart) %>%
        summarise(avg_value = mean(Data_Value))
      
      ggplot(US_year_question, aes(ï..YearStart, avg_value)) +
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
    } else if (input$category == "income") {
      income <- data %>% 
        filter(Question == "Percent of adults aged 18 years and older who have an overweight classification", Income != "", ï..YearStart == 2020) %>%
        select(Data_Value, Income) %>%
        na.omit(Income) %>%
        group_by(Income) %>%
        summarize(avg_value = mean(Data_Value))
      
      # http://www.cookbook-r.com/Graphs/Legends_(ggplot2)/ used for help with some of the formatting
      # https://stackoverflow.com/questions/35090883/remove-all-of-x-axis-labels-in-ggplot used to figure out how to remove x-axis labels
      ggplot(income, aes(x=Income, y=avg_value, fill = Income)) + geom_bar(stat="identity") +
        scale_x_discrete(guide = guide_axis(n.dodge=3))  +
        xlab("Income") + 
        ylab("Average % Overweight Classification") +
        ggtitle("Average % Overweight Classification by Income") +
        guides(fill=guide_legend(title="Income")) +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              plot.title = element_text(size = 14, face = "bold"))
    } else {
      age <- data %>% 
        filter(Question == "Percent of adults aged 18 years and older who have an overweight classification", Age.years. != "", ï..YearStart == 2020) %>%
        select(Data_Value, Age.years.) %>%
        na.omit(Age.years.) %>%
        group_by(Age.years.) %>%
        summarize(avg_value = mean(Data_Value))
      
      # http://www.cookbook-r.com/Graphs/Legends_(ggplot2)/ used for help with some of the formatting
      # https://stackoverflow.com/questions/35090883/remove-all-of-x-axis-labels-in-ggplot used to figure out how to remove x-axis labels
      ggplot(age, aes(x=Age.years., y=avg_value, fill = Age.years.)) + geom_bar(stat="identity") +
        scale_x_discrete(guide = guide_axis(n.dodge=3))  +
        xlab("Age") + 
        ylab("Average % Overweight Classification") +
        ggtitle("Average % Overweight Classification by Age") +
        guides(fill=guide_legend(title="Age")) +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              plot.title = element_text(size = 14, face = "bold"))
    }
  })
  
  
  # START OF PAGE 2 (LOLLIPOP)
  question_df <- data %>%
    group_by(Question) %>%
    filter(ï..YearStart == 2019) %>%
    summarize(Average.Percentage = round(mean(na.omit(Data_Value)), digits = 2))
  
  output$lollipop <- renderPlot({
    ggplot(question_df, aes(x=Question, y=Average.Percentage)) +
      geom_segment( aes(x=Question, xend=Question, y=0, yend=Average.Percentage), color="grey") +
      geom_point( color="orange", size=4) +
      theme_light() +
      theme(
        panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
        axis.ticks.x = element_blank()
      ) +
      ylab("% of people that classify") +
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank()) +
      labs(title = str_wrap("2019 Behavioral Risk Factor Surveillance System Data", 60), subtitle = "Most Recent Year With Data For All Questions") +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5))
  })
  
  output$question <- renderTable({
    nearPoints(question_df, input$question_click, xvar = "Question", yvar = "Average.Percentage")
  })
  
  # START OF PAGE 3 (MAP)
  output$map <- renderPlot({
    only_obesity_perc <- data %>%
      filter(LocationDesc != "National", YearEnd == input$year) %>%
      filter(Question=="Percent of adults aged 18 years and older who have obesity") %>%
      select(LocationDesc,Data_Value) %>%
      rename(state = LocationDesc) %>%
      group_by(state) %>%
      summarize('mean_percentage' = mean(na.omit(Data_Value)))
    
    plot_usmap(data = only_obesity_perc, values = "mean_percentage") + 
      scale_fill_continuous(low = "white", high = "red", name = "% obesity") +
      ggtitle(input$year) +
      theme(plot.title = element_text(size = 25, hjust = 0.5, vjust = -5))
      
  })
  
  #output$states <- renderTable({
  #  nearPoints(only_obesity_perc, input$state_brush, xvar ="state", yvar = "mean_percentage")
  # })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)