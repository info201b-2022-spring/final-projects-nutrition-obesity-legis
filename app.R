library(shiny)
library(ggplot2)
library(usmap)
library(dplyr)
# Define UI ----

data <- read.csv("Nutrition__Physical_Activity__and_Obesity_-_Behavioral_Risk_Factor_Surveillance_System.csv")


introduction <-
  tabPanel(
    "Introduction",
    fluidPage(
      
    )
  )

pageOne <-                   
  tabPanel(                 
    "Health by Group",               
    fluidPage(
      titlePanel("Stratification"),
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
    "Page two", 
    fluidPage(
      
    )
  )

pageThree <-
  tabPanel(
    "US Map Visual",
    fluidPage(
      titlePanel("Map of % of Adults 18+ Who Have Obesity"),
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
      US_year_question <- dataset %>%
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
      income <- dataset %>% 
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
      age <- dataset %>% 
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