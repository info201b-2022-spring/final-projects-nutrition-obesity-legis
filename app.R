library(shiny)
library(ggplot2)
library(usmap)
# Define UI ----

read.csv("Nutrition__Physical_Activity__and_Obesity_-_Behavioral_Risk_Factor_Surveillance_System.csv")


introduction <-
  tabPanel(
    "Introduction",
    fluidPage(
      
    )
  )

pageOne <-                   
  tabPanel(                 
    "Page one",               
    fluidPage(                
      
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
          plotOutput(outputId = "map", click = "state_brush")
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
  
  
  
  # START OF PAGE 3 (MAP)
  output$map <- renderPlot({
    only_obesity_perc <- data %>%
      filter(LocationDesc != "National") %>%
      filter(YearEnd == input$year) %>%
      filter(Question=="Percent of adults aged 18 years and older who have obesity") %>%
      select(LocationDesc,Data_Value) %>%
      rename(state = LocationDesc) %>%
      group_by(state) %>%
      summarize('mean_percentage' = mean(na.omit(Data_Value)))
    
    plot_usmap(data = only_obesity_perc, values = "mean_percentage") + 
      scale_fill_continuous(low = "white", high = "red", name = "% obesity") +
      ggtitle("Percentage of Adults 18+ Who Have Obesity Per State")
  })
  
  #output$states <- renderTable({
  #  nearPoints(only_obesity_perc, input$state_brush, xvar ="state", yvar = "mean_percentage")
  # })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)