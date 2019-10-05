library(ggplot2)
library(dplyr)
library(shiny)
library(gridExtra)

df <- read.csv('https://raw.githubusercontent.com/chesterpoon8/CUNY_DATA_608/master/module3/data/cleaned-cdc-mortality-1999-2010-2.csv')

ui <- navbarPage(
  "US Mortality Rates",
  tabPanel(
    "State Level Comparison",
    sidebarLayout(
      sidebarPanel(
        selectInput("year",
                    "Year",
                    unique(df$Year),
                    selected = 2010),
        selectInput('COD', 
                    'Cause of Death', 
                    unique(df$ICD.Chapter), 
                    selected='Neoplasms')
      ),
      mainPanel(
        plotOutput("plot1",width = "100%",height = "600px")
      )
    )
  ),
  tabPanel(
    "Annual Trend",
    sidebarLayout(
      sidebarPanel(
        selectInput('COD', 
                    'Cause of Death', 
                    unique(df$ICD.Chapter), 
                    selected='Neoplasms'),
        selectInput('state', 
                    'State', 
                    unique(df$State), 
                    selected='NY')
      ),
      
      mainPanel(
        plotOutput("plot2")
    )
  )
)
)
   

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$plot1 <- renderPlot({
    
    dfSlice <- df %>%
      filter(Year == input$year, ICD.Chapter == input$COD)
    
    ggplot(dfSlice, aes(x = reorder(State,Crude.Rate),
                        y = Crude.Rate,
                        fill=Crude.Rate)) +
      geom_bar(stat="identity") +
      coord_flip() +
      xlab("State") + ylab("Crude Mortality") +
      scale_fill_gradient(low = "yellow",high = "red") +
      labs(fill = "Crude Mortality Rate") +
      geom_text(aes(label=Crude.Rate),hjust=1.5) +
      ggtitle(paste("States by Crude Mortality: ", input$COD))
  })
  
  output$plot2 <- renderPlot({
    
    nat_df <- df %>%
      select(-Crude.Rate) %>%
      group_by(ICD.Chapter,Year) %>%
      summarise_at(vars(Deaths, Population), sum)
    nat_df$Crude.Rate <- nat_df$Deaths/nat_df$Population * 100000
    nat_df$State <- 'USA'
    nat_df <- nat_df %>% select(ICD.Chapter,State,Year,Deaths,Population,Crude.Rate)
    all_df <- bind_rows(df,nat_df)
    
    all_slice <- all_df %>%
      filter(ICD.Chapter == input$COD, State %in% c("USA",input$state)) 
    
    ggplot(all_slice, aes(x = Year, y = Crude.Rate, color = State)) +
      geom_line() +
      ggtitle(paste("National Crude Mortality Trend Compared to ", 
                    input$state, 
                    " - ",
                    input$COD))
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)



