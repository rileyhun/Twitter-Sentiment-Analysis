library(ggvis)
library(shiny)

langs <- list("All"="all", "English"="en", "French"="fr" , "German"="ge" , "Italian"="it", "Spanish"="sp")

fluidPage(theme="bootstrap.min.css",
       
          titlePanel("Twitter Sentiment Analysis"),
          
          sidebarLayout(
            
            sidebarPanel(
              textInput("twitter_data", "Search On Twitter", value="#canucks"),
              sliderInput("number_tweets", "Select Number of Tweets", 0, 5000, 500, 500),
              actionButton("plot_data", "Extract and Analyse", icon = icon("twitter")),
              hr(),
              selectInput("lang", "Language:", langs)
            ),
            
            mainPanel(
              
              tabsetPanel(
                tabPanel("Sentiment Analysis", verbatimTextOutput("twitter_text"),
              
                  h4("Trends"),
                  plotOutput("trends"),
                  plotOutput("trends2")),
                
                tabPanel("Tweets", 
                         h4("Tweets"),
                         dataTableOutput('table')),
                
                tabPanel("Word Cloud",
                           h4("Word Cloud"),
                           plotOutput("wordcloudplot"))
            
              
              
              )
            )
          )
)