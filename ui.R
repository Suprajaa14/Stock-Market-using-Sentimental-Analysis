library(RCurl)

library(shiny)

library(lubridate)

library(quantmod)

library(ggplot2)

library(reshape)



shinyUI(fluidPage(
  
  
  
  titlePanel("Stock Market Deviation Using ARIMA Algorithm"),
  
  tabsetPanel(              
    
    tabPanel(title = "Home",
             
             tags$h1("Start Prediction and Analysis!"),
             
             
             selectInput(inputId = "home_input",label="Companies",c("Apple Inc"="AAPL","Google Inc"="GOOGL",
                                                                    "Microsoft Corporation"="MSFT",
                                                                    "Amazon Inc"="AMZN",
                                                                    "Facebook"="FB","Vodafone Group"="VOD",
                                                                    "Cisco Systems"="CSCO","SnapChat"="SNAP")),
             
             
             
             
             
             dateRangeInput("dates", 
                            
                            "Select the Date Range",
                            
                            start = "2010-01-01", 
                            
                            end = as.character(Sys.Date())),
             
             plotOutput("plot")
             
             
             
             
             
    ),
    
    tabPanel(title="Risk predictor",tags$h1("Let's start predicting"),
             
             sidebarLayout(
               
               sidebarPanel(
                 
                 selectInput(inputId = "sym1",label="Companies",c("Apple Inc"="AAPL",
                                                                  "Google Inc"="GOOGL",
                                                                  "Microsoft Corporation"="MSFT",
                                                                  "Amazon Inc"="AMZN","Facebook"="FB",
                                                                  "Vodafone Group"="VOD",
                                                                  "Cisco Systems"="CSCO","SnapChat"="SNAP")),
                 
                 
                 dateRangeInput("dates", label = h6("Select the Date range"),start = "2010-01-01", 
                                
                                end = NULL),
                 
                 sliderInput("parp", label = h6("Autoregression Parameter"),
                             
                             min = 0, max = 10, value = 2,dragRange = FALSE),
                 
                 sliderInput("pard", label = h6("Integration Parameter"),
                             
                             min = 0, max = 10, value = 2),
                 
                 sliderInput("parq", label = h6("Moving Average Parameter"),
                             
                             min = 0, max = 10, value = 2)
                 
               ),
               
               
               
               
               
               # Show a plot of the generated distribution
               
               mainPanel(
                 
               
                 
                plotOutput("senti"),
                 
               
                 
                br(""),
                 plotOutput("p3"),
                 
                 br(""),
                 
                 
                 tableOutput("t1"),
                 
                 br(""),
                 tableOutput("t2"),
                 
                plotOutput("p1")
                 
                 
               )
               
             )
    ))
))