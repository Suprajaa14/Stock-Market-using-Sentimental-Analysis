library(RCurl)
library(lubridate)
library(ggplot2)
library(shiny)
library(reshape)
library(forecast)
library(devtools)
library(twitteR)
library(stringr)
library(plyr)
library(sos)

shinyServer(function(input, output) {
  
  output$plot <- renderPlot({
    fdata1 <- getSymbols(input$home_input, src = "yahoo", 
                         from = input$dates[1],
                         to = input$dates[2],
                         auto.assign = FALSE)
    
    chartSeries(fdata1, theme = chartTheme("black"), 
                type = "line", TA = NULL)
  })
  
  output$p1 <- renderPlot({
    
    pp <<- input$parp
    pd <<- input$pard
    pq <<- input$parq
    
    
    
    findata1<-getSymbols(input$sym1, src = "yahoo", 
                         from = input$dates[1],
                         to = input$dates[2],
                         auto.assign = FALSE)
    
    
    
    findata1 <<- findata1[,c(1,5,6)]
  #})
    
    
    output$p3 <- renderPlot({
      fit1 <<- stats::arima(findata1[,3], order = c(pp, pd, pq))
      
      
      plot(forecast(fit1, h = 40), col="violet")
      title(col.main="red", 
            xlab="Time lag", ylab="Stock Price",
            col.lab="Black", cex.lab=1.50)
      
    })
    
    output$t2 <- renderTable({
      accuracy(fit1)
    })
    
    
  
  
  output$p2 <- renderPlot({
    
    
    plot(findata1[,1],findata1[,3])
    
    lines(findata1[,1],findata1[,2], col = "Green")
    title(main="Share Volume Vs Date")
    par(new = T)
    
  })
})
  
  output$t1 <- renderTable({
    
    v1 <- input$sym1
    v2 <- mean(findata1[,3])
    v3 <- sd(findata1[,3])
    v4 <- mean(findata1[,2])
    v5 <-sd(findata1[,2])
    v <- data.frame(v1,v2,v3,v4,v5)
    names(v) <- c("Name","Avg.Price","Std.Deviation.Price","Avg.Volume","Std.Deviation.Volume")
    v
  })
  output$senti<-renderPlot({
    #Authenticate with twitter
    api_key <- "At56VQuB5BWzYSCPU3o26LRhI"
    api_secret <-"b10n2KUHOjeqC7SedE0WoDAR6qfryGAQ8ueEFltKzhTMJwshpY"
    access_token <-"951742361175867392-BCpCtwCSn23K9b6QF094Yy86rQiJxTm"
    access_token_secret <-"s6XAsEyr53FyYZl2FIX4JNg8B4tLqOD5ckJVrlL2e3znC"
    setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
    
    setwd("C:/Users/Supraja/Desktop/words")
    neg <- scan("negative-words.txt", what="character", comment.char=";")
    pos <- scan("positive-words.txt", what="character", comment.char=";")
    score.sentiment <- function(tweets, pos.words, neg.words)
      
    {
      
      
      scores <- laply(tweets, function(tweet, pos.words, neg.words) {
        
        
        
        tweet <- gsub('https://','',tweet) # removes https://
        tweet <- gsub('http://','',tweet) # removes http://
        tweet<-gsub('[^[:graph:]]', ' ',tweet) ## removes graphic characters 
        #like emoticons 
        tweet <- gsub('[[:punct:]]', '', tweet) # removes punctuation 
        tweet <- gsub('[[:cntrl:]]', '', tweet) # removes control characters
        tweet <- gsub('\\d+', '', tweet) # removes numbers
        tweet=str_replace_all(tweet,"[^[:graph:]]", " ") 
        
        # makes all letters lowercase
        tweet <- tolower(tweet) 
        # splits the tweets by word in a list
        word.list <- str_split(tweet, '\\s+') 
        # turns the list into vector
        words <- unlist(word.list) 
        
        ## returns matching 
        pos.matches <- match(words, pos.words) 
        #values for words from list 
        neg.matches <- match(words, neg.words)
        
        ## converts matching values to true of false
        pos.matches <- !is.na(pos.matches)
        neg.matches <- !is.na(neg.matches)
        
        # true and false are treated as 1 and 0 so they can be added
        score <- sum(pos.matches) - sum(neg.matches) 
        
        return(score)
        
      }, pos.words, neg.words )
      
      scores.df <- data.frame(score=scores, text=tweets)
      
      return(scores.df)
      
    }
    tweets <- searchTwitter(input$sym1,n=500)
    
    # gets text from Tweets
    Tweets.text <- laply(tweets,function(t)t$getText()) 
    
    # calls sentiment analysis function
    analysis<-score.sentiment(Tweets.text, pos, neg)
    
    hist(analysis$score,main=input$sym1,col = "pink", border = "black")
  })
})
