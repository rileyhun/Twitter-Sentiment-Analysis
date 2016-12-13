library(twitteR)
library(plyr)
library(stringr)
library(ggvis)
library(ggplot2)
library(gridExtra)
library(wordcloud)
library(tm)

options(shiny.trace=TRUE)

n_summary <- 10

options(httr_oauth_cache = T)
consumer_key = "YazCs1RAZt1DTVmUjI62LHIza"
consumer_secret = "j4Qk4G0VSd08J982mC7IT90QdRI6l4jNWeZSVkFF122OWsiyLc"
access_token = "427040407-livDtlMiN8UoNIbLje2JgHkE3HYEv8nr3QLVwKKL"
access_secret = "uvPKSQrnm2neXnf85dNhgK7U4FALp3lI90YuiFKx5dnch"
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)


shinyServer(function(input, output, session) {
  
  CleanTweets <- function(tweets)
  {
    tweets <- gsub(tweets, " ", " ")
    tweets <- gsub(tweets, "http://t.co/[a-z,A-Z,0-9]*{8}","")
    tweets <- gsub(tweets, "RT @[a-z,A-Z]*: ","")
    tweets <- gsub(tweets, "#[a-z,A-Z]*", "")
    tweets <- gsub(tweets, "@[a-z, A-Z]*", "")
    return(tweets)
  }
  
  wordcloudentity <- function(entitycleantext) {
    tweetCorpus <- Corpus(VectorSource(entitycleantext))
    tweetTDM <- TermDocumentMatrix(tweetCorpus, control=list(removePunctuation=TRUE,
                                                             stopwords=c(stopwords('english')),
                                                             removeNumbers=TRUE, tolower=TRUE))
    tdMatrix <- as.matrix(tweetTDM)
    sortedMatrix <- sort(rowSums(tdMatrix), decreasing=TRUE)
    cloudFrame <- data.frame(word=names(sortedMatrix), freq=sortedMatrix)
    wcloudentity <- wordcloud(cloudFrame$word, cloudFrame$freq, max.words=100, colors=brewer.pal(8, "Dark2"),
                              scale=c(8,1), random.order=TRUE)
    print(wcloudentity)
  }
  
  trytolower = function(x){
    y = NA
    try_error = tryCatch(tolower(x), error = function(e) e)
    if (!inherits(try_error, "error"))
      y = tolower(x)
    return(y)
  }
  
  score.sentiment <- function(sentences, positive_words, negative_words, .progress='none')
  {
    scores = laply(sentences, function(sentence, positive_words, negative_words) {
      sentence = gsub('[[:punct:]]', '', sentence)
      sentence = gsub('[[:cntrl:]]', '', sentence)
      sentence = gsub('\\d+', '', sentence)
      sentence = tolower(sentence)
      
      listofwords = str_split(sentence, '\\s+')
      words = unlist(listofwords)
      
      positive_matches = !is.na(match(words, positive_words))
      negative_matches = !is.na(match(words, negative_words))
      score = sum(positive_matches) - sum(negative_matches)
      
      return(score)
      }, positive_words, negative_words, .progress=.progress)
    
    scores.df = data.frame(score=scores, text=sentences)
    return(scores.df)
  }
  
  removeHTMLtag <- function(htmlString) {
    return(gsub("<.*?>", "", htmlString))
  }
  
  get_source <- function(x){
    x <- removeHTMLtag(x[["statusSource"]])
    x
  }
  
  tweets_df <- reactive({
    input$plot_data
    
    isolate({
      withProgress({
        setProgress(message = "Processing tweets...")
        
        if(input$lang=="All")
          tweets <- searchTwitter(input$twitter_data, n=input$number_tweets)
        else
          tweets <- searchTwitter(input$twitter_data, n=input$number_tweets, lang=input$lang)
          tweets <- strip_retweets(tweets, strip_manual=TRUE, strip_mt=TRUE)
          df <- twListToDF(tweets)
         
          
          df$Date <- format(df$created, '%m/%d/%Y %H:%I:%S')
          df$Source <- apply(df, 1, get_source)
          
          sentences <- sapply(df$text, function(x) trytolower(x))
          scores <- score.sentiment(sentences, positive_words, negative_words)
          df <- cbind(df, scores)
          df <- subset(df, select=c("id", "text", "Source", "Date", "created", "score"))
          names(df) <- c("id", "Post", "Source", "Date", "created", "score")
          df$Post<-iconv(df$Post, 'UTF-8', 'ASCII')
          df
      })
    })
  })

  
  output$trends <- renderPlot({
    df <- tweets_df()
    plot1 <- ggplot(df, aes(x=created, y=score)) + geom_point(shape=1, size=0)+geom_smooth(se=F)+labs(title=input$twitter_data, 
    x = "Date / Time", y = "Sentiment Score") + ylim(-5, 5)
    print(plot1)
  }) 
  
  output$trends2 <- renderPlot({
    df <- tweets_df()
    df$category <- ifelse(df$score>0, "positive", ifelse(df$score == 0, "neutral", "negative"))
    plot2 <- ggplot(df, aes(factor(category), fill=category)) + geom_bar() +labs(x = "Sentiment classification", y = "Count")
    print(plot2)
#     plot3 <- qplot(y=df$score, df$source, geom = "boxplot") + geom_jitter() + labs(x = "", y = "Sentiment score")
#     print(grid.arrange(plot2, plot3, ncol=2))
    
    
  })
  
  output$table <- renderDataTable({tweets_df()}, options = list(lengthMenu = c(1, 5, 50), 
                                                                pageLength = 5))
  
  output$wordcloudplot <- renderPlot({
    wordcloudentity(tweets_df()$Post)
  })

#   output$twitter_text <- renderPrint({
#      cat(input$twitter_data)
#    })
#    
#   output$view <- renderTable({
#      df <- tweets_df()
#      head(df, n = n_summary, addrownums=F)
#   })
#    
#   movie_tooltip <- function(x) {
#      if (is.null(x)) return(NULL)
#      if (is.null(x$id)) return(NULL)
#     
#      all_tweets <- isolate(tweets_df())
#      tweet <- all_tweets[all_tweets$id == x$id, ]
#      paste0("<b>", tweet$Post, "</b><br><em><small>from (", tweet$Date, ")</small></em>")
#    }
#    
#   vis <- reactive({
#      legend_val <- c(input$twitter_data)
#      df <- tweets_df()
#      df %>% ggvis(~created, ~score) %>% add_tooltip(movie_tooltip, "click")
#    })
#    
#   vis %>% bind_shiny("plot1")
})