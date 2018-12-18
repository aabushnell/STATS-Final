# Run this code to install packages if not alredy installed
# install.packages("httr")
# install.packages("rtweet")
# install.packages('maps')

# Load required libraries
library(shiny)
library(httr)
library(rtweet)
library(readr)
library(mdsr)
library(tidyr)
library(maps)

# Seperate file defining values for all required API keys and tokens
source('config.R')

# Words file to be used for matches
words_file <- read_csv('words.csv')
# Converts the words columns into a list for later reference
words <- words_file$Words

# Loads pregenerated data of tweets for each state
state_data <- read_csv('state_data.csv')

# Loads pregenerated data of tweets for each hour (6AM-7PM)
time_data_read <- read_csv('time_data.csv')

# Defines a function that sends the GET request to the nounproject api
get_nouns_api <- function(endpoint, baseurl = "http://api.thenounproject.com/", app) {
  url <- modify_url(baseurl, path = endpoint)
  info <- oauth_signature(url, app = app)
  header_oauth <- oauth_header(info)
  GET(url, header_oauth) 
}

# Defines a function that finds matches with our nouns list within tweets
match_words <- function(tweet){
  # Runs grepl function for a particular tweet
  matched_values <- sapply(words, grepl, tweet)
  # Returns df with boolean values for each noun indicating presence in the tweet
  return(matched_values)
}

# Defines a function that searches for the most common matches from previous function
most_common_words <- function(df){
  # Runs match_words function for each tweet in df of tweets
  text_matches <- data.frame(mapply(match_words, df$text)) 
  # Counts number of tweets in which a noun occurs
  word_occurences <- text_matches %>%
    cbind(rownames(text_matches)) %>%
    rename(word = 'rownames(text_matches)') %>%
    mutate(occurences = rowSums(. == TRUE)) %>%
    select(word, occurences) %>%
    arrange(desc(occurences))
  
  return(word_occurences)
}

# Defines a function that lists the top 'n' words by number of matches
top_n_words <- function(df, n) {
  words <- most_common_words(df) %>%
    select(word)
  # Defines list with the n most common words
  top_words <- list()
  for (i in 1:n) {
    top_words[i] <- as.character(words$word)[i]
  }
  return(top_words)
}

# Defines a function that lists urls for the top 'n' images by matches
top_n_icons <- function(napp, df, n){
  words <- top_n_words(df, n)
  # Generates endpoints for words from top_n_words function and then queries a response from nounproject API
  # A url with a link to a .png of the corresponding icon is then taken from this response
  urls <- list()
  for (i in 1:n) {
    endpoint <- paste("icon", words[i], sep = "/")
    res <- get_nouns_api(endpoint = endpoint, app = napp)
    icon_res <- content(res,"parsed")
    urls[i] <- icon_res$icon$preview_url
  }
  
  return(urls)
}

# Defines a function to generate map graphic with color based on some data proportion
MapPlot <- function(ds, cfill) {
  ggplot(ds, aes(x = long, y = lat, fill = !! sym(cfill), group = map_group), color = "white") + 
    coord_fixed(1.3) +
    geom_polygon(alpha = 0.7) +
    geom_polygon(color = "black", fill = NA) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.text = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      axis.title = element_blank()) +
    labs(title = "Average Usage of Words Per Tweet Per State (Data generated at 3:00PM EST 12/10/18)") +
    scale_fill_gradient("Average usage")
      
}


# Define UI for application
ui <- fluidPage(
   
   # Application title
   titlePanel("PicBook"),
   
   # Sidebar inputs
   sidebarLayout(
     
      sidebarPanel(
        
        # Selection for type of geographic search. Affects apearance of following inputs
        radioButtons("search_meth",
                     "Search Method:",
                     choices = c("Address" = "adr",
                                 "Lat/Long" = "lat_long"),
                     inline = TRUE),
        
        # Address search uses google maps searches. Can be as specific as street address or as general as countries
        conditionalPanel(
          condition = "input.search_meth == 'adr'",
          
          textInput("address",
                    "Address")
        ),
        
        # Inputs to define search using lat/long and radius of area
        conditionalPanel(
          condition = "input.search_meth == 'lat_long'",
          
          textInput("lat",
                    "Latitude:"),
          
          textInput("long",
                    "Longitude:"),
          
          numericInput("radius",
                       "Radius(km):",
                       value = 40)
        ),
        
        # Button that runs the app
        actionButton("execute",
                     "Execute")
      ),
      
      # Output panels
      mainPanel(
          tabsetPanel(type = "tabs",
                    # First panel displays top three words
                    tabPanel("Application", uiOutput("word1"),
                                            uiOutput("word2"),
                                            uiOutput("word3")),
                    # Panel with map of noun usage by state
                    tabPanel("US Usage", 
                                         selectInput("state_word",
                                                     "Word:",
                                                     choices = words),
                                         dataTableOutput(outputId = "codebook"),
                                         plotOutput("statemap")),
                    # Panel with map of noun usage over time
                    tabPanel("Usage Over Time",
                                         selectInput("time_word",
                                                     "Word:",
                                                     choices = words),
                                         plotOutput("timegraph")),
                    # Panel with premade world map image displayed
                    tabPanel("World Map", 
                             h5(textOutput("gatherdate")),
                             br(),
                             imageOutput("mappic"))
        )
      )
   )
)

# Define server logic
server <- function(input, output) {
  
   # Generates geocode input based on selected input method
   google_coords <- reactive({
     # If search method is address, uses google places api to find corresponding coords
     if (input$search_meth == 'adr') {
       q <- lookup_coords(input$address, apikey = google_key)
     }
     # If search method is lat/long, simply pastes together an appropriate query
     else if (input$search_meth == 'lat_long') {
       q <- paste(input$lat, input$long, paste(input$radius, "km", sep = ""), sep = ",")
     }
    print(q)
    return(q)
   })
   
   # Generates df with tweets from twitter API based on generated geocode
   tweet_results <- eventReactive(input$execute, {
     # Defines rtweet token based in keys/tokens from config.R
     create_token(
       app = "PicBook",
       consumer_key = twitter_key,
       consumer_secret = twitter_secret_key,
       access_token = twitter_token,
       access_secret = twitter_secret_token)
     # Runs search of 500 recent tweets
     results <- search_tweets(
       "lang:en", geocode = google_coords(), n = 500)
     print(results)
     print(top_n_words(results, 3))
     
     return(results)
   })
   
   # Generates an httr app with corresponding keys from config.R
   noun_app_info <- eventReactive(input$execute, {
     nouns_app <- oauth_app("nouns_api", nouns_key, nouns_secret_key)
   })
   
   # First word/icon output
   output$word1 <- renderUI({
     # Saves url of first image
     text <- tweet_results()
     icon_1 <- top_n_icons(noun_app_info(), text, 3)[1]
     # Displats word and corresponding image
     tagList(
       HTML(paste("1st:" , top_n_words(text, 3)[1])),
       tags$img(src = icon_1)
     )
     
   })
   
   # Second word/icon output
   output$word2 <- renderUI({
     
     text <- tweet_results()
     icon_2 <- top_n_icons(noun_app_info(), text, 3)[2]
     
     tagList(
       HTML(paste("2nd:" , top_n_words(text, 3)[2])),
       tags$img(src = icon_2)
     )
   })
   
   # Third word/icon output
   output$word3 <- renderUI({
     
     text <- tweet_results()
     icon_3 <- top_n_icons(noun_app_info(), text, 3)[3]
     
     tagList(
       HTML(paste("3rd:" , top_n_words(text, 3)[3])),
       tags$img(src = icon_3)
     )
   })
   
   # State map showing proportion of word over total tweets by state
   output$statemap <- renderPlot({
     # Calls state geodata from maps package
     states <- map_data("state")
     # Adds our data to states geodata
     matched_state_data <- states %>%
       rename(map_group = group) %>%
       full_join(state_data, by = c('region' = 'State'))
     # Defines word to look at
     selected_word_state <- input$state_word
     # Runs function to generate plot
     MapPlot(matched_state_data, selected_word_state)
     
   })
   
   # Graph showing proportion of word over total tweets by hour
   output$timegraph <- renderPlot({
     # Defines word to look at
     selected_word_time <- input$time_word
     # Generates time plot
     ggplot(time_data_read, aes_string(x='Time', y=selected_word_time)) + geom_line() + geom_point() +
       labs(title = "Average Usage of Words Per Tweet Per Hour (Data generated for California from 6:00AM - 7:00PM PST 12/10/18)") +
       scale_x_continuous("Hour (24 hr format)", breaks = c(6,7,8,9,10,11,12,13,14,15,16,17,18,19)) +
       scale_y_continuous("Average Word Usage Per Tweet")
   })
   
  # Info about world map
   output$gatherdate <- renderText({
     paste("Top tweeted word's from locations around the globe! (results for data collected 12/09/2018, 7:58 - 8:21 pm)")
   })
   
  # World map pic showing top words around the world!
   output$mappic <- renderImage({
     list(src = "IconMap.png", height="500px")
   }, deleteFile = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)

