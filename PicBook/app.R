# install.packages("httr")
# install.packages("rtweet")

library(shiny)
library(httr)
library(rtweet)
library(readr)
library(mdsr)
library(tidyr)

# Words file to be used for matches
words_file <- read_csv('words.csv')
words <- words_file$Words

# Data of proportion of words tweeted by state
state_data <- read_csv('state_data.csv')

# Data of proportion of words tweeted by hour
time_data_read <- read_csv('time_data.csv')

# Function that generates the request to nounproject api
get_nouns_api <- function(endpoint, baseurl = "http://api.thenounproject.com/", app) {
  url <- modify_url(baseurl, path = endpoint)
  info <- oauth_signature(url, app = app)
  header_oauth <- oauth_header(info)
  GET(url, header_oauth) 
}

# Function that finds matches with our nouns list within tweets
match_words <- function(tweet){
  matched_values <- sapply(words, grepl, tweet)
  
  return(matched_values)
}

# Function that searches for the most common matches from previous function
most_common_words <- function(df){
  text_matches <- data.frame(mapply(match_words, df$text)) 
  
  word_occurences <- text_matches %>%
    cbind(rownames(text_matches)) %>%
    rename(word = 'rownames(text_matches)') %>%
    mutate(occurences = rowSums(. == TRUE)) %>%
    select(word, occurences) %>%
    arrange(desc(occurences))
  
  return(word_occurences)
}

# Function that lists the top 'n' words by number of matches
top_n_words <- function(df, n) {
  words <- most_common_words(df) %>%
    select(word)
  
  top_words <- list()
  for (i in 1:n) {
    top_words[i] <- as.character(words$word)[i]
  }
  return(top_words)
}

# Function that lists urls for the top 'n' images by matches
top_n_icons <- function(napp, df, n){
  words <- top_n_words(df, n)
  
  urls <- list()
  for (i in 1:n) {
    endpoint <- paste("icon", words[i], sep = "/")
    res <- get_nouns_api(endpoint = endpoint, app = napp)
    icon_res <- content(res,"parsed")
    urls[i] <- icon_res$icon$preview_url
  }
  
  return(urls)
}

# Function to generate map graphic with color based on some data proportion
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
    labs(title = "Average Usage of Words Per Tweet Per State (Data generated at 3:00PM EST)") +
    scale_fill_gradient("Average usage")
      
}


# Define UI for application
ui <- fluidPage(
   
   # Application title
   titlePanel("PicBook"),
   
   # Sidebar inputs
   sidebarLayout(
     
      sidebarPanel(
        
        # Inputs for all the api keys...
        textInput("key",
                  "Twitter API Key:"),
        
        textInput("secret_key",
                  "Twitter API Secret Key:"),
        
        textInput("token",
                  "Twitter API Access Token"),
        
        textInput("secret_token",
                  "Twitter API Access Token Secret:"),
        
        textInput("noun_key",
                  "Noun Project API Key:"),
        
        textInput("noun_secret_key",
                  "Noun Project API Secret Key:"),
        
        textInput("google_key",
                  "Google API Key:"),
        
        # Selection for type of geographic search
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
        
        # Specific search using lat/long and radius of area
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
        
        # radioButtons("words_list_choice",
                     # "Words to search for:",
                     # choices = c("Top 100 Nouns" = "words_100",
                                 # "Top 1000 Nouns" = "words_1000",
                                 # "Custom List" = "words_custom"),
                     # inline = TRUE),
        
        # conditionalPanel(
           # condition = "input.words_list_choice == 'words_custom'",
           
           # fileInput("customfile",
                     # "Upload custom .csv file",
                     # accept = c("text/csv",
                                # "text/comma-separated-values,text/plain",
                                # ".csv"))
        # ),
        
        # Button that runs the app
        actionButton("execute",
                     "Execute")
      ),
      
      # Displays top 3 images
      mainPanel(
          tabsetPanel(type = "tabs",
                    tabPanel("Application", uiOutput("word1"),
                                               uiOutput("word2"),
                                               uiOutput("word3")),
                    tabPanel("US Usage", 
                                         selectInput("state_word",
                                                     "Word:",
                                                     choices = words),
                                         dataTableOutput(outputId = "codebook"),
                                         plotOutput("statemap")),
                    tabPanel("Usage Over Time",
                                         selectInput("time_word",
                                                     "Word:",
                                                     choices = words),
                                         plotOutput("timegraph")),
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
     
     req(input$google_key)
     
     if (input$search_meth == 'adr') {
       q <- lookup_coords(input$address, apikey = input$google_key)
     }
     
     else if (input$search_meth == 'lat_long') {
       q <- paste(input$lat, input$long, paste(input$radius, "km", sep = ""), sep = ",")
     }
    print(q)
    
    return(q)
   })
   
   # Searches for actual data from twitter
   tweet_results <- eventReactive(input$execute, {
     
     req(input$key, input$secret_key, input$token, input$secret_token)
     
     create_token(
       app = "PicBook",
       consumer_key = input$key,
       consumer_secret = input$secret_key,
       access_token = input$token,
       access_secret = input$secret_token)
     
     results <- search_tweets(
       "lang:en", geocode = google_coords(), n = 460)
     print(results)
     print(top_n_words(results, 3))
     
     return(results)
   })
   
   # Generates api info to be used by 'get_nouns_api' function
   noun_app_info <- eventReactive(input$execute, {
     nouns_app <- oauth_app("nouns_api", input$noun_key, input$noun_secret_key)
   })
   
   # First word/icon
   output$word1 <- renderUI({
     
     text <- tweet_results()
     icon_1 <- top_n_icons(noun_app_info(), text, 3)[1]
     
     tagList(
       HTML(paste("1st:" , top_n_words(text, 3)[1])),
       tags$img(src = icon_1)
     )
     
   })
   
   # Second word/icon
   output$word2 <- renderUI({
     
     text <- tweet_results()
     icon_2 <- top_n_icons(noun_app_info(), text, 3)[2]
     
     tagList(
       HTML(paste("2nd:" , top_n_words(text, 3)[2])),
       tags$img(src = icon_2)
     )
   })
   
   # Third word/icon
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
     
     matched_state_data <- states %>%
       rename(map_group = group) %>%
       full_join(state_data, by = c('region' = 'State'))
     
     selected_word_state <- input$state_word
     
     MapPlot(matched_state_data, selected_word)
     
   })
   
   output$timegraph <- renderPlot({
     
     selected_word_time <- input$time_word
     
     ggplot(time_data_read, aes_string(x='Time', y=selected_word_time)) + geom_line() + geom_point() +
       labs(title = "Average Usage of Words Per Tweet Per Hour (Data generated for California from 6:00AM - 2:00PM PST)") +
       scale_x_continuous("Hour (24 hr format)", breaks = c(6,7,8,9,10,11,12,13,14)) +
       scale_y_continuous("Average Usages Per Hour")
   })
   
  #World Map Data Gather Date
   output$gatherdate <- renderText({
     paste("Showing results for data collected 12/09/2018, 7:58 - 8:21 pm")
   })
   
  #World Map Pic
   output$mappic <- renderImage({
     list(src = "IconMap.png", height="500px")
   }, deleteFile = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)

