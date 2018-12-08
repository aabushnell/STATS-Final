
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("PicBook"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     
      sidebarPanel(
        
        textInput("key",
                  "Twitter API Key:"),
        
        textInput("secret_key",
                  "Twitter API Secret Key:"),
        
        textInput("token",
                  "Twitter API Access Token"),
        
        textInput("secret_token",
                  "Twitter API Access Token Secret:"),
        
        textInput("google_key",
                  "Google API Key:"),
        
        radioButtons("search_meth",
                     "Search Method:",
                     choices = c("Lat/Long" = "lat_long",
                                 "Adress" = "adr"),
                     inline = TRUE),
        
        conditionalPanel(
          condition = "input.search_meth == 'adr'",
          
          textInput("address",
                    "Address")
        ),
        
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
        
        actionButton("search",
                     "Search")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         uiOutput("word1"),
         uiOutput("word2"),
         uiOutput("word3")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
   google_coords <- eventReactive(input$search, {
     
     if (input$search_meth == 'adr') {
       q <- lookup_coords(input$address, apikey = input$google_key)
     }
     
     else if (input$search_meth == 'lat_long') {
       q <- paste(input$lat, input$long, paste(input$radius, "km", sep = ""), sep = ",")
     }
    print(q)
    
    return(q)
   })
  
   tweet_results <- eventReactive(input$search, {
     
     create_token(
       app = "PicBook",
       consumer_key = input$key,
       consumer_secret = input$secret_key,
       access_token = input$token,
       access_secret = input$secret_token)
     
     rt <- search_tweets(
       "lang:en", geocode = google_coords(), n = 100)
     print(rt)
     
     return(rt)
   })
   
   words_list <- eventReactive(input$search, {
    
     text <- tweet_results() %>%
       select(screen_name, text)
     
     match_words <- function(tweet){
       matched_values <- sapply(words, grepl, tweet)
       
       return(matched_values)
     }
     
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
     
     top_n_words <- function(df, n) {
       words <- most_common_words(text) %>%
         select(word)
       
       top_words <- list()
       for (i in 1:n) {
         top_words[i] <- as.character(words$word)[i]
       }
       return(top_words)
     }
     
     print(top_n_words(text, 3))
     
     return(top_n_words(text, 3))
   })
   
   output$word1 <- renderUI({
     
     
     
   })
   
   output$word2 <- renderUI({
     
     
     
   })
   
   output$word3 <- renderUI({
     
     
     
   })

}

# Run the application 
shinyApp(ui = ui, server = server)

