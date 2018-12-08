
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
        
        submitButton("Search")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         uiOutput("word1"),
         uiOutput("word2"),
         uiOutput("word3"),
         textOutput("test")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
   google_coords <- reactive({
     
     if (input$search_meth == 'adr') {
       q <- lookup_coords(input$address, apikey = input$google_key)
     }
     
     else if (input$search_meth == 'lat_long') {
       q <- paste(input$lat, input$long, paste(input$radius, "km", sep = ""), sep = ",")
     }
    print(q)
    
    return(q)
   })
  
   #tweet_results <- reactive({
     
     #create_token(
       #app = "PicBook",
       #consumer_key = input$key,
       #consumer_secret = input$secret_key,
       #access_token = input$token,
       #access_secret = input$secret_token)
     
     #rt <- search_tweets(
       #"lang:en", geocode = q, n = 100)
     
     #return(rt)
   #})
   
   output$word1 <- renderUI({
     
     tags$img(src = lookup_nth_icon(text, 1))
     
   })
   
   output$word2 <- renderUI({
     
     tags$img(src = lookup_nth_icon(text, 2))
     
   })
   
   output$word3 <- renderUI({
     
     tags$img(src = lookup_nth_icon(text, 3))
     
   })
   
   output$test <- renderText({
     
     google_coords()
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

