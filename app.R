#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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
        
        radioButtons("google",
                     "Address Search?",
                     choices = c("No" = "no",
                                 "Yes" = "yes"),
                     inline = TRUE),
        
        conditionalPanel(
          condition = "input.google == 'no'",
          
          textInput("lat_long",
                    "Latitude, Longitude:")
        ),
        
        conditionalPanel(
          condition = "input.google == 'yes'",
          
          textInput("address",
                    "Address"),
          
          textInput("google_key",
                    "Google API Key:")
        ),
        
        numericInput("radius",
                     "Radius(km):",
                     value = 40),
        
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
   
   output$word1 <- renderUI({
     
     tags$img(src = lookup_nth_icon(text, 1))
     
   })
   
   output$word2 <- renderUI({
     
     tags$img(src = lookup_nth_icon(text, 2))
     
   })
   
   output$word3 <- renderUI({
     
     tags$img(src = lookup_nth_icon(text, 3))
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

