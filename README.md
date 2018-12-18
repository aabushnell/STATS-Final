# STATS-Final

To run 'PicBook'

1. Clone/download all contents of this repository to a local file, including .csv files and images.
2. Make sure you have access to (i.e. api keys/tokens) the Google Places API (https://developers.google.com/places/web-service/intro), the standard Twitter APIs through a developer account (https://developer.twitter.com/en/docs/basics/getting-started), and the Noun Project api through a Noun Project account (https://api.thenounproject.com/getting_started.html).
3. Create a file called config.R. Save it to the same directory as app.R and all the other files. In config.R the following variables should be defined (as strings) in this format:
	
		#####################################
		google_key <- '*Insert Google Places api key here*'
		twitter_key <- '*Insert Twitter api public key here*'
		twitter_secret_key <- '*Insert Twitter api secret key here*'
		twitter_token <- '*Insert Twitter api public token here*'
		twitter_secret_token <- '*Insert Twitter api secret token here*'
		nouns_key <- '*Insert nounproject api public key here*'
		nouns_secret_key <- '*Insert nounproject api secret key here*'
		#####################################

4. If necessary run the commented out code at the top of app.R to install the required packages for PicBook (httr,rtweet, and maps)
5. Run app.R to execute the shiny app for PicBook
