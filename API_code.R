install.packages("httr")
install.packages("rtweet")
install.packages("magick")
install.packages("rsvg")

library(httr)
library(rtweet)
library(magick)
library(rsvg)

setup_twitter_oauth("T3GvwW1WZ0svXvFgjAWPRbIRq", "DWaSGahKAsDmo2j5TFywRBOgmpdOUo9zmGr3IdTVWyPGewl5X2")

GOOGLE_MAPS_KEY <- "AIzaSyBtt_cqXkkEYIewZp5ZYp1fdhUksN92OA8"

q = lookup_coords("davis, CA", apikey = "AIzaSyBtt_cqXkkEYIewZp5ZYp1fdhUksN92OA8")
q

create_token(
  app = "PicBook",
  consumer_key = "T3GvwW1WZ0svXvFgjAWPRbIRq",
  consumer_secret = "DWaSGahKAsDmo2j5TFywRBOgmpdOUo9zmGr3IdTVWyPGewl5X2",
  access_token = "1061380837659344896-iuoPsbhuXodf9SW86i1GhVJZ7iY8pI",
  access_secret = "pbuRacyoSRgOQC8QAPYI65ci6MZEBNdtKt2lLwvqC0yTb")

rt <- search_tweets(
  "lang:en", geocode = q, n = 100)

google_api <- "https://maps.googleapis.com/maps/api/geocode/json"
r <- GET(google_api, query=list(address=address))

nouns_app <- oauth_app("nouns_api", "052ecc4e19c341758fdd1bb618ce6cb4", "b1ca275beca84816ad5c16efe798b4e4")

get_nouns_api <- function(endpoint, baseurl = "http://api.thenounproject.com/", app = nouns_app) {
  url <- modify_url(baseurl, path = endpoint)
  info <- oauth_signature(url, app = app)
  header_oauth <- oauth_header(info)
  GET(url, header_oauth)
}


res <- get_nouns_api("icon/15")
httr::status_code(res)
str(res)
icon_res <- content(res,"parsed")
icon_url <- icon_res$icon$preview_url
icon_url

icon <- image_read(icon_url)
print(icon)



