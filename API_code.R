install.packages("httr")
install.packages("rtweet")
install.packages("magick")
install.packages("rsvg")

library(httr)
library(rtweet)
library(magick)
library(rsvg)


GOOGLE_MAPS_KEY <- ""

q = lookup_coords("paris", apikey = "")
q



rt <- search_tweets(
  "lang:en", geocode = q, n = 100)

paste("1","mi", sep = "")

google_api <- "https://maps.googleapis.com/maps/api/geocode/json"
r <- GET(google_api, query=list(address=address))

nouns_app <- oauth_app("nouns_api", "", "")

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



