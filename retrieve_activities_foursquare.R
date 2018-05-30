library("jsonlite")
library("dplyr")
library("stringr")
library("RCurl")

categories <- read.csv("C:/Users/paolo/Documents/Data/Projects/Club M/Healthy volunteers study/Datasets/foursquare_categories.csv")

client_id <- "TRDTAHS4ZSJV2ROTNEQXVY5HE1DWOWJUJXIBSMQ1BUAD3DO1"

client_secret <- "ZCAEILOCQ4NGEOJIIJABRBAU3MPMWKLV3ABN5GACSSIBREY4"

lat <- 53.43560

lon <- -2.246267

radius <- 100

category_id <- str_c(as.character(unlist(categories$id)), collapse = ",")

query_string <- str_c("https://api.foursquare.com/v2/venues/search?",
                      "ll=", lat, ",", lon, "&",
                      "intent=checkin&",
                      "client_id=", client_id, "&",
                      "client_secret=", client_secret, "&",
                      "radius=", radius, "&",
                      "categoryId=", category_id, "&",
                      "v=20180530",
                      sep = "")

response <- getURL(query_string)

tmp <- response %>% fromJSON()

venues <- tmp[["response"]][["venues"]]

venues$address <- venues$location$address
venues$lat <- venues$location$lat
venues$lng <- venues$location$lng
venues$distance <- venues$location$distance

venues <- venues %>% 
  select(-location, -venuePage) %>% 
    filter(distance == min(distance))
