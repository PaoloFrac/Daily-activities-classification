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


########## categories
categories_full <- getURL(str_c("https://api.foursquare.com/v2/venues/categories?",
                                "client_id=", client_id, "&",
                                "client_secret=", client_secret, "&",
                                "v=20180530", sep = "")) %>% 
                    fromJSON()
                     

glimpse(categories_full$response$categories)

# Observations: 10
# Variables: 6
# $ id         <chr> "4d4b7104d754a06370d81259", "4d4b7105d754a06372d81259", "4d4b7105d754a06373d81259", "4d4b7105d754a0...
# $ name       <chr> "Arts & Entertainment", "College & University", "Event", "Food", "Nightlife Spot", "Outdoors & Recr...
# $ pluralName <chr> "Arts & Entertainment", "Colleges & Universities", "Events", "Food", "Nightlife Spots", "Outdoors &...
# $ shortName  <chr> "Arts & Entertainment", "College & Education", "Event", "Food", "Nightlife", "Outdoors & Recreation...
# $ icon       <data.frame> c("https://ss3.4sqi.net/img/categories_v2/arts_entertainment/default_", "https://ss3.4sqi.ne...
#                             $ categories <list> [<c("56aa371be4b08b9a8d5734db", "4fceea171983d5d06c3e9823", "4bf58dd8d48988d1e1931735", "4bf58dd8d...

tmp <- categories_full$response$categories

glimpse(tmp$categories[1][[1]])

# Observations: 36
# Variables: 6
# $ id         <chr> "56aa371be4b08b9a8d5734db", "4fceea171983d5d06c3e9823", "4bf58dd8d48988d1e1931735", "4bf58dd8d48988...
# $ name       <chr> "Amphitheater", "Aquarium", "Arcade", "Art Gallery", "Bowling Alley", "Casino", "Circus", "Comedy C...
# $ pluralName <chr> "Amphitheaters", "Aquariums", "Arcades", "Art Galleries", "Bowling Alleys", "Casinos", "Circuses", ...
# $ shortName  <chr> "Amphitheater", "Aquarium", "Arcade", "Art Gallery", "Bowling Alley", "Casino", "Circus", "Comedy C...
# $ icon       <data.frame> c("https://ss3.4sqi.net/img/categories_v2/arts_entertainment/default_", "https://ss3.4sqi.ne...
# $ categories <list> [<>, <>, <>, <>, <>, <>, <>, <>, <>, <>, <>, <>, <>, <>, <>, <>, <>, <>, <>, <c("56aa371be4b08b9a8...


categories <- NULL

for(i in 1:nrow(tmp)){
  
  categories <- tmp$categories[i][[1]] %>% 
    
  
}
