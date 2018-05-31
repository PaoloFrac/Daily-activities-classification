library("jsonlite")
library("dplyr")
library("stringr")
library("RCurl")
library("readr")

categories <- read_csv("~/Data/Projects/Club M/Healthy volunteers study/Datasets/foursquare_categories.csv")

client_id <- "TRDTAHS4ZSJV2ROTNEQXVY5HE1DWOWJUJXIBSMQ1BUAD3DO1"

client_secret <- "ZCAEILOCQ4NGEOJIIJABRBAU3MPMWKLV3ABN5GACSSIBREY4"

distance_threshold <- 50

minutes_threshold <- 10

timeThreshold <- 60*minutes_threshold

places <- readRDS(paste("~/Data/Projects/Club M/Healthy volunteers study/Datasets/places", timeThreshold,".rds"))

category_id <- categories %>% 
                filter(id == parent_id) %>% # only the 10 main categories
                  select(id) %>% 
                    unlist()

category_id <- str_c(category_id, collapse = ",") # collapse into single string

places_classfied <- NULL

# for each found place
for(i in 1:nrow(places)){
  
  #create query to interrogate API
  query_string <- str_c("https://api.foursquare.com/v2/venues/search?",
                        "ll=", places$CenterLat[i], ",", places$CenterLong[i], "&",
                        "intent=checkin&",
                        "client_id=", client_id, "&",
                        "client_secret=", client_secret, "&",
                        "radius=", distance_threshold, "&",
                        "categoryId=", category_id, "&",
                        "v=20180530",
                        sep = "")
  
  #send request
  response <- getURL(query_string)
  
  # convert fromJson to list
  tmp <- response %>% fromJSON()
  
  if(length(tmp[["response"]][["venues"]])){ # check if the venues list is empty
    
    venues <- tmp[["response"]][["venues"]]
    
    # get info from location
    venues$address <- venues$location$address
    venues$lat <- venues$location$lat
    venues$lng <- venues$location$lng
    venues$distance <- venues$location$distance
    
    venues <- venues %>% 
      select(-location) # get rid of df column
    
    #get rid of other unnecessary columns if present
    if(sum(colnames(venues) == "events")>0){
      
      venues <- venues %>% 
        select(-events) 
      
    }
    if(sum(colnames(venues) == "venuePage")>0){
      
      venues <- venues %>% 
        select(-venuePage) 
      
    }
    
    venue <- venues %>%
      filter(distance <= distance_threshold) %>% #only if within threshold
        filter(distance == min(distance)) #keep only the closest
    
    if(nrow(venue) > 0){
      
      #extract category id and name
      venue$category_id <- venue$categories[[1]][["id"]]
      venue$category_name <- venue$categories[[1]][["name"]]
      venue <- venue %>%
        select(-categories)
      
      # add to results db
      places_classfied <- bind_rows(places_classfied,
                                data.frame(patient = places$patient[i],
                                           placeID = places$placeID[i],
                                           venue))
      
    }
    
  }
  
}






