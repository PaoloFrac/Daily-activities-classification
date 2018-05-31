# Out of home activity recognition
#rm(list=ls())
# column names
gps_data_column_names <- c("Latitude", "Longitude", "Altitude", "Bearing", "Accuracy", "Speed", "TimeStamp", "sessionid", "patient", "X")
activity_data_column_name <- c("patient", "date", "time.start", "time.end", "place", "activityType", "activityCategory")

# # Directories
directory <- "~/Data/Projects/Club M/Healthy volunteers study/Datasets"
# sourcing file with functions
source('~/Data/Projects/Club M/Healthy volunteers study/R/FunctionScript.R')
# libraries
library(osmar)
library(ggmap)
library(ggplot2)
library(xlsx)
library(chron)
library(lubridate)
library(plotKML)
library(RCurl)
library(XML)
library("tidyverse")
library("PostcodesioR")

# key Google Place API
key= c("AIzaSyDWs7eStEfQRGG8tuNDheo2SJR8ooPjr14",
       "AIzaSyCdyXaICjKXqefkUUzebnw7A6wDvcQac7",
       "AIzaSyCl_RMVOmZfOVdfj8Umn9RRytSHdSIIV3k",
       "AIzaSyAoZSLAoWUxPlOBGE3EeHUlxJ9arrPJt90")

#  GPS data acquisition and final harmonisation
df <- readRDS(paste(directory, "analysible_gps_data.rds", sep = "/")) %>% 
  select(Latitude, Longitude, Altitude, Bearing, Accuracy, Speed, TimeStamp, sessionid, patient, X) %>% 
    mutate(patient = factor(patient),
           sessionid = factor(sessionid))

df <- as.data.frame(df)

# activity diary data acquisition and final harmonisation
sfd <- readRDS(paste(directory, "analysible_activity_diary.rds", sep = "/")) %>% 
  rename(time.start = time_start,
         time.end = time_end) %>%
   # select(patient, date, time.start, time.end, place, activityType, activityCategory) %>% 
      mutate(patient = factor(patient),
             time.end = ifelse(time.end == "6OM", "6:00 PM", time.end),
             time.start = ymd_hm(paste(as.character(date), time.start)),
             time.end = ymd_hm(paste(as.character(date), time.end)),
             place = factor(place),
             activityType = factor(activityType),
             activityCategory = factor(activityCategory))

sfd <- as.data.frame(sfd)

  minutes_threshold <- 10

  timeThreshold <- 60*minutes_threshold
  distanceThreshold = 50
  radius = 50
  geo.visited = timeBasedMethod(df,timeThreshold,distanceThreshold) 
  geo.visited = rbind(geo.visited,
                      densityBasedMethod(df,timeThreshold,radius) )
# }
# Step 2: Places visited identification
# ______________________________________
# 'spaceClustering' is a function that cluster geolocations visited in places
# 'assignPlaceID' is the function that classify GPS data points with place ID 
places = spaceClustering(geo.visited, radius)   
saveRDS(places,  paste("~/Data/Projects/Club M/Healthy volunteers study/Datasets/places", timeThreshold,".rds"))

df.places = assignPlaceID(df,places,radius)

places.visited = getPlaceList(df.places, places)
saveRDS(places.visited,  paste("~/Data/Projects/Club M/Healthy volunteers study/Datasets/places_visited",timeThreshold,".rds"))

