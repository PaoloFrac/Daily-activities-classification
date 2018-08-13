# Out of home activity recognition
rm(list=ls())
# Before run the code, specify 
# 1) if the dataset is from Healthy volounteers (HV) or from patients (P)
# 2) which pipeline you want to run (time-based or density-based)
# 3) the name of the Computer user
datasetType = "P"
pipeline = "timePlusDensity-based"
user = "HeRCuser"
# Directories
dir = paste0("~/Data/Projects/Club M/Sonia_old/")
functionDir = paste0(dir, "R Scripts/FunctionScript.R")
directory <- paste0(dir,datasetType,"_data") 
filesDirectory <- paste0(dir,"Files")

# sourcing file with functions

# libraries
source(functionDir)
library("osmar")
library("ggmap")
library("ggplot2")
library("xlsx")
library("chron")
library("lubridate")
library("plotKML")
library("RCurl")
library("XML")
library("tidyverse")
library("PostcodesioR")

# GPS data acquisition
df = getDataset(directory)

# Social functioning diary acquisition
setwd(filesDirectory)
if(datasetType=="HV"){
  # load social functioning diary
  sfd = read.xlsx("HV_social functioning diaries.xlsx", sheetIndex = 1, header = TRUE )
  sfd$date = as.Date(sfd$date) 
  sfd$time.start = paste0(sfd$date, "T", format(sfd$time.start, "%H:%M:%S")) # get only time
  sfd$time.start = as.POSIXct(strptime(sfd$time.start, "%Y-%m-%dT%H:%M:%S")) # combine with date
  sfd$time.end = paste0(sfd$date, "T", format(sfd$time.end, "%H:%M:%S")) # get only time
  sfd$time.end = as.POSIXct(strptime(sfd$time.end, "%Y-%m-%dT%H:%M:%S")) # combine with date
  
  # General data cleaning date-based: only if we have information related to the time
  df = dateBasedCleaning(df, sfd)
  sfd = dateBasedCleaningSF(df, sfd)
}else{
  sfd = read.xlsx("P_social functioning diaries.xlsx", sheetIndex = 1, header = TRUE )
  sfd$date = as.Date(sfd$date)
  # General data cleaning date-based: only if we have information relaed to the time
  df = df[!(df$patient=="A1000_JF" & df$sessionid==1),]
  df = df[!(df$patient=="A1000_JF" & df$sessionid==2),]
  df = df[!(df$patient=="A1000_JF" & df$sessionid==5),]
  df = df[!(df$patient=="A8000_SK"),]
  df$patient = as.character(df$patient)
  sfd$patient = as.character(sfd$patient)
  df = dateBasedCleaning(df, sfd)
  sfd = dateBasedCleaningSF(df, sfd) %>% 
          select(patient, date, activityType, activityCategory)
}

source('~/Data/Projects/Club M/Healthy volunteers study/R/Daily-activities-classification/FunctionScript.R', echo=FALSE)
directory <- "~/Data/Projects/Club M/Healthy volunteers study/Datasets"

  minutes_threshold <- 10
  
  #analysis_type <- "time_based"
   #analysis_type <- "density_based"
 analysis_type <- "combined"

  timeThreshold <- 60*minutes_threshold
  
  distanceThreshold = 50
  
  geo.visited <- NULL
  
  if(analysis_type %in% c("density_based", "combined")){
    
    # Step 1: Find geolocation visited
    geo.visited = densityBasedMethod(df,timeThreshold,distanceThreshold) # apply time-based method
    
  }
  
  if(analysis_type %in% c("time_based", "combined")){
    
    geo.visited = rbind(geo.visited,
                        timeBasedMethod(df,timeThreshold,distanceThreshold)) # apply density-based method
    
  }
 
  # geo.visited <- geo.visited %>% 
  #                   distinct(patient, sessionid, Latitude, Longitude, TimeStamp)
  # 
 # geo.visited = densityBasedMethod(df,timeThreshold,radius)
  # }
# Step 2: Places visited identification
# ______________________________________
# 'spaceClustering' is a function that cluster geolocations visited in places
places = spaceClustering(geo.visited, distanceThreshold, weighted = TRUE)   

places <- places %>% 
            distinct(patient, placeID, CenterLat, CenterLong)

saveRDS(places,  paste("~/Data/Projects/Club M/Healthy volunteers study/Datasets/",
                       analysis_type,
                       "/places", 
                       timeThreshold,
                       "s_",
                       distanceThreshold,
                       "_",
                       datasetType,
                       ".rds", 
                       sep = ""))

# 'assignPlaceID' is the function that classify GPS data points with place ID
df.places = assignPlaceID(df,places,distanceThreshold)


places.visited = getPlaceList(df.places, places)

saveRDS(places.visited,  paste("~/Data/Projects/Club M/Healthy volunteers study/Datasets/",
                               analysis_type,
                               "/places_visited", 
                               timeThreshold,
                               "s_",
                               distanceThreshold,
                               "_",
                               datasetType,
                               ".rds", 
                               sep = ""))



