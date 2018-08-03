# Functions script


kalman_filter <- function(day){
  
  #initializing variables
  count <- nrow(day) # amount of data points in the day
  z <- cbind(day$lon,day$lat) #measurements
  
  #Allocate space:
  xhat <- matrix(rep(0,2*count),ncol =2) #a posteri estimate at each step
  P <- array(0,dim=c(2,2,count))  #a posteri error estimate
  xhatminus <- matrix(rep(0,2*count),ncol =2) #a priori estimate
  Pminus <- array(0,dim=c(2,2,count)) #a priori error estimate
  K <- array(0,dim=c(2,2,count)) #gain
  
  #Initializing matrices
  A <-diag(2)
  H<-diag(2)
  R<-function(k) diag(2)* day$accuracy[k]^2#estimate of measurement variance
  Q<-function(k) diag(2)* as.numeric(day$timeNextMeasure[k])^1.5# the process variance
  
  #initialise guesses:
  xhat[1,] <- z[1,]
  P[,,1] <- diag(2)
  
  
  for (k in 2:count){
    #time update
    #project state ahead
    xhatminus[k,] <- A %*% xhat[k-1,] #+ B %*% u[k-1]
    
    #project error covariance ahead
    Pminus[,,k] <- A %*% P[,,k-1] %*%  t(A) + (Q(k))
    
    #measurement update
    # kalman gain
    K[,,k] <- Pminus[,,k] %*% t(H)/ (H %*% Pminus[,,k] %*% t(H) + R(k))
    
    #what if NaaN?
    K[,,k][which(is.nan(K[,,k]))]<-0
    
    # update estimate with measurement
    xhat[k,] <-  xhatminus[k,] + K[,,k] %*% (z[k,] - H %*% xhatminus[k,])
    #update error covariance
    P[,,k] = (diag(2) - K[,,k]%*% H) %*% Pminus[,,k]
  }
  
  return(data.frame(TimeStamp = day$TimeStamp, Latitude_kalman = xhat[, 2], Longitude_kalman = xhat[, 1]))
  
}





getDataset = function(directory){
  print("Data acquisition...")
  # Set directory
  setwd(directory)
  # Variables names
  variablesName <- c("TimeStamp",
                     "Latitude",
                     "Longitude",
                     "Altitude",
                     "Accuracy",
                     "Bearing",
                     "Speed",
                     "Satellites",
                     "Provider",
                     "hdop",
                     "vdop",
                     "pdop",
                     "geoidheight",
                     "ageofdgpsdata",
                     "ggpsid",
                     "activity" )
  # Files Reading
  partecipants <- list.files()
  # initalise dataset
  df=NULL
  # for each participant
  for(p in partecipants){
    # get name of all files in participants folder 
    pDirectory = paste0(directory,p)
    filesList = list.files(path =pDirectory,
                           pattern = ".txt")
    # initialise session and counter
    sessions=NULL; i = 1
    for(f in filesList){
      # load data from file
      sessionData <- read.table(file = paste0(pDirectory,
                                              "/",
                                              f),
                                header = TRUE,
                                sep=",")
      # if there are data
      if(nrow(sessionData)>0){
        # rename columns
        names(sessionData) <- variablesName
        # select only relevant columns
        sessionData=sessionData[,c("Latitude",
                                   "Longitude",
                                   "Altitude",
                                   "Bearing",
                                   "Accuracy",
                                   "Speed",
                                   "TimeStamp",
                                   "Provider")]
        # transform time stamp into date
        sessionData$TimeStamp <- as.POSIXct(strptime(sessionData$TimeStamp, "%Y-%m-%dT%H:%M:%S")) 
        # order data by time stamp
        sessionData = sessionData[order(sessionData$TimeStamp),]
        # remove duplicates
        sessionData = unique(sessionData)
        # assign session ID
        sessionData$sessionid = i
        # add current section to sessions data
        sessions <- rbind(sessions,
                          sessionData)
        # increase counter
        i = i+1
      }
    }
    # add PatID to dataset
    sessions$patient = p
    # add to overall dataset
    df = rbind(df,
               sessions)
  }
  
  # Set type of variables
  df$patient = as.factor(df$patient)
  df$sessionid = as.factor(df$sessionid)
  df$Latitude = as.numeric(as.character(df$Latitude))
  df$Longitude = as.numeric(as.character(df$Longitude))
  df$Altitude = as.numeric(as.character(df$Altitude))
  df$Bearing = as.numeric(as.character(df$Bearing))
  df$Speed = as.numeric(as.character(df$Speed))
  df$Accuracy = as.numeric(as.character(df$Accuracy))
  df$Provider=as.factor(df$Provider)
  df$X = c(1:nrow(df))
  print("Data acquisition completed.")
  return (df)
}

getDatasetGPX = function(directory){
  # Set directory
  setwd(directory)
  # Files Reading
  partecipants <- list.files()
  df=NULL
  for(p in partecipants){
    pDirectory = paste0(directory,"\\",p)
    setwd(pDirectory)
    filesList = list.files()
    sessions=NULL; i = 1
    print(p)
    for(f in filesList){
      print(f)
      gpx = readGPX(f)
      tracks = gpx$tracks
      Longitude = tracks[[1]][[1]]$lon
      Latitude = tracks[[1]][[1]]$lat
      TimeStamp = tracks[[1]][[1]]$time
      sessionData = data.frame(Latitude, Longitude, TimeStamp)
      sessionData$TimeStamp = as.POSIXct(strptime(sessionData$TimeStamp, "%Y-%m-%dT%H:%M:%SZ"))
      sessionData$sessionid = i
      sessions = rbind(sessions,sessionData)
      i=i+1
    }
    sessions$patient = p
    df = rbind(df,sessions)
  }
  df$X = c(1:nrow(df))
  return (df)
}

dateBasedCleaning = function(df, sfd){
  datePatients = unique(sfd[,c("patient","date")])
  df$date = as.Date(df$TimeStamp)
  output = NULL
  for(i in 1:nrow(datePatients)){
    tmp = NULL
    tmp = df[df$patient==datePatients$patient[i] & df$date==datePatients$date[i],]
    output = rbind(output,tmp)
  }
  output$date = NULL
  return(output)
}

dateBasedCleaningSF = function(df, sfd){
  df$date = as.Date(df$TimeStamp)
  datePatients = unique(df[,c("patient","date")])
  output = NULL
  for(i in 1:nrow(datePatients)){
    tmp = NULL
    tmp = sfd[sfd$patient==datePatients$patient[i] & sfd$date==datePatients$date[i],]
    output = rbind(output,tmp)
  }
  return(output)
}

timeBasedMethod <- function(df, timeThreshold, distanceThreshold){
  print("Geolocations visited detection in progress...")
  df$geoVisited = FALSE
  
  # temporary data frame with information:
  # 1) time difference between consecutive GPS points (i.e., dwell time)
  # 2) distance difference between consecutive GPS points
  tmp = getMediumSpeed(df)
  
  # A geolocation visited was identified when: 
  # 1) the time difference between two consecutive GPS data points exceeded a time threshold 
  # 2) their distance was less than a distance threshold  
  tmp.geovisited=tmp[tmp$dwellTime >= timeThreshold & tmp$deltaDistance<=distanceThreshold,]
  
  if(nrow(tmp.geovisited)>0){
    for(i in 1:dim(tmp.geovisited)[1]){
      df[df$X==(tmp.geovisited$xPrev[i]),]$geoVisited=TRUE
    }
    geo.visited = df[df$geoVisited==TRUE,]
    geo.visited = geo.visited[,c("patient","sessionid","Latitude","Longitude")]
  } else{
    geo.visited = NULL
  }
  return(geo.visited)
}

getMediumSpeed <- function(df){
  dfTemp = data.frame(df, 
                      xPrev = c(NA, df$X[-nrow(df)]),
                      patientPrev = c(NA, as.character(df$patient[-dim(df)[1]])), 
                      sessionIdPrev = c(NA, as.character(df$sessionid[-dim(df)[1]])),    
                      timeStampPrev = c(NA, as.character(df$TimeStamp[-dim(df)[1]])),
                      latPrev = c(NA, df$Latitude[-dim(df)[1]]),
                      longPrev = c(NA, df$Longitude[-dim(df)[1]]))
  # NA raw removal
  dfTemp = dfTemp[dfTemp$patient==dfTemp$patientPrev & 
                    dfTemp$sessionid==dfTemp$sessionIdPrev & 
                    !is.na(dfTemp$patientPrev) &
                    !is.na(dfTemp$latPrev) &
                    !is.na(dfTemp$longPrev), ]
  
  dfTemp$timeStampPrev = as.POSIXct(strptime(dfTemp$timeStampPrev, "%Y-%m-%d %H:%M:%S"))
  dfTemp$dwellTime = as.numeric(difftime(dfTemp$TimeStamp,dfTemp$timeStampPrev,units="secs"))
  dfTemp$deltaDistance = getDistance(dfTemp$Latitude, dfTemp$Longitude, dfTemp$latPrev, dfTemp$longPrev)
  dfTemp$mediumSpeed = (dfTemp$deltaDistance/dfTemp$dwellTime)
  
  return(dfTemp)
}

getDistance <- function(lat1, long1, lat2, long2){
  # Generic function that given two GPS data points, returns their distance in meters
  ER = 6371000 # Earth medium radius in m
  phi1 = lat1 * pi / 180
  phi2 = lat2 * pi / 180
  lam1 = long1 * pi / 180
  lam2 = long2 * pi / 180
  x = (lam2 - lam1) * cos((phi1 + phi2)/2)
  y = (phi2 - phi1)
  distance = ER* sqrt((x*x) + (y*y))
  return(distance)
}

densityBasedMethod <- function(df,timeThreshold,radius, cluster = TRUE){
  # Patient and sessions
  ps=df[,c("patient","sessionid")]
  ps=ps[!duplicated(ps),]
  geo.visited = NULL
  # For each patient, for each session
  for(row in 1:dim(ps)[1]){
    df.tmp = df[df$patient == ps$patient[row] & df$sessionid == ps$sessionid[row],]
    # For each point 
    j = 1
    while(j <=nrow(df.tmp)){
      # take 10 minutes points
      tenMinPoints = getDeltaTimeMinutesPoints(df.tmp, j, timeThreshold)
      # center calculation
      tenMinPoints = getCenter(tenMinPoints)
      # distance from the center 
      tenMinPoints$distance = getDistance(tenMinPoints$latC, tenMinPoints$longC, tenMinPoints$Latitude, tenMinPoints$Longitude)
      # points in a circle?
      inCircle = arePointsInCircle(tenMinPoints,radius)
      if(inCircle==TRUE){
        
        # decide whether to report the centroid of the geolocation visited or the actual GPS data points
        if(cluster){
          
          geoVisited = data.frame(patient = ps$patient[row],
                                  sessionid = ps$sessionid[row],
                                  Latitude = tenMinPoints$latC[1],
                                  Longitude = tenMinPoints$longC[1])
          
        }else{
          
          geoVisited = data.frame(patient = ps$patient[row],
                                  sessionid = ps$sessionid[row],
                                  Latitude = tenMinPoints$Latitude,
                                  Longitude = tenMinPoints$Longitude)
          
        }
        
        geo.visited = rbind(geo.visited,
                            geoVisited)
        
        xEnd = tenMinPoints$X[nrow(tenMinPoints)]
        j = nrow(df.tmp[df.tmp$X<=xEnd,])
      }
      j = j+1
    }
  }
  return(geo.visited)
}

getDeltaTimeMinutesPoints <- function(df, start, timeThreshold){
  if(start+1 > nrow(df)){
    return(df[start,])
  }
  tStart = df$TimeStamp[start]
  tEnd = tStart + timeThreshold
  if(tEnd <= df$TimeStamp[nrow(df)]){
    tmp = df[df$TimeStamp>=tStart & df$TimeStamp<=tEnd,]
  }else{
    tmp = df[start:nrow(df),]
  }
  return(tmp)
}

getCenter <- function(df){
  df$latC = mean(df$Latitude)
  df$longC = mean(df$Longitude)
  return(df)
}

arePointsInCircle <- function(df, radius){
  if(sum(df$distance <= radius) == dim(df)[1]){
    return (TRUE)
  }
  return (FALSE)
}

spaceClustering <- function(geo.visited, distanceThreshold){
  # Create a new variable
  print("Space clustering in progress...")
  geo.visited$placeID = "" 
  # Give an ID (x) to each row
  geo.visited$X = c(1:nrow(geo.visited))
  geo.visited$X = as.factor(geo.visited$X)
  geo.visited$CenterLat = 0
  geo.visited$CenterLong = 0
  patients = unique(geo.visited[,c("patient")])
  for(p in patients){
    # initialise cluster counter
    clusterID = 1
    # get all patients visited geolocations
    tmp = geo.visited[geo.visited$patient==p,]
    # initialise centroid at the first point
    center = c(tmp$Latitude[1],tmp$Longitude[1])
    # for each visited geolocation
    while(nrow(tmp)>0){
      # calculate the distance between the centre and all remaining visited geolocations
      tmp$distance = getDistance(center[1],center[2], tmp$Latitude,tmp$Longitude)
      # keep only the ones with a distance to the centre lower than the threshold
      cluster = tmp[tmp$distance<=distanceThreshold,]
      # if there is only one point in the cluster
      if(nrow(cluster)==1){
        # point is cluster on its own
        geo.visited[geo.visited$X==tmp$X[1],]$placeID = paste0("",clusterID)
        geo.visited[geo.visited$X==tmp$X[1],]$CenterLat = center[1]
        geo.visited[geo.visited$X==tmp$X[1],]$CenterLong = center[2]
        # remove point from data to process
        tmp = tmp[-1,]
        # initialise the centre at the next data point to be processed
        center = c(tmp$Latitude[1],tmp$Longitude[1])
        # increment cluster counter
        clusterID = clusterID+1
      }else{
        # calculate cluster center coordinates
        cluster$latC = mean(cluster$Latitude)
        cluster$longC = mean(cluster$Longitude)
        # if cluster coordinates equal to the ones at the previous iteration
        if(cluster$latC[1]==center[1] & cluster$longC[1]==center[2]){
          # assign visited geolocations to current cluster and remove them from the data to process
          for(z in cluster$X){
            geo.visited[geo.visited$X==z,]$placeID = paste0("",clusterID)
            geo.visited[geo.visited$X==z,]$CenterLat = center[1]
            geo.visited[geo.visited$X==z,]$CenterLong = center[2]
            tmp = tmp[!(tmp$X==z),]
          }
          # increment counter
          clusterID = clusterID+1
          # if there are more points assign the center to the next point to be processed
          if(nrow(tmp)>0){
            center = c(tmp$Latitude[1],tmp$Longitude[1])
          }
          
        }else{ # if center different from previous iteration update centre
          center = c(cluster$latC[1],cluster$longC[1])
        }
      }
    }
  }
  clusters= unique(geo.visited[,c("patient","placeID","CenterLat","CenterLong")])
  return(clusters)
}

assignPlaceID <- function(df, places, distanceThreshold){
  print("Classification in progress...")
  df$X = c(1:nrow(df))
  df$placeID=""
  patients <- unique(places[,c("patient")])
  # for each patient
  for(p in patients){
    # get all places visited for each patient
    places.tmp = places[places$patient==p,]
    # get all GPS raw data for each patient
    df.tmp = df[df$patient==p,]
    # set up distance threshold
    df.tmp$distance = distanceThreshold
    # for each place visited
    for(i in 1:nrow(places.tmp)){
      # get first location
      location = places.tmp[i,]
      # calculate distance to current place
      df.tmp$newDistance = getDistance(location$CenterLat[1], location$CenterLong[1], df.tmp$Latitude, df.tmp$Longitude)
      # calculate difference between old and new distance
      df.tmp$diffDistance = df.tmp$distance - df.tmp$newDistance
      # keep only points with new distance lower than old one
      temp = df.tmp[df.tmp$diffDistance>0,]
      # assign new distance and cluster ID
      if(nrow(temp)>0){
        df.tmp[df.tmp$diffDistance>0,]$placeID = location$placeID[1]
        df.tmp[df.tmp$diffDistance>0,]$distance = df.tmp[df.tmp$diffDistance>0,]$newDistance
      }
    }
    # update data frame for the patient
    df[df$patient==p,]$placeID = df.tmp$placeID
  }
  df$X = NULL
  return(df)
}

getPlaceList <- function(df,places){
  print("Places visited detection...")
  placeList = NULL
  df$date = as.Date(df$TimeStamp)
  ps = unique(df[,c("patient","sessionid","date")])
  # for each unique combination of patient, session and date
  for(i in 1:nrow(ps)){
    p = ps$patient[i]
    s = ps$sessionid[i]
    d = ps$date[i]
    # get data
    tmp = df[df$patient==p & df$sessionid==s & df$date==d,]
    tmp$x = c(1:nrow(tmp))
    # keep only data with an assigned place
    tmp = tmp[tmp$placeID!="",]
    jStart = 1
    nrow(tmp)
    j = 1
    # for each row
    while(j <= (nrow(tmp)-1)){
      cond1 = ((tmp$x[j]+1)!=tmp$x[j+1])
      cond2 = ((j+1)==nrow(tmp)) 
      cond3 = (tmp$placeID[j] != tmp$placeID[j + 1])
      # if they are not consecutive or got to the end
      if(cond1 | cond2 | cond3){
        intervalTime = interval(tmp$TimeStamp[jStart],tmp$TimeStamp[j])
        place = data.frame(patient = p,
                           sessionid = s,
                           placeID=tmp$placeID[j],
                           latitude = places[places$placeID==tmp$placeID[j] & places$patient==p,]$CenterLat,
                           longitude = places[places$placeID==tmp$placeID[j] & places$patient==p,]$CenterLong,
                           intervalTime = intervalTime)
        placeList = rbind(placeList,place)
        jStart = j+1
      }
      j = j+1
    }
  }
  return(placeList)
}

place_activity_recognition = function(places.visited, key, filesDirectory){
  
  places.visited = places.visited[as.numeric(as.duration(places.visited$intervalTime)) >= (5*60),] # only activities lasting for more than 5 minutes
  places.visited = mergePlacesVisited(places.visited, 10*60) # merge activity for same place and within 10 minutes
  places.visited$placeType=""; places.visited$activityType=""; places.visited$activityCategory=""; # initialise variables for categorisation
  places.visited$duration = as.duration(places.visited$intervalTime); places.visited$sessionid=NULL
  # Find the home
  print("Home recognition in progress...")
  activities = getHome(places.visited)
  # Find other activities 
  print("Other activity recognition in progress...")
  activities = getOtherActivities(activities,key, filesDirectory)
  # Find the work place
  print("Work recognition in progress...")
  activities = getWorkPlace(activities, 4*60*60)
  # Clear activities with duration less than 5 min
  activities$duration = as.numeric(activities$duration)
  #activities = activities[activities$duration>=(5*60),]
  # Dataset cleaning
  activities$duration=NULL # remove duration 
  activities$placeID=NULL # remove place ID
  return(activities)
}

mergePlacesVisited = function(places.visited, threshold = 600){
  
  ps <- unique(places.visited[,c("patient","sessionid")]) # get all part and sessions
  
  # for each session
  for(i in 1:nrow(ps)){ #
    # get places visited in each session
    tmp = places.visited[places.visited$patient==ps$patient[i] & places.visited$sessionid==ps$sessionid[i],] 
    # if there are places visited
    if(nrow(tmp)>1){
      jStart = -1
      for(j in 1:(nrow(tmp)-1)){
        loc1 = as.numeric(as.character(tmp$placeID[j])) 
        loc2 = as.numeric(as.character(tmp$placeID[j+1]))
        end1 = int_end(tmp$intervalTime[j])
        start2 = int_start(tmp$intervalTime[j+1])
        diff = as.numeric(difftime(start2, end1, units = "secs"))
        if((loc1==loc2) & (diff<=threshold)){ # if same location and time difference < threshold
          if(jStart==-1){
            jStart = j
          }
          tmp[jStart:(j+1),]$intervalTime = interval(int_start(tmp$intervalTime[jStart]),int_end(tmp$intervalTime[j+1])) # recalculate interval time
        }else{
          jStart = -1 # if not same location or more than time threshold re-initialise pointer
        }
      }
      places.visited[places.visited$patient==ps$patient[i] & places.visited$sessionid==ps$sessionid[i],] = tmp
    }
  }
  places.visited$duration = as.numeric(as.duration(places.visited$intervalTime))
  places.visited = unique(places.visited)
  return(places.visited)
}



# mergePlacesVisited = function(places.visited, threshold = 600){
#   ps = unique(places.visited[,c("patient","sessionid")]) # get all part and sessions
#   for(i in 1:nrow(ps)){ # 
#     tmp = places.visited[places.visited$patient==ps$patient[i] & places.visited$sessionid==ps$sessionid[i],] # get places visited in each session
#     if(nrow(tmp)>1){
#       jStart = -1
#       for(j in 1:(nrow(tmp)-1)){
#         loc1 = as.numeric(tmp$placeID[j]) 
#         loc2 = as.numeric(tmp$placeID[j+1])
#         end1 = int_end(tmp$intervalTime[j])
#         start2 = int_start(tmp$intervalTime[j+1])
#         diff = as.numeric(difftime(start2, end1, units = "secs"))
#         if((loc1==loc2) & (diff<=threshold)){ # if same location and time difference < threshold
#           if(jStart==-1){
#             jStart = j
#           }
#           tmp[jStart:(j+1),]$intervalTime = interval(int_start(tmp$intervalTime[jStart]),int_end(tmp$intervalTime[j+1])) # recalculate interval time
#         }else{
#           jStart = -1 # if not same location or more than time threshold re-initialise pointer
#         }
#       }
#       places.visited[places.visited$patient==ps$patient[i] & places.visited$sessionid==ps$sessionid[i],] = tmp
#     }
#   }
#   places.visited$duration = as.numeric(as.duration(places.visited$intervalTime))
#   places.visited = unique(places.visited)
#   return(places.visited)
# }





# getHome <- function(places.visited){
#   nightPlaces = getNightPlaces(places.visited)
#   activities = homeFinder(nightPlaces, places.visited)
#   return(activities)
# }


getHome <- function(places.visited){
  
  
  places.visited <- places.visited %>% 
    mutate(start = intervalTime@start,
           date = as.Date(start))
  # get only rows with max and min time and count occurrences
  possible_homes <- places.visited %>%
    mutate(X = 1:nrow(places.visited)) %>% 
      group_by(patient, date) %>% 
        filter(X == min(X) | X == max(X)) %>% 
          ungroup() %>%
            group_by(patient) %>% 
              count(placeID) %>% 
                arrange(desc(n))
  
  activities = homeFinder(possible_homes, places.visited)
  
  return(activities)

  
}


getNightPlaces <- function(places.visited){
  patients = unique(places.visited[,c("patient")]) 
  nightPlaces = NULL
  for(p in patients){ # for each participant
    tmp = places.visited[places.visited$patient == p,] # get all places visited 
    for(j in 1:nrow(tmp)){ # for each place
      start = int_start(tmp$intervalTime[j]) # get start time
      hour(start)=21;minute(start)=0;second(start)=0 # bring it to 8PM
      end = start + (12*60*60) # add 12 hours
      interval = interval(start = start, end = end) # create interval
      if(int_overlaps(tmp$intervalTime[j],interval)){ # if there is overlap
        place = data.frame(patient = p, placeID = tmp$placeID[j]) # extract place
        nightPlaces=rbind(nightPlaces,place) # put it in the list of night places
      }
    }
  }
  return(nightPlaces)
}

# homeFinder <- function(nightPlaces, places.visited){
#   
#   require("tidyverse")
#   
#   nightPlaces$patient = as.character(nightPlaces$patient)
#   places.visited$patient = as.character(places.visited$patient)
#   #nightPlaces$timeSum = 0
#   pl = unique(nightPlaces[,c("patient","placeID")]) # get all unique night places
#   pl$timeSum = 0
#   
#   maxTimes = data.frame(patient = pl$patient, max=0)
#   
#   for(i in 1:nrow(pl)){
#     p = pl$patient[i]
#     l = pl$placeID[i]
#     pl[pl$patient==p & pl$placeID==l,]$timeSum = sum(places.visited[places.visited$patient==p & places.visited$placeID==l,]$duration) # cumulative time for each night place
#   }
#   
#   activities = places.visited
#   
#   pl <- pl[order(pl$patient, -pl$timeSum), ]
#   
#   pl <- pl %>% 
#     left_join(activities %>% 
#                 distinct(patient, placeID, longitude, latitude), by = c("patient", "placeID"))
#   
#   for(pat in unique(pl$patient)){ # for each patient
#     
#     #get all night places for each patient
#     df_tmp <- pl %>% 
#       filter(patient == pat) 
#     
#     # find most likely place around residency area
#     placeID <- findHomeInResidenceCity(df_tmp)
#     
#     if(placeID > 0){
#       
#       activities[activities$patient==pat & activities$placeID==placeID,]$activityType = "home"
#       activities[activities$patient==pat & activities$placeID==placeID,]$activityCategory = "home"
#       activities[activities$patient==pat & activities$placeID==placeID,]$placeType = "residential"
#       
#     }
#     
#   }
#   
#   return(activities)
# }

homeFinder <- function(possible_homes, places.visited){
  
  require("tidyverse")
  
  activities = places.visited
  
  pl <- possible_homes %>% 
    left_join(activities %>% 
                distinct(patient, placeID, longitude, latitude, placeType), by = c("patient", "placeID")) %>% 
      filter(placeType %in% c("residential","dormitory", "house")) # only places with tag residential and dormitory
  
  for(pat in unique(pl$patient)){ # for each patient
    
    #get possible homes for each patient
    df_tmp <- pl %>% 
      filter(patient == pat) 
    
    # find most likely place around residency area
    placeID <- findHomeInResidenceCity(df_tmp)
    
    if(placeID > 0){
      
      activities[activities$patient==pat & activities$placeID==placeID,]$activityType = "home"
      activities[activities$patient==pat & activities$placeID==placeID,]$activityCategory = "home"
      activities[activities$patient==pat & activities$placeID==placeID,]$placeType = "residential"
      
    }
    
  }
  
  return(activities)
}





findHomeInResidenceCity <- function(pl, city = "Manchester", nuts = "Greater Manchester", region = "North West"){
  
  # initialise index
  placeID <- -1
  
  pl$city <- pl$nuts <- pl$region <- ""
  
  for(i in 1:nrow(pl)){
    
    # get reverse geocode 
    tmp <- reverse_geocoding(pl$longitude[i], pl$latitude[i])
    # if something was found
    if(!is.null(tmp)){
      pl$city[i] <- ifelse(!is.null(tmp[[1]]$admin_district), tmp[[1]]$admin_district, "")
      pl$nuts[i] <- ifelse(!is.null(tmp[[1]]$nuts), tmp[[1]]$nuts, "")
      pl$region[i] <- ifelse(!is.null(tmp[[1]]$region), tmp[[1]]$region, "")
    }
    
  }
  
  # check if residency city
  if(sum(pl$city == city) > 0){
    
    placeID <- pl[pl$city == city, ]$placeID[1]
  # check if residency area
  } else if(sum(grepl(pattern = paste0("^",nuts), x = pl$nuts)) > 0){
    
    placeID <- pl[grepl(pattern = paste0("^",nuts), x = pl$nuts), ]$placeID[1]
  # check if residency region  
  } else if(sum(pl$region == region) > 0){
    
    placeID <- pl[pl$region == region, ]$placeID[1]
  # check if found night place  
  } else if(nrow(pl) > 0){
    
    placeID <- pl$placeID[1]
    
  }
  
  return(as.numeric(as.character(placeID)))
  
}

getWorkPlace = function(activities, time){
  activities$date = as.Date(int_start(activities$intervalTime)) # dates for each activity
  timeThreshold = time
  pl = unique(activities[,c("patient", "placeID", "date")]) # get all possible combinations with date
  wp = unique(activities[,c("patient", "placeID")]) # get all possible combinations no date
  wp$totTime = 0
  wp$days = 0
  for(i in 1:nrow(pl)){ # for each part, place and date combination
    p = pl$patient[i]
    l = pl$placeID[i]
    d = pl$date[i]
    tmp = activities[activities$patient==p # get data for place
                     & activities$placeID==l
                     & activities$date==d
                     & !(activities$placeType %in% c("residential", "house")),] # exclude already identified residential 

    if(nrow(tmp)>0){
      totDuration = sum(tmp$duration)# sum total duration on the specific date
      if(totDuration>timeThreshold){# check if above threshold
        wp[wp$patient==p & wp$placeID==l,]$days = wp[wp$patient==p & wp$placeID==l,]$days[1] + 1 # increase day of one
        wp[wp$patient==p & wp$placeID==l,]$totTime = wp[wp$patient==p & wp$placeID==l,]$totTime[1] + totDuration # increase total duration
      }
    }
  }
  wp = wp[wp$days>=3,] # keep places with more than 3 days
  if(nrow(wp)>0){
    patients = unique(wp[,c("patient")])
    for(p in 1:length(patients)){
      item = wp[wp$patient==patients[p],] # get all wp for each patient
      max = item[item$totTime == max(item$totTime),] # find the one with more time spent
      activities[activities$patient==patients[p] & activities$placeID==max$placeID[1],]$placeType = "work"
      activities[activities$patient==patients[p] & activities$placeID==max$placeID[1],]$activityType = "working"
      activities[activities$patient==patients[p] & activities$placeID==max$placeID[1],]$activityCategory = "employment"
    }
  }
  return(activities)
}

# WorkPlace <- function(places.visited){
#   dayPlaces = getDayPlaces(places.visited)
#   activities = homeFinder(dayPlaces, places.visited)
#   return(activities)
# }
# 
# getDayPlaces <- function(places.visited){
#   patients = unique(places.visited[,c("patient")]) 
#   dayPlaces = NULL
#   for(p in patients){ # for each participant
#     tmp = places.visited[places.visited$patient == p,] # get all places visited 
#     for(j in 1:nrow(tmp)){ # for each place
#       start = int_start(tmp$intervalTime[j]) # get start time
#       hour(start)=9;minute(start)=0;second(start)=0 # bring it to 9PM
#       end = start + (12*60*60) # add 12 hours
#       interval = interval(start = start, end = end) # create interval
#       if(int_overlaps(tmp$intervalTime[j],interval)){ # if there is overlap
#         place = data.frame(patient = p, placeID = tmp$placeID[j]) # extract place
#         dayPlaces=rbind(dayPlaces,place) # put it in the list of day places
#       }
#     }
#   }
#   return(dayPlaces)
# }
# 
# workFinder <- function(dayPlaces, places.visited){
#   dayPlaces$patient = as.character(dayPlaces$patient)
#   places.visited$patient = as.character(places.visited$patient)
#   dayPlaces$timeSum = 0
#   pl = unique(dayPlaces[,c("patient","placeID")]) # get all unique day places
#   
#   maxTimes = data.frame(patient = pl$patient, max=0)
#   
#   for(i in 1:nrow(pl)){
#     p = pl$patient[i]
#     l = pl$placeID[i]
#     dayPlaces[dayPlaces$patient==p & dayPlaces$placeID==l,]$timeSum = sum(places.visited[places.visited$patient==p & places.visited$placeID==l,]$duration) # cumulative time for each day place
#     maxTimes[maxTimes$patient==p,]$max = max(dayPlaces[dayPlaces$patient==p,]$timeSum) # take note of the max time at this iteration
#   }
#   
#   activities = places.visited
#   
#   for(i in 1:nrow(maxTimes)){ # for each patient
#     p = maxTimes$patient[i]
#     t = maxTimes$max[i]
#     l = dayPlaces[dayPlaces$patient==p & dayPlaces$timeSum==t,]$placeID[1] # get location with the longest time spent
#     activities[activities$patient==p & activities$placeID==l,]$activityType = "work"
#     activities[activities$patient==p & activities$placeID==l,]$activityCategory = "working"
#     activities[activities$patient==p & activities$placeID==l,]$placeType = "employment"
#   }
#   return(activities)
# }

getOtherActivities=function(activities, key, filesDirectory){
  
  activities$patient=as.character(activities$patient)
  activities$placeID=as.character(activities$placeID)
  tmp = activities[activities$placeType!="work" & activities$placeType!="residential", ] # remove homes and work places
  #tmp <- activities
  places = unique(tmp[,c("patient","placeID","latitude","longitude")]) # get unique places
  places$patient = as.character(places$patient)
  places$placeID = as.character(places$placeID)
  
  for(j in 1:nrow(places)){
    place = places[j,]
    # get POIs from Google Place API and Geonames
    placePOI = NULL
    placePOIs = getGooglePlacesPOI(place$latitude, place$longitude, key)
    placePOIs = rbind(placePOIs,
                      getGeonamePOI(place$latitude, place$longitude))
    
    # get residential POIs from OpenStreetMap
    if(is.null(placePOIs)){ # if no place found
      min = 100
    }else{
      min = min(placePOIs$distance)*2 # to see if there is something closer than the min
    }
    placePOIs = rbind(placePOIs,
                      getResidentialPOIs(place$latitude, place$longitude, min))
    if(!is.null(placePOIs)){
      # If there are POIs, it orders by distance and take only POIs within 50 meters
      placePOIs = placePOIs[order(placePOIs$distance),]
      placePOIs = placePOIs[placePOIs$distance<=50,]
      if(nrow(placePOIs)>0){
        # Check the validity of POI and map the activity type and category
        POI = getValidPOI(placePOIs, filesDirectory) # keep only POIs in the ontologies
        activities[activities$patient==place$patient[1] & activities$placeID==place$placeID[1],]$placeType = as.character(POI$placeType) # first one is the closest
        activities[activities$patient==place$patient[1] & activities$placeID==place$placeID[1],]$activityType = as.character(POI$activityType)
        activities[activities$patient==place$patient[1] & activities$placeID==place$placeID[1],]$activityCategory = as.character(POI$activityCategory)
      }
    }
  }
  return(activities)
}

getGooglePlacesPOI = function(latitude, longitude, key){
  # Variables
  type = getGooglePlaceType() # get list of types
  i = 1
  POIs = NULL
  # Request submission
  response = googlePlacesAPI_nearbyResearch(key[i], latitude, longitude, type)
  # if status OK
  if(response$status=="OK"){
    if(length(response)>1){
      POIs = getGooglePOIs(response) 
    }
  }else if(response$status=="OVER_QUERY_LIMIT"){ # if over query limit
    # Change of key and request submission again
    i = i + 1
    response = googlePlacesAPI_nearbyResearch(key[i], latitude, longitude, type)
    # check response status again and extract information
    if(response$status=="OK"){
      POIs = getGooglePOIs(response)
    }else{
      print(response$status)
    }
  }else{
    print(response$status)
  }
  if(!is.null(POIs)){ # if some places were found calculate difference and put label
    POIs$distance = getDistance(latitude, longitude, POIs$placeLatitude, POIs$placeLongitude)
    POIs$flag = "google"
  }
  return(POIs)
}

# googlePlacesAPI_nearbyResearch = function(key, latitude, longitude, type){ # get all places of specific types within a certain threshold of the long and lat provided
#   url=paste("https://maps.googleapis.com/maps/api/place/nearbysearch/xml?location=",latitude,",",longitude,"&radius=100&types=",type,"&key=",key,sep="")
#   # submit the request
#   data=getURL(url)
#   # convert XML to list
#   response=xmlToList(data)
#   return(response)
# }

googlePlacesAPI_nearbyResearch = function(key, latitude, longitude, type){ # get all places of specific types within a certain threshold of the long and lat provided
  url=paste("https://maps.googleapis.com/maps/api/place/nearbysearch/xml?location=",latitude,",",longitude,"&radius=100&types=",type,"&key=",key,sep="")
  # submit the request
  data=getURL(url)
  # convert XML to list
  response=xmlToList(data)
  
  return(response)
}



getGooglePOIs = function(response){
  POIs = NULL
  for(j in 2:length(response)){ # for each returnes POI
    if(length(response[[j]])>4){ # if there is enough information
      POIs = rbind(POIs,
                   newPOI(response[[j]]$name, 
                          response[[j]]$type, 
                          (response[[j]]$geometry$location$lat), 
                          (response[[j]]$geometry$location$lng),
                          ifelse(!is.null(response[[j]]$place_id), response[[j]]$place_id, "")))
    }
  }
  return(POIs)
}

newPOI = function(placeName, placeType, placeLatitude, placeLongitude, placeID){
  if(!is.null(placeName)){
    POI = data.frame(name = as.character(placeName),
                     placeType = as.character(placeType),
                     placeLatitude= as.numeric(placeLatitude),
                     placeLongitude= as.numeric(placeLongitude),
                     place_id = placeID)
  }else{
    POI = data.frame(name = "no name",
                     placeType = as.character(placeType),
                     placeLatitude= as.numeric(placeLatitude),
                     placeLongitude= as.numeric(placeLongitude),
                     place_id = placeID)
  }
  return(POI)
}

getGooglePlaceType = function(){
  return("accounting|airport|amusement_park|aquarium|art_gallery|bakery|bank|bar|beauty_salon|bicycle_store|book_store|bowling_alley|bus_station|cafe|campground|car_dealer|car_rental|car_repair|car_wash|casino|church|city_hall|clothing_store|convenience_store|courthouse|dentist|department_store|doctor|electrician|electronics_store|embassy|finance|fire_station|florist|food|funeral_home|furniture_store|grocery_or_supermarket|gym|hair_care|hardware_store|health|hindu_temple|home_goods_store|hospital|insurance_agency|jewelry_store|laundry|lawyer|library|liquor_store|local_government_office|locksmith|meal_delivery|meal_takeaway|mosque|movie_rental|movie_theater|museum|night_club|painter|park|pet_store|pharmacy|physiotherapist|place_of_worship|plumber|police|post_office|real_estate_agency|restaurant|roofing_contractor|rv_park|school|shoe_store|shopping_mall|spa|stadium|storage|store|subway_station|synagogue|taxi_stand|train_station|transit_station|travel_agency|university|veterinary_care|zoo")
}

getGeonamePOI = function(latitude, longitude){
  POIs = NULL
  url = paste("api.geonames.org/findNearbyPOIsOSM?lat=",latitude,"&lng=",longitude,"&radius=",0.1,"&username=universityofmanchest") 
  url = URLencode(url)
  # submit the request
  output=getURL(url)
  # convert XML to list
  response=xmlToList(output)
  if(!is.null(response)){
    for(j in 1: length(response)){
      if(!is.null(response[j]$poi)){
        POIs = rbind(POIs,
                     newPOI(response[j]$poi$name,
                            response[j]$poi$typeName,
                            (response[j]$poi$lat),
                            (response[j]$poi$lng),
                            ""))
      }
    }
    POIs$distance = getDistance(latitude, longitude, POIs$placeLatitude, POIs$placeLongitude)
    POIs$flag = "geonames"
  }
  return(POIs)
}

getResidentialPOIs <- function(latitude, longitude, squareSize){
  # Output variable
  POIs = NULL
  
  src = osmsource_api()
  centerBox = center_bbox(longitude, latitude, squareSize, squareSize)
  map = get_osm(centerBox, source = src)
  ontology = data.frame(key = c("building","building"), value = c("residential", "house"))
  ontology$key = as.character(ontology$key)
  ontology$value = as.character(ontology$value)
  
  for(j in 1:nrow(map$ways$tags)){
    tag = map$ways$tags[j,]
    item = ontology[ontology$key==as.character(tag$k) & ontology$value==as.character(tag$v),]
    if(nrow(item)==1){
      POIs = rbind(POIs,
                   newPOI("building",
                          item$value,
                          latitude,
                          longitude,
                          ""))
    }
  }
  if(!is.null(POIs)){
    POIs$distance = 0
    POIs$flag = "osm"
  }
  return(POIs)
}

getValidPOI = function(placePOIs, filesDirectory){
  placePOIs$placeType = as.character(placePOIs$placeType)
  # Ontology acquisition
  setwd(filesDirectory)
  osmOntology = read.xlsx("Ontology.xlsx", sheetIndex = 1, header = TRUE)
  osmOntology$value = as.character(osmOntology$value)
  googleOntology = read.xlsx("Ontology.xlsx", sheetIndex = 2, header = TRUE)
  googleOntology$place_type = as.character(googleOntology$place_type)
  valid = FALSE
  j = 1
  
  while((!valid) & (j<=nrow(placePOIs))){ # until not valid and there are rows
    POI = placePOIs[j,]
    if(POI$flag=="google"){
      item = googleOntology[googleOntology$place_type==POI$placeType,]
      if(nrow(item)==1){ # if there is an activity type and category
        POI$activityType=as.character(item$activity)
        POI$activityCategory= as.character(item$category)
        valid = TRUE
      }
    }else{ 
      item = osmOntology[osmOntology$value==POI$placeType,] # if there is an activity type and category
      if(nrow(item)>0){
        POI$activityType=as.character(item$activity[1])
        POI$activityCategory= as.character(item$category[1])
        valid = TRUE
      }
    }
    j = j+1
  }
  if(!valid){
    POI$placeType=""
    POI$activityType=""
    POI$activityCategory=""
  }
  
  return(POI)
}

getActivityList = function(activities){
  pd = unique(activities[,c("patient","date")])
  activityList = NULL
  for(i in 1:nrow(pd)){ # for each patient and date
    p = pd$patient[i]
    d = pd$date[i]
    tmp = activities[activities$patient==p & activities$date==d,]
    if(nrow(tmp)>1){
      for(j in 1:(nrow(tmp)-1)){ # for each row
        at1 = tmp$activityType[j];   
        at2 = tmp$activityType[j+1];
        if(at1!=at2){ # if different activity 
          activityList = rbind(activityList,
                               tmp[j,])
        }
      }
    }else{
      activityList = rbind(activityList,
                           tmp)
    }
    
  }
  activityList = activityList[,c("patient","date","placeType","activityType","activityCategory")]
  return(activityList)
}

getDailyActivities = function(activities){
  output = unique(activities[,c("patient","date","activityType", "activityCategory")])
  return(output)
}

getDailyCategoriesActivities = function(activities){
  output = unique(activities[,c("patient","date", "activityCategory")])
  return(output)
}

getPerformance = function(sfd, activityList){
  # initialise variables
  meanRecall = 0; meanPrecision = 0 
  
  activityList = unique(activityList[,c("patient","date","activityCategory")])
  
  patients = unique(sfd$patient)
  
  performance = data.frame(patient=patients, recall=0, precision=0, n_activities=0)
  
  for(p in patients){ # for each patient
    # get SF diary
    sfd.tmp = sfd[sfd$patient==p,] 
    # get activities list
    activityList.tmp = activityList[activityList$patient==p,]
    # calculate confusion matrix
    confusionMatrix = getConfusionMatrix(sfd.tmp, 
                                         activityList.tmp)
    #calculate performance
    recall = (confusionMatrix[1,1]/(confusionMatrix[1,1]+confusionMatrix[2,1]))
    precision = (confusionMatrix[1,1]/(confusionMatrix[1,1]+confusionMatrix[1,2]))
    n_activities = confusionMatrix[1,1] + confusionMatrix[2,1]
   
    # add performance to performance df
    performance[performance$patient==p, ]$recall = recall
    performance[performance$patient==p, ]$precision = precision
    performance[performance$patient==p, ]$n_activities = n_activities
    #update overall performance variables
    meanRecall = meanRecall + (recall*(n_activities)) 
    meanPrecision = meanPrecision + (precision*(n_activities))
  }
  #calculate overall performance
  meanRecall = meanRecall/sum(performance$n_activities)
  meanPrecision = meanPrecision / sum(performance$n_activities)
  sdRecall = wheightedSD(meanRecall,length(patients),performance$n_activities, performance$recall)
  sdPrecision = wheightedSD(meanPrecision,length(patients),performance$n_activities, performance$precision)
  #create overall performance row
  mean = c("mean (sd)", 
           paste0(round(meanRecall, 3)," (",round(sdRecall, 3),")"),
           paste0(round(meanPrecision, 3)," (",round(sdPrecision, 3),")"),
           paste0(round(mean(performance$n_activities), 0), " (", round(sd(performance$n_activities), 0), " )"))
  #format data
  performance$patient = as.character(performance$patient)
  performance$recall = format(round(performance$recall, 3), nsmall = 3)
  performance$precision = format(round(performance$precision, 3), nsmall = 3)
  performance = rbind(performance, mean)
  return(performance)
}


# getConfusionMatrix = function(sfd,activityList){
#   matrix = matrix(0,nrow = 2, ncol = 2)
#   TP = 0; FN = 0; FP = 0
#   dates = unique(sfd$date)
#   for(j in 1:length(dates)){ # for each date
#     sfd.tmp = sfd[sfd$date==dates[j],]
#     activityList.tmp = activityList[activityList$date==dates[j],]
#     categories = unique(sfd.tmp[,c("activityCategory")]) # get all unique categories for the day
#     k = 1
#     while(k<=length(categories)){ # for each category
#       sfd.c = sfd.tmp[sfd.tmp$activityCategory==categories[k],] # get all SF activities with Kth category
#       act.c = activityList.tmp[activityList.tmp$activityCategory==categories[k] & !is.na(activityList.tmp$activityCategory),] # get all GPS activities with Kth category
#       TP = TP + nrow(act.c) # true positive as the number of activities found with same category
#       if(nrow(sfd.c)>nrow(act.c)){
#         FN = FN + (nrow(sfd.c)-nrow(act.c)) # false negative as the difference between the activities in the activity diary and GPS activities
#       }
#       k = k+1
#     }
#   }
#   dates = unique(activityList$date)
#   for(j in 1:length(dates)){ # the other way around for false positives 
#     sfd.tmp = sfd[sfd$date==dates[j],]
#     activityList.tmp = activityList[activityList$date==dates[j],]
#     categories = unique(activityList.tmp[,c("activityCategory")])
#     k = 1
#     while(k<=length(categories)){
#       sfd.c = sfd.tmp[sfd.tmp$activityCategory==categories[k],]
#       act.c = activityList.tmp[activityList.tmp$activityCategory==categories[k],]
#       if(nrow(act.c)>nrow(sfd.c)){
#         FP = FP + (nrow(act.c)-nrow(sfd.c))
#       }
#       k = k+1
#     }
#   }
#   matrix[1,1]=TP
#   matrix[1,2]=FP
#   matrix[2,1]=FN
#   return(matrix)
# }

getConfusionMatrix = function(sfd,activityList){
  
  # initialise variables 
  matrix = matrix(0,nrow = 2, ncol = 2)
  TP = 0; FN = 0; FP = 0
  dates = unique(sfd$date)
  
  for(j in 1:length(dates)){ # for each date
    sfd.tmp = sfd[sfd$date==dates[j],]
    activityList.tmp = activityList[activityList$date==dates[j],]
    
    # TP as activity category in both the algo category and the diary category
    TP.j <- nrow(activityList.tmp %>% 
                   filter(activityCategory %in% sfd.tmp$activityCategory))
    # FN as the difference between the number of activites in the diary and the TP
    FN.j <- nrow(sfd.tmp) - TP.j
    # FP as the difference between the number of activities found by the algo and the TP
    FP.j <- nrow(activityList.tmp) - TP.j
    
    #update overall patients results
    TP <- TP + TP.j
    
    FN <- FN + FN.j
    
    FP <- FP + FP.j
    
  }
  
  matrix[1,1] <-  TP
  matrix[1,2] <-  FP
  matrix[2,1] <-  FN
  
  return(matrix)
}

wheightedSD = function(mean, M, w, x){
  sd = 0
  for(i in 1:length(x)) {
    diff = (x[i]-mean)^2
    sd = sd + (w[i]*diff)
  }
  den = ((M-1)/M)*sum(w)
  sd = sd/den
  sd = sqrt(sd)
  return(sd)
}

getDataSummary = function(df, sfd){
  df$date = as.Date(df$TimeStamp)
  df$patient=as.character(df$patient)
  patients = unique(df$patient)
  summary = data.frame(participant = patients)
  summary$participant = as.character(summary$participant)
  summary$days = 0
  summary$hours = 0
  summary$activities = 0
  sfd = getListActivitiesSFD(sfd)
  for(i in 1:length(patients)){
    p = summary$participant[i]
    tmp = df[df$patient==p,]
    summary$days[i] = length(unique(tmp$date))
    summary$hours[i] = round(getTotalHoursParticipant(tmp), 1)
    summary$activities[i] = nrow(sfd[sfd$patient==p,])
    summary$unique_activities[i] = length(unique(sfd[sfd$patient==p, ]$activityCategory))
  }
  sum = c("TOTAL",
          sum(summary$days),
          round(sum(summary$hours), 1),
          sum(summary$activities),
          sum(summary$unique_activities))
  mean = c("mean (sd)",
           paste0(round(mean(summary$days), 1), " (",round(sd(summary$days), 1),")"),
           paste0(round(mean(summary$hours), 1), " (",round(sd(summary$hours), 1),")"),
           paste0(round(mean(summary$activities), 1), " (",round(sd(summary$activities), 1),")"),
           paste0(round(mean(summary$unique_activities), 1), " (",round(sd(summary$unique_activities),1),")"))
  
  summary$days = as.character(summary$days)
  summary$hours = as.character(summary$hours)
  summary$activities = as.character(summary$activities)
  summary$unique_activities = as.character(summary$unique_activities)
  summary = rbind(summary,
                  sum,
                  mean)
  return(summary)
}

getTotalHoursParticipant = function(df){
  dates = unique(df[,c("date")])
  hours = 0
  for(j in 1:length(dates)){
    d = dates[j]
    tmp = df[df$date==d,]
    tmp = tmp[order(tmp$TimeStamp),]
    h = as.numeric(difftime(tmp$TimeStamp[nrow(tmp)],tmp$TimeStamp[1],units="hours"))
    hours = hours + h
  }
  return(hours)
}

getListActivitiesSFD = function(sfd){
  dates = unique(sfd[,c("patient","date")])
  output = NULL
  for(k in 1:nrow(dates)){ # for each patient for each date
    d = dates$date[k]
    p = dates$patient[k]
    tmp = sfd[sfd$patient==p & sfd$date==d,]
    tmp = tmp[,c("patient","date","activityCategory")]
    # tmp = rbind(tmp,
    #             data.frame(patient=tmp$patient[1],
    #                        date = tmp$date[1],
    #                        activityCategory = "home"))
    output = rbind( output,
                    unique(tmp))
  }
  
  return(output)
}

plotMap = function(coordinates){
  # costant
  variation = 0.0009
  rangeLat = range(coordinates$latitude)
  rangeLong = range(coordinates$longitude)
  location = c(rangeLong[1]-variation,rangeLat[1]-variation,rangeLong[2]+variation,rangeLat[2]+variation)
  map = get_map(location = location, source = "osm")
  map1 = ggmap(map)
  map1 = map1 + geom_point(data = coordinates, aes(x=longitude, y=latitude), size = 5, col = "red") 
  map1
}


getNPV = function(activityList){ 
  pd = unique(activityList[,c("patient","date")])
  activityList = activityList[!(activityList$activityCategory=="home"),]
  NPV = pd
  NPV$NPV = 0
  activityList = 
    for(j in 1:nrow(pd)){
      p = pd$patient[j]
      d = pd$date[j]
      tmp = activityList[activityList$patient==p & activityList$date==d,]
      NPV[NPV$patient==p & NPV$date==d,]$NPV = nrow(tmp)
    }
  return(NPV)
}

getOutOfHomeHours = function(activities){
  pd = unique(activities[,c("patient","date")])
  outOfHomeHours = pd
  outOfHomeHours$hours = 0
  for(j in 1:nrow(pd)){
    p = pd$patient[j]
    d = pd$date[j]
    tmp = activities[activities$patient==p & activities$date==d,]
    if(tmp$activityCategory[1]=="home"){
      start = int_end(tmp$intervalTime[1])
    }else{
      start = int_start(tmp$intervalTime[1])
    }
  }
}

activitiesAcquisition = function(datasetType, pipeline){
  activitiesLabel = paste0(datasetType," activities ",pipeline, " method.xlsx")
  activities = read.xlsx(activitiesLabel, sheetIndex = 1)
  dates <- t(data.frame(strsplit(as.character(activities$intervalTime),"--")))
  start= as.POSIXct((strptime(dates[,1], "%Y-%m-%d %H:%M:%S GMT")))
  end= as.POSIXct((strptime(dates[,2], "%Y-%m-%d %H:%M:%S GMT")))
  activities$intervalTime = interval(start, end)
  activities$NA.=NULL
  activities$activityCategory=as.character(activities$activityCategory)
  activities$method = pipeline
  activities = activities[,c("patient","latitude","longitude","activityType","activityCategory","method","date")]
  return(activities)
}


plotMapComparison <- function(df){
  
  m = unique(df$method)
  # costant
  variation = 0.0025
  
  rangeLat = range(df$latitude)
  rangeLong = range(df$longitude)
  
  location = c(rangeLong[1]-variation,rangeLat[1]-variation,rangeLong[2]+variation,rangeLat[2]+variation)
  print(location)
  map = get_map(location = location, source = "osm")
  map = ggmap(map)
  
  map1 =  map + geom_point(data = df[df$method==m[1],], aes(x=longitude, y=latitude), size = 6, colour = "red" ) + ggtitle("Time-based pipeline")
  map2 =  map + geom_point(data = df[df$method==m[2],], aes(x=longitude, y=latitude), size = 6, colour = "red" ) + ggtitle("Density-based pipeline")
  map3 =  map + geom_point(data = df[df$method==m[3],], aes(x=longitude, y=latitude), size = 6, colour = "red" ) + ggtitle("Combination of pipelines")
  
  fileName = paste0("Comparison_",".png")
  png(filename = fileName, width = 1600, height = 1600, units = "px" )
  multiplot(map1,map2,map3, cols = 3)
  dev.off()
}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


remove_multipolygons_from_polygons <- function(sf_polygons, sf_polygons_geom){
  
  polygons_to_remove <- NULL
  # for each polygon check whether it contains other polygons -> it is a mislabelled multypoligon
  for(i in 1:nrow(sf_polygons_geom)){
    
    st_intersects(sf_polygons_geom[i, ], sf_polygons_geom, sparse = FALSE)
    
  }
  
} 



remove_building_yes <- function(shape, shape_geom){ # remove buildings with no value (e.g. only "yes")
  
  if(!is.null(shape)){
    
    shape <- shape %>%
      filter(value != "yes")
    
    shape_geom <- shape_geom %>% 
      filter(ID %in% shape$ID)
    
  }
  
  return(list(tags = shape, geom = shape_geom))
}



find_overlapping_duration <- function(start1, end1, start2, end2, units = "mins"){
  
  if(!is.na(start1) & !is.na(end1) & !is.na(start2) & !is.na(end2)){
    
    start <- end <- NA
    
    if(start1 > start2){
      
      start <- start1
      
    } else{
      
      start <- start2
      
    }
    
    if(end1 > end2){
      
      end <- end2
      
    } else{
      
      end <- end1
      
    }
    
    if(!is.na(start) & !is.na(start)){
      
      duration <- as.numeric(difftime(end, start, units = units))
      
    }else{
      
      duration <- 0
      
    }
    
  } else{
    
    duration <- 0
    
  }
  
  return(duration)
  
}


create_overlapping_dataset <- function(df_alg, df_diary){
  
  df_diary <- df_diary %>% 
    mutate(intervalTime = interval(time.start, time.end, tzone="GMT"))
  
  df_alg <- df_alg %>% 
    mutate(intervalTime2 = interval(int_start(intervalTime),int_end(intervalTime), tzone = "GMT"))
  
  df_over <- NULL
  
  for(i in 1:nrow(df_alg)){ # for each identified activity
    
    tmp <- df_alg[i, ] # get ith activity
    
    tmp2 <- df_diary %>% 
      filter(patient == tmp$patient) 
    # %>% 
    #     mutate(intervalTime = as.interval(as.character(intervalTime)))
    
    tmp3 <- intersect(tmp2$intervalTime, tmp$intervalTime)
    
  }
  
}


create_osmdata_query_sf <- function(){
  
  opq_string <- "c("
  
  # for each relevant key
  for(j in 1:length(key_values)){ # it builts the query that will be used by the sf package
    
    opq_string <- paste(opq_string,
                        "opq(bbox) %>% add_osm_feature(key_values[",
                        j,
                        "]) %>% osmdata_sf()",
                        ifelse(j != length(key_values), ", ", ""),
                        sep = "")
  }
  
  opq_string <- paste(opq_string, ")", sep = "")
  
  return(opq_string)
  
}

create_osmdata_query_sp <- function(){
  
  opq_string <- "c("
  
  # for each relevant key
  for(j in 1:length(key_values)){
    
    opq_string <- paste(opq_string,
                        "opq(bbox) %>% add_osm_feature(key_values[",
                        j,
                        "]) %>% osmdata_sp()",
                        ifelse(j != length(key_values), ", ", ""),
                        sep = "")
  }
  
  opq_string <- paste(opq_string, ")", sep = "")
  
  return(opq_string)
  
}

extract_info_sf <- function(df, 
                            ontology){
  
  sf_data_info <- sf_data_geom <- NULL
  
  # if there are shapes
  if(!is.null(df)){ 
    # convert to sf
    sf_data <- df %>% 
      mutate(ID =  1:nrow(df)) %>% 
      st_as_sf() %>% 
      st_set_crs(4326) 
    
    
    # extract relevant info
    sf_data_info <- sf_data %>% 
      gather(key = "key", value = "value", -ID, -osm_id, -geometry) %>%
      mutate(value = as.character(value)) %>% # convert to character to match ontology
      semi_join(ontology, by = c("key", "value")) %>% # only records matching ontology
      as_data_frame() %>% 
      select(-geometry)
    
    sf_data_geom <- sf_data %>% 
      filter(ID %in% sf_data_info$ID) # only shapes with relevant info
    
  }
  
  return(list(geom = sf_data_geom, info = sf_data_info))
  
}

calculate_distance <- function(dist, 
                               point, 
                               shape, 
                               type){
  
  for(i in 1:nrow(shape)){
    
    dist <- dist %>%
      bind_rows(
        data.frame(
          ID = shape$ID[i],
          relevant_object_name = type,
          dist_m = min(as.numeric(st_distance(point,shape[i, ])))
        )
      )
    
  }
  
  dist
  
}


calculate_distance_to_each_result_found <- function(sf_places_geom,
                                                    sf_points_geom,
                                                    sf_lines_geom,
                                                    sf_polygons_geom){
  
  # calculate distances to each result found
  dist <- NULL
  
  #POIs
  if(!is.null(sf_points_geom)){
    
    if(nrow(sf_points_geom) > 0){
      
      dist <- calculate_distance(dist, sf_places_geom, sf_points_geom, "sf_points")
      
    }
    
  }
  
  #highways
  if(!is.null(sf_lines_geom)){
    
    if(nrow(sf_lines_geom) > 0){
      
      dist <- calculate_distance(dist, sf_places_geom, sf_lines_geom, "sf_lines")
      
    }
    
  }
  
  #buildings
  if(!is.null(sf_polygons_geom)){
    
    if(nrow(sf_polygons_geom) > 0){
      
      dist <- calculate_distance(dist, sf_places_geom, sf_polygons_geom %>% st_cast("LINESTRING"), "sf_polygons")
      
    }
    
  }
  
  #areas
  # if(!is.null(sf_multipolygons_geom)){
  #   
  #   if(nrow(sf_multipolygons_geom) > 0){
  #     
  #     dist <- calculate_distance(dist, sf_places_geom[i, ], sf_multipolygons_geom %>% st_cast("POLYGON") %>% st_cast("LINESTRING"), "sf_multipolygons")
  #     
  #   }
  #   
  # }
  
  dist
  
}


add_place_classification <- function(sf_places_classified, 
                                     patient, 
                                     placeID, 
                                     shape_info, 
                                     ontology){
  
  sf_places_classified <- sf_places_classified %>% 
    bind_rows(
      data.frame(
        patient = patient,
        placeID = placeID,
        ontology %>% 
          semi_join(shape_info, by = c("key", "value"))
      )
    )
  
  sf_places_classified
}

