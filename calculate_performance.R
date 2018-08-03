source('~/Data/Projects/Club M/Healthy volunteers study/R/Daily-activities-classification/FunctionScript.R', echo=TRUE)
library("readxl")
library("stringr")
library("lubridate")
library("tidyverse")
library("PostcodesioR")

#analysis_type <- "time_based"
#analysis_type <- "density_based"
analysis_type <- "combined"

minutes_threshold <- 10

timeThreshold <- 60*minutes_threshold

load(paste("~/Data/Projects/Club M/Healthy volunteers study/Datasets/",
           analysis_type,
           "/labelling_results",
           timeThreshold,
           ".RData",
           sep = ""))

<<<<<<< HEAD
=======
source('~/Data/Projects/Club M/Healthy volunteers study/R/Daily-activities-classification/FunctionScript.R', echo=TRUE)

>>>>>>> 55efc7a3ba2d7e965afdf99b3d784d0de6a88639
# sf_places_classified <- sf_places_classified %>% 
#   mutate(activityCategory = ifelse(activityType == "outdoor activities", "recreational activities", activityCategory))

############## assign classified places to places visited
places.visited_classified  <- readRDS(paste("~/Data/Projects/Club M/Healthy volunteers study/Datasets/",
                                            analysis_type,
                                            "/places_visited",
                                            timeThreshold,
                                            ".rds",
                                            sep = "")) %>% 
    mutate(duration = as.duration(intervalTime)) %>% 
        left_join(sf_places_classified, by = c("patient","placeID")) # add place info to daily activities

places.visited_classified$date <- as.Date(places.visited_classified$intervalTime@start)

places.visited_classified  <- places.visited_classified %>%
  mergePlacesVisited(threshold = 20*60)# merge together intervals that pertain to same activity, and are consecutive within the time threshold

places.visited_classified <- places.visited_classified %>% 
  getHome() %>% 
    mutate(activityCategory = ifelse(placeType == "hospital",  "employment", activityCategory))  %>% # participants were mostly medical students going to different hospitals for training
  mutate(intervalTime = as.character(intervalTime)) %>% 
<<<<<<< HEAD
  filter(duration > 0)
=======
  filter(duration > 0) %>% 
    select(-start)
>>>>>>> 55efc7a3ba2d7e965afdf99b3d784d0de6a88639

places.visited_classified  <- places.visited_classified %>% 
  separate(col = "intervalTime", into = c("start", "end"), sep = "--") %>% 
  mutate(start = ymd_hms(start),
         intervalTime = as.interval(duration, start = start)) %>%
    select(-start, -end) %>% 
    mergePlacesVisited(threshold = 20*60) %>% 
      mutate(intervalTime = as.character(intervalTime))# merge together intervals that pertain to same activity, and are consecutive within the time threshold

#### Results
# load diary
sfd <- readRDS("~/Data/Projects/Club M/Healthy volunteers study/Datasets/analysible_activity_diary.rds") %>% 
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


# sfd[sfd$activityType == "outdoor activities", ]$activityCategory <- "recreational activities"

# introduce date column

sfd <- sfd %>% 
  mutate(date = as.Date(date))

##### exclude days outside the UK
min_lat <- 49.642879
max_lat <- 59.720106
min_long <- -12.572754
max_long <- 1.623168


days_to_exclude <- places.visited_classified %>% 
  filter(!(latitude >= min_lat &
             latitude <= max_lat &
             longitude >= min_long & 
             longitude <= max_long)) %>% 
  distinct(patient, date)


places.visited_classified <- places.visited_classified %>% 
  anti_join(days_to_exclude, by = c("patient", "date"))

sfd <- sfd %>% 
  anti_join(days_to_exclude, by = c("patient", "date"))

# get unique daily categories found by the algorithm
dailyCategories <- getDailyCategoriesActivities(places.visited_classified) %>% 
  filter(!is.na(activityCategory)) # removing not labelled places that in a real case scenarios would have to be asked to the participant

# get unique categories recorded in diary
sfd.tmp <-  getListActivitiesSFD(sfd)
sfd.tmp$activityCategory <-  tolower(as.character(sfd.tmp$activityCategory))

# calculate recall and precision
performance = getPerformance(sfd.tmp, dailyCategories)

# add number of non classified places per patient
performance_non_class <- sf_places_classified %>%
    group_by(patient) %>%
      summarise(n_unique_places = n(),
                proportion_unique_places_labelled = round(sum(!is.na(activityCategory))/n_unique_places, digits = 2))

# create summary row to add
performance_non_class_mean <- data_frame(patient = "mean (sd)",
                       n_unique_places = paste(
                                            round(mean(performance_non_class$n_unique_places),0),
                                            " (",
                                            round(sd(performance_non_class$n_unique_places), 0),
                                            ")",
                                            sep = ""),
                      mean_proportion_unique_places_labelled = round(sum(performance_non_class$n_unique_places*performance_non_class$proportion_unique_places_labelled)/sum(performance_non_class$n_unique_places), 2),

                       sd = round(wheightedSD(mean = mean_proportion_unique_places_labelled,
                                        M = nrow(performance_non_class),
                                        w = performance_non_class$n_unique_places,
                                        x = performance_non_class$proportion_unique_places_labelled), 2)) %>% 
                        mutate(proportion_unique_places_labelled = paste(mean_proportion_unique_places_labelled,
                                                                         " (",
                                                                         sd,
                                                                         ")",
                                                                         sep = "")) %>% 
                          select(-mean_proportion_unique_places_labelled, -sd)


# append summary row 
performance_non_class <- performance_non_class %>% 
  mutate(patient = as.character(patient),
         n_unique_places = as.character(n_unique_places),
         proportion_unique_places_labelled = as.character(proportion_unique_places_labelled)) %>% 
    bind_rows(performance_non_class_mean)

# append to performance
performance_combined <- performance %>%
  left_join(performance_non_class, by = "patient")


write.csv(performance_combined, paste("~/Data/Projects/Club M/Healthy volunteers study/Analysis/",
                                      analysis_type,
                                      "/performance_OSM",
                                      timeThreshold,".csv", sep = ""))

#calculate performance on number of daily categories
dailyCategories <- getDailyCategoriesActivities(places.visited_classified)

NPV_GPS <- dailyCategories %>% 
  group_by(patient, date) %>% 
    count()

NPV_diary <- sfd.tmp %>% 
  group_by(patient, date) %>% 
    count()

NPV_comp <- NPV_GPS %>% 
  left_join(NPV_diary, by =  c("patient", "date")) %>% # join data to have one row for each patient/date
    rename(GPS = n.x,
           diary = n.y) %>% 
      mutate(diff = diary - GPS,
             diff_squared = diff^2) %>% 
        group_by(patient) %>% 
          summarise(number_of_days = n(),
                 RMSE = sqrt(sum(diff_squared)/number_of_days))  

NPV_comp$activity_n <- as.numeric(performance$n_activities[-dim(performance)[1]])

# calculate summary row
overall_performance <- NPV_comp %>% 
  ungroup() %>% 
    summarise(overall_RMSE = sum(RMSE * activity_n)/sum(activity_n),
              overall_sd = wheightedSD(overall_RMSE,
                                         dim(NPV_comp)[1],
                                         activity_n,
                                         RMSE),
              RMSE = str_c(round(overall_RMSE, 1), " (", round(overall_sd, 1), ")")) %>% 
      mutate(patient = "mean (sd)",
             number_of_days = str_c(round(mean(NPV_comp$number_of_days),0),
                                    " (",
                                    round(sd(NPV_comp$number_of_days),0),
                                    ")",
                                    sep = ""),
             activity_n = str_c(round(mean(NPV_comp$activity_n),0),
                                " (",
                                round(sd(NPV_comp$activity_n),0),
                                ")",
                                sep = "")) %>% 
        select(-overall_RMSE, -overall_sd)

# add summary row
NPV_comp <- NPV_comp %>% 
  mutate(patient = as.character(patient),
         number_of_days = as.character(number_of_days),
         RMSE = as.character(RMSE),
         activity_n = as.character(activity_n)) %>% 
    bind_rows(overall_performance)


write.csv(NPV_comp, paste("~/Data/Projects/Club M/Healthy volunteers study/Analysis/",
                          analysis_type,
                          "/patients_places_visited", timeThreshold,".csv", sep = ""))

# write activities
saveRDS(places.visited_classified, paste("~/Data/Projects/Club M/Healthy volunteers study/Analysis/",
              analysis_type,
              "/activities_OSM",timeThreshold,".rds", sep = ""))

## calculate mean number of unique categories per patient
sfd.tmp %>% 
  group_by(patient) %>% 
    distinct(activityCategory) %>% 
      count() %>% 
        ungroup() %>% 
          summarise(mean = round(mean(n), 0),
                    sd = round(sd(n), 0))