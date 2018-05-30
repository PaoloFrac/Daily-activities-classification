source('~/Data/Projects/Club M/Healthy volunteers study/R/Daily activities classification/FunctionScript.R', echo=FALSE)
library("readxl")
library("stringr")
library("lubridate")
library("tidyverse")
library("PostcodesioR")

minutes_threshold <- 10

timeThreshold <- 60*minutes_threshold

load(paste("~/Data/Projects/Club M/Healthy volunteers study/labelling_results", timeThreshold,".RData"))


############## assign classified places to places visited
places.visited_classified  <- readRDS(paste("C:/Users/paolo/Documents/Data/Projects/Club M/Healthy volunteers study/Datasets/places_visited", timeThreshold,".rds")) %>% 
    #mergePlacesVisited(threshold = 20*60) %>% 
      mutate(duration = as.duration(intervalTime)) %>% 
        left_join(sf_places_classified, by = c("patient","placeID"))


places.visited_classified <- places.visited_classified[as.numeric(places.visited_classified$duration) >= 60, ] # keep only activities that last more than one minute
 
places.visited_classified  <- places.visited_classified %>% 
  mergePlacesVisited(threshold = 20*60)

places.visited_classified <- places.visited_classified %>% 
  getHome() %>% 
    mutate(activityCategory = ifelse(placeType == "hospital",  "employment", activityCategory))


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

places.visited_classified$date <- as.Date(places.visited_classified$intervalTime@start)

sfd <- sfd %>% 
 # semi_join(places.visited_classified, by = c("patient", "date")) %>% 
    mutate(date = as.Date(date))

dailyCategories <- getDailyCategoriesActivities(places.visited_classified) %>% 
  filter(!is.na(activityCategory)) # removing not labelled places

sfd.tmp <-  getListActivitiesSFD(sfd)
sfd.tmp$activityCategory <-  tolower(as.character(sfd.tmp$activityCategory))
performance = getPerformance(sfd.tmp, dailyCategories)

# add number of non classified places per patient

performance_non_class <- sf_places_classified %>%
    group_by(patient) %>%
      summarise(n_unique_places = n(),
                proportion_unique_places_labelled = round(sum(!is.na(activityCategory))/n_unique_places, digits = 2))

# create summary row
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


write.csv(performance_combined, paste("C:/Users/paolo/Documents/Data/Projects/Club M/Healthy volunteers study/Analysis/performance_OSM",timeThreshold,".csv"))

#calculate performance on number of daily categories
dailyCategories <- getDailyCategoriesActivities(places.visited_classified)

NPV_GPS <- dailyCategories %>% 
  group_by(patient, date) %>% 
    count()

NPV_diary <- sfd.tmp %>% 
  group_by(patient, date) %>% 
    count()

NPV_comp <- NPV_GPS %>% 
  left_join(NPV_diary, by =  c("patient", "date")) %>%
    rename(GPS = n.x,
           diary = n.y) %>% 
      mutate(diff = diary - GPS,
             diff_squared = diff^2) %>% 
        group_by(patient) %>% 
          summarise(number_of_days = n(),
                 RMSE = sqrt(sum(diff_squared)/number_of_days))  

NPV_comp$activity_n <- as.numeric(performance$n_activities[-dim(performance)[1]])

overall_performance <- NPV_comp %>% 
  ungroup() %>% 
    summarise(overall_RMSE = sum(RMSE * activity_n)/sum(activity_n),
              overall_sd = wheightedSD(overall_RMSE,
                                         dim(NPV_comp)[1],
                                         activity_n,
                                         RMSE),
              `overall mean (sd)` = str_c(round(overall_RMSE, 1), " (", round(overall_sd, 1), ")"))


write.csv(overall_performance, paste("C:/Users/paolo/Documents/Data/Projects/Club M/Healthy volunteers study/Analysis/overall_places_visited",timeThreshold,".csv"))

write.csv(NPV_comp, paste("C:/Users/paolo/Documents/Data/Projects/Club M/Healthy volunteers study/Analysis/patients_places_visited", timeThreshold,".csv"))


# write activities
saveRDS(places.visited_classified, paste("C:/Users/paolo/Documents/Data/Projects/Club M/Healthy volunteers study/Analysis/activities_OSM",timeThreshold,".rds"))


# ############# create overlapping dataset
# schedule <- read_excel("~/Data/Projects/Club M/Healthy volunteers study/Datasets/Schedule.xlsx") %>% 
#   distinct(ID, day, date) %>% 
#   rename(patient = ID,
#          sessionid = day) %>% 
#   mutate(patient = toupper(patient),
#          patient = str_replace(string = patient, pattern = toupper("GPSPartID"), replacement = ""),
#          date = as.Date(date))
# 
# 
# # create interval
# # activity diary data acquisition and final harmonisation
# sfd <- sfd %>% 
#           mutate(interval = interval(start = time.start, end = time.end, tzone = "GMT"),
#                X = 1:nrow(sfd),
#                activityCategory = tolower(activityCategory)) %>% 
#             inner_join(schedule, by = c("patient", "date"))
# 
# tz(sfd$time.start) <- "GMT"
# tz(sfd$time.end) <- "GMT"
# 
# ## keep only sfd with correct time
# sfd_old <- sfd
# 
# sessions_to_remove <- sfd %>%
#   select(-interval) %>% 
#     group_by(patient, sessionid) %>% 
#       summarise(N = n(),
#                 N_changes = sum(time_start_changed == TRUE | time_end_changed == TRUE)) %>%
#         filter(!(N >= N_changes & N_changes <= 2)) # to remove only those without legitimate time changes (e.g. first/last record)
# 
# # sfd <- sfd %>%
# #   anti_join(sessions_to_remove, by = c("patient", "sessionid")) %>% 
# #     arrange(patient, sessionid, time.start)
# # 
# # sfd$interval@start <- sfd$time.start
#  
# # set up time-zone
# places.visited_classified <- places.visited_classified %>% 
#   mutate(intervalTime2 = interval(start = int_start(intervalTime) + 120*60, end = int_end(intervalTime) + 120*60, tzone = "GMT"),
#          start = int_start(intervalTime2),
#          end = int_end(intervalTime2)) %>% 
#             mutate(date = as.Date(intervalTime@start)) %>%
#               select(- sessionid, -intervalTime) %>% 
#                 inner_join(schedule, by = c("patient", "date")) %>% 
#                   #anti_join(sessions_to_remove, by = c("patient", "sessionid")) %>% 
#                     arrange(patient, sessionid, start)# remove sessions with time changes
# 
# 
# #places.visited_classified$intervalTime2@start <- places.visited_classified$start
# 
# # initialise results
# df_overlap <- data.frame()
# 
# for(i in 1:nrow(places.visited_classified)){
#   
#   # get only overlapping intervals for the specific patient
#   overlaps <- (int_overlaps(places.visited_classified$intervalTime2[i], sfd$interval)) &
#                places.visited_classified$patient[i] == sfd$patient 
#     
#   
#   # if we find something
#   if(sum(overlaps) > 0){
#     
#     df_overlap <- bind_rows(df_overlap,
#                             data.frame(
#                               places.visited_classified[rep(i,sum(overlaps)), ], # replicate rows for each hit
#                               actual_place = sfd$place[overlaps],
#                               actual_activity = sfd$activityType[overlaps],
#                               actual_category = sfd$activityCategory[overlaps],
#                               actual_start = sfd$time.start[overlaps],
#                               actual_end = sfd$time.end[overlaps],
#                               X = sfd$X[overlaps]
#                             ))
#     
#   } else{
#     
#     df_overlap <- bind_rows(df_overlap,
#                             data.frame(
#                               places.visited_classified[i, ], # replicate rows for each hit
#                               actual_place = "",
#                               actual_activity = "",
#                               actual_category = "",
#                               actual_start = NA,
#                               actual_end = NA,
#                               X = NA_integer_
#                             ))
#     
#   }
#   
# }
# 
# ### empty string when no tag was found
# df_overlap <- df_overlap %>% 
#   mutate(placeType = ifelse(is.na(placeType), "", placeType),
#          activityType = ifelse(is.na(activityType), "", activityType),
#          activityCategory = ifelse(is.na(activityCategory), "", activityCategory))
# 
# 
# #add missing activities from diary
# sfd.tmp <- sfd %>% 
#   select(-interval) %>% 
#   filter(!(X %in% df_overlap$X))
# 
# df_overlap <- df_overlap %>% 
#   bind_rows(data.frame(
#     actual_place = sfd.tmp$place,
#     actual_activity = sfd.tmp$activityType,
#     actual_category = sfd.tmp$activityCategory,
#     actual_start = sfd.tmp$time.start,
#     actual_end = sfd.tmp$time.end,
#     patient = sfd.tmp$patient,
#     sessionid = sfd.tmp$sessionid,
#     X = sfd.tmp$X
#   )) %>% 
#   arrange(patient, sessionid, actual_start) 
# 
# df_overlap <- df_overlap %>% 
#   select(-intervalTime2) %>% 
#     anti_join(sessions_to_remove, by = c("patient", "sessionid")) %>% 
#       arrange(patient, sessionid, actual_start) 
# 
# # df_overlap$overlap_duration <- -1
# # 
# # ### calculate overlap in minutes
# # for(i in 1:nrow(df_overlap)){
# # 
# #   df_overlap$overlap_duration[i] <- find_overlapping_duration(df_overlap$start[i], df_overlap$end[i], df_overlap$actual_start[i], df_overlap$actual_end[i])
# # 
# # }
# # 
# # df_overlap <- df_overlap %>%
# #   mutate(overlapping_percentage = overlap_duration/(int_length(interval(start, end))/60)) %>%
# #   filter( (overlapping_percentage >= 0.90) | (overlap_duration == 0)) # keep only records with 90% overlap or no overlap at all
# 
# 
# saveRDS(df_overlap, "~/Data/Projects/Club M/Healthy volunteers study/R/ResultsExplorer/ExploringGPSwithLeaflet2/results.rds")
# 
# # tmp <- df_overlap %>% 
# #   filter(activityType == "visiting friends and relatives" & actual_category == "home")
# # 
# # df_overlap <- df_overlap %>% 
# #   mutate(actual_category = ifelse(activityType == "visiting friends and relatives" & actual_category == "home", "social activities", actual_category))
# # 
# # df_overlap[df_overlap$activityType == "visiting friends and relatives" & df_overlap$actual_category == "home", ]$actual_category <- "social activities"
# # 
# # sum(df_overlap$activityCategory == df_overlap$actual_category, na.rm = TRUE)/nrow(df_overlap)
# # 
# # 
# # tmp <- df_overlap %>% 
# #   group_by(activityCategory, actual_category) %>% 
# #     count()
# # 
# # 
# # sum(!(sfd$X %in% df_overlap$X))
