#### after identified places
library("sp")
library("ggmap")
library("sf")
library("stringr")
library("leaflet")
library("mapview")
library("dplyr")
library("purrr")
library("osmar")
library("osmdata")
library("readxl")
library("zeallot")
library("tidyverse")
library("PostcodesioR")
library("RCurl")
library("XML")
source('~/Data/Projects/Club M/Healthy volunteers study/R/Daily-activities-classification/FunctionScript.R', echo=TRUE)

sf_buildings <- buildings_info <- NULL

#analysis_type <- "time_based"
#analysis_type <- "density_based"
analysis_type <- "combined"

distance_threshold <- 50

minutes_threshold <- 10

timeThreshold <- 60*minutes_threshold

sf_places <- readRDS(paste("~/Data/Projects/Club M/Healthy volunteers study/Datasets/",
                           analysis_type,
                           "/places", 
                           timeThreshold,
                           ".rds",
                           sep = "")) # load found places

sf_places_geom <-  sf_places %>% # initialise as sf object
  st_as_sf(coords = c("CenterLong", "CenterLat")) %>%
  st_set_crs(4326) # set coordinate reference system

sf_places_classified <- NULL

ontology <- read_excel("~/Data/Projects/Club M/Healthy volunteers study/Datasets/Ontology.xlsx", sheet = 1)

key_values <- unique(ontology$key)

error_index <- NULL

for(i in 1:nrow(sf_places)){ # for each place

  #initialise result from OSM
  opq <- NULL
  is_in_building <- FALSE
  
  #create bbox around ith cluster
  bbox <- center_bbox(center_lon = st_coordinates(sf_places_geom[i,])[1], center_lat = st_coordinates(sf_places_geom[i,])[2], width = 300, height = 300)
  
  # create query
  opq_string <- create_osmdata_query_sf()
  
  tryCatch({
    
    opq <- parse(text = opq_string) %>% 
      eval()
  },
  error = function(e) {})
  
  if(!is.null(opq)){
    
    ### extract results info and geometries
    c(sf_points_geom, sf_points) %<-% extract_info_sf(opq$osm_points, ontology) #POIs
    
    c(sf_lines_geom, sf_lines) %<-% extract_info_sf(opq$osm_lines, ontology) #highways
    
    c(sf_polygons_geom, sf_polygons) %<-% extract_info_sf(opq$osm_polygons, ontology) #buildings
    
    c(sf_multipolygons_geom, sf_multipolygons) %<-% extract_info_sf(opq$osm_multipolygons, ontology) #wider areas
    
    
    source('~/Data/Projects/Club M/Healthy volunteers study/R/Daily-activities-classification/tmp.R', echo=TRUE)
    
    dist <- calculate_distance_to_each_result_found(sf_places_geom[i, ],
                                                    sf_points_geom,
                                                    sf_lines_geom,
                                                    sf_polygons_geom)
    
    #areas
    if(!is.null(sf_multipolygons_geom)){

      if(nrow(sf_multipolygons_geom) > 0){

        dist <- calculate_distance(dist, sf_places_geom[i, ], sf_multipolygons_geom %>% st_cast("POLYGON") %>% st_cast("LINESTRING"), "sf_multipolygons") %>% 
          group_by(ID, relevant_object_name) %>% 
            filter(dist_m == min(dist_m)) %>% 
              ungroup()

      }

    }
    
    
    if(!is.null(dist)){
      
      dist <- dist %>% 
        filter(dist_m == min(dist_m)) %>% # get min distance
        filter(dist_m <= distance_threshold) # only if smaller than threshold
      
    }else{
      
      dist <- data.frame()
      
    }
    
      # if shape within distance threshold  
    if(nrow(dist)>0){
        
        sf_places_classified <- add_place_classification(sf_places_classified, 
                                                         sf_places$patient[i], 
                                                         sf_places$placeID[i], 
                                                         get(as.character(dist$relevant_object_name[1])) %>% filter(ID == dist$ID[1]),
                                                         ontology)
        
    }
    
  } else{
    
    error_index <- c(error_index, i)
    
  }
  
  # check if classified or not
  check_if_classified <- sf_places_classified$placeID == sf_places$placeID[i] & 
                          sf_places_classified$patient == sf_places$patient[i]
  
  if(sum(check_if_classified) == 0){ # if not classified add empty row
    
    sf_places_classified <- sf_places_classified %>% 
      bind_rows(
        data.frame(
          patient = sf_places$patient[i],
          placeID = sf_places$placeID[i],
          key = NA_character_,
          value = NA_character_,
          category = NA_character_,
          activity = NA_character_
        )
      )
  }
 
}
  
saveRDS(sf_places_classified, paste("~/Data/Projects/Club M/Healthy volunteers study/Datasets/",
                                    analysis_type,
                                    "/sf_places_closest",
                                    timeThreshold,
                                    ".rds",
                                    sep = ""))
  
###### check if different categories for the same place 
(tmp <- sf_places_classified %>% 
  group_by(patient, placeID) %>% 
    count() %>% 
      ungroup() %>% 
        filter(n > 1)) %>% 
          summary()

## select duplicates
sf_places_classified_duplicates <- sf_places_classified %>% 
  semi_join(tmp, by = c("patient", "placeID")) %>% 
    distinct(patient, placeID, category) %>% 
      group_by(patient, placeID) %>% 
        count() %>% 
          filter(n > 1)

### removing uncertain classifications
sf_places_classified <- sf_places_classified %>% 
  anti_join(sf_places_classified_duplicates, by = c("patient", "placeID"))


## chaning names and removing duplicates
sf_places_classified <- sf_places_classified %>% 
  rename(placeType = value,
         activityType = activity,
         activityCategory = category) %>% 
  distinct(patient, placeID, activityCategory, .keep_all = TRUE)


save.image(paste("~/Data/Projects/Club M/Healthy volunteers study/Datasets/",
                 analysis_type,
                 "/labelling_results_closest",
                 timeThreshold,
                 ".RData", 
                 sep = ""))

