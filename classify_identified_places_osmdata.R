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
source('~/Data/Projects/Club M/Healthy volunteers study/R/labelling_functions.R', echo=TRUE)

sf_buildings <- buildings_info <- NULL

distance_threshold <- 50

minutes_threshold <- 10

timeThreshold <- 60*minutes_threshold

sf_places <- readRDS(paste("~/Data/Projects/Club M/Healthy volunteers study/Datasets/places", timeThreshold,".rds"))

sf_places_geom <-  sf_places %>%
  st_as_sf(coords = c("CenterLong", "CenterLat")) %>%
  st_set_crs(4326)

sf_places_classified <- NULL

ontology <- read_excel("~/Data/Projects/Club M/Healthy volunteers study/Datasets/Ontology.xlsx", sheet = 1) %>% 
  bind_rows(data.frame(
    key = "building",
    value = "yes"
  ))

key_values <- unique(ontology$key)

error_index <- NULL

for(i in 1:nrow(sf_places)){

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
  error = function(e) {
    #cat(paste0("`osmdata_sf` returned an error: ", e))
    
    #error_occured <- TRUE
    
  #  next
    
  })
  
  if(!is.null(opq)){
    
    ### extract results info and geometries
    c(sf_points_geom, sf_points) %<-% extract_info_sf(opq$osm_points, ontology) #POIs
    
    c(sf_lines_geom, sf_lines) %<-% extract_info_sf(opq$osm_lines, ontology) #highways
    
    c(sf_polygons_geom, sf_polygons) %<-% extract_info_sf(opq$osm_polygons, ontology) #buildings
    
    c(sf_multipolygons_geom, sf_multipolygons) %<-% extract_info_sf(opq$osm_multipolygons, ontology) #wider areas
    
    
    # plot results
    source('~/Data/Projects/Club M/Healthy volunteers study/R/tmp.R', echo=TRUE)
    
    if(!is.null(sf_polygons_geom)){ # if there are buildings around
      
      if(nrow(sf_polygons_geom) > 0){
        
        # save data to use them later eventually
        sf_polygons_all <- sf_polygons
        sf_polygons_geom_all <- sf_polygons_geom
        
        # # keep only actual buildings
        # building_IDs <- sf_polygons %>% 
        #   filter(key == "building") %>% 
        #     select(ID)
        # 
        # sf_polygons <- sf_polygons %>% 
        #   filter(ID %in% building_IDs$ID)
        # 
        # sf_polygons_geom <- sf_polygons_geom %>% 
        #   filter(ID %in% sf_polygons$ID)
        
        is_in_building <- st_intersects(sf_places_geom[i, ], sf_polygons_geom, sparse = FALSE) # check if place in building
        
        if(sum(is_in_building)>0){
          
          #update buildings
          sf_polygons_geom <- sf_polygons_geom[is_in_building, ]
          sf_polygons <- sf_polygons %>% 
            filter(ID %in% sf_polygons_geom$ID)
          
          # check if in multiple polygons
          if(sum(is_in_building)>1){
            
            # calculate distance in the case point contained in multiple polygons
            dist_buildings <- st_distance(sf_places_geom[i, ], sf_polygons_geom %>% st_cast("LINESTRING"))
            
            #update buildings
            sf_polygons_geom <- sf_polygons_geom[which.min(dist_buildings), ]
            sf_polygons <- sf_polygons %>% 
              filter(ID %in% sf_polygons_geom$ID)
            
          }
          
          #check if POIs in building
          are_POIs_same_building <- st_intersects(sf_points_geom, sf_polygons_geom, sparse = FALSE)
          
          # if POIs same building
          if(sum(are_POIs_same_building)>0){
            
            # consider only points inside
            sf_points_geom <- sf_points_geom[are_POIs_same_building, ]
            sf_points <- sf_points %>% 
              filter(ID %in% sf_points_geom$ID)
            
            dist_POIs <- as.numeric(st_distance(x = sf_places_geom[i, ], y = sf_points_geom))
            
            # keep closest
            sf_points_geom <- sf_points_geom[which.min(dist_POIs), ]
            
            #get info
            sf_points <- sf_points %>% 
              filter(ID %in% sf_points_geom$ID)
            
            if(min(dist_POIs) < distance_threshold){
              
              sf_places_classified <- add_place_classification(sf_places_classified, 
                                                               sf_places$patient[i], 
                                                               sf_places$placeID[i], 
                                                               sf_points,
                                                               ontology)
              
            } else{
              
              # check if building has other tags to building=yes attached
              sf_polygons_class <- sf_polygons %>% 
                filter(value != "yes")
              
              if(nrow(sf_polygons_class) > 0){
                
                sf_places_classified <- add_place_classification(sf_places_classified, 
                                                                 sf_places$patient[i], 
                                                                 sf_places$placeID[i], 
                                                                 sf_polygons_class,
                                                                 ontology)
                
              }
              
            }
            
          } else{ # if no POIs in building
            
            # check if building has other tags to building=yes attached
            sf_polygons_class <- sf_polygons %>% 
              filter(value != "yes")
            
            if(nrow(sf_polygons_class) > 0){
              
              sf_places_classified <- add_place_classification(sf_places_classified, 
                                                               sf_places$patient[i], 
                                                               sf_places$placeID[i], 
                                                               sf_polygons_class,
                                                               ontology)
              
            }
            
          }
          
        }
        
      }
      
    } 
    
    check_if_classified <- sf_places_classified$placeID == sf_places$placeID[i] & sf_places_classified$patient == sf_places$patient[i]
    
    if(sum(check_if_classified) == 0){ # if not classified yet
       if(sum(is_in_building)>0){
         
         if(!is.null(sf_multipolygons_geom)){
           
           building_is_in_area <- st_intersects(sf_polygons_geom, sf_multipolygons_geom %>% st_cast("POLYGON"), sparse = FALSE)
           
           if(sum(building_is_in_area) > 0){
             
             sf_multipolygons_geom <- sf_multipolygons_geom[building_is_in_area, ]
             
             sf_multipolygons <- sf_multipolygons %>% 
               filter(ID %in% sf_multipolygons_geom$ID)
             
             if(nrow(sf_multipolygons_geom) > 1){
               
               dist_areas <- calculate_distance(dist = NULL, sf_places_geom[i, ], sf_multipolygons_geom %>% st_cast("POLYGON") %>% st_cast("LINESTRING"), type = "multy")
               
               ID_closest <- unique(dist_areas[which.min(dist_areas$dist_m), ]$ID) 
               
               sf_multipolygons_geom <- sf_multipolygons_geom %>% 
                 filter(ID == ID_closest)
               
               sf_multipolygons <- sf_multipolygons %>% 
                 filter(ID == sf_multipolygons_geom$ID)
               
             }
             
             sf_places_classified <- add_place_classification(sf_places_classified, 
                                                              sf_places$patient[i], 
                                                              sf_places$placeID[i], 
                                                              sf_multipolygons,
                                                              ontology)
             
             
           }else{
             
             sf_polygons_all <- sf_polygons_all %>% 
               filter(value != "yes")
             
             sf_polygons_geom_all <- sf_polygons_geom_all %>% 
               filter(ID %in% sf_polygons_all$ID)
             
             if(nrow(sf_polygons_geom_all)>0){
               
               # check whether in area mistakenly labelled as building
               building_is_in_area <- st_intersects(sf_polygons_geom, sf_polygons_geom_all, sparse = FALSE)
               
               if(sum(building_is_in_area) > 0){
                 
                 sf_polygons_geom_all <- sf_polygons_geom_all[building_is_in_area, ]
                 
                 sf_polygons_all <- sf_polygons_all %>% 
                   filter(ID %in% sf_polygons_geom_all$ID)
                 
                 if(nrow(sf_polygons_geom_all) > 1){
                   
                   dist_areas <- as.numeric(st_distance(sf_places_geom[i, ], sf_polygons_geom_all %>% st_cast("LINESTRING")))
                   
                   sf_polygons_geom_all <- sf_polygons_geom_all[which.min(dist_areas), ]
                   
                   sf_polygons_all <- sf_polygons_all %>% 
                     filter(ID == sf_polygons_geom_all$ID)
                   
                 }
                 
                 sf_places_classified <- add_place_classification(sf_places_classified, 
                                                                  sf_places$patient[i], 
                                                                  sf_places$placeID[i], 
                                                                  sf_polygons_all,
                                                                  ontology)
                 
               }
               
             }
             
           }
         
       }
      
      } else{
        
        c(sf_points, sf_points_geom) %<-% remove_building_yes(sf_points, sf_points_geom)
        
        c(sf_lines, sf_lines_geom) %<-% remove_building_yes(sf_lines, sf_lines_geom)
        
        c(sf_polygons, sf_polygons_geom) %<-% remove_building_yes(sf_polygons, sf_polygons_geom)
        
        c(sf_multipolygons, sf_multipolygons_geom) %<-% remove_building_yes(sf_multipolygons, sf_multipolygons_geom)
        
        
        dist <- calculate_distance_to_each_result_found(sf_places_geom[i, ],
                                                        sf_points_geom,
                                                        sf_lines_geom,
                                                        sf_polygons_geom)
        
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
          
        }else{ # check if point in area
          
          if(!is.null(sf_multipolygons_geom)){
            
            if(nrow(sf_multipolygons_geom)>0){
              
              is_in_area <- st_intersects(sf_places_geom[i, ], sf_multipolygons_geom %>% st_cast("POLYGON"), sparse = FALSE)
              
            }else{
              
              is_in_area <- 0
              
            }
            
          }else{
            
            is_in_area <- 0
            
          }
          
          # if place in area
          if(sum(is_in_area)> 0){
            
            #update areas
            sf_multipolygons_geom <- sf_multipolygons_geom[is_in_area, ]
            sf_multipolygons <- sf_multipolygons %>% 
              filter(ID == sf_multipolygons_geom$ID)
            
            if(nrow(sf_multipolygons_geom)>1){ # if more place in more than one area
              
              # calculate distance to place
              dist_multi <- calculate_distance(NULL, point = sf_places_geom[i, ], shape = sf_multipolygons_geom %>% st_cast("POLYGON") %>% st_cast("LINESTRING"), type = "sf_multipolygons") %>% 
                filter(dist_m == min(dist_m))
              
              # keep closest
              sf_multipolygons_geom <- sf_multipolygons_geom %>% 
                filter(ID == dist_multi$ID)
              
              #get info
              sf_points <- sf_points %>% 
                filter(ID %in% sf_multipolygons_geom$ID)
              
            }
            
            # classify
            sf_places_classified <- add_place_classification(sf_places_classified, 
                                                             sf_places$patient[i], 
                                                             sf_places$placeID[i], 
                                                             sf_multipolygons,
                                                             ontology) 
            
          }
          
        }
        
      }
      
    }
    

    rm(sf_points_geom, sf_points, sf_lines_geom, sf_lines, sf_polygons_geom, sf_polygons, sf_multipolygons_geom, sf_multipolygons, sf_polygons_all, sf_polygons_geom_all, sf_polygons_class)
    
  } else{
    
    error_index <- c(error_index, i)
    
  }
  
  # check if classified or not
  check_if_classified <- sf_places_classified$placeID == sf_places$placeID[i] & sf_places_classified$patient == sf_places$patient[i]
  
  if(sum(check_if_classified) == 0){
    
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
  
saveRDS(sf_places_classified, paste("~/Data/Projects/Club M/Healthy volunteers study/Datasets/sf_places",timeThreshold,".rds"))
  
###### check if different categories 
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

### try to use Google Places API to classify unclassified places
# key Google Place API
key= c("AIzaSyDWs7eStEfQRGG8tuNDheo2SJR8ooPjr14",
       "AIzaSyCdyXaICjKXqefkUUzebnw7A6wDvcQac7",
       "AIzaSyCl_RMVOmZfOVdfj8Umn9RRytSHdSIIV3k",
       "AIzaSyAoZSLAoWUxPlOBGE3EeHUlxJ9arrPJt90")

# select unclassified places 
sf_places_unclassified <- sf_places_classified %>% 
  filter(is.na(placeType)) %>% 
    select(patient, placeID) %>% 
      left_join(sf_places, by = c("patient", "placeID")) # merging with place coordinates

# initialise variable
sf_places_unclassified_google <- NULL

for(i in 1:nrow(sf_places_unclassified)){
  
  tmp <- getGooglePlacesPOI(sf_places_unclassified$CenterLat[i], sf_places_unclassified$CenterLong[i], key)
  
  if(is.null(tmp)){ # if nothing was found
    
    sf_places_unclassified_google <- bind_rows(sf_places_unclassified_google,
                                               data.frame(patient = sf_places_unclassified$patient[i],
                                                          placeID = sf_places_unclassified$placeID[i],
                                                          placeType = NA_character_)) # add only patient and placeID
    
  } else{
    
    if(tmp$distance <= distance_threshold){ # only if it respects the distance threshold
      
      sf_places_unclassified_google <- bind_rows(sf_places_unclassified_google,
                                                 data.frame(patient = sf_places_unclassified$patient[i],
                                                            placeID = sf_places_unclassified$placeID[i],
                                                            placeType = tmp$placeType))
      
    } else{
      
      sf_places_unclassified_google <- bind_rows(sf_places_unclassified_google,
                                                 data.frame(patient = sf_places_unclassified$patient[i],
                                                            placeID = sf_places_unclassified$placeID[i],
                                                 placeType = NA_character_))
      
    }
    
  }
  
}




save.image(paste("labelling_results",timeThreshold,".RData"))