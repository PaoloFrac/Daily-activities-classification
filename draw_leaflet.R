library("leaflet")

pat <- "1"
#session <- "3"


df1 <- places_old %>% 
  filter(patient == pat)

df2 <- places %>% 
  filter(patient == pat)

df3 <- geo.visited %>% 
  filter(patient == pat)


  leaflet() %>%
    addTiles() %>% 
    # addCircleMarkers(lng = raw_gps_sub$Longitude, lat = raw_gps_sub$Latitude, popup = raw_gps_sub$TimeStamp) %>% 
    addCircleMarkers(lng = df3$Longitude, lat = df3$Latitude) %>% 
    addMarkers(lng = df1$CenterLong, lat = df1$CenterLat, popup = df1$placeID) %>% 
  addCircleMarkers(lng = df2$CenterLong, lat = df2$CenterLat, color = "red")
 
  #   addMarkers(lng = algorithm_results_sub$CenterLong, lat = algorithm_results_sub$CenterLat,  popup = algorithm_results_sub$placeID)
  # 
  
  
  ###################################################################################
  
  tmp <- places %>% filter(patient == 1)
  
  groups = as.character(unique(tmp$placeID))
  
  groupColors = colorFactor(palette = "RdYlBu", domain = tmp$placeID)
  
  map = leaflet(df3) %>% addTiles(group = "OpenStreetMap")
  for(g in groups){
    d = tmp[tmp$placeID == g, ]
    map = map %>% addCircleMarkers(data = d, lng = ~Longitude, lat = ~Latitude, 
                                   color = ~groupColors(placeID),
                                   group = g,
                                   popup = ~placeID)
    
  }
  map %>% addLayersControl(overlayGroups = groups) %>% 
    addMarkers(data = tmp %>% distinct(placeID, CenterLong, CenterLat), lng = ~CenterLong, lat = ~CenterLat, popup = ~placeID)
  
  
  
  
  
  
  
  leaflet() %>%
    addTiles() %>% 
      