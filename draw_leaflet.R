library("leaflet")

pat <- "1"
#session <- "3"

tmp <- places %>% filter(patient == 1)
  
  groups = as.character(unique(tmp$placeID))
  
  groupColors = colorFactor(palette = "RdYlBu", domain = tmp$placeID)
  
  map = leaflet(tmp) %>% addTiles(group = "OpenStreetMap")
  for(g in groups){
    d = tmp[tmp$placeID == g, ]
    map = map %>% addCircleMarkers(data = d, lng = ~Longitude, lat = ~Latitude, 
                                   color = ~groupColors(placeID),
                                   group = g,
                                   popup = ~placeID)
    
  }
  map %>% addLayersControl(overlayGroups = groups) %>% 
    addMarkers(data = tmp %>% distinct(placeID, CenterLong, CenterLat), lng = ~CenterLong, lat = ~CenterLat, popup = ~placeID)
  
  
  