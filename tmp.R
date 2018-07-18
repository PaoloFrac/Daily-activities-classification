plot_string <- 'mapview(sf_places_geom[i, ], color = "red")'
 
if(!is.null(sf_multipolygons_geom)){
  
  if(nrow(sf_multipolygons_geom) > 0){
    
    plot_string <- paste(plot_string, ' + mapview(sf_multipolygons_geom %>% st_cast("POLYGON") %>% st_cast("LINESTRING"))', sep = "")
    
  }
  
}

if(!is.null(sf_points_geom)){
  
  if(nrow(sf_points_geom) > 0){
    
    plot_string <- paste(plot_string, ' + mapview(sf_points_geom)', sep = "")
  
  }
  
}

if(!is.null(sf_lines_geom)){
  
  if(nrow(sf_lines_geom) > 0){
    
    plot_string <- paste(plot_string, ' + mapview(sf_lines_geom)', sep = "")
  }
  
}

if(!is.null(sf_polygons_geom)){
  
  if(nrow(sf_polygons_geom) > 0){
  
    plot_string <- paste(plot_string, ' + mapview(sf_polygons_geom %>% st_cast("LINESTRING"))', sep = "")
  
  }
  
}



parse(text = plot_string) %>% 
  eval()







 