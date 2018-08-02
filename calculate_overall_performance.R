timeThreshold <- c(10:5)*60

analysis_type <- c("time_based", "density_based", "combined")

for(i in 1:length(analysis_type)){
  
  overall_performance <- NULL
  
  for(j in 1:length(timeThreshold)){
    
    overall_performance <- read.csv( paste("~/Data/Projects/Club M/Healthy volunteers study/Analysis/",
                              analysis_type[i],
                              "/performance_OSM",
                              timeThreshold[j],".csv", sep = "")) %>% 
                select(-X, -patient) %>% 
                  slice(nrow(.)) %>% 
                    mutate(analysis = paste(analysis_type[i],
                                            timeThreshold[j],
                                            sep = "_")) %>% 
                      bind_rows(overall_performance)
    
    
    
  }
  
  write.csv(overall_performance, paste("~/Data/Projects/Club M/Healthy volunteers study/Analysis/",
                                       analysis_type[i],
                                       "/performance_OSM_overall.csv", sep = ""))
  
}
