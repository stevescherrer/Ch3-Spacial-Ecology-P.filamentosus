#### Bottomfish Movement Analysis - November/December 2015
#### Written by: Stephen R. Scherrer

#### Importing Data Files ---------------------------------------------------------

vue_data = load_vemco('/Users/stephenscherrer/Dropbox/Lab Folder/Oahu Receiver Data Files/VUE_Export_2015-June-3.csv')
receiver_data = load_receiver(filename = 'DEPLOYMENT_RECOVERY_LOG.csv', filepath = '/Users/stephenscherrer/Dropbox/Lab Folder/Oahu Receiver Data Files/')
tagging_data = load_tagging_data('/Users/stephenscherrer/Dropbox/Lab Folder/Oahu Receiver Data Files/Bottomfish_Tag_Master.csv')

#### Data Analysis ----------------------------------------------------------------
tag_ids = 18250:18270
vue_data = vue_data[which(vue_data$tag_id %in% tag_ids), ]
vue_data = vue_data[which(vue_data$datetime >= as.POSIXct('2015-01-01')), ]

### Number of detections
n_detections = function(vue_data, tag_ids = NULL){
  if(tag_ids != NULL){
    n_detections = dim(vue_data[vue_data$tag_id %in% tag_ids, ])[1]
  }else{
    n_detections = dim(vue_data)[1]
  }
  return(n_detections)
}

## For all fish


## For each fish


## For average fish



### Time at Liberty

## For all fish

## For each fish

## For average fish



### Number of days detected at a station

## For all fish

## For each fish

## For average fish



### Number of movements detected

## For all fish

## For each fish

## For average fish



### Unique stations detected

## For all fish

## For each fish

## For average fish



### Total Distance Traveled

## For all fish

## For each fish

## For average fish


### Movements over BRFA Boundary

## For all fish

## For each fish

## For average fish


### Days detected in BRFA

## For all fish

## For each fish

## For average fish



### Days detected out of BRFA

## For all fish

## For each fish

## For average fish



### Number of consecutive detections before switching station

## For all fish

## For each fish

## For average fish



### Spatial evenness

## For all fish

## For each fish

## For average fish



### Maximum distance traveled

## For each fish

## For average fish



#### Visualization --------------------------------------------------------------

### Plotting individual movement maps

#### Plotting Receiver Locations
plot_receivers_phase_1 = load_receiver("/Users/stephenscherrer/Documents/Work/UH/Projects/dissertation work/Opakapaka Tagging/Bottomfish Analysis Feb 2015/plotting_files/phase_1_deployment_recovery_log.csv")
plot_receivers_phase_2 = load_receiver("/Users/stephenscherrer/Documents/Work/UH/Projects/dissertation work/Opakapaka Tagging/Bottomfish Analysis Feb 2015/plotting_files/phase_2_deployment_recovery_log.csv")
plot_receivers_phase_2.5 = load_receiver("/Users/stephenscherrer/Documents/Work/UH/Projects/dissertation work/Opakapaka Tagging/Bottomfish Analysis Feb 2015/plotting_files/phase_2.5_deployment_recovery_log.csv")
plot_receivers_phase_2.5.1 = load_receiver("/Users/stephenscherrer/Documents/Work/UH/Projects/dissertation work/Opakapaka Tagging/Bottomfish Analysis Feb 2015/plotting_files/phase_2.5.1_deployment_recovery_log.csv")

## For plotting station numbers on receiver locations
receiver_data = load_receiver("/Users/stephenscherrer/Dropbox/Lab Folder/vessel_operations/Receiver Swap 2015/stations to service.csv")
station_numbers = sapply(strsplit(as.character(receiver_data$station_name), split = " "), function(x) x[5])


plot_receiver_map = function(receiver_data){
    png('receiver_map.png')
    if(exists('bathymetry') == FALSE){
      bathymetry = getNOAA.bathy(lon1 = -158.5, 
                                 lon2 = -157.1, 
                                 lat1 = 20.8, 
                                 lat2 = 22.00,
                                 resolution = 10)
    }
    

    ## Plotting basemap
    plot.bathy(bathymetry, land = TRUE, image=TRUE, bpal = gray.colors(10), deepest.isobath = c(-500), shallowest.isobath = c(0), step = c(100), drawlabels = TRUE)
    ## Adding scale legend
    scaleBathy(bathymetry, deg = .48, cex = .5)
    #scaleBathy(bathymetry, deg = .48, cex = .5)
    ## Adding receiver locations
    points(lat~lon, data = receiver_data, pch = 19, col = 'red', cex = .4)
    ## Adding BRFA boundaries
    brfa_e = as.data.frame(rbind(c(-157.68333333, 21.28333333), 
                                 c(-157.53333333, 21.28333333), 
                                 c(-157.53333333, 21.4166666), 
                                 c(-157.68333333, 21.4166666)))
    colnames(brfa_e) = c('lon', 'lat')
    
    brfa_f = as.data.frame(rbind(c(-157.5666667, 20.9666667), 
                                 c(-157.5666667, 21.0333333333),
                                 c(-157.3666667, 21.0333333333),
                                 c(-157.3666667, 20.9666667),
                                 c(-157.5666667, 20.9666667)))
    colnames(brfa_f) = c('lon', 'lat')
    
    lines(lat~lon, data = brfa_e, pch = 19, col = 'purple', cex = .6)
    lines(lat~lon, data = brfa_f, pch = 19, col = 'purple', cex = .6)

  dev.off
  return(bathymetry)
}

depth_of_station_18 = get.depth(mat = bathymetry, x = cbind(receiver_data$lon[receiver_data$station_name == 'Oahu - Makapuu BRFA 18'], receiver_data$lat[receiver_data$station_name == 'Oahu - Makapuu BRFA 18']), locator = FALSE)
receiver_data$depth = get.depth(mat = bathymetry, x = cbind(receiver_data$lon, receiver_data$lat), locator = FALSE)



#### Plotting Movements of individual fish
plot_movements = function(vue_data, receiver_data, bottomfish_tag_ids = FALSE){
  if (bottomfish_tag_ids[1] == FALSE){
    bottomfish_tag_ids = unique(as.numeric(levels(vue_data$tag_id))[vue_data$tag_id])}
  for (id in bottomfish_tag_ids){
    indv_data = vue_data[vue_data$tag_id == id, ]
    png(paste(id, 'Movement Map.png'))
    if(exists('bathymetry') == FALSE){
      bathymetry = getNOAA.bathy(lon1 = -157.8, 
                                 lon2 = -157.5, 
                                 lat1 = 21.2, 
                                 lat2 = 21.5,
                                 resolution = .7)
    }
    
    ## Plotting basemap
    plot.bathy(bathymetry, land = TRUE, image=TRUE, bpal = gray.colors(10), deepest.isobath = c(-500), shallowest.isobath = c(0), step = c(100), drawlabels = TRUE)
    ## Adding scale legend
    scaleBathy(bathymetry, deg = .1, cex = .5)
    #scaleBathy(bathymetry, deg = .48, cex = .5)
    ## Adding receiver locations
    text(x = receiver_data$lon, y = receiver_data$lat, labels = station_numbers, cex = .5)
    points(lat~lon, data = receiver_data, pch = 19, col = 'red', cex = .4)
    text(y = receiver_data$lat, x = receiver_data$lon, labels = strsplit(receiver_data$station_name, split = " ")[5],  cex = .5)
    ## Adding BRFA boundaries
    brfa_e = as.data.frame(rbind(c(-157.68333333, 21.28333333), 
                                 c(-157.53333333, 21.28333333), 
                                 c(-157.53333333, 21.4166666), 
                                 c(-157.68333333, 21.4166666)))
    colnames(brfa_e) = c('lon', 'lat')
    
    brfa_f = as.data.frame(rbind(c(-157.5666667, 20.9666667), 
                                 c(-157.5666667, 21.0333333333),
                                 c(-157.3666667, 21.0333333333),
                                 c(-157.3666667, 20.9666667),
                                 c(-157.5666667, 20.9666667)))
    colnames(brfa_f) = c('lon', 'lat')
    
    lines(lat~lon, data = brfa_e, pch = 19, col = 'purple', cex = .6)
    lines(lat~lon, data = brfa_f, pch = 19, col = 'purple', cex = .6)
    
    ## Plotting fish movements
    lines(lat~lon, data = indv_data, col = 'blue', lty = 1, lwd = 5)
    points(lat~lon, data = indv_data, col = 'blue',cex = 0.6, pch = 19)
    dev.off()
  }
  return(bathymetry)
}

