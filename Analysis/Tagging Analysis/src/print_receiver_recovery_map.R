#### Print map for receiver recovery
## Written by Stephen Scherrer
library('marmap')

print_receiver_map = function(receiver_data = NULL, snapshot_date = format(Sys.time(), "%Y-%m-%d"), save_plot = TRUE){
plot_map_for_date = as.POSIXct(snapshot_date)

load_receiver_data = function(filename, format = '%m/%d/%y %H:%M', tz = 'HST'){
  #### Loads in .csv file containing receiver deployment and recovery data and cleans up file as appropriate
  ### Loading in datafile
  receiver_data = read.csv(filename, stringsAsFactors = FALSE)
  ### Adjusting Column Names
  colnames(receiver_data)[1] = 'station_name'
  colnames(receiver_data)[2] = 'consecutive_deployment_number'
  colnames(receiver_data)[3] = 'deployment_date'
  colnames(receiver_data)[4] = 'recovery_date'
  colnames(receiver_data)[5] = 'recovered'
  colnames(receiver_data)[6] = 'lat_deg'
  colnames(receiver_data)[7] = 'lat_min'
  colnames(receiver_data)[8] = 'lon_deg'
  colnames(receiver_data)[9] = 'lon_min'
  colnames(receiver_data)[10] = 'depth'
  colnames(receiver_data)[11] = 'vr2w_serial'
  colnames(receiver_data)[12] = 'acoustic_release_serial'
  colnames(receiver_data)[13] = 'acoustic_release_battery_life'
  colnames(receiver_data)[14] = 'acoustic_release_voltage_at_deployment'
  colnames(receiver_data)[15] = 'acoustic_release_serial_code'
  colnames(receiver_data)[16] = 'temperature_logger_serial'
  colnames(receiver_data)[17] = 'deployed_by'
  colnames(receiver_data)[18] = 'recovered_by'
  colnames(receiver_data)[19] = 'comments_deployment'
  colnames(receiver_data)[20] = 'comments_recovery'
  ### Converting deployment and recovery dates to POSIX objects
  receiver_data$deployment_date = as.POSIXct(receiver_data$deployment_date, format = '%m/%d/%y %H:%M', tz = 'HST')
  receiver_data$recovery_date = as.POSIXct(receiver_data$recovery_date, format = '%m/%d/%y %H:%M', tz = 'HST')
  ## Converting latitude and longitude from degree minutes to decimal degrees
  receiver_data$lat = convert_lat_lon(receiver_data$lat_deg, receiver_data$lat_min)
  receiver_data$lon = convert_lat_lon(receiver_data$lon_deg, receiver_data$lon_min)
  return (receiver_data)
}
convert_lat_lon = function(ll_deg, ll_min = FALSE){
  ## Converts latitude and longitude between ll minutes and ll decimal degrees
  # 2 usages:
  # Convert decimal degrees to degree minutes
  # 1 argument
  # ll_pref is a single argument of latitude or longitude in decimal degrees
  # Returns a prefix and decimal for that argument
  # Convert degree minutes to decimal degrees
  # 2 arguments
  # ll_pref is the latitude or longitude's degree
  # ll_min is the degree minutes
  # returns a single float of ll in decimal degrees
  if (ll_min[1] == FALSE){ #then we are going from one number to two
    ll_deg = as.numeric(as.character(ll_deg))
    ll_bin = matrix(0, length(ll_deg), 2)
    for (r in 1:length(ll_deg)){
      if (isTRUE(ll_deg[r] >= 0)){
        ll_dec = ll_deg[r] - floor(ll_deg[r])
        ll_bin[r, ] = c(floor(ll_deg[r]), (ll_dec)*60)
      } else {
        ll_dec = (ll_deg[r] - ceiling(ll_deg[r]))*-1
        ll_bin[r, ] = c(ceiling(ll_deg[r]), (ll_dec)*60)
      }
    }
  }else{ #if we are converting from two numbers to one
    ll_deg = as.numeric(as.character(ll_deg))
    ll_min = as.numeric(as.character(ll_min))
    ll_bin = matrix(0, length(ll_deg), 1)
    for (r in 1:length(ll_deg)){
      ll_dec_deg = abs(ll_deg[r]) + (abs(ll_min[r])/60)
      if (isTRUE(ll_deg[r] < 0)){
        ll_dec_deg = ll_dec_deg*(-1)
      }
      ll_bin[r] = ll_dec_deg
    }
  }
  return (ll_bin)
}

if(is.null(receiver_data)){
  receiver_data = load_receiver_data("/Users/stephenscherrer/Google Drive/Weng Lab/Data/Bottomfish/Oahu Receiver Data Files/DEPLOYMENT_RECOVERY_LOG.csv")
}
stations_to_plot = receiver_data[which(receiver_data$deployment_date <= plot_map_for_date & (is.na(receiver_data$recovery_date) | receiver_data$recovery_date >= plot_map_for_date)), ]

### Removing anything that's not one of our receivers in Makapuu
station_names_to_keep = c()
for(i in 1:length(stations_to_plot$station_name)){
  temp_name = strsplit(stations_to_plot$station_name[i], split = " ")[[1]]
  if(temp_name[3] == 'Makapuu'){
    stations_to_plot$station_number[i] = temp_name[5]
    station_names_to_keep = c(station_names_to_keep, temp_name[5])
  }
}

stations_to_plot = stations_to_plot[stations_to_plot$station_number %in% station_names_to_keep, ] 

#### Plotting Map of Receivers in Study
bathymetry = get_bathymetry(region = 'Makapuu', resolution = "Medium")

if(save_plot){
    pdf(paste('Station Map ', plot_map_for_date, '.pdf', sep = ""), width = 8, height = 8 )
}
  ## Plotting basemap
    plot(bathymetry, main = 'Station Deployment Chart', land = TRUE, image=TRUE, bpal = list(c(-100, -400, "lightblue")), deepest.isobath = c(-10000), shallowest.isobath = c(-1), step = c(100), drawlabels = TRUE)
    
  #plot.bathy(bathymetry, land = TRUE, bpal = "white", image=TRUE, deepest.isobath = c(-10000), shallowest.isobath = c(0), step = c(100), drawlabels = TRUE, lty = 1.5, col = 'gray')
  ## Adding scale legend
  scaleBathy(bathymetry, deg = .10, cex = .5, y = 21.26, x = -157.73)
  #scaleBathy(bathymetry, deg = .48, cex = .5)
  
  ## Adding BRFA boundaries
  brfa_e = as.data.frame(rbind(c(-157.68333333, 21.28333333), 
                               c(-157.53333333, 21.28333333), 
                               c(-157.53333333, 21.4166666), 
                               c(-157.7, 21.4166666)))
  colnames(brfa_e) = c('lon', 'lat')
  lines(lat~lon, data = brfa_e, pch = 19, col = 'purple', lwd = 1)
  

  points(lat ~ lon, data = stations_to_plot[which(stations_to_plot$recovered == ""), ],  pch = 19, col = 'red', cex = 5.5)
  points(lat ~ lon, data = stations_to_plot[which(stations_to_plot$recovered != ""), ],  pch = 1, col = 'red', cex = 5.5)
  text((lat)~ lon, labels = station_number, data = stations_to_plot, col = 'white', cex = .75)
  text((lat)~ lon, labels = station_number, data = stations_to_plot[stations_to_plot$recovered != "", ],col = 'dark red', cex = .75)
  
  

  brfa_f = as.data.frame(rbind(c(-157.5666667, 20.9666667), 
                               c(-157.5666667, 21.0333333333),
                               c(-157.3666667, 21.0333333333),
                               c(-157.3666667, 20.9666667),
                               c(-157.5666667, 20.9666667)))
  colnames(brfa_f) = c('lon', 'lat')
  

   lines(lat~lon, data = brfa_f, pch = 19, col = 'purple', cex = .6)
   if(save_plot){
  dev.off()
   }
  }

print_receiver_map(receiver_data = receiver_df, snapshot_date = as.POSIXct("2017-07-01 00:00:00"), save_plot = FALSE)
