### Fish movement, homerange behavior, etc.
load(file.path(project_dir, "workspace_image_updated"))

if(!file.exists('scratchpad')){
  dir.create(file.path(src_dir, 'scratchpad'))
}
setwd(file.path(src_dir, 'scratchpad'))

analysis_summary = phase_10_valid

##### Diel Movements #####

### Assign each detection to dawn, day, dusk, and night
  ## Already implemented... see function calculate_time_of_day
  
### Calculate how long each cycle lasts
calculate_time_of_day = function(vue_df){
  
    ## Calculating sunrise, sunset and dawn for each detection using coordinates of Station Makapuu BRFA 15
    sun_times = getSunlightTimes(date = unique(vue_df$date), lat = unique(vue_df$lat[vue_df$station == "Oahu - Makapuu BRFA 15 (Makapuu In BRFA)"])[1], lon = unique(vue_df$lon[vue_df$station == "Oahu - Makapuu BRFA 15 (Makapuu In BRFA)"])[1], keep = c("sunrise", "sunset", "nauticalDawn", "nauticalDusk"), tz = "HST")
    for(i in 1:length(sun_times$date)){
      dawn_length = difftime(sun_times$sunrise[i], sun_times$nauticalDawn[i])
    }
    ## 
    
}


### Look at changes in depth between dawn, day, dusk, and night
### Look at changes in depth between dawn - dusk and dusk to dawn
### Look at changes in detection location during these periods (per fish basis and in aggregate)

calculate_length_of_day_periods = function(vue_df){
  ## Calculating sunrise, sunset and dawn for each detection using coordinates of Station Makapuu BRFA 15
  sun_times = getSunlightTimes(date = seq.Date(from = min(vue_df$date) - 1, to = max(vue_df$date) + 1, by = 1) , lat = unique(vue_df$lat[vue_df$station == "Oahu - Makapuu BRFA 15 (Makapuu In BRFA)"])[1], lon = unique(vue_df$lon[vue_df$station == "Oahu - Makapuu BRFA 15 (Makapuu In BRFA)"])[1], keep = c("sunrise", "sunset", "nauticalDawn", "nauticalDusk"), tz = "HST")
  sun_times$previous_sunset = c(sun_times$sunset[1], sun_times$sunset[1:(length(sun_times$sunset)-1)])
  sun_times$next_sunrise = c(sun_times$sunrise[2:length(sun_times$sunrise)], sun_times$sunrise[length(sun_times$sunrise)])
  ## Assigning time of day to each detection
  length_of_day_night_periods = data.frame('prior_night' = as.numeric(difftime(sun_times$nauticalDawn, sun_times$previous_sunset, units = 'hour')), 
                                           'dawn' = as.numeric(difftime(sun_times$sunrise, sun_times$nauticalDawn, units = 'hour')), 
                                           'day' = as.numeric(difftime(sun_times$sunset, sun_times$sunrise, units = 'hour')), 
                                           'dusk' = as.numeric(difftime(sun_times$nauticalDusk, sun_times$sunset, units = 'hour')), 
                                           'night' = as.numeric(difftime(sun_times$next_sunrise, sun_times$nauticalDusk, units = 'hour')), 
                                           stringsAsFactors = FALSE)
  
  return(length_of_day_night_periods)
}

day_night_periods = calculate_length_of_day_periods(vue_df = analysis_summary$data)


#### Comparing diurnal things
diurnal_depth_analysis = function(vue_df){
  ## Does depth change with phase
  depth_by_tag_and_diurnal = aggregate(vue_df$depth[!is.na(vue_df$depth)], by = list(vue_df$tag_id[!is.na(vue_df$depth)], vue_df$time_of_day[!is.na(vue_df$depth)]), FUN = mean) 
  colnames(depth_by_tag_and_diurnal) = c("tag_id", "period", "mean")
  depth_by_tag_and_diurnal$sd = aggregate(vue_df$depth[!is.na(vue_df$depth)], by = list(vue_df$tag_id[!is.na(vue_df$depth)], vue_df$time_of_day[!is.na(vue_df$depth)]), FUN = sd) 
  
  ## Which tags had detections during all 4 periods
  tags_detected_in_each_period = c()
  for(i in 1:length(unique(depth_by_tag_and_diurnal$tag_id))){
    if(length(which(depth_by_tag_and_diurnal$tag_id == unique(depth_by_tag_and_diurnal$tag_id)[i])) == 4){
      tags_detected_in_each_period = c(tags_detected_in_each_period, unique(depth_by_tag_and_diurnal$tag_id)[i])
    }
  }
  depth_by_diurnal = depth_by_tag_and_diurnal[depth_by_tag_and_diurnal$tag_id %in% tags_detected_in_each_period, ]
  
  depth_by_diurnal = depth_by_diurnal[depth_by_diurnal$mean < 0, ]
  
  png('Depth by period - Fish with data for each period .png', height = 800, width = 1000)
  boxplot(depth_by_diurnal$mean ~ depth_by_diurnal$period)
  dev.off()
  
  ## standardizing depth for each fish relative to the mean depth of that fish
  vue_df$mean_depth = NA
  for(i in 1:length(unique(vue_df$tag_id))){
    vue_df$mean_depth[vue_df$tag_id == unique(vue_df$tag_id)[i]] = vue_df$depth[vue_df$tag_id == unique(vue_df$tag_id)[i]] - mean(vue_df$depth[vue_df$tag_id == unique(vue_df$tag_id)[i]], na.rm = TRUE)
  }
  
  depth_by_tag_and_diurnal_std = aggregate(vue_df$mean_depth[!is.na(vue_df$mean_depth)], by = list(vue_df$tag_id[!is.na(vue_df$mean_depth)], vue_df$time_of_day[!is.na(vue_df$mean_depth)]), FUN = mean) 
  colnames(depth_by_tag_and_diurnal_std) = c("tag_id", "period", "mean")
  depth_by_tag_and_diurnal_std$sd = aggregate(vue_df$mean_depth[!is.na(vue_df$mean_depth)], by = list(vue_df$tag_id[!is.na(vue_df$mean_depth)], vue_df$time_of_day[!is.na(vue_df$mean_depth)]), FUN = sd) 
  
  depth_by_diurnal_std = depth_by_tag_and_diurnal_std[depth_by_tag_and_diurnal_std$tag_id %in% tags_detected_in_each_period, ]
  
  png('Normalized Depth by period - Fish with data for each period .png', height = 800, width = 1000)
  boxplot(depth_by_diurnal_std$mean ~ depth_by_diurnal_std$period)
  dev.off()
  
  TukeyHSD(aov(depth_by_diurnal_std$mean ~ depth_by_diurnal_std$period))
}


diurnal_depth_station_analysis = function(vue_df){
  ## Does depth change with phase
  depth_by_tag_and_diurnal = aggregate(vue_df$station_depth, by = list(vue_df$tag_id, vue_df$time_of_day), FUN = mean) 
  colnames(depth_by_tag_and_diurnal) = c("tag_id", "period", "mean")
  depth_by_tag_and_diurnal$sd = aggregate(vue_df$station_depth, by = list(vue_df$tag_id, vue_df$time_of_day), FUN = sd) 
  
  ## Which tags had detections during all 4 periods
  tags_detected_in_each_period = c()
  for(i in 1:length(unique(depth_by_tag_and_diurnal$tag_id))){
    if(length(which(depth_by_tag_and_diurnal$tag_id == unique(depth_by_tag_and_diurnal$tag_id)[i])) == 4){
      tags_detected_in_each_period = c(tags_detected_in_each_period, unique(depth_by_tag_and_diurnal$tag_id)[i])
    }
  }
  depth_by_diurnal = depth_by_tag_and_diurnal[depth_by_tag_and_diurnal$tag_id %in% tags_detected_in_each_period, ]
  
  depth_by_diurnal = depth_by_diurnal[which(depth_by_diurnal$mean > 0), ]
  
  png('Depth by period - Fish with data for each period .png', height = 800, width = 1000)
  boxplot(depth_by_diurnal$mean ~ depth_by_diurnal$period)
  dev.off()
  
  ## standardizing depth for each fish relative to the mean depth of that fish
  vue_df$mean_station_depth = NA
  for(i in 1:length(unique(vue_df$tag_id))){
    vue_df$mean_depth[vue_df$tag_id == unique(vue_df$tag_id)[i]] = vue_df$station_depth[vue_df$tag_id == unique(vue_df$tag_id)[i]] - mean(vue_df$station_depth[vue_df$tag_id == unique(vue_df$tag_id)[i]], na.rm = TRUE)
  }
  
  depth_by_tag_and_diurnal_std = aggregate(vue_df$mean_depth, by = list(vue_df$tag_id, vue_df$time_of_day), FUN = mean) 
  colnames(depth_by_tag_and_diurnal_std) = c("tag_id", "period", "mean")
  depth_by_tag_and_diurnal_std$sd = aggregate(vue_df$mean_depth, by = list(vue_df$tag_id, vue_df$time_of_day), FUN = sd) 
  
  depth_by_diurnal_std = depth_by_tag_and_diurnal_std[depth_by_tag_and_diurnal_std$tag_id %in% tags_detected_in_each_period, ]
  
  png('Normalized Depth by period - Fish with data for each period .png', height = 800, width = 1000)
  boxplot(depth_by_diurnal_std$mean ~ depth_by_diurnal_std$period)
  dev.off()
  
  TukeyHSD(aov(depth_by_diurnal_std$mean ~ depth_by_diurnal_std$period))
}


##### Current Direction #####


##### Tidal movements #####


##### Temperature Gradients #####

##### Lunar Phases #####
### Assigning Lunar Phase
assign_lunar_phase = function(vue_df){
  # Maps of stations where each station is colored by number of fish detected during that quarter of moon phase.  For entire dataset
  ### A fish detected at that receiver, during that week (moon quadrant) gets a 1. 
  
  ## Constructing a data frame of lunar phases for each day during the period of interest
  lunar_weeks = data.frame('day' = seq(min(vue_df$date), max(vue_df$date), by = 'day'), 'week' = NA, stringsAsFactors = FALSE)
  lunar_weeks$phase = lunar.phase(x = lunar_weeks$day, shift = -10, name = 4)
  starting_lunar_cycle = lunar_weeks$phase[1]
  
  ## Assigning each lunar phase a "week". Ex: The first appearance of a full moon is a 1, the second is a 2 etc....
  lunar_counter = 1
  for(i in 2:length(lunar_weeks$phase)){
    if(lunar_weeks$phase[i] == starting_lunar_cycle & lunar_weeks$phase[i] != lunar_weeks$phase[i-1]){
      lunar_counter = lunar_counter+1
    }
    lunar_weeks$week[i] = lunar_counter
  }
  
  ## Assigning lunar phase and week to vue_data stored within the run obj
  vue_df = merge(x = vue_df, y = lunar_weeks, by.x = 'date', by.y = 'day')
  
  return(vue_df)
}











############## COPYING BECKER ET AL 2016 ##################
# detection format()

library('rgdal')

tag_id = 51588


## Lets look only at tag 51588 first.
t51588 = vue_data[vue_data$tag_id == tag_id, ]
tagging_datetime = as.POSIXct(tagging_data$datetime[tagging_data$vem_tag_id == tag_id])


X = 1:length(t51588$datetime)
ID = rep(tag_id, length(X))
StationCode = t51588$station
DateTime.GMT = with_tz(t51588$datetime, tzone = 'GMT')
TransmitterSerial = t51588$full_tag_id
ReceiverSerial = paste('VR2W-', t51588$receiver, sep = "")
Deploy.Date.GMT = c()
for(i in 1:length(t51588$station)){
  Deploy.Date.GMT = c(Deploy.Date.GMT, receiver_data$deployment_date[which(receiver_data$station_name == t51588$station[i] & receiver_data$deployment_date <= t51588$datetime[i] & receiver_data$recovery_date >= t51588$datetime[i])])
}
Owner = rep('Kevin Weng', length(X))
Lat = t51588$lat
Lon = t51588$lon
  cord.dec = SpatialPoints(cbind(t51588$lon, t51588$lat), proj4string=CRS("+proj=longlat"))
  cord.UTM <- spTransform(cord.dec, CRS("+init=epsg:26963"))
NewEastingUTM = cord.UTM@coords[ ,1]
NewNorthingUTM = cord.UTM@coords[ ,2]
TimeDiff = c(as.numeric(difftime(t51588$datetime[1], tagging_datetime)), as.numeric(diff(t51588$datetime)))

Deploy.Date.GMT = c()
for(i in 1:length(t51588$station)){
  Deploy.Date.GMT = c(Deploy.Date.GMT, as.character(receiver_data$deployment_date[which(receiver_data$station_name == t51588$station[i] & receiver_data$deployment_date <= t51588$datetime[i] & receiver_data$recovery_date >= t51588$datetime[i])]))
}
Deploy.Date.GMT = with_tz(as.POSIXct(Deploy.Date.GMT), 'UTC')

cord.dec = SpatialPoints(cbind(t51588$lon, t51588$lat), proj4string=CRS("+proj=longlat"))
cord.UTM <- spTransform(cord.dec, CRS("+init=epsg:26963"))

indv_df = data.frame('X' = 1:length(t51588$datetime), 
                     'ID' = rep(tag_id, length(X)),
                     'StationCode' = t51588$station,
                     'DateTime.GMT' = with_tz(t51588$datetime, tzone = 'GMT'),
                     'TransmitterSerial' = t51588$full_tag_id,
                     'ReceiverSerial' = paste('VR2W-', t51588$receiver, sep = ""),
                     'Deploy.Date.GMT'  = Deploy.Date.GMT ,
                     'Owner' = rep('Kevin Weng', length(X)),
                     'Date_GMT.x' = with_tz(t51588$datetime, tzone = 'GMT'),
                     'Date_Local' = as.Date(with_tz(t51588$datetime, tzone = 'GMT')),
                     'Deploy.Date.GMT' = as.Date(with_tz(Deploy.Date.GMT, 'HST')),
                     'Lat' = t51588$lat,
                     'Lon' = t51588$lon,
                     'NewEastingUTM' = cord.UTM@coords[ ,1],
                     'NewNorthingUTM' = cord.UTM@coords[ ,2],
                     'TimeDiff' = c(as.numeric(difftime(t51588$datetime[1], tagging_datetime)), as.numeric(diff(t51588$datetime)))
)

#Bring indv_df into move format
# The `move()` function wants the location (x, y), time (in POSIXct format), projection (latlong), data frame name, animal identifier (Tag ID), and sensor. (We could just say sensor="VR2W" for the raw acoustic array).

indv_df.move <- move(x=indv_df$Lon, y=indv_df$Lat, time=indv_df$Date_GMT.x, 
                      proj=CRS("+proj=longlat +datum=WGS84"),
                      data=indv_df, animal=indv_df$TransmitterSerial, sensor="VR2W")

save(indv_df.move, file="indv_df.move.RData")

#Make a Dynamic Brownian Bridge Movement Model (DBBMM)
# We need to project the locations to an Azimuthal EquiDistant projection ("aeqd") to make move happy. Once it is in aeqd projection, we just send it off to the brownian.bridge.dyn() function.
# We need to specify: 
#   - the variable containing the error estimate for each point (approx. detection range, in this case),  
# - the size of the raster with either 
# a RasterLayer object R (raster=R), 
# a cell size x in meters (raster=x) or
# the number of cells d along the largest dimension of the track (dimSize = d).
# - the extension of the bounding box around the animal track (ext=.3 enlarges the edges by about 30%)

# First, check the currect projection
proj4string(indv_df.move)
head(indv_df.move)
indv_df.move$LocationError = 850 #based on range testing
head(indv_df.move)

# Convert projection to Azimuthal Equi-Distance projection (aeqd)
r = spTransform(indv_df.move, center=TRUE)

# Make sure it changed correctly
proj4string(r)

# Make a raster for the UD to plot into
# Start with UTM. These coordinates need to be big enough to cover your data.
cord.dec = SpatialPoints(cbind(c(), t51588$lat), proj4string=CRS("+proj=longlat"))
cord.UTM <- spTransform(cord.dec, CRS("+init=epsg:26963"))

# May need to expand x and y ranges if encountering errors when making the DBBMM in the next chunk
xUTM = raster(xmn=542198.4, xmx=546795, ymn=11906.78, ymx=17151.17, 
              crs =CRS("+proj=utm +zone=20 +datum=WGS84"), resolution=50)
xUTM
class(xUTM)

# We now need to reproject this into aeqd
# Make a dummy object to get the correct projection
# (Use the CRS string from the r object above)
x = raster(xmn=542198.4, xmx=546795, ymn=11906.78, ymx=17151.17, crs = CRS(" +proj=aeqd +ellps=WGS84 +lon_0=-64.641165 +lat_0=17.79437 "))
proj4string(x)

# Ok now use that to reproject
newTemplate = projectExtent(xUTM, proj4string(x))
newTemplate # get the number of pixels in newTemplate and write that number into the next function

# Give newTemplate some values. Make Rep equal to the ncell dimension
ones = rep(1, 9660)
xAEQD = setValues(newTemplate, ones)
xAEQD

class(xAEQD) 

# Reproject the move object r into AEQD 
rNew = spTransform(r, proj4string(xAEQD))
rNew

#we need to make sure that the projection of the move object is in the same format as our raster
proj4string(xAEQD)# "+proj=aeqd +ellps=WGS84 +lon_0=-64.641165 +lat_0=17.79437"
proj4string(rNew)# "+proj=aeqd +ellps=WGS84 +lon_0=-64.641165 +lat_0=17.79437"

# Plot xAEQD and r
plot(xAEQD)
points(rNew)
head(rNew)
max(rNew$TimeDiff)/(60*60)



### Self detections by VR2ARs
tag_id = 61375 # From receiver "Oahu - Makapuu BRFA 336"
station_id = "Oahu - Makapuu BRFA 14"
receiver_id = receiver_data$vr2w_serial[which(receiver_data$station_name == station_id & receiver_data$deployment_date > as.POSIXct('2017-06-01') & !is.na(receiver_data$recovery_date))]
vue_df = raw_vue_data 
vue_df = vue_df[vue_df$datetime > fy2018 & vue_df$tag_id == tag_id & vue_df$receiver == receiver_id, ]
vue_df = clean_vue_data(vue_df, receiver_data)

dist = distance_between_receivers(receiver_df = receiver_data, start_date = fy2018)
dist['Oahu - Makapuu BRFA 14', "Oahu - Makapuu BRFA 336"] * 1000 # m
  # 827


## Binning into hour groups. 
hours_per_bin = 1
hourbins = seq.POSIXt(from = as.POSIXct('2017-08-30'), to = max(vue_df$datetime), by = 'hours') 
  hourbins = hourbins[seq(from = 1, to = length(hourbins), by = hours_per_bin)]
  transmissions_per_hour = 6 * hours_per_bin
  
hourly_self_detections = c()
for(i in 1:length(hourbins)){
  hourly_self_detections = c(hourly_self_detections, length(vue_df$datetime[vue_df$datetime >= hourbins[i] & vue_df$datetime < hourbins[i+1]]))
}

hourly_det = cbind(hourly_self_detections, hourbins)

plot(hourly_det[,1] ~ hourly_det[,2])

# What percentage of hourly detections were less than the threshold?
length(which(hourly_det <= transmissions_per_hour * 0.05)) / length(hourly_det)

# How many total hour bins are we talking about?
length(hourly_det)

## So for 2 receivers separated by 827 m, the number of times shit falls beneath the acceptable threshold is 7.5%. 
## I feel reasonably confident that this means the detection rate is adaquate at half that distance.

## 38% of the time we're below our 25% mark at this distance, which means the majority of the time, we're more than good with half of the current receiver spacing. 
  # to me it stands to reason that at 502 m (half of 1046 m, the dist between two  stations in fence) we're probably well and good.