#### Analysis for Annual Progress Report August 2017

library('marmap') # getNOAAbathy()
library('plotrix') # std.error

se = std.error

##### Objective D: Download and servicing of receiver network.
  #### Plotting Receiver Network Status Maps

### Functions needed for run
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

receiver_data = load_receiver_data("/Users/stephenscherrer/Google Drive/Weng Lab/Data/Bottomfish/Oahu Receiver Data Files/DEPLOYMENT_RECOVERY_LOG.csv")
stations_to_plot = receiver_data[which(receiver_data$deployment_date <= plot_map_for_date & (is.na(receiver_data$recovery_date) | receiver_data$recovery_date >= plot_map_for_date)), ]

#### Assigning a series of dates to get the network status on
dates_to_plot_receiver_map = c(as.POSIXct("2016-07-01"),
                               as.POSIXct("2016-12-07"),
                               as.POSIXct("2017-03-18"),
                               as.POSIXct("2017-06-12"),
                               as.POSIXct("2017-06-30"))


### Looping through all of those dates to produce maps 
for(plot_map_for_date in dates_to_plot_receiver_map){

### Removing anything that's not one of our receivers in Makapuu
station_names_to_keep = c()
for(i in 1:length(stations_to_plot$station_name)){
  temp_name = strsplit(stations_to_plot$station_name[i], split = " ")[[1]]
  if(temp_name[3] == 'Makapuu'){
    stations_to_plot$station_name[i] = temp_name[5]
    station_names_to_keep = c(station_names_to_keep, temp_name[5])
  }
}

stations_to_plot = stations_to_plot[stations_to_plot$station_name %in% station_names_to_keep, ] 

#### Plotting Map of Receivers in Study

pdf(paste('Station Map ', plot_map_for_date, '.pdf', sep = ""), width = 6, height = 6 )
bathymetry = getNOAA.bathy(lon1 = -157.75, 
                           lon2 = -157.47, 
                           lat1 = 21.25, 
                           lat2 = 21.48,
                           resolution = 1)

## Plotting basemap
plot.bathy(bathymetry, land = TRUE, bpal = "white", image=TRUE, deepest.isobath = c(-10000), shallowest.isobath = c(0), step = c(100), drawlabels = TRUE, lty = 1.5, col = 'gray')
## Adding scale legend
scaleBathy(bathymetry, deg = .10, cex = .5, y = 21.255, x = -157.73)
#scaleBathy(bathymetry, deg = .48, cex = .5)

## Adding BRFA boundaries
brfa_e = as.data.frame(rbind(c(-157.68333333, 21.28333333), 
                             c(-157.53333333, 21.28333333), 
                             c(-157.53333333, 21.4166666), 
                             c(-157.7, 21.4166666)))
colnames(brfa_e) = c('lon', 'lat')
lines(lat~lon, data = brfa_e, pch = 19, col = 'purple', lwd = 3, cex = .6)


text((lat - .007) ~ lon, labels = stations_to_plot$station, data = stations_to_plot, cex = .5)
points(lat ~ lon, data = stations_to_plot[which(stations_to_plot$recovered == ""), ],  pch = 19, col = 'blue')
points(lat ~ lon, data = stations_to_plot[which(stations_to_plot$recovered != ""), ],  pch = 19, col = 'red')


brfa_f = as.data.frame(rbind(c(-157.5666667, 20.9666667), 
                             c(-157.5666667, 21.0333333333),
                             c(-157.3666667, 21.0333333333),
                             c(-157.3666667, 20.9666667),
                             c(-157.5666667, 20.9666667)))
colnames(brfa_f) = c('lon', 'lat')


#  lines(lat~lon, data = brfa_f, pch = 19, col = 'purple', cex = .6)
dev.off()
}


##### Objective E:
#### Algorthm for determining track status



### Status Check Functions
    track_status_check_1 = function(indv_df){
      ## Was tag detected on receiver array?
      dim(indv_df)[1] > 0
    }
    
    track_status_check_2 = function(indv_df){
      ## Was the time at liberty longer than 14 days?
      abs(difftime(min(indv_df$datetime), max(indv_df$datetime), units = 'days')) > 14
    }
    
    track_status_check_3 = function(indv_df){
      ## Was the tag detected at more than one station (excluding tagging location?)
      length(unique(indv_df$station[indv_df$station != "Tagging Location"])) > 1
    }
    
    track_status_check_4 = function(indv_df){
      ## Was movement detected between stations after 14 days?
      ## 4. Did movement occur after 14 days?
      dates_of_station_switching = c()
      # Looping through each indvidiual detection to get when station switching occurred
      for(j in 2:length(indv_df$station)){
        # If the current station is not the same as the previous
        if(indv_df$station[j] != indv_df$station[j-1]){
          dates_of_station_switching = c(dates_of_station_switching, indv_df$datetime[j])
        }
      }
      
      any(abs(difftime(min(indv_df$datetime), as.POSIXct(dates_of_station_switching, origin="1970-01-01"), units = 'days')) >= 14)
    }
    
    track_status_check_5 = function(indv_df){
      ## Does tag have depth sensor?
      any(!is.na(indv_df$depth))
    }
    
    track_status_check_6 = function(indv_df){
      ## Are there any changes in depth after 14 days at liberty?
      indv_df_after_14_days = indv_df[indv_df$datetime >= min(indv_df$datetime) + 14*24*60*60, ]
      (max(indv_df_after_14_days$depth[!is.na(indv_df_after_14_days$depth)]) - min(indv_df_after_14_days$depth[!is.na(indv_df_after_14_days$depth)]) >  5)
    }
    
    track_status_check_7 = function(indv_df, receiver_df){
      ## 7. Mean Daily Detections > 1 standard deviation of daily detections in a single receiver deployment period
      status_changes = c()
      
      ## We are binning the number of detections by date, so we get the day each detection occurred
      indv_df$date = floor_date(indv_df$datetime, unit = "day")
      
      last_station_detected = indv_df$station[length(indv_df$datetime)]
      receiver_dates = data.frame("deployment_date" = floor_date(unique(receiver_df$deployment_date[receiver_df$station_name == last_station_detected]), unit = 'days'), 
                                  "recovery_date"  = floor_date(unique(receiver_df$recovery_date[receiver_df$station_name == last_station_detected]), unit = 'days'))
      
      ## Removing station if it's still deployed
      receiver_dates = receiver_dates[!is.na(receiver_dates$recovery_date), ]
      
      # Removing any dates where the receiver was recovered before tag was present at that receiver
      receiver_dates = receiver_dates[receiver_dates$recovery_date >= min(indv_df$date[indv_df$station == last_station_detected]), ]
      
      # Removing any dates where the receiver ere deployed after tag was no longer present at that receiver
      receiver_dates = receiver_dates[receiver_dates$deployment_date <= max(indv_df$date[indv_df$station == last_station_detected]), ]
      
      # Since we will loop through receiver dates, we want to adjust the first deployment date to reflect when the tag was first detected at the receiver (in the event that a fish was tagged, then ended up at that station)
      if(min(indv_df$datetime[indv_df$station == last_station_detected]) > receiver_dates$deployment_date[1]){
        receiver_dates$deployment_date[1] = min(indv_df$date[indv_df$station == last_station_detected])
      }
      
      # Since we will loop through receiver dates, we want to adjust the last recovery date to reflect when the tag was last detected at the receiver (in the event that a tag battery died).
      if(max(indv_df$datetime[indv_df$station == last_station_detected]) < receiver_dates$recovery_date[length(receiver_dates$recovery_date)]){
        receiver_dates$recovery_date[length(receiver_dates$recovery_date)] = max(indv_df$date[indv_df$station == last_station_detected] + 24*60*60) 
      }

      ## Now looping through all receiver dates
      for(j in 1:length(receiver_dates$deployment_date)){
        # Subsetting just detections of an individual from the day after a receiver was deployed, until the day before the receiver was recovered
        detections_during_receiver_window = indv_df[which(indv_df$date >= receiver_dates$deployment_date[j] & indv_df$date <= receiver_dates$recovery_date[j]), ]
        if(dim(detections_during_receiver_window)[1] > 0){
          detections_per_day = aggregate(detections_during_receiver_window$date, by = list(detections_during_receiver_window$date), FUN = length)
          colnames(detections_per_day) = c('date', 'n_detections')
          # Adding in any dates where no detections happened
          sequence_of_dates = seq.POSIXt(from = receiver_dates$deployment_date[j] - 24*60*60, to = receiver_dates$recovery_date[j] + 24*60*60,  by = 'day')
          if((length(sequence_of_dates) - length(which(sequence_of_dates %in% detections_per_day$date))) > 0){
            detections_per_day = rbind(detections_per_day, data.frame('date' = sequence_of_dates[-which(sequence_of_dates %in% detections_per_day$date)], 'n_detections' = 0))
          }
          ## Assigning status
          status_changes = c(status_changes, 2*sd(detections_per_day$n_detections) >= mean(detections_per_day$n_detections))
          # print(paste('Tag id:', tag_id, 'sd:', round(sd(detections_per_day$n_detections)), 'mean:', round(mean(detections_per_day$n_detections)), 'cv:', round((sd(detections_per_day$n_detections)/mean(detections_per_day$n_detections))*100), "2 sdev:", 2*sd(detections_per_day$n_detections) >= mean(detections_per_day$n_detections), "1 sdev:", 1*sd(detections_per_day$n_detections) >= mean(detections_per_day$n_detections)))
        }
      }
        # If there are more receiver status bins with True than False, we're calling the fish alive
        (length(which(status_changes == TRUE)) / length(status_changes)) >= 0.5
    }
    
    
    ##### TUNING CHECK FUNCTION 7
    
    ### CV is the ratio std/mean * 100. Used this run over dead tags we know to get an idea of what this number is for a dead tag
    dead_tags = c(26, 15, 27, 28, 21, 22, 23)
    for(tag_id in dead_tags){
      indv_df = vue_data[vue_data$tag_id == tag_id, ]
      indv_df$station[which(indv_df$station == "Makapuu BRFA 39"     )] = 'Oahu - Makapuu BRFA 39'
      print(track_status_check_7(indv_df, receiver_data))
    }
    
  determine_track_status = function(vue_df, tagging_df, receiver_df){
    #### Creating a dataframe with all tags to put their status in. This will be returned at the end
    tag_status_df = data.frame("tag_id" = tagging_df$vem_tag_id[tagging_df$species == "Opakapaka"], "tagging_date" = tagging_df$datetime[tagging_df$species == "Opakapaka"] , "status" = '', "rationale" = "", stringsAsFactors = FALSE)
      
    #### Looping through individual tags
    for(i in 1:length(tag_status_df$tag_id)){
    tag_id = tag_status_df$tag_id[i]
    indv_data = vue_data[vue_data$tag_id == tag_id, ]
    #### Now climbing down the decision tree
    ## Was tag detected on the array?
    if(track_status_check_1(indv_df = indv_data)){
      ## Was tag detected for more than 14 unique days?
      if(track_status_check_2(indv_df = indv_data)){
        if(track_status_check_3(indv_df = indv_data)){
          if(!track_status_check_4(indv_df = indv_data)){
            if(track_status_check_5(indv_df = indv_data)){
              if(track_status_check_6(indv_df = indv_data)){
                tag_status_df$status[i] = 'Valid'
                tag_status_df$rationale[i] = 'Depth changes more than 5 m after 14 days at liberty'
                }else{
                  tag_status_df$status[i] = 'Expired'
                  tag_status_df$rationale[i] = 'Depth changes less than 5 m after 14 days at liberty'
                }
              }else{
                if(track_status_check_7(indv_df = indv_data, receiver_df = receiver_data)){
                  tag_status_df$status[i] = 'Valid'
                  tag_status_df$rationale[i] = 'Mean daily detections < two standard deviations'
                }else{
                  tag_status_df$status[i] = 'Expired'
                  tag_status_df$rationale[i] = 'Mean daily detections > two standard deviations'
                }
              }
          }else{
            tag_status_df$status[i] = 'Valid'
            tag_status_df$rationale[i] = 'Movement detected after 14 Days' 
          }
        }else{
          if(track_status_check_5(indv_df = indv_data)){
            if(track_status_check_6(indv_df = indv_data)){
              tag_status_df$status[i] = 'Valid'
              tag_status_df$rationale[i] = 'Depth changes more than 5 m after 14 days at liberty'
            }else{
              tag_status_df$status[i] = 'Expired'
              tag_status_df$rationale[i] = 'Depth changes less than 5 m after 14 days at liberty'
            }
          }else{
            if(track_status_check_7(indv_df = indv_data, receiver_df = receiver_data)){
              tag_status_df$status[i] = 'Valid'
              tag_status_df$rationale[i] = 'Mean daily detections < two standard deviations'
            }else{
              tag_status_df$status[i] = 'Expired'
              tag_status_df$rationale[i] = 'Mean daily detections > two standard deviations'
            }
          }
        }
      }else{
        tag_status_df$status[i] = 'Uncertain'
        tag_status_df$rationale[i] = 'Time at liberty less than 14 days' 
      }
    }else{
      tag_status_df$status[i] = 'Uncertain'
      tag_status_df$rationale[i] = 'Tag was not detected on receiver array'
    }
    }
    
    tag_status = list('status_df' = tag_status_df,  'valid_tracks' = tag_status_df$tag_id[tag_status_df$status == 'Valid'], 'uncertain' = tag_status_df$tag_id[tag_status_df$status == 'Uncertain'], 'expired' = tag_status_df$tag_id[tag_status_df$status == 'Expired'])
  return(tag_status)
  }
      
### Setup for running analysis
  ## Removing false detections
vue_data = vue_data[vue_data$detection_status == TRUE, ]
  ## Creating a tagging detection
vue_data = generate_tagging_detection(tagging_data = tagging_data, vue_data = vue_data)
  ## Getting tag status
tag_status = determine_track_status(vue_df = vue_data, tagging_df = tagging_data, receiver_df = receiver_data)
write.csv(tag_status$status_df, 'tag status.csv', row.names = FALSE)
vue_data = vue_data[vue_data$station != 'Tagging Location', ]

#### How did we do for phase 2
tag_status_phase_2 = list()
tag_status_phase_2$status_df = tag_status$status_df[tag_status$status_df$tagging_date > as.POSIXct("2015-03-01"), ]
tag_status_phase_2$valid_tracks = tag_status_phase_2$status_df$tag_id[tag_status_phase_2$status_df$status == "Valid"]
tag_status_phase_2$uncertain = tag_status_phase_2$status_df$tag_id[tag_status_phase_2$status_df$status == "Uncertain"]
tag_status_phase_2$expired = tag_status_phase_2$status_df$tag_id[tag_status_phase_2$status_df$status == "Expired"]


phase_2_all_tags = run(run_description = "Phase 2 all tracks current with fdf_new_func_test", 
                       plot = TRUE, 
                       vue_df = vue_data, 
                       tagging_df = tagging_data, 
                       receiver_df = receiver_data, 
                       start_date = "2015-1-1", 
                       end_date = NULL, 
                       tag_ids = NULL, 
                       region = 'Makapuu', 
                       valid_tracks = tag_status$valid_tracks, 
                       questionable_tracks = tag_status$uncertain,
                       expired_tracks = tag_status$expired)
      

scenario_1 = run(run_description = "Phase 2 scenario 1 with fdf", 
                         vue_df = vue_data, 
                         tagging_df = tagging_data, 
                         receiver_df = receiver_data, 
                         start_date = "2015-1-1", 
                         end_date = NULL, 
                         tag_ids = tag_status$valid_tracks, 
                         plot = TRUE,
                         report = TRUE,
                         region = 'Makapuu')

scenario_2 = run(run_description = "Phase 2 scenario 2 with fdf", 
                                          vue_df = vue_data, 
                                          tagging_df = tagging_data, 
                                          receiver_df = receiver_data, 
                                          start_date = "2015-1-1", 
                                          end_date = NULL, 
                                          tag_ids = c(tag_status$valid_tracks, tag_status$uncertain), 
                                          plot = TRUE, 
                                          region = 'Makapuu')




###### Change this bit to change what anlayis is being looked at ######
run_results = scenario_2

##### Objective E: Analyze data to getst the following hypotheses - Data screening
## Total fish tagged in phase 2
length(tag_status_phase_2$valid_tracks) + length(tag_status_phase_2$uncertain) + length(tag_status_phase_2$expired)

## Number of valid tracks
length(tag_status_phase_2$valid_tracks)
## Number of questionable tracks that are in data set
length(which(tag_status_phase_2$uncertain %in% vue_data$tag_id))
## Number of questionable and valid tracks
length(tag_status_phase_2$valid_tracks) + length(which(tag_status_phase_2$uncertain %in% vue_data$tag_id))
## Number of dead tracks
length(tag_status_phase_2$expired)
## Number of tracks with no data (not on array)
length(tag_status_phase_2$status_df$tag_id[tag_status_phase_2$status_df$rationale == "Tag was not detected on receiver array"])

## Survivor Recovery percent - Valid
round(length(tag_status_phase_2$valid_tracks) / (length(tag_status_phase_2$valid_tracks)+length(tag_status_phase_2$uncertain)+length(tag_status_phase_2$expired)) * 100, digits = 1)
## Recovery percent - Valid + Uncertain
round((length(tag_status_phase_2$valid_tracks)+length(which(tag_status_phase_2$uncertain %in% vue_data$tag_id))) / (length(tag_status_phase_2$valid_tracks)+length(tag_status_phase_2$uncertain)+length(tag_status_phase_2$expired)) * 100, digits = 1)


#### H1. Bottomfish routinely move across the borders of existing BRFAs. 

## Total BRFA Crossings 
run_results$brfa_stats$total_crossings = rowSums(cbind(run_results$brfa_stats$in_to_out, run_results$brfa_stats$out_to_in))

## Number of tracks making BRFA crossings - All Fish
length(which(run_results$brfa_stats$total_crossings > 0))

### Number of tag tracks
length(run_results$tag_ids)

## Number of movements across brfa boundaries - All Fish
sum(run_results$brfa_stats$total_crossings)

## The number of times crossings were recorded - All Fish
table(run_results$brfa_stats$total_crossings)
  ## Top row is number of crossings, bottomrow is number of fish

## Summary stats on number of movements - fish that crossed boundary
round(mean(run_results$brfa_stats$total_crossings[run_results$brfa_stats$total_crossings > 0]), digits = 2)
round(se(run_results$brfa_stats$total_crossings[run_results$brfa_stats$total_crossings > 0]), digits = 2)
fivenum(run_results$brfa_stats$total_crossings[run_results$brfa_stats$total_crossings > 0])

## Number of fish that didn't cross boundary
length(which(run_results$brfa_stats$total_crossings == 0))

## Summary stats on number of movements - All Fish
round(mean(run_results$brfa_stats$total_crossings), digits = 2)
round(se(run_results$brfa_stats$total_crossings), digits = 2)
fivenum(run_results$brfa_stats$total_crossings)

## Number of movements standardized by time at liberty - fish that crossed boundary
round(mean(run_results$brfa_movements_standardized_by_time_at_liberty$total[run_results$brfa_movements_standardized_by_time_at_liberty$total > 0]), digits = 2)
round(se(run_results$brfa_movements_standardized_by_time_at_liberty$total[run_results$brfa_movements_standardized_by_time_at_liberty$total > 0]), digits = 2)

## Number of days for a movement - fish that crossed boundary
round(1/mean(run_results$brfa_movements_standardized_by_time_at_liberty$total[run_results$brfa_movements_standardized_by_time_at_liberty$total > 0]), digits = 2)
round(fivenum(run_results$brfa_movements_standardized_by_time_at_liberty$total[run_results$brfa_movements_standardized_by_time_at_liberty$total > 0]), digits = 2)

## Number of tracks not making BRFA crossings 
length(which(run_results$brfa_stats$total_crossings == 0))

## Number of movements standardized by time at liberty - All Fish
round(mean(run_results$brfa_movements_standardized_by_time_at_liberty$total), digits = 2)
round(se(run_results$brfa_movements_standardized_by_time_at_liberty$total), digits = 2)

## Number of days for a movement - All Fish
round(1/mean(run_results$brfa_movements_standardized_by_time_at_liberty$total), digits = 2)
round(fivenum(run_results$brfa_movements_standardized_by_time_at_liberty$total), digits = 2)

# ## Number of days a fish was in and out - Fish that crossed the line
# mean(run_results$brfa_stats$time_tracked_in[run_results$brfa_movements_standardized_by_time_at_liberty$total > 0])
# se(run_results$brfa_stats$time_tracked_in[run_results$brfa_movements_standardized_by_time_at_liberty$total > 0])
# fivenum(run_results$brfa_stats$time_tracked_in[run_results$brfa_movements_standardized_by_time_at_liberty$total > 0])

## Total time tracked for all fish
run_results$brfa_stats$total_time_tracked = rowSums(cbind(run_results$brfa_stats$time_tracked_in, run_results$brfa_stats$time_tracked_out))

## Summary Stats total time tracked - Fish that crossed the line
round(mean(run_results$brfa_stats$total_time_tracked[run_results$brfa_movements_standardized_by_time_at_liberty$total > 0]), digits = 2)
round(se(run_results$brfa_stats$total_time_tracked[run_results$brfa_movements_standardized_by_time_at_liberty$total > 0]), digits = 2)
round(fivenum(run_results$brfa_stats$total_time_tracked[run_results$brfa_movements_standardized_by_time_at_liberty$total > 0]), digits = 2)

## Percent tracked in
run_results$brfa_stats$percent_tracked_in = run_results$brfa_stats$time_tracked_in / rowSums(cbind(run_results$brfa_stats$time_tracked_in, run_results$brfa_stats$time_tracked_out)) * 100

## Percent tracked out
run_results$brfa_stats$percent_tracked_out = run_results$brfa_stats$time_tracked_out / rowSums(cbind(run_results$brfa_stats$time_tracked_in, run_results$brfa_stats$time_tracked_out)) * 100

## Summary stats time tracked in - Fish that crossed lines
round(mean(run_results$brfa_stats$percent_tracked_in[run_results$brfa_movements_standardized_by_time_at_liberty$total > 0]), digits = 1)
round(se(run_results$brfa_stats$percent_tracked_in[run_results$brfa_movements_standardized_by_time_at_liberty$total > 0]), digits = 1)
round(fivenum(run_results$brfa_stats$percent_tracked_in[run_results$brfa_movements_standardized_by_time_at_liberty$total > 0]), digits = 1)

## Summary Stats total time tracked - All Fish
round(mean(run_results$brfa_stats$total_time_tracked), digits = 1)
round(se(run_results$brfa_stats$total_time_tracked), digits = 1)
round(fivenum(run_results$brfa_stats$total_time_tracked), digits = 1)

## Summary stats time tracked in - All Fish
round(mean(run_results$brfa_stats$percent_tracked_in), digits = 1)
round(se(run_results$brfa_stats$percent_tracked_in), digits = 1)
round(fivenum(run_results$brfa_stats$percent_tracked_in), digits = 1)


###### H2. Bottomfish movements exceed the scale of individual fishery closed areas (BRFAs). 

### BRFA Area Summary - sqrt-ed
round(fivenum(sqrt(get_brfa_areas())), 1)

### Number of tag tracks
length(run_results$tag_ids)

### Number of tracks with 3 or more positions
length(run_results$homerange[,"max polygon area"][which(run_results$n_stations_by_tag >= 3)])

### Summary Stats - homerange area for tags with 3 or more positions
mean(run_results$homerange[,"max polygon area"][which(run_results$n_stations_by_tag >= 3)])
se(run_results$homerange[,"max polygon area"][which(run_results$n_stations_by_tag >= 3)])
fivenum(run_results$homerange[,"max polygon area"][which(run_results$n_stations_by_tag >= 3)])

### Summary Stats - Sqrt of homerange area for tags with 3 or more positions
round(mean(sqrt(run_results$homerange[,"max polygon area"][which(run_results$n_stations_by_tag >= 3)])), digits = 1)
se(sqrt(run_results$homerange[,"max polygon area"][which(run_results$n_stations_by_tag >= 3)]))
fivenum(sqrt(run_results$homerange[,"max polygon area"][which(run_results$n_stations_by_tag >= 3)]))

### Number of tag tracks
length(run_results$tag_ids)

### Summary Stats - linear homerange distance for all tags
round(mean(run_results$homerange[,"max linear area"]), digits = 1)
round(se(run_results$homerange[,"max linear area"]), digits = 1)
round(fivenum(run_results$homerange[,"max linear area"]), digits = 1)

####### H3. Bottomfish do not utilize habitat uniformly.
View(run_results$movement_graph)
  # Note:   # Rows indicate movement from a receiver
            # Columns indicate movement to a receiver

## Creating a master graph that we can add the results of each individual graph too.
all_indv_graph_presence = run_results$movement_graph
all_indv_graph_presence[all_indv_graph_presence != 0] = 0

graph_dims = c()

## Looping through making an individual graph for each fish. Turning that graph into a 1 or zero based on if they were detected making that movement
for(i in 1:length(run_results$tag_ids)){
  indv_graph = get_graph(vue_df = run_results$data, receiver_df = run_results$receiver_data, tag_ids = run_results$tag_ids[i], remove_zeros = FALSE, igraph = FALSE)
  ones_graph = indv_graph
  ones_graph[which(ones_graph > 0, arr.ind = TRUE)] = 1
  graph_dims = c(graph_dims, sum(ones_graph))
  all_indv_graph_presence[which(rownames(all_indv_graph_presence) %in% rownames(ones_graph)), which(colnames(all_indv_graph_presence) %in% colnames(ones_graph))] =  all_indv_graph_presence[which(rownames(all_indv_graph_presence) %in% rownames(ones_graph)), which(colnames(all_indv_graph_presence) %in% colnames(ones_graph))] + ones_graph
}


### By how many fish were detected making no movements
length(which(graph_dims == 1))
### What was the most common number of receivers? were most fish stationary?
  mode(graph_dims)

### Which three stations were the most fish resident?
### Stationary Graph - Graph of just the loops
residence_graph = all_indv_graph_presence * diag(dim(all_indv_graph_presence)[1])

## Which node reported the most fish resident
which(residence_graph == sort(residence_graph, decreasing = TRUE)[1], arr.ind = TRUE)
residence_graph[which(residence_graph == sort(residence_graph, decreasing = TRUE)[1], arr.ind = TRUE)]
## Which node reported the second most fish resident
which(residence_graph == sort(residence_graph, decreasing = TRUE)[2], arr.ind = TRUE)
residence_graph[which(residence_graph == sort(residence_graph, decreasing = TRUE)[2], arr.ind = TRUE)]
## Which node reported the thrid most fish resident
which(residence_graph == sort(residence_graph, decreasing = TRUE)[3], arr.ind = TRUE)
residence_graph[which(residence_graph == sort(residence_graph, decreasing = TRUE)[3], arr.ind = TRUE)]

### Removing loops from the presence graph
inverse_identity = abs(diag(dim(all_indv_graph_presence)[1]) - 1)

### Constructing graph of just movement
movement_graph = all_indv_graph_presence * inverse_identity

### Which path was used by the most fish?
which(movement_graph == unique(sort(movement_graph, decreasing = TRUE))[1], arr.ind = TRUE)
## and by how many fish?
movement_graph[movement_graph == unique(sort(movement_graph, decreasing = TRUE))[1]][1]
### Which path was used by the second most fish?
which(movement_graph == unique(sort(movement_graph, decreasing = TRUE))[2], arr.ind = TRUE)
## and by how many fish?
movement_graph[movement_graph == unique(sort(movement_graph, decreasing = TRUE))[2]][1]
### Which path was used by the third most fish?
which(movement_graph == unique(sort(movement_graph, decreasing = TRUE))[3], arr.ind = TRUE)
## and by how many fish?
movement_graph[movement_graph == unique(sort(movement_graph, decreasing = TRUE))[3]][1]



#### Movement into and out of BRFA

## What stations are in the BRFA?
stations_in_out = data.frame('station' = colnames(movement_graph), 'inside' = FALSE, stringsAsFactors = FALSE)
for(i in 1:length(stations_in_out$station)){
  stations_in_out$inside[i] = in_brfa_e(lat = run_results$data[which(run_results$data$station == stations_in_out$station[i]), ][1, "lat"], 
            lon = run_results$data[which(run_results$data$station == stations_in_out$station[i]), ][1, "lon"])
}

## Matrix of movement in to out
in_to_out_ones = all_indv_graph_presence[rownames(all_indv_graph_presence) %in% stations_in_out$station[stations_in_out$inside == TRUE], colnames(all_indv_graph_presence) %in% stations_in_out$station[stations_in_out$inside == FALSE]]
in_to_out_counts = run_results$movement_graph[rownames(run_results$movement_graph) %in% stations_in_out$station[stations_in_out$inside == TRUE], colnames(run_results$movement_graph) %in% stations_in_out$station[stations_in_out$inside == FALSE]]

### Which Stations facilitated movemeents out of the BRFA? - origin
rownames(in_to_out_ones)
(sort(rowSums(in_to_out_ones), decreasing = TRUE))[as.numeric(sort(rowSums(in_to_out_ones), decreasing = TRUE)) != 0]
as.numeric(sort(rowSums(in_to_out_ones), decreasing = TRUE))

### Depths of these receivers - origin
in_out_endpoint_station_depths = run_results$receiver_data$depth[run_results$receiver_data$station_name %in% names(sort(rowSums(in_to_out_ones), decreasing = TRUE))[as.numeric(sort(rowSums(in_to_out_ones), decreasing = TRUE)) != 0]]
fath_to_m(range(as.numeric(unlist(sapply(X = in_out_endpoint_station_depths, FUN = strsplit, split = " "))[ c(TRUE,FALSE) ])))

### Which Stations facilitated movemeents out of the BRFA? - endpoint
colnames(in_to_out_ones)
sort(colSums(in_to_out_ones), decreasing = TRUE)[as.numeric(sort(colSums(in_to_out_ones), decreasing = TRUE)) != 0]
as.numeric(sort(colSums(in_to_out_ones), decreasing = TRUE))
length(sort(colSums(in_to_out_ones), decreasing = TRUE)[as.numeric(sort(colSums(in_to_out_ones), decreasing = TRUE)) != 0])
 # - stations that were in the north fence
### Depths of these receivers - endpoints
in_out_endpoint_station_depths = run_results$receiver_data$depth[run_results$receiver_data$station_name %in% names(sort(colSums(in_to_out_ones), decreasing = TRUE))[as.numeric(sort(colSums(in_to_out_ones), decreasing = TRUE)) != 0]]
fath_to_m(range(as.numeric(unlist(sapply(X = in_out_endpoint_station_depths, FUN = strsplit, split = " "))[ c(TRUE,FALSE) ])))

### By how many fish was the most frequented path observed?
unique(sort(in_to_out_ones, decreasing = TRUE))[1]
### Which path was used by the most fish?
data.frame('from' = rownames(in_to_out_ones)[which(in_to_out_ones == unique(sort(in_to_out_ones, decreasing = TRUE))[1], arr.ind = TRUE)[, "row"]], 'to' = colnames(in_to_out_ones)[which(in_to_out_ones == unique(sort(in_to_out_ones, decreasing = TRUE))[1], arr.ind = TRUE)[, "col"]])

 
### By how many fish was the second most frequented path observed
unique(sort(in_to_out_ones, decreasing = TRUE))[2]
### Which path was used by the second most fish?
data.frame('from' = rownames(in_to_out_ones)[which(in_to_out_ones == unique(sort(in_to_out_ones, decreasing = TRUE))[2], arr.ind = TRUE)[, "row"]], 'to' = colnames(in_to_out_ones)[which(in_to_out_ones == unique(sort(in_to_out_ones, decreasing = TRUE))[2], arr.ind = TRUE)[, "col"]])


## and by how many fish?
### Which path(s) was used by the third most fish?
data.frame('from' = rownames(in_to_out_ones)[which(in_to_out_ones == unique(sort(in_to_out_ones, decreasing = TRUE))[3], arr.ind = TRUE)[, "row"]], 'to' = colnames(in_to_out_ones)[which(in_to_out_ones == unique(sort(in_to_out_ones, decreasing = TRUE))[3], arr.ind = TRUE)[, "col"]])
unique(sort(in_to_out_ones, decreasing = TRUE))[3]

## What was the path used the most
  # From
rownames(in_to_out_ones)[which(in_to_out_counts == max(in_to_out_counts), arr.ind = TRUE)[1]]
  # To
colnames(in_to_out_ones)[which(in_to_out_counts == max(in_to_out_counts), arr.ind = TRUE)[2]]
## How many times was it used?
max(in_to_out_counts)


## Matrix of movement out to in
out_to_in_ones = all_indv_graph_presence[rownames(all_indv_graph_presence) %in% stations_in_out$station[stations_in_out$inside == FALSE], colnames(all_indv_graph_presence) %in% stations_in_out$station[stations_in_out$inside == TRUE]]
out_to_in_counts = run_results$movement_graph[rownames(run_results$movement_graph) %in% stations_in_out$station[stations_in_out$inside == FALSE], colnames(run_results$movement_graph) %in% stations_in_out$station[stations_in_out$inside == TRUE]]

### Which Stations facilitated movements in to the BRFA? - origin
rownames(out_to_in_ones)
(sort(rowSums(out_to_in_ones), decreasing = TRUE))[as.numeric(sort(rowSums(out_to_in_ones), decreasing = TRUE)) != 0]
as.numeric(sort(rowSums(out_to_in_ones), decreasing = TRUE))

### Depths of these receivers - origin
out_in_endpoint_station_depths = run_results$receiver_data$depth[run_results$receiver_data$station_name %in% names(sort(rowSums(out_to_in_ones), decreasing = TRUE))[as.numeric(sort(rowSums(out_to_in_ones), decreasing = TRUE)) != 0]]
round(fath_to_m(range(as.numeric(unlist(sapply(X = out_in_endpoint_station_depths, FUN = strsplit, split = " "))[ c(TRUE,FALSE) ]))), digits = 1)

### Which Stations facilitated movements in to the BRFA? - endpoint
colnames(out_to_in_ones)
(sort(colSums(out_to_in_ones), decreasing = TRUE))[as.numeric(sort(colSums(out_to_in_ones), decreasing = TRUE)) != 0]
as.numeric(sort(colSums(out_to_in_ones), decreasing = TRUE))

### Depths of these receivers - endpoints
in_out_endpoint_station_depths = run_results$receiver_data$depth[run_results$receiver_data$station_name %in% names(sort(colSums(out_to_in_ones), decreasing = TRUE))[as.numeric(sort(colSums(out_to_in_ones), decreasing = TRUE)) != 0]]
round(fath_to_m(range(as.numeric(unlist(sapply(X = in_out_endpoint_station_depths, FUN = strsplit, split = " "))[ c(TRUE,FALSE) ]))), digits = 1)

### Which path was used by the most fish?
data.frame('from' = rownames(out_to_in_ones)[which(out_to_in_ones == unique(sort(out_to_in_ones, decreasing = TRUE))[1], arr.ind = TRUE)[, "row"]], 'to' = colnames(out_to_in_ones)[which(out_to_in_ones == unique(sort(out_to_in_ones, decreasing = TRUE))[1], arr.ind = TRUE)[, "col"]])
## and by how many fish?
unique(sort(out_to_in_ones, decreasing = TRUE))[1]

### Which path was used by the second most fish?
data.frame('from' = rownames(out_to_in_ones)[which(out_to_in_ones == unique(sort(out_to_in_ones, decreasing = TRUE))[2], arr.ind = TRUE)[, "row"]], 'to' = colnames(out_to_in_ones)[which(out_to_in_ones == unique(sort(out_to_in_ones, decreasing = TRUE))[2], arr.ind = TRUE)[, "col"]])
unique(sort(out_to_in_ones, decreasing = TRUE))[2]

## and by how many fish?
### Which path(s) was used by the third most fish?
data.frame('from' = rownames(out_to_in_ones)[which(out_to_in_ones == unique(sort(out_to_in_ones, decreasing = TRUE))[3], arr.ind = TRUE)[, "row"]], 'to' = colnames(out_to_in_ones)[which(out_to_in_ones == unique(sort(out_to_in_ones, decreasing = TRUE))[3], arr.ind = TRUE)[, "col"]])
unique(sort(out_to_in_ones, decreasing = TRUE))[3]

## Which was most traveled path
rownames(out_to_in_counts)[which(out_to_in_counts == max(out_to_in_counts), arr.ind = TRUE)[1]]
colnames(out_to_in_counts)[which(out_to_in_counts == max(out_to_in_counts), arr.ind = TRUE)[2]]
## and by how much
max(out_to_in_counts)

### How about within the BRFA?
## Matrix of movement in to in
in_to_in_ones = all_indv_graph_presence[rownames(all_indv_graph_presence) %in% stations_in_out$station[stations_in_out$inside == TRUE], colnames(all_indv_graph_presence) %in% stations_in_out$station[stations_in_out$inside == TRUE]]
in_to_in_counts = run_results$movement_graph[rownames(run_results$movement_graph) %in% stations_in_out$station[stations_in_out$inside == TRUE], colnames(run_results$movement_graph) %in% stations_in_out$station[stations_in_out$inside == TRUE]]
inverse_identity =  abs(diag(dim(in_to_in_ones)[1]) - 1)
in_to_in_ones = in_to_in_ones * inverse_identity
## From station
rownames(in_to_in_ones)[which(in_to_in_ones == max(in_to_in_ones), arr.ind = TRUE)[, 'row']]
## To station
colnames(in_to_in_ones)[which(in_to_in_ones == max(in_to_in_ones), arr.ind = TRUE)[, 'col']]
## Used How many total times?
in_to_in_counts[which(in_to_in_ones == max(in_to_in_ones), arr.ind = TRUE)]
## By how many total fish?
in_to_in_ones[which(in_to_in_ones == max(in_to_in_ones), arr.ind = TRUE)]

### How about outside the BRFA?
## Matrix of movement in to in
out_to_out_ones = all_indv_graph_presence[rownames(all_indv_graph_presence) %in% stations_in_out$station[stations_in_out$inside == FALSE], colnames(all_indv_graph_presence) %in% stations_in_out$station[stations_in_out$inside == FALSE]]
out_to_out_counts = run_results$movement_graph[rownames(run_results$movement_graph) %in% stations_in_out$station[stations_in_out$inside == FALSE], colnames(run_results$movement_graph) %in% stations_in_out$station[stations_in_out$inside == FALSE]]
inverse_identity =  abs(diag(dim(out_to_out_ones)[1]) - 1)
out_to_out_ones = out_to_out_ones * inverse_identity
## From station
rownames(out_to_out_ones)[which(out_to_out_ones == max(out_to_out_ones), arr.ind = TRUE)[, 'row']]
## To station
colnames(out_to_out_ones)[which(out_to_out_ones == max(out_to_out_ones), arr.ind = TRUE)[, 'col']]
## Used by how many total fish? 
out_to_out_ones[which(out_to_out_ones == max(out_to_out_ones), arr.ind = TRUE)]
## Used How many times?
out_to_out_counts[which(out_to_out_ones == max(out_to_out_ones), arr.ind = TRUE)]

print(getwd())
setwd("/Users/stephenscherrer/Desktop")
plot_path_use(movement_graph = movement_graph, vue_df = run_results$data, receiver_df = run_results$receiver_data, region = 'Makapuu')
