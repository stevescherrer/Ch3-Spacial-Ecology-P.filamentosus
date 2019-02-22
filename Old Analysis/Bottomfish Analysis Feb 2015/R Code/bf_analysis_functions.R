#### Functions-------------------------------------------------------------------

### experiment_dates
experiment_dates = function(vue_data, bottomfish_tag_ids = FALSE){
  ## Function takes a vue_data dataframe and returns the start date, end date and
  ## elapsed time of study. If provided a list of tag IDs, study starts on 
  ## first detection from a tag, and ends on last detection
  ## Arguments: 
  ## vue_data: A dataframe from a vue export called with function load_vemco
  ## bottomfish_tag_ids: an optional list of specific tags for analysis
  ## Returns:
  ## Dataframe of 3 clolumns
  temp_data = clean_vue(vue_data, bottomfish_tag_ids)
  date_range = as.POSIXct(range(strftime(temp_data$datetime, format = "%Y-%m-%d")))
  elapsed_dates = date_range[2] - date_range[1]
  dates = as.data.frame(cbind(as.character(date_range[1]), as.character(date_range[2]), elapsed_dates))
  colnames(dates)[1] = 'Start Date'
  colnames(dates)[2] = 'End Date'
  colnames(dates)[3] = 'Length of Study'
  return (dates)
}

### Adding/Adjusting study dates

adjust_vue_study_dates = function(vue_data, tagging_data, bottomfish_tag_ids = FALSE){
  ## Function turns POSIXct date into a single number indicating the number of
  ## days of the study that have elapsed at the time of a detection
  ## Arguments:
  ## vue_data: a data frame from a vue export, called with function load_vemco
  ## tagging_data: a dataframe of tagging metadata, called with function 
  ##load_tagging_data
  ## bottomfish_tag_ids = an optional list of specific tags to analyze
  ## Returns:
  ## Dataframe similar to vue_data but with additional study date column
  if(bottomfish_tag_ids[1] != FALSE){
    vue_data = vue_data[vue_data$tag_id %in% bottomfish_tag_ids, ]
    tagging_data = tagging_data[tagging_data$vem_tag_id %in% bottomfish_tag_ids, ]
  }
  earliest_date = min(tagging_data$datetime)
  vue_data$study_date = as.numeric(difftime(vue_data$datetime, earliest_date), units = 'days')
  #tagging_data$study_date = as.numeric(difftime(tagging_data$datetime, earliest_date, na.rm = TRUE), units = 'days')
  return(vue_data)
}

adjust_tagging_study_dates = function(vue_data, tagging_data, bottomfish_tag_ids = FALSE){
  ## Function turns POSIXct date into a single number indicating the number of
  ## days of the study that have elapsed at the time of a detection
  ## Arguments:
  ## vue_data: a data frame from a vue export, called with function load_vemco
  ## tagging_data: a dataframe of tagging metadata, called with function 
  ##load_tagging_data
  ## bottomfish_tag_ids = an optional list of specific tags to analyze
  ## Returns:
  ## Dataframe similar to tagging_data but with additional study date column
  if(bottomfish_tag_ids[1] != FALSE){
    vue_data = vue_data[vue_data$tag_id %in% bottomfish_tag_ids, ]
    tagging_data = tagging_data[tagging_data$vem_tag_id %in% bottomfish_tag_ids, ]
  }
  earliest_date = min(tagging_data$datetime)
  #vue_data$study_date = as.numeric(difftime(vue_data$datetime, earliest_date, na.rm = TRUE), units = 'days')
  tagging_data$study_date = as.numeric(difftime(tagging_data$datetime, earliest_date), units = 'days')
  return(tagging_data)
}


adjust_receiver_study_dates = function(receiver_data, tagging_data, bottomfish_tag_ids = FALSE){
  ## Function turns POSIXct date into a single number indicating the number of
  ## days of the study that have elapsed at the time of a detection
  ## Arguments:
  ## receiver_data: a data frame from a receiver export, called with function load_receiver
  ## tagging_data: a dataframe of tagging metadata, called with function 
  ##load_tagging_data
  ## bottomfish_tag_ids = an optional list of specific tags to analyze
  ## Returns:
  ## Dataframe similar to tagging_data but with additional study date column
  if(bottomfish_tag_ids[1] != FALSE){
    #vue_data = vue_data[vue_data$tag_id %in% bottomfish_tag_ids, ]
    tagging_data = tagging_data[tagging_data$vem_tag_id %in% bottomfish_tag_ids, ]
  }
  earliest_date = min(tagging_data$datetime)
  #vue_data$study_date = as.numeric(difftime(vue_data$datetime, earliest_date, na.rm = TRUE), units = 'days')
  receiver_data$deployment_study_date = as.numeric(difftime(receiver_data$deployment_date, earliest_date), units = 'days')
  receiver_data$recovery_study_date = as.numeric(difftime(receiver_data$recovery_date, earliest_date), units = 'days')
  return(receiver_data)
}

### tagging_date
tagging_date = function(tagging_data, bottomfish_tag_ids = FALSE){
  ## Function to retrieve date transmitters were deployed
  ## Arguments:
  ## tagging_data: a data dataframe of tag ids and tagging POSIXct datetimes
  ## called by function load_tagging_data
  ## bottomfish_tag_ids: an optonal list of specific tags to analyze
  ## Returns:
  ## dataframe with three columns. first column is tag id, second is date 
  ## tag was deployed, third is study date of tag deployment
  tagging_data = tagging_data[which(is.na(tagging_data$vem_tag_id) == 0), ]
  if (bottomfish_tag_ids[1] == FALSE){
    bottomfish_tag_ids = unique(as.numeric(levels(tagging_data$vem_tag_id))[tagging_data$vem_tag_id])}
  tagging_date = matrix(0,length(bottomfish_tag_ids), 3)
  tagging_date[ ,1] = as.numeric(as.character(bottomfish_tag_ids))
  for (i in 1:length(bottomfish_tag_ids)){
    tagging_date[i,2] = as.character(tagging_data$datetime[tagging_data$vem_tag_id == bottomfish_tag_ids[i]])
    tagging_date[i,3] = as.numeric(tagging_data$study_date[tagging_data$vem_tag_id == bottomfish_tag_ids[i]])
  }
  tag_date_out = as.data.frame(tagging_date)
  colnames(tag_date_out)[1] =  'tag_id'
  colnames(tag_date_out)[2] =  'tagging_date'
  colnames(tag_date_out)[3] =  'study_date'
  return (tag_date_out)
}


first_transmission = function(vue_data, bottomfish_tag_ids = FALSE){
  if (bottomfish_tag_ids[1] == FALSE){
    bottomfish_tag_ids = unique(as.numeric(levels(vue_data$tag_id))[vue_data$tag_id])}
  first_transmissions = matrix(0, length(bottomfish_tag_ids), 3)
  first_transmissions[ ,1] = bottomfish_tag_ids
  for (i in 1:length(bottomfish_tag_ids)){
    indv = vue_data[vue_data$tag_id == bottomfish_tag_ids[i], ]
    first_transmissions[i,2] = as.character(min(indv$datetime[as.character(indv$station) != 'Tagging Location']))
    first_transmissions[i,3] = as.numeric(min(indv$study_date[as.character(indv$station) != 'Tagging Location']))
  }
  first_transmissions = as.data.frame(first_transmissions)
  colnames(first_transmissions) = c('tag_id', 'first_transmission_datetime', 'first_transmission_study_date')
  return (first_transmissions)
}


last_transmission = function(vue_data, bottomfish_tag_ids = FALSE){
  if (bottomfish_tag_ids[1] == FALSE){
    bottomfish_tag_ids = unique(as.numeric(levels(vue_data$tag_id))[vue_data$tag_id])}
  last_transmissions = matrix(0, length(bottomfish_tag_ids), 3)
  last_transmissions[ ,1] = bottomfish_tag_ids
  for (i in 1:length(bottomfish_tag_ids)){
    indv = vue_data[vue_data$tag_id == bottomfish_tag_ids[i], ]
    last_transmissions[i,2] = as.character(max(indv$datetime))
    last_transmissions[i,3] = as.numeric(max(indv$study_date))
  }
  last_transmissions = as.data.frame(last_transmissions)
  colnames(last_transmissions) = c('tag_id', 'last_transmission_datetime', 'last_transmission_study_date')
  return (last_transmissions)
}

time_between_first_transmission_and_tagging_date = function(vue_data, tagging_data, bottomfish_tag_ids){
  if (bottomfish_tag_ids[1] == FALSE){
    bottomfish_tag_ids = unique(as.numeric(levels(vue_data$tag_id))[vue_data$tag_id])}
  time_diff = as.numeric(as.character(first_transmission(vue_data, bottomfish_tag_ids)$first_transmission_study_date)) - as.numeric(as.character(tagging_date(tagging_data, bottomfish_tag_ids)$study_date))
  time_between_first_and_tag_date = as.data.frame(cbind(bottomfish_tag_ids, time_diff))
  colnames(time_between_first_and_tag_date) = c('tag_id', 'time_diff_between_tagging_and_first_detection')
  return(time_between_first_and_tag_date)
}

time_at_liberty = function(vue_data, tagging_data, bottomfish_tag_ids){
  if (bottomfish_tag_ids[1] == FALSE){
    bottomfish_tag_ids = unique(as.numeric(levels(vue_data$tag_id))[vue_data$tag_id])}
  time_diff = as.numeric(as.character(last_transmission(vue_data, bottomfish_tag_ids)$last_transmission_study_date)) - as.numeric(as.character(tagging_date(tagging_data, bottomfish_tag_ids)$study_date))
  time_at_lib = as.data.frame(cbind(bottomfish_tag_ids, time_diff))
  colnames(time_at_lib) = c('tag_id', 'time_at_liberty')
  return(time_at_lib)
}

time_beteween_first_last_transmission = function(vue_data, bottomfish_tag_ids = FALSE){
  if (bottomfish_tag_ids[1] == FALSE){
    bottomfish_tag_ids = unique(as.numeric(levels(vue_data$tag_id))[vue_data$tag_id])}
  time_between_first_last_transmission = matrix(0,length(bottomfish_tag_ids), 2)
  time_between_first_last_transmission[ ,1] = bottomfish_tag_ids
  time_between_first_last_transmission[ ,2] = (as.numeric(as.character(last_transmission(vue_data, bottomfish_tag_ids)$last_transmission_study_date)) - 
                                                 as.numeric(as.character(first_transmission(vue_data, bottomfish_tag_ids)$first_transmission_study_date)))
  #   for (i in 1:length(bottomfish_tag_ids)){
  #     indv_data = vue_data[vue_data$tag_id == bottomfish_tag_ids[i], ]
  #     time_between_first_last_transmission[i,2] = as.numeric(as.character(max(indv_data$study_date))) - )
  #   }
  time_between_first_last = as.data.frame(time_between_first_last_transmission)
  colnames(time_between_first_last) = c('tag_id', 'time_between_first_last_transmission')
  return(time_between_first_last)
}




transmission_stats = function(vue_data, tagging_data, bottomfish_tag_ids = FALSE){
  if (bottomfish_tag_ids[1] == FALSE){
    bottomfish_tag_ids = unique(as.numeric(levels(vue_data$tag_id))[vue_data$tag_id])}
  t_stats_mat = matrix(0, length(bottomfish_tag_ids), 6)
  # tag_id
  t_stats_mat[ ,1] = as.numeric(as.character(bottomfish_tag_ids))
  # study date of tagging
  t_stats_mat[ ,2] = ceiling(as.numeric(as.character(tagging_date(tagging_data, bottomfish_tag_ids)$study_date)))
  # study date of first transmission
  t_stats_mat[ ,3] = ceiling(as.numeric(as.character(first_transmission(vue_data, bottomfish_tag_ids)$first_transmission_study_date)))
  # study date of last transmission
  t_stats_mat[ ,4] = ceiling(as.numeric(as.character(last_transmission(vue_data, bottomfish_tag_ids)$last_transmission_study_date)))
  # time at liberty
  t_stats_mat[ ,5] = ceiling(as.numeric(as.character(time_at_liberty(vue_data, tagging_data, bottomfish_tag_ids)$time_at_liberty)))
  # time between first and last detection
  t_stats_mat[ ,6] = ceiling(as.numeric(as.character(time_beteween_first_last_transmission(vue_data, bottomfish_tag_ids)$time_between_first_last_transmission)))
  indv_date_stats = as.data.frame(t_stats_mat)
  colnames(indv_date_stats)[1] =  'tag_id'
  colnames(indv_date_stats)[2] =  'tagging_study_date'
  colnames(indv_date_stats)[3] =  'first_transmission'  
  colnames(indv_date_stats)[4] =  'last_transmission'
  colnames(indv_date_stats)[5] =  'time_at_liberty'
  colnames(indv_date_stats)[6] =  'time_between_first_and_last_transmissions'
  return(indv_date_stats)
}

unique_days_detected = function(vue_data, bottomfish_tag_ids = FALSE){
  ## Returns the number of unique days each fish was detected on the array
  if (bottomfish_tag_ids[1] == FALSE){
    bottomfish_tag_ids = unique(as.numeric(levels(vue_data$tag_id))[vue_data$tag_id])}
  unique_days_detected = matrix(0,length(bottomfish_tag_ids),1)
  for (i in 1:length(bottomfish_tag_ids)){
    indv_data = vue_data[vue_data$tag_id == bottomfish_tag_ids[i],]
    unique_days_detected[i] = length(unique(floor(indv_data$study_date)))
  }
  print(fivenum(unique_days_detected))
  
  pdf('Unique_days_detected_boxplot.pdf')
  boxplot(unique_days_detected[1:length(bottomfish_tag_ids)], main = 'Unique Days Detected', ylab = 'Days' )
  dev.off()
  
  unique_detection_days = as.data.frame(cbind(bottomfish_tag_ids, unique_days_detected))
  colnames(unique_detection_days)[1] =  'tag_id'
  colnames(unique_detection_days)[2] =  'unique_days_detected'
  return(unique_detection_days)
}

## Number of detections
number_of_detections = function(vue_data, bottomfish_tag_ids = FALSE){
  ## Determines the number of detections for each tag on the array. Also determines 
  # the percentage of all detections those individuals account for. 
  if (bottomfish_tag_ids[1] == FALSE){
    bottomfish_tag_ids = unique(as.numeric(levels(vue_data$tag_id))[vue_data$tag_id])}
  number_of_detections = matrix(0,length(bottomfish_tag_ids),1)
  for (a in 1:length(bottomfish_tag_ids)){
    number_of_detections[a] = length(vue_data$tag_id[vue_data$tag_id == bottomfish_tag_ids[a]])
  }
  percentage_of_detections = (number_of_detections / length(vue_data$datetime))*100
  #print(length(vue_data))
  fivenum(number_of_detections)
  #pdf('Total Number of Detections Boxplot.pdf')
  boxplot(number_of_detections[1:length(bottomfish_tag_ids)], main = 'Total Number of Detections', ylab = 'Number of Detections' )
  #dev.off()
  number_and_percent_detections = as.data.frame(cbind(bottomfish_tag_ids, number_of_detections, percentage_of_detections))
  colnames(number_and_percent_detections)[1] = 'tag_id'
  colnames(number_and_percent_detections)[2] = '#_of_detections'
  colnames(number_and_percent_detections)[3] = '%_of_all_detections'
  return(number_and_percent_detections)
}

detections_per_day = function(vue_data, tagging_data, bottomfish_tag_ids = FALSE){
  if (bottomfish_tag_ids[1] == FALSE){
    bottomfish_tag_ids = unique(as.numeric(levels(vue_data$tag_id))[vue_data$tag_id])}
  tol = floor(transmission_stats(vue_data, tagging_data, bottomfish_tag_ids)$time_at_liberty)+1
  detections_per_day = number_of_detections(vue_data, bottomfish_tag_ids)[,2]/
    tol
  detections_per_day_out = as.data.frame(cbind(bottomfish_tag_ids, detections_per_day))
  colnames(detections_per_day_out)[1] = 'tag_id'
  colnames(detections_per_day_out)[2] = 'detections / day'
  return(detections_per_day_out)
}



count_detections_per_day = function(vue_data, bottomfish_tag_ids = FALSE){
  ## Function returns a vector that is the same length as the number of total days
  ## in the study. Each bin represents a day and the number is the total number
  ## of transmissions detected for that day.
  if (bottomfish_tag_ids[1] == FALSE){
    bottomfish_tag_ids = unique(as.numeric(levels(vue_data$tag_id))[vue_data$tag_id])}
  counting_detections = rep(0, max(ceiling(vue_data$study_date)))
  for (i in 1:length(vue_data$study_date)){
    counting_detections[floor(vue_data$study_date[i])] = counting_detections[floor(vue_data$study_date[i])]+1
  }
  return(counting_detections)
}

plot_transmission_frequency = function(vue_data, bottomfish_tag_ids = FALSE){
  
  ### Function to plot transmission recovered per day for all tags and individual
  ## tags
  
  if (bottomfish_tag_ids[1] == FALSE){
    bottomfish_tag_ids = unique(as.numeric(levels(vue_data$tag_id))[vue_data$tag_id])}
  # counting_detections = rep(0, max(ceiling(vue_data$study_date))) 
  
  pdf('Detections Per Day For All Tags Histogram.pdf')
  par(mfrow = c(1,1), oma=c(0,0,2,0))
  hist(floor(vue_data[,1]), xlim = c(0, max(vue_data[,1])), breaks = c(0, max(vue_data[,1])+1), xlab = 'Study Date', ylab = 'Detection Frequency', main = 'Detections per Day for All Tags')
  dev.off
  
  for (tag_id in bottomfish_tag_ids){
    indv_data = vue_data[vue_data$tag_id == tag_id, ]
    pdf(paste(tag_id,' Detection Per Day Histogram.pdf'))
    par(mfrow = c(1,1), oma=c(0,0,2,0))
    hist(floor(indv_data$study_date),  
         breaks = c(max(indv_data$study_date)+1), 
         xlim = c(0, max(vue_data$study_date)), xlab = 'Study Date', 
         ylab = 'Detection Frequency', 
         main = paste(tag_id, 'Detections per Day'))
    # Plotting abline for tagging date
    abline(v = min(indv_data$study_date), col = zissou.red) # since tagging date
    # represented as first detection, use min study date
    dev.off
  }
  
  for (i in 1:length(bottomfish_tag_ids)){
    indv_data = vue_data[vue_data$tag_id == tag_id, ]
    
    ## Determining if a detection represents a tag at a new station
    date_of_station_change = c()
    for (a in 2:length(indv_data$station)){
      if (indv_data$station[a] != indv_data$station[a-1]){
        date_of_station_change = c(date_of_station_change, indv_data$study_date[a])
      }
    }
    pdf(paste(tag_id, 'Detections Per Day Histogram.pdf'))
    par(mfrow = c(1,1), oma=c(0,0,2,0))
    hist(floor(indv_data$study_date), 
         breaks = max(vue_data[,1])+1, 
         xlim = c(0, max(vue_data[,1])), xlab = 'Study Date',
         ylab = 'Daily Detections', 
         main = paste(tag_id, 'Detections per Day')) 
    ## plotting abline for tagging date
    abline(v = min(indv_data$study_date), col = zissou.red) # since tagging date
    # represented as first detection, use min study date
    ## Plotting abline for dates when transmitter is detected at a new station
    abline(v = date_of_station_change, col = zissou.blue)
    dev.off()
  }  
}


### distance traveled
distance_traveled = function(vue_data, bottomfish_tag_ids = FALSE){
  ## Function to measure the total distance in km between subsequent receivers
  ## that a fish was detected
  if (bottomfish_tag_ids[1] == FALSE){
    bottomfish_tag_ids = unique(as.numeric(levels(vue_data$tag_id))[vue_data$tag_id])}
  individual_distance = matrix(0, length(bottomfish_tag_ids), 1)
  for (i in 1:length(bottomfish_tag_ids)){
    subset = vue_data[vue_data$tag_id == bottomfish_tag_ids[i],]
    for (a in 2:length(subset$lon)){
      individual_distance[i] = individual_distance[i] + lldist(point1 = c(subset$lon[a-1], subset$lat[a-1]), point2 = c(subset$lon[a], subset$lat[a]))
    }
  }
  total_distance_tracked = sum(individual_distance)
  print (total_distance_tracked)
  #pdf('distance_tracked_boxplot.pdf')
  #frame()
  #boxplot(individual_distance, main = 'Distance Tracked (km)', ylab = 'km' )
  #dev.off()
  #individual_distance_5num = fivenum(individual_distance[1:4])
  #print (individual_distance_5num)
  distance_traveled_out = as.data.frame(cbind(bottomfish_tag_ids,individual_distance))
  colnames(distance_traveled_out) = c('tag_id', 'distance_tracked')
  return(distance_traveled_out)
}

distance_per_day = function(vue_data, tagging_data, bottomfish_tag_ids =FALSE){
  ## Function to measure the total distance / day at liberty in km between 
  ## subsequent receivers that a fish was detected at
  if (bottomfish_tag_ids[1] == FALSE){
    bottomfish_tag_ids = unique(as.numeric(levels(vue_data$tag_id))[vue_data$tag_id])}
  distance_per_day = distance_traveled(vue_data, bottomfish_tag_ids)[,2] / 
    transmission_stats(vue_data, tagging_data, bottomfish_tag_ids)$time_at_liberty
  print(fivenum(distance_per_day))
  boxplot(distance_per_day, main = 'Distance Standardized by time at liberty', ylab = 'km/day')
  distance_per_day_out = as.data.frame(cbind(bottomfish_tag_ids, distance_per_day))
  colnames(distance_per_day_out) = c('tag_id', 'distance/day')
  return(distance_per_day_out)
}

number_of_movements = function(vue_data, bottomfish_tag_ids = FALSE){
  ## Function to return the number of movements between stations a transmitter was
  ## detected making
  if (bottomfish_tag_ids[1] == FALSE){
    bottomfish_tag_ids = unique(as.numeric(levels(vue_data$tag_id))[vue_data$tag_id])}
  movements = matrix(-1, length(bottomfish_tag_ids), 2) ## This begins at -1 because tagging location shouldn't count as a movement between stations
  movements[ ,1] = bottomfish_tag_ids
  for (i in 1:length(bottomfish_tag_ids)){
    indv_data =  vue_data[vue_data$tag_id == bottomfish_tag_ids[i],]
    for (a in 2:length(indv_data$station)){
      if (isTRUE(indv_data$station[a] != indv_data$station[a-1])){
        movements[i,2] = movements[i,2]+1
      }
    }
  }
  movements = as.data.frame(movements)
  colnames(movements) = c('tag_id', 'detected_movements')
  return (movements)
}

number_of_movements_per_day = function(vue_data, tagging_data, bottomfish_tag_ids =FALSE){
  ## Function to return the number of movements between stations a transmitter was
  ## detected making standardized by dividing by transmitter's days at liberty
  if (bottomfish_tag_ids[1] == FALSE){
    bottomfish_tag_ids = unique(as.numeric(levels(vue_data$tag_id))[vue_data$tag_id])}
  move_per_day = number_of_movements(vue_data, bottomfish_tag_ids)$detected_movements / transmission_stats(vue_data, tagging_data, bottomfish_tag_ids)$time_at_liberty
  move_per_day_out = as.data.frame(cbind(bottomfish_tag_ids, move_per_day))
  colnames(move_per_day_out)[1] = 'tag_id'
  colnames(move_per_day_out)[2] = 'detected movements per day at liberty'
  return(move_per_day_out)
}

brfa_crossings = function(vue_data, tagging_data, bottomfish_tag_ids =FALSE){
  ## Function to return the number of movements between stations a transmitter was
  ## detected making across BRFA boundaries
  if (bottomfish_tag_ids[1] == FALSE){
    bottomfish_tag_ids = unique(as.numeric(levels(vue_data$tag_id))[vue_data$tag_id])}
  movement_matrix = as.data.frame(matrix(0, length(bottomfish_tag_ids), 3))
  movement_matrix[ ,1] = bottomfish_tag_ids
  for (i in 1:length(bottomfish_tag_ids)){
    indv_data = vue_data[vue_data$tag_id == bottomfish_tag_ids[i], ]
    if (isTRUE(length(indv_data) > 1)){
      for (a in 2:length(indv_data$station)){
        if (in_brfa(indv_data$lon[a], indv_data$lat[a]) != in_brfa(indv_data$lon[a-1], indv_data$lat[a-1])){
          movement_matrix[i,2] = movement_matrix[i,2]+1
        }
      }
    }
    movement_matrix[i,3] = movement_matrix[i,2] / 
      transmission_stats(vue_data, tagging_data, bottomfish_tag_ids[i])$time_at_liberty
  }
  colnames(movement_matrix)[1] = 'tag_id'
  colnames(movement_matrix)[2] = 'BRFA_crossings'
  colnames(movement_matrix)[3] = 'BRFA_crossings_per_day'
  return (movement_matrix)
}

in_brfa_e = function(lon, lat){
  if((lon >= -157.68333333 && lon <= -157.53333333) && 
       (lat >= 21.28333333 && lat <= 21.4166666)){
    return(in_bfra = TRUE)
  }else{ 
    return(FALSE)
  }
}

in_brfa_f = function(lon, lat){
  if((lon >= -157.5666667 && lon <= -157.3666667) && 
       (lat >= 20.9666667 && lat <= 21.333333333)){
    return(in_bfra = TRUE)
  }else{ 
    return(FALSE)
  } 
}

in_brfa = function(lon, lat){
  if(isTRUE(in_brfa_e(lon, lat)) || isTRUE(in_brfa_f(lon,lat))){
    return (TRUE)
  }else{
    return(FALSE)
  }
}

brfa_crossings_per_day = function(vue_data, tagging_data, bottomfish_tag_ids = FALSE){
  ## Function to return the number of movements between stations a transmitter was
  ## detected making across BRFA boundaries standardized by dividing by 
  ## transmitter's days at liberty
  if (bottomfish_tag_ids[1] == FALSE){
    bottomfish_tag_ids = unique(as.numeric(levels(vue_data$tag_id))[vue_data$tag_id])}
  cross_per_day = brfa_crossings(vue_data, bottomfish_tag_ids)[,2] /
    transmission_stats(vue_data, tagging_data, bottomfish_tag_ids)$time_at_liberty
  cross_per_day_out = as.data.frame(cbind(bottomfish_tag_ids, cross_per_day))
  colnames(cross_per_day_out)[1] = 'tag_id'
  colnames(cross_per_day_out)[2] = 'BRFA_crossings_per_day'
  return(cross_per_day_out)
}

dates_of_location_switching = function(vue_data, bottomfish_tag_ids =FALSE){
  if (bottomfish_tag_ids[1] == FALSE){
    bottomfish_tag_ids = unique(as.numeric(levels(vue_data$tag_id))[vue_data$tag_id])}
  date_changes_all = c()
  for (i in 1:length(bottomfish_tag_ids)){
    indv_data = vue_data[vue_data$tag_id == bottomfish_tag_ids[i],]
    for (a in 2:length(indv_data$station)){
      if (isTRUE(indv_data$station[a] != indv_data$station[a-1])){
        date_changes_all = rbind(date_changes_all, indv_data[a-1, ], indv_data[a, ])
      }
    }
  }
  date_changes_all = as.data.frame(date_changes_all)
  #colnames(date_changes_all)[1] = 'tag_id'
  #colnames(date_changes_all)[2] = 'receiver'
  #colnames(date_changes_all)[3] = 'location'
  #colnames(date_changes_all)[4] = 'date_of_detection'
  #colnames(date_changes_all)[5] = 'arrive/depart' # 0 = depature at a receiver, 1 = arrival
  return (date_changes_all)
}

arrive_date = function(vue_data, bottomfish_tag_ids =FALSE){
  if (bottomfish_tag_ids[1] == FALSE){
    bottomfish_tag_ids = unique(as.numeric(levels(vue_data$tag_id))[vue_data$tag_id])}
  arrive_dates = c()
  for (i in 1:length(bottomfish_tag_ids)){
    indv_data = vue_data[vue_data$tag_id == bottomfish_tag_ids[i], ]
    for (a in 2:length(indv_data$station)){
      if(isTRUE(indv_data$station[a] != indv_data$station[a-1])){
        arrive_dates = rbind(c(bottomfish_tag_ids[i], indv_data$station[a], indv_data$datetime[a]))}
    }
    arrive_dates = as.data.frame(arrive_dates)
    colnames(arrive_dates) = c('tag_id', 'station_arrived', 'arrival_study_date')
  }
  return(arrive_dates)
}

depart_date = function(vue_data, bottomfish_tag_ids =FALSE){
  if (bottomfish_tag_ids[1] == FALSE){
    bottomfish_tag_ids = unique(as.numeric(levels(vue_data$tag_id))[vue_data$tag_id])}
  depart_dates = c()
  for (i in 1:length(bottomfish_tag_ids)){
    indv_data = vue_data[vue_data$tag_id == bottomfish_tag_ids[i], ]
    for (a in 2:length(indv_data$station)){
      if(isTRUE(indv_data$station[a] != indv_data$station[a-1])){
        depart_dates = rbind(c(bottomfish_tag_ids[i], indv_data$station[a-1], indv_data$study_date[a-1]))}
    }
    depart_dates = as.data.frame(depart_dates)
    colnames(depart_dates) = c('tag_id', 'station_departed', 'departure_study_date')
  }
  return(depart_dates)
}

all_stations_detected = function(vue_data, bottomfish_tag_ids = FALSE){
  ## Function to return a list of each subsequent station a transmitter was detected
  if (bottomfish_tag_ids[1] == FALSE){
    bottomfish_tag_ids = unique(as.numeric(levels(vue_data$tag_id))[vue_data$tag_id])
    to_return = FALSE
  }else{
    to_return = TRUE}
  for (i in 1:length(bottomfish_tag_ids)){
    indv_data = vue_data[vue_data$tag_id == bottomfish_tag_ids[i], ]
    all_stations = c(paste('Detection History for Tag no. ', bottomfish_tag_ids[i], ': '), indv_data$station[1])
    if (length(indv_data)>1){
      for (a in 2:length(indv_data$station)){
        if (isTRUE(indv_data$station[a] != indv_data$station[a-1])){
          all_stations = as.character(c(all_stations, ',', indv_data$station[a]))
        }
      }
    }
    if(to_return == FALSE){
      print(c(all_stations))
    }
  }
  if(to_return == TRUE){
    return(cat(all_stations))
  }
}

unique_stations_detected = function(vue_data, bottomfish_tag_ids = FALSE){
  ## Function to return a list of each unique station a transmitter was detected
  if (bottomfish_tag_ids[1] == FALSE){
    bottomfish_tag_ids = unique(as.numeric(levels(vue_data$tag_id))[vue_data$tag_id])}
  for (i in 1:length(bottomfish_tag_ids)){
    indv_data = vue_data[vue_data$tag_id == bottomfish_tag_ids[i], ]
    unique_station_detections = unique(indv_data$station)
    unique_stations = c(paste('Unique stations visited for tag no.', bottomfish_tag_ids[i], ': '), unique_station_detections)
    print(unique_stations)
  }
}

### Plotting_movements
plotting_movements = function(vue_data, receiver_data, bottomfish_tag_ids =FALSE){
  ## Function to return the number of movements between stations a transmitter was
  ## detected making across BRFA boundaries
  if (bottomfish_tag_ids[1] == FALSE){
    bottomfish_tag_ids = unique(as.numeric(levels(vue_data$tag_id))[vue_data$tag_id])}
  
  ## Defining map shapefile
  pngMAP_df<- get_map(location = c(lon = -157.75, lat = 21.251), 
                      source = "google", zoom = 9,color='bw')
  
  ## Cycling through tags and plotting
  for (tag_id in bottomfish_tag_ids){
    lonlats = as.matrix(cbind(vue_data$lon[vue_data$tag_id == tag_id], vue_data$lat[vue_data$tag_id == tag_id]))
    ll_plot_index = 1
    if (length(lonlats[ ,1]) > 1){
      for (i in 2:length(lonlats[ ,1])){
        if (lonlats[i,1] != lonlats[i-1,1] || lonlats[i,2] != lonlats[i-1,2]){
          ll_plot_index = c(ll_plot_index, i)
        }
      }
      plot_lonlats = as.data.frame(lonlats[ll_plot_index, ])
      colnames(plot_lonlats) = c('lon', 'lat')
    }
    
    plot.new()
    
    png(sprintf('%s Movement Map.png', tag_id))
    print(ggmap(pngMAP_df) + 
            geom_point(color = zissou.gold, size = 2, data = receiver_data,
                       aes(x = lon, y = lat)) +
            geom_path(color = zissou.blue, mapping = aes(x = c(-157.566, -157.566), y = c(21.0333, 20.9166))) +
            geom_path(color = zissou.blue, mapping = aes(x = c(-157.566, -157.366), y = c(20.9166, 20.9166))) +
            geom_path(color = zissou.blue, mapping = aes(x = c(-157.366, -157.366), y = c(20.9166, 21.0333))) +
            geom_path(color = zissou.blue, mapping = aes(x = c(-157.366, -157.566), y = c(21.0333, 21.0333))) +
            geom_path(color = zissou.blue, mapping = aes(x = c(-157.683, -157.533), y = c(21.4166, 21.4166))) +
            geom_path(color = zissou.blue, mapping = aes(x = c(-157.533, -157.533), y = c(21.4166, 21.2833))) +
            geom_path(color = zissou.blue, mapping = aes(x = c(-157.683, -157.533), y = c(21.2833, 21.2833))) +
            geom_point(color = zissou.red, size = 2, data = plot_lonlats, aes(x = lon, y = lat)) +
            geom_path(color = zissou.red, size = 2, data = plot_lonlats, aes(x = lon, y = lat)) + 
            labs(title = sprintf('Detections of Tag %s', tag_id)))
    dev.off()
  }
}

get_tagging_metadata = function(tagging_data, bottomfish_tag_ids = FALSE){
  if (bottomfish_tag_ids[1] == FALSE){
    bottomfish_tag_ids = unique(as.numeric(levels(vue_data$tag_id))[vue_data$tag_id])}
  tagging_meta = as.data.frame(matrix(0, length(bottomfish_tag_ids), 4))
  for (i in 1:length(bottomfish_tag_ids)){
    tagging_meta[i,1] = as.numeric(as.character(tagging_data$vem_tag_id[which(tagging_data$vem_tag_id == bottomfish_tag_ids[i])]))
    tagging_meta[i,2] = as.character(tagging_data$"species"[which(tagging_data$vem_tag_id == bottomfish_tag_ids[i])])
    tagging_meta[i,3] = as.numeric(as.character(tagging_data$"fork_length(cm)"[which(tagging_data$vem_tag_id == bottomfish_tag_ids[i])]))
    tagging_meta[i,4] = as.character(tagging_data$"area_of_capture"[which(tagging_data$vem_tag_id == bottomfish_tag_ids[i])])
  }
  colnames(tagging_meta) = c('tag_id', 'species', 'fork_length', 'tagging_location')
  return(tagging_meta)
}

unique_receivers = function(vue_data, tagging_data, bottomfish_tag_ids = FALSE){
  if (bottomfish_tag_ids[1] == FALSE){
    bottomfish_tag_ids = unique(as.numeric(levels(vue_data$tag_id))[vue_data$tag_id])}
  receivers_visited = as.data.frame(matrix(0, length(bottomfish_tag_ids), 3))
  for (i in 1:length(bottomfish_tag_ids)){
    indv_data = vue_data[which(vue_data$tag_id == bottomfish_tag_ids[i]), ]
    receivers_visited[i,1] = as.numeric(as.character(tagging_data$vem_tag_id[which(tagging_data$vem_tag_id == bottomfish_tag_ids[i])]))
    receivers_visited[i,2] = length(unique(indv_data$station)) - 1 # Subtract to account for tagging location
    receivers_visited[i,3] = receivers_visited[i,2] / transmission_stats(vue_data, tagging_data, bottomfish_tag_ids = bottomfish_tag_ids[i])$time_at_liberty
  }
  colnames(receivers_visited) = c('tag_id', 'receivers_detected', 'receivers_detected_per_day')
  return(receivers_visited)
}

## Making strip chart of detections
vemco_stripchart = function(vue_data, bottomfish_tag_ids = FALSE){
  plot_data = clean_vue(vue_data, bottomfish_tag_ids)
  pdf('Stripchart of Detections.pdf')
  par(las = 1)
  stripchart(study_date~tag_id, data = plot_data,
             #ylab = 'Tag ID',
             xlab = 'Study Date',
             main = 'Detections During Study Period')
  dev.off()
}

spatial_evenness = function(vue_data, receiver_data, bottomfish_tag_ids = FALSE){
  if (bottomfish_tag_ids[1] == FALSE){
    bottomfish_tag_ids = unique(as.numeric(levels(vue_data$tag_id))[vue_data$tag_id])}
  ### function to calculate spacitail eveness based on Pielou 1966 from TinHan 2014
  ## outputs a dataframe first column is tag id, second column is spatial evenness index
  vue_data = vue_data[vue_data$tag_id %in% bottomfish_tag_ids, ]
  vue_data = vue_data[vue_data$station != 'Tagging Location', ]
  spatial_evenness_df = as.data.frame(matrix(data = 0, nrow = length(unique(vue_data$tag_id)), ncol = 2))
  colnames(spatial_evenness_df) = c('tag_id', 'spatial_evenness_metric')
  spatial_evenness_df$tag_id = unique(vue_data$tag_id)
  R = length(unique(receiver_data$station_name)) #could replaces "station_name" with "zone"
  for(i in 1:length(unique(vue_data$tag_id))){
    indv_data = vue_data[which(vue_data$tag_id == unique(vue_data$tag_id)[i]), ]
    spatial_sum = c()
    for(a in 1:length(unique(receiver_data$station_name))){
      rho_i = length(indv_data$datetime[which(as.character(indv_data$station) == as.character(receiver_data$station_name[a]))]) / length(indv_data$station)
      spatial_sum = c(spatial_sum, (rho_i * log(rho_i)))
    }
    print(spatial_sum)
    spatial_evenness_df[i, 2] = (-1 * sum((spatial_sum[spatial_sum != 'NaN']))) / log(R)
  }
  return(spatial_evenness_df)
}

calculate_time_between_detections = function(vue_data, bottomfish_tag_ids = FALSE){
  if (bottomfish_tag_ids[1] == FALSE){
    bottomfish_tag_ids = unique(as.numeric(levels(vue_data$tag_id))[vue_data$tag_id])
  }
  time_between_df = as.data.frame(matrix(0, length(bottomfish_tag_ids), 7))
  colnames(time_between_df) = c('tag_id', 'min', '1st_quartile', 'median', 'third_quartile', 'max', 'mean')
  for (i in 1:length(bottomfish_tag_ids)){
    indv_data = vue_data[vue_data$tag_id == bottomfish_tag_ids[i], ]
    diff_time = c()
    for (a in 3:length(indv_data$datetime)){ # Begin indexing at 3 because first detection is generated tagging date
      diff_time = c(diff_time, difftime(time1 = indv_data$datetime[a], time2 = indv_data$datetime[a-1], units = 'days'))
    }
    time_between_df[i, 2:6] = fivenum(diff_time)
    time_between_df[i, 7]   = mean(diff_time)
  }
  return(time_between_df)
}