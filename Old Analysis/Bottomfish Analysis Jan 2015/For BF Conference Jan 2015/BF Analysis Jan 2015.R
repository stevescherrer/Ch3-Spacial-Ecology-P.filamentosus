#TODO: stripchart for detections?

# Comparing Bottomfish tag returns to Acoustic Web App Model
# Written 14 January 2015 by Stephen Scherrer

rm(list=ls()) # Clear workspace
setwd('/Users/stephenscherrer/Documents/Programming/R/')

# Import any principle dependencies----------
# install.packages('wesanderson') # color palett for plotting
# install.packages('matlab')
# install.packages('maps')
# install.packages('mapdata')
# install.packages('maptools')
# install.packages('scales')
# install.packages('ggmap')
library("wesanderson", lib.loc)
library("matlab", lib.loc)
library('maps')
library('mapdata')
library('maptools')  #for shapefiles
library('scales')  #for transparency
library('ggmap')
source('/Users/stephenscherrer/Documents/Programming/R/utility_functions.R')


## Functions-----------------------

## Importing receiver log ------------
## To do: make this section a function or 2
#receiver_dates = read.csv(file = 'receiver_dates.csv', header = FALSE)
#receiver_deployment_recovery_log = receiver_dates
#colnames(receiver_dates)[1] = 'location'
#colnames(receiver_dates)[2] = 'receiver'
#colnames(receiver_dates)[3] = 'deployment_date'
#colnames(receiver_dates)[4] = 'recovery_date'


#vue_data = as.data.frame(read.csv(file = 'bottomfish_for_phase_1_analysis_all_tags.csv', header = FALSE))
#colnames(vue_data)[1] =  'datetime'
#colnames(vue_data)[2] =  'receiver'
#colnames(vue_data)[3] =  'tag_id'
#colnames(vue_data)[4] =  'location'
#colnames(vue_data)[5] =  'BRFA'
#colnames(vue_data)[6] =  'tag_date'
#colnames(vue_data)[7] =  'tag_death'
#colnames(vue_data)[8] =  'lon'
#colnames(vue_data)[9] =  'lat'
#colnames(vue_data)[10] = 'ping_interval'

 
# Analysis Functions------------
# Experiment dates
experiment_dates = function(vue_data, bottomfish_tag_ids = FALSE){
    temp_data = clean_vue(vue_data, bottomfish_tag_ids)
    date_range = as.POSIXct(range(strftime(temp_data$datetime, format = "%Y-%m-%d")))
    elapsed_dates = date_range[2] - date_range[1]
    dates = cbind(as.character(date_range[1]), as.character(date_range[2]), elapsed_dates)
    colnames(dates)[1] = 'Start Date'
    colnames(dates)[2] = 'End Date'
    colnames(dates)[3] = 'Length of Study'
    return (dates)
    }

#### Working until here 7 Feb 2015----------

# Study dates
study_dates = function(vue_data, bottomfish_tag_ids = FALSE){
  if (bottomfish_tag_ids[1] == FALSE){
    bottomfish_tag_ids = unique(as.numeric(levels(vue_data$tag_id))[vue_data$tag_id])}
  vue_data_out = cbind(study_date = as.numeric(difftime(vue_data$datetime, min(vue_data$datetime, na.rm = TRUE), units = 'days')), vue_data)
  return (vue_data_out)
}

#### Working until here 7 Feb 2015

tagging_date = function(tagging_data, bottomfish_tag_ids = FALSE){
  if (bottomfish_tag_ids[1] == FALSE){
    bottomfish_tag_ids = unique(tagging_date$vem_tag_id)}
  tagging_date = matrix(0,length(bottomfish_tag_ids))
  for (i in 1:length(tagging_date)){
    tagging_date[i] = tagging_data$datetime[tagging_data$vem_tag_id == bottomfish_tag_ids[i]]
  }
  tag_date_out = cbind(bottomfish_tag_ids, tagging_date)
  colnames(tag_date_out)[1] =  'tag_id'
  colnames(tag_date_out)[2] =  'tagging_date'
  return (tag_date_out)
}

tagging_date = function(tagging_data, bottomfish_tag_ids = FALSE){
  if (bottomfish_tag_ids[1] == FALSE){
    bottomfish_tag_ids = as.numeric(unique(tagging_data$vem_tag_id))}
  tagging_dates = as.data.frame(matrix(0, length(bottomfish_tag_ids), 2), colnames = c('tag_id', 'tagging_date'))
  tagging_dates[ ,1] = as.numeric(as.character(bottomfish_tag_ids))
  for (i in 1:length(tagging_dates[ ,1])){
    tagging_dates[i,2] = as.character(tagging_data$datetime[which(tagging_data$vem_tag_id == tagging_dates[i,1])])
  }
  return(tagging_dates)
}

## First transmision - equivilent to tagging date since tagging date detection has been added to vue_data
transmission_stats = function(vue_data, bottomfish_tag_ids = FALSE){
    if (bottomfish_tag_ids[1] == FALSE){
      bottomfish_tag_ids = unique(as.numeric(as.character(vue_data$tag_id)))}
    first_transmission = matrix(0,length(bottomfish_tag_ids),1)
    for (a in 1:length(bottomfish_tag_ids)){
      first_transmission[a] = min(vue_data$study_date[vue_data$tag_id == bottomfish_tag_ids[a]])
    }
  
  ## tagging date
  #tag_date = tagging_date(tagging_data, bottomfish_tag_ids) ## This is returning a large integer, not a POSIX date
  #tag_date = matrix(0, length(bottomfish_tag_ids), 2)
  #tag_date[ ,1] = bottomfish_tag_ids
  #tag_date[ ,2] = first_transmission
    
  ## Last transmission
  last_transmission = matrix(0,length(bottomfish_tag_ids), 1)
  for (a in 1:length(bottomfish_tag_ids)){
  dates_subset = (vue_data$study_date[which(vue_data$tag_id == bottomfish_tag_ids[a])])
  last_transmission[a] = max(dates_subset)
  }
    
  ## Time at liberty
  time_at_liberty = ceiling(last_transmission - as.numeric(tag_date[ ,2]))
  time_between_first_and_last_detections = floor(last_transmission) - floor(first_transmission)
  indv_date_stats = as.data.frame(cbind(bottomfish_tag_ids,first_transmission, last_transmission, time_at_liberty, time_between_first_and_last_detections))
  colnames(indv_date_stats)[1] =  'tag_id'
  colnames(indv_date_stats)[2] =  'first_transmission'  
  colnames(indv_date_stats)[3] =  'last_transmission'
  colnames(indv_date_stats)[4] =  'time_at_liberty'
  colnames(indv_date_stats)[5] =  'time_between_first_and_last_transmissions'
  return (indv_date_stats)
  
  print(fivenum(time_at_liberty))
  
  pdf('Time at Liberty Boxplot.pdf')
  boxplot(time_at_liberty, main = 'Time at Liberty', ylab = 'Days Between First and Last Transmission')
  dev.off()
  
  pdf('Time at Liberty Hist.pdf')
  hist(time_at_liberty, breaks = round(max(time_at_liberty)/30)+1, main = 'Time at liberty', xlab = 'Days at liberty (Binned Every 30 Days)', ylab = 'Number of Tags', xlim = c(0, max(vue_data$datetime)), ylim = c(0,length(unique(vue_data$tag_id))))
  dev.off
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
  
  unique_detection_days = cbind(bottomfish_tag_ids, unique_days_detected)
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
  number_and_percent_detections = cbind(bottomfish_tag_ids, number_of_detections, percentage_of_detections)
  colnames(number_and_percent_detections)[1] = 'tag_id'
  colnames(number_and_percent_detections)[2] = '#_of_detections'
  colnames(number_and_percent_detections)[3] = '%_of_all_detections'
  return(number_and_percent_detections)
}

detections_per_day = function(vue_data, bottomfish_tag_ids = FALSE){
  if (bottomfish_tag_ids[1] == FALSE){
    bottomfish_tag_ids = unique(as.numeric(levels(vue_data$tag_id))[vue_data$tag_id])}
  tol = floor(transmission_stats(vue_data, bottomfish_tag_ids)[,4])+1
  detections_per_day = number_of_detections(vue_data, bottomfish_tag_ids)[,2]/
    tol
  detections_per_day_out = cbind(bottomfish_tag_ids, detections_per_day)
  colnames(detections_per_day_out)[1] = 'tag_id'
  colnames(detections_per_day_out)[2] = 'detections / day'
  return(detections_per_day_out)
}


## FIX ME
##
##
count_detections = function(vue_data, bottomfish_tag_ids = FALSE){
  if (bottomfish_tag_ids[1] == FALSE){
    bottomfish_tag_ids = unique(as.numeric(levels(vue_data$tag_id))[vue_data$tag_id])}
  counting_detections = rep(0, max(vue_data$datetime))
  for (i in 1:length(vue_data[,1])){
    counting_detections[vue_data[i,1]] = counting_detections[vue_data[i,1]]+1
  }
  return(counting_detections)
}

station_switch = function(vue_data, bottomfish_tag_ids = FALSE){
  if (bottomfish_tag_ids[1] == FALSE){
    bottomfish_tag_ids = unique(as.numeric(levels(vue_data$tag_id))[vue_data$tag_id])}
  counting_detections = rep(0, max(vue_data$datetime))  
  all_tags = count_detections(vue_data)
  #pdf('Detections Per Day For All Tags Histogram.pdf')
  hist(floor(vue_data[,1]), xlim = c(0, max(vue_data[,1])), breaks = max(vue_data[,1])+1, xlab = 'Study Date', ylab = 'Detection Frequency', main = 'Detections per Day for All Tags')
  #dev.off
  
  #pdf('57459 Detection Per Day Histogram.pdf')
  indv_data = vue_data[vue_data[,3] == 57459,]
  hist(floor(indv_data[,1]), xlim = c(0, max(indv_data[,1])), breaks = max(indv_data[,1])+1, xlab = 'Study Date', ylab = 'Detection Frequency', main = 'Detections per Day for Tag 57459')
  #dev.off

   #pdf('Detections Per Day Histogram.pdf')
  par(mfrow = c(2,length(bottomfish_tag_ids)/2))
  for (i in 1:4){
    subset = vue_data[vue_data[,3] == bottomfish_tag_ids[i],]
    one_tag = count_detections(subset)
    hist(floor(subset[,1]), xlim = c(0, max(vue_data[,1])), 
         breaks = max(vue_data[,1])+1, xlab = 'Study Date', ylab = 'Daily Detections', 
         main = toString(c('Detections Per Day for Tag ID ', bottomfish_tag_ids[i])),) 
    abline(v = subset[1,6], col = 'red')
  }
  #dev.off()
  counting_detections_out = cbind(bottomfish_tag_ids, counting_detections)
  colnames(counting_detections_out)[1] = 'tag_id'
  colnames(counting_detections_out)[2] = 'detections_per_day'
  return (counting_detections_out)
  }


## DONE TO HERE 3:46 21 Jan 2015


dist_traveled = function(vue_data, bottomfish_tag_ids = FALSE){
  if (bottomfish_tag_ids[1] == FALSE){
    bottomfish_tag_ids = unique(as.numeric(levels(vue_data$tag_id))[vue_data$tag_id])}
  individual_distance_matrix = matrix(0,length(bottomfish_tag_ids), 2)
  individual_distance_matrix[ ,1] = bottomfish_tag_ids
  for (i in 1:length(bottomfish_tag_ids)){
    indv_data = vue_data[vue_data$tag_id == bottomfish_tag_ids[i], ]
    if (length(indv_data$datetime) > 1){
      indv_data = indv_data[order(indv_data$datetime)] # ensures that indv_data is sorted by datetime
      for (a in 2:length(indv_data$datetime)){
      individual_distance_matrix[i,1] = individual_distance_matrix[i,1] + lldist(point1 = c(indv_data$lon[a], indv_data$lat[a]), point2 = c(indv_data$lon[a-1], indv_data$lat[a-1]))
      }
    }
  }
  print (paste('Total distance of all tags: ', sum(indvidual_distance_matrix[ ,2])))
  return (individual_distance_matrix)
}

## distance traveled
distance_traveled = function(vue_data, bottomfish_tag_ids = FALSE){
  if (bottomfish_tag_ids[1] == FALSE){
    bottomfish_tag_ids = unique(as.numeric(levels(vue_data$tag_id))[vue_data$tag_id])}
  individual_distance = matrix(0, length(bottomfish_tag_ids), 1)
  for (i in 1:length(bottomfish_tag_ids)){
    subset = vue_data[vue_data$tag_id == bottomfish_tag_ids[i],]
    for (a in 2:length(subset$lon)){
      individual_distance[i] = individual_distance[i] + lldist(point1 = c(subset$lon[a-1], subset$lat[a-1]), point2 = c(subset$lon[a], subset$lat[a]))
      #print(bottomfish_tag_ids[i])
      #print(unique(subset$lon))
      #print(individual_distance)
    }
  }
  total_distance_tracked = sum(individual_distance)
  print (total_distance_tracked)
  #pdf('distance_tracked_boxplot.pdf')
  #frame()
  boxplot(individual_distance, main = 'Distance Tracked (km)', ylab = 'km' )
  #dev.off()
  individual_distance_5num = fivenum(individual_distance[1:4])
  print (individual_distance_5num)
  distance_traveled_out = cbind(bottomfish_tag_ids,individual_distance)
  colnames(distance_traveled_out)[1] = 'tag_id'
  colnames(distance_traveled_out)[2] = 'distance traveled'
  return(distance_traveled_out)
}

distance_per_day = function(vue_data, bottomfish_tag_ids =FALSE){
  if (bottomfish_tag_ids[1] == FALSE){
    bottomfish_tag_ids = unique(as.numeric(levels(vue_data$tag_id))[vue_data$tag_id])}
  distance_per_day = distance_traveled(vue_data, bottomfish_tag_ids)[,2] / 
    transmission_stats(vue_data, bottomfish_tag_ids)$time_at_liberty
  print(fivenum(distance_per_day))
  boxplot(distance_per_day, main = 'Distance Standardized by Day', ylab = 'km/day')
  distance_per_day_out = cbind(bottomfish_tag_ids, distance_per_day)
  colnames(distance_per_day_out)[1] = 'tag_id'
  colnames(distance_per_day_out)[2] = 'distance/day'
  return(distance_per_day_out)
}

number_of_movements = function(vue_data, bottomfish_tag_ids =FALSE){
  if (bottomfish_tag_ids[1] == FALSE){
    bottomfish_tag_ids = unique(as.numeric(levels(vue_data$tag_id))[vue_data$tag_id])}
  movements = matrix(-1, length(bottomfish_tag_ids), 1) ## This begins at -1 because tagging location shouldn't count as a movement between stations
  for (i in 1:length(bottomfish_tag_ids)){
    indv_data =  vue_data[vue_data$tag_id == bottomfish_tag_ids[i],]
    for (a in 2:length(indv_data$station)){
      if (isTRUE(indv_data$station[a] != indv_data$station[a-1])){
        movements[i] = movements[i]+1
      }
    }
  }
  return (cbind(bottomfish_tag_ids, movements))
}

number_of_movements_per_day = function(vue_data, bottomfish_tag_ids =FALSE){
  if (bottomfish_tag_ids[1] == FALSE){
    bottomfish_tag_ids = unique(as.numeric(levels(vue_data$tag_id))[vue_data$tag_id])}
  move_per_day = number_of_movements(vue_data, bottomfish_tag_ids)[,2] / transmission_stats(vue_data, bottomfish_tag_ids)$time_at_liberty
  move_per_day_out = cbind(bottomfish_tag_ids, move_per_day)
  colnames(move_per_day_out)[1] = 'tag_id'
  colnames(move_per_day_out)[2] = '# movements / day at liberty'
  return(move_per_day_out)
}

brfa_crossings = function(vue_data, bottomfish_tag_ids =FALSE){
  if (bottomfish_tag_ids[1] == FALSE){
    bottomfish_tag_ids = unique(as.numeric(levels(vue_data$tag_id))[vue_data$tag_id])}
  movement_matrix = matrix(0, length(bottomfish_tag_ids), 3)
  for (i in 1:length(bottomfish_tag_ids)){
    indv_data = vue_data[vue_data$tag_id == bottomfish_tag_ids[i], ]
    if (isTRUE(length(indv_data) > 1)){
      for (a in 2:length(indv_data[,1])){

        if (isTRUE(as.character(indv_data$station[a-1]) != 'Oahu - Makapuu in BRFA' && 
                   as.character(indv_data$station[a]) == 'Oahu - Makapuu in BRFA')){
          movement_matrix[i,2] = movement_matrix[i,2]+1
        }else if (isTRUE(as.character(indv_data$station[a-1]) == 'Oahu - Makapuu in BRFA' && 
          as.character(indv_data$station[a]) != 'Oahu - Makapuu in BRFA')){
          movement_matrix[i,3] = movement_matrix[i,3]+1
        }
      }
    }
  }
  movement_matrix[ ,1] = movement_matrix[ ,2] + movement_matrix[ ,3]
  movement_matrix = as.data.frame(movement_matrix)
  colnames(movement_matrix)[1] = 'Number of BRFA Crossings'
  colnames(movement_matrix)[2] = 'Movements into BRFA'
  colnames(movement_matrix)[3] = 'Movements out of BRFA'
  return (cbind(bottomfish_tag_ids,movement_matrix))
  }

brfa_crossings_per_day = function(vue_data, bottomfish_tag_ids =FALSE){
  if (bottomfish_tag_ids[1] == FALSE){
    bottomfish_tag_ids = unique(as.numeric(levels(vue_data$tag_id))[vue_data$tag_id])}
    cross_per_day = brfa_crossings(vue_data, bottomfish_tag_ids)[,2] /
      transmission_stats(vue_data, bottomfish_tag_ids)[,4]
  cross_per_day_out = cbind(bottomfish_tag_ids, cross_per_day)
  colnames(cross_per_day_out)[1] = 'tag_id'
  colnames(cross_per_day_out)[1] = 'BRFA crossings / day'
  return(cross_per_day_out)
}
  
dates_of_location_switching = function(vue_data, bottomfish_tag_ids =FALSE){
  if (bottomfish_tag_ids[1] == FALSE){
    bottomfish_tag_ids = unique(as.numeric(levels(vue_data$tag_id))[vue_data$tag_id])}
    date_changes_all = c()
    for (i in 1:length(bottomfish_tag_ids)){
      indv_data = vue_data[vue_data$tag_id == bottomfish_tag_ids[i],]
      for (a in 2:length(indv_data)){
        if (isTRUE(indv_data$location[a] != indv_data$location[a-1])){
          date_changes_all = rbind(date_changes_all, rbind(c(bottomfish_tag_ids[i], indv_data$receiver[a-1], indv_data$location[a-1], indv_data$datetime[a-1], 0), c(bottomfish_tag_ids[i], indv_data$receiver[a],indv_data$location[a], indv_data$datetime[a],1)))
        }
      }
    }
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
    indv_data = vue_data[vue_data$bottomfish_tag_ids[i],]
    for (a in 2:length(indv_data$location)){
      if(isTRUE(indv_data$location[a] != indv_data$location[a-1])){
        arrive_dates = rbind(c(bottomfish_tag_ids[i], indv_data$location[a], indv_data$datetime[a]))}
  }
  }
  return(arrive_dates)
  }

all_stations_detected = function(vue_data, bottomfish_tag_ids = FALSE){
  if (bottomfish_tag_ids[1] == FALSE){
    bottomfish_tag_ids = unique(as.numeric(levels(vue_data$tag_id))[vue_data$tag_id])}
  for (i in 1:length(bottomfish_tag_ids)){
    indv_data = vue_data[vue_data$tag_id == bottomfish_tag_ids[i], ]
    all_stations = c(paste('Sequential movmeents for tag no. ', bottomfish_tag_ids[i], ': '), indv_data$station[1])
    if (length(indv_data)>1){
      for (a in 2:length(indv_data$station)){
        if (isTRUE(indv_data$station[a] != indv_data$station[a-1])){
        all_stations = c(all_stations, indv_data$station[a])
        }
      }
    }
    print(all_stations)
  }
}

unique_stations_detected = function(vue_data, bottomfish_tag_ids = FALSE){
  if (bottomfish_tag_ids[1] == FALSE){
    bottomfish_tag_ids = unique(as.numeric(levels(vue_data$tag_id))[vue_data$tag_id])}
  for (i in 1:length(bottomfish_tag_ids)){
    indv_data = vue_data[vue_data$tag_id == bottomfish_tag_ids[i], ]
    unique_station_detections = unique(indv_data$station)
    unique_stations = c(paste('Unique stations visited for tag no.', bottomfish_tag_ids[i], ': '), unique_station_detections)
    print(unique_stations)
  }
}
  
### DEBUGGED AND WORKING UNTIL HERE 24 Jan 2015

#### USAGE -------------------------------------

### Importing Data -----------------------------

## Importing receiver data
receiver_data = load_receiver(filename = 'DEPLOYMENT_RECOVERY_LOG.csv', filepath = '/Users/stephenscherrer/Dropbox/Lab Folder/Oahu Receiver Data Files/')

## Import tagging log
tagging_data = load_tagging_data('/Users/stephenscherrer/Dropbox/Lab Folder/Oahu Receiver Data Files/Bottomfish_Tag_Master.csv')

## Importing VUE Data
vue_data = load_vemco('/Users/stephenscherrer/Dropbox/Lab Folder/Oahu Receiver Data Files/VUE_Export_2015-Jan-20.csv')

### Cleaning data

## Fixing missing Lat and Lons
  vue_data = clean_vue_lat_lon(vue_data, receiver_data)

## Adding tagging location for all tags present
  vue_data = generate_tagging_detection(tagging_data, vue_data)

## Removing detections from botcam crew
  vue_data = remove_location(vue_data, 'With BotCam Crew')

## Pulling out bottomfish tag ids
  opakapaka_tag_ids = c(37970,37971,37972,37973,37974,37975,37976,37977,
                        14412,57447,57445,57446,57447,57448,57449,57450,
                        57456,57455,57457,57458,57460,57462,
                        57463,37979,57464,57465,57466,37978,37980,37981,
                        37982,37983,37984,37965,37967,37968,37969,37960,
                        37961,37962,37963,37935,37936,37937,37938,37939,
                        37940,37941,37949,37950,37951,37952,37953,37954,
                        37955,37956,37957,37958,37959,37948)
      ##NOTE: Removed tag 57451 because it was detected at cross and moved a lot and fucked up analysis

## Removing tags with no data
  opakapaka_tag_ids = na.exclude(tagging_data$vem_tag_id[tagging_data$species == 'Opakapaka']) # All Tag IDs associated with Opakapaka
  opakapaka_tag_ids = unique(as.numeric(levels(opakapaka_tag_ids))[opakapaka_tag_ids])
  opakapaka_tag_ids = clean_tags(opakapaka_tag_ids, vue_data[vue_data$station != 'Tagging Location', ])

# Removing data from unwanted tags
  vue_data = clean_vue(vue_data, opakapaka_tag_ids)

# Removing data from days receiver swapped... this could be looked at and better receiver datetimes estimated... 
#### vue_data = clean_dates(vue_data, floor(c(receiver_dates$deployment_date, receiver_dates$recovery_date))) #removes detections for days of receiver swaps

# Removing botcam detections for which I don't have time to look up lon/lats
  vue_data = remove_location(vue_data, 'With BotCam Crew')

# Add column to vue_data data frame for what day of study a detection occurred
  vue_data = study_dates(vue_data, opakapaka_tag_ids)

# Determining first, last and elapsed number of days in study
  exp_dates = experiment_dates(vue_data)
  print(exp_dates)

# pulling first detection, last detection, and time at liberty for each tag
  all_transmission_stats = transmission_stats(vue_data, opakapaka_tag_ids)
  print(all_transmission_stats)

# pulling out tags with > 7 days detection history
  tags_of_interest = all_transmission_stats$tag_id[all_transmission_stats[,5]>7]
  toi_transmission_stats = transmission_stats(vue_data, tags_of_interest)
  print(toi_transmission_stats)

# getting tagging dates
  all_tagging_date = tagging_date(tagging_data, opakapaka_tag_ids)
  toi_tagging_date = tagging_date(tagging_data, tags_of_interest)

# number of detections and percentage of total detections for each individual
  all_number_of_detections = number_of_detections(vue_data)
  print(all_number_of_detections)

  toi_number_of_detections = number_of_detections(vue_data, tags_of_interest)
  print(toi_number_of_detections)

# number of detections/day for each individual
  #### IS THIS ALL DAYS OF STUDY OR ALL DAYS AT LIBERTY?
  all_detections_per_day = detections_per_day(vue_data)
  print(all_detections_per_day)

  toi_detections_per_day = detections_per_day(vue_data, tags_of_interest)
  print(toi_detections_per_day)

# unique days detected for each individual
  all_unique_days = unique_days_detected(vue_data)
  print(all_unique_days)

  toi_unique_days = unique_days_detected(vue_data, tags_of_interest)
  print(toi_unique_days)

# distance traveled by individual
  all_distance_traveled = distance_traveled(vue_data, opakapaka_tag_ids)
  print(all_distance_traveled)

  toi_distance_traveled = distance_traveled(vue_data, tags_of_interest)
  print(toi_distance_traveled)

# distance/day at liberty
  all_distance_per_day = distance_per_day(vue_data)
  print(all_distance_per_day)

  toi_distance_per_day = distance_per_day(vue_data, tags_of_interest)
  print(toi_distance_per_day)

# All/Unique stations detected per individual
  all_stations_detected(vue_data, opakapaka_tag_ids)
  unique_stations_detected(vue_data, opakapaka_tag_ids)

  all_stations_detected(vue_data, tags_of_interest)
  unique_stations_detected(vue_data, tags_of_interest)

# number of movements
  all_movements = number_of_movements(vue_data)
  print(all_movements)

  toi_movements = number_of_movements(vue_data, tags_of_interest)
  print(toi_movements)

#number of movements / day at liberty
  all_movements_per_day = number_of_movements_per_day(vue_data)
  print(all_movements_per_day)

  toi_movements_per_day = number_of_movements_per_day(vue_data, tags_of_interest)
  print(toi_movements_per_day)

# number of BRFA crossings and crossings/day
all_brfa_crossings = brfa_crossings(vue_data)
print (all_brfa_crossings)

toi_brfa_crossings = brfa_crossings(vue_data, tags_of_interest)
print(toi_brfa_crossings)

# number of BRFA crossings/day
all_brfa_crossings_per_day = brfa_crossings_per_day(vue_data)
print(all_brfa_crossings_per_day)

toi_brfa_crossings_per_day = brfa_crossings_per_day(vue_data, tags_of_interest)
print(toi_brfa_crossings_per_day)
#-------------

# Putting it all together
all_data_output = cbind(all_tagging_date, all_transmission_stats[,4], all_number_of_detections[,-1],  
                        all_detections_per_day[,-1], all_unique_days[,-1], 
                        all_distance_traveled[,-1], all_distance_per_day[,-1],
                         all_movements[,-1], 
                        all_movements_per_day[,-1], all_brfa_crossings[,2], 
                        all_brfa_crossings_per_day[,2])

toi_data_output = cbind(toi_tagging_date, toi_number_of_detections[,-1],  
                        toi_detections_per_day[,-1], toi_unique_days[,-1], 
                        toi_distance_traveled[,-1], toi_distance_per_day[,-1],
                         toi_movements[,-1], 
                        toi_movements_per_day[,-1], toi_brfa_crossings[,2], 
                        toi_brfa_crossings_per_day[,2])

write.csv(all_data_output, file = 'bottomfish_summary.csv')

plot.new()
par(mfrow = c(1,1), oma=c(0,0,2,0))  
median = signif(fivenum(toi_data_output[,3])[3], 3)
first_q = signif(fivenum(toi_data_output[,3])[2], 3)
third_q = signif(fivenum(toi_data_output[,3])[4], 3)
boxplot(toi_data_output[,3], main = 'Number of Detections by Tag', ylab = 'Detections')
legend('topright', cex = .75, legend = c(sprintf('1st Quartile: %s', first_q), sprintf('Median: %s', median), sprintf('3rd Quartile: %s', third_q)),
        col = 'black')

plot.new()
median = signif(fivenum(toi_data_output[,4])[3], 3)
first_q = signif(fivenum(toi_data_output[,4])[2], 3)
third_q = signif(fivenum(toi_data_output[,4])[4], 3)
boxplot(toi_data_output[,4], main = 'Percentage of All Detections by Tag', ylab = '%')
legend('topright', cex = .75, legend = c(sprintf('1st Quartile: %s', first_q), sprintf('Median: %s', median), sprintf('3rd Quartile: %s', third_q)),
       col = 'black')

plot.new()
median = signif(fivenum(toi_data_output[,5])[3], 3)
first_q = signif(fivenum(toi_data_output[,5])[2], 3)
third_q = signif(fivenum(toi_data_output[,5])[4], 3)
boxplot(toi_data_output[,5], main = 'Mean Detections By Day', ylab = '# Detections/Day')
legend('topright', cex = .75, legend = c(sprintf('1st Quartile: %s', first_q), sprintf('Median: %s', median), sprintf('3rd Quartile: %s', third_q)),
       col = 'black')

plot.new()
median = signif(fivenum(toi_data_output[,6])[3], 3)
first_q = signif(fivenum(toi_data_output[,6])[2], 3)
third_q = signif(fivenum(toi_data_output[,6])[4], 3)
boxplot(toi_data_output[,6], main = 'Unique Days Detected', ylab = 'Days')
legend('topright', cex = .75, legend = c(sprintf('1st Quartile: %s', first_q), sprintf('Median: %s', median), sprintf('3rd Quartile: %s', third_q)),
       col = 'black')

plot.new()
median = signif(fivenum(toi_data_output[,7])[3], 3)
first_q = signif(fivenum(toi_data_output[,7])[2], 3)
third_q = signif(fivenum(toi_data_output[,7])[4], 3)
boxplot(toi_data_output[,7], main = 'Total Distance Tracked', ylab = 'km')
legend('topright', cex = .75, legend = c(sprintf('1st Quartile: %s', first_q), sprintf('Median: %s', median), sprintf('3rd Quartile: %s', third_q)),
       col = 'black')

median = signif(fivenum(toi_data_output[,8])[3], 3)
first_q = signif(fivenum(toi_data_output[,8])[2], 3)
third_q = signif(fivenum(toi_data_output[,8])[4], 3)
boxplot(toi_data_output[,8], main = 'Distance Tracked/Day', ylab = 'km/Day')
legend('topright', cex = .75, legend = c(sprintf('1st Quartile: %s', first_q), sprintf('Median: %s', median), sprintf('3rd Quartile: %s', third_q)),
       col = 'black')

plot.new()
median = signif(fivenum(toi_data_output[,9])[3], 3)
first_q = signif(fivenum(toi_data_output[,9])[2], 3)
third_q = signif(fivenum(toi_data_output[,9])[4], 3)
boxplot(toi_data_output[,9], main = 'Unique Receivers Visited', ylab = '# Receivers')
legend('topright', cex = .75, legend = c(sprintf('1st Quartile: %s', first_q), sprintf('Median: %s', median), sprintf('3rd Quartile: %s', third_q)),
       col = 'black')

plot.new()
median = signif(fivenum(toi_data_output[,11])[3], 3)
first_q = signif(fivenum(toi_data_output[,11])[2], 3)
third_q = signif(fivenum(toi_data_output[,11])[4], 3)
boxplot(toi_data_output[,11], main = 'Movements Between Receivers/Day', ylab = 'Movements/Day')
legend('topright', cex = .75, legend = c(sprintf('1st Quartile: %s', first_q), sprintf('Median: %s', median), sprintf('3rd Quartile: %s', third_q)),
       col = 'black')

plot.new()
median = signif(fivenum(toi_data_output[,12])[3], 3)
first_q = signif(fivenum(toi_data_output[,12])[2], 3)
third_q = signif(fivenum(toi_data_output[,12])[4], 3)
boxplot(toi_data_output[,12], main = 'Total BRFA Crossings', ylab = 'BRFA Crossings')
legend('topright', cex = .75, legend = c(sprintf('1st Quartile: %s', first_q), sprintf('Median: %s', median), sprintf('3rd Quartile: %s', third_q)),
       col = 'black')

plot.new()
median = signif(fivenum(toi_data_output[,13])[3], 3)
first_q = signif(fivenum(toi_data_output[,13])[2], 3)
third_q = signif(fivenum(toi_data_output[,13])[4], 3)
boxplot(toi_data_output[,13], main = 'BRFA Crossings/Day', ylab = 'BRFA Crossings/Day')
legend('topright', cex = .75, legend = c(sprintf('1st Quartile: %s', first_q), sprintf('Median: %s', median), sprintf('3rd Quartile: %s', third_q)),
       col = 'black')


## Plotting out detections by study date
for (i in 1:length(tags_of_interest)){
  tag_id = tags_of_interest[i]
plot_title = sprintf('Detections of Tag %s', tag_id)
hist(vue_data[vue_data$tag_id == tag_id,1],  breaks = length(vue_data[vue_data$tag_id == tag_id, 1]) , main = plot_title, xlab = 'Days at liberty (Binned Every 30 Days)', ylab = 'Number of Transmissions', ylim = c(0,25), xlim = c(0, max(vue_data$datetime))+5)
abline(v = vue_data[vue_data$tag_id == tag_id, 6][1], col = 'red')
dates = dates_of_location_switching(vue_data, tags_of_interest)
abline(v = dates[dates[,5] == 1, 4], col = 258)
}

## All Detections
hist(vue_data[,1], breaks = ceiling(max(vue_data[,1])/30), main = 'Total Transmissions Detected During Study', xlab = 'Days (Binned Every 30 Days)', ylab = 'Transmissions Detected', ylim = c(0,3000), xlim = c(0, max(vue_data$datetime))+5)
## Making Maps

pngMAP_df<- get_map(location = c(lon = -157.75, lat = 21.251), 
                    source = "google", zoom = 9,color='bw')
# Plots
for (tag_id in tags_of_interest){
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
  geom_path(color = zissou.red, size = 2, data = plot_lonlats, aes(x = lon, y = lat)) + 
  labs(title = sprintf('Detections of Tag %s', tag_id)))
dev.off()
}


### Plotting Receiver Positions
receiver_pos_p_1 = read.csv(file = 'Receiver_Log_for_Phase_1_plotting.csv', header = TRUE)
receiver_pos_p_2 = read.csv(file = 'Receiver_Log_for_plotting_phase_2_array.csv', header = TRUE)

ggmap(pngMAP_df) + 
  geom_path(color = zissou.blue, mapping = aes(x = c(-157.566, -157.566), y = c(21.0333, 20.9166))) +
  geom_path(color = zissou.blue, mapping = aes(x = c(-157.566, -157.366), y = c(20.9166, 20.9166))) +
  geom_path(color = zissou.blue, mapping = aes(x = c(-157.366, -157.366), y = c(20.9166, 21.0333))) +
  geom_path(color = zissou.blue, mapping = aes(x = c(-157.366, -157.566), y = c(21.0333, 21.0333))) +
  geom_path(color = zissou.blue, mapping = aes(x = c(-157.683, -157.533), y = c(21.4166, 21.4166))) +
  geom_path(color = zissou.blue, mapping = aes(x = c(-157.533, -157.533), y = c(21.4166, 21.2833))) +
  geom_path(color = zissou.blue, mapping = aes(x = c(-157.683, -157.533), y = c(21.2833, 21.2833))) +
  #geom_path(color = zissou.red, mapping = aes(x = c(-157.57, -157.57), y = c(21.308, 21.256))) +
  #geom_path(color = zissou.red, aes(x = c(-157.57, -157.69), y = c(21.256, 21.426))) + 
  #geom_path(color = zissou.red, aes(x = c(-157.69, -157.57), y = c(21.426, 21.308))) + 
  geom_point(color = zissou.gold, size = 3, data = receiver_pos_p_1,
             aes(x = c(lon_plot), y = c(lat_plot))) + 
  #geom_point(color = zissou.red, size = 2, 
             #aes(x = -157.57, y = 21.308))+
  labs(title = 'Phase 1 Acoustic Array')

ggmap(pngMAP_df) + 
  geom_path(color = zissou.blue, mapping = aes(x = c(-157.566, -157.566), y = c(21.0333, 20.9166))) +
  geom_path(color = zissou.blue, mapping = aes(x = c(-157.566, -157.366), y = c(20.9166, 20.9166))) +
  geom_path(color = zissou.blue, mapping = aes(x = c(-157.366, -157.366), y = c(20.9166, 21.0333))) +
  geom_path(color = zissou.blue, mapping = aes(x = c(-157.366, -157.566), y = c(21.0333, 21.0333))) +
  geom_path(color = zissou.blue, mapping = aes(x = c(-157.683, -157.533), y = c(21.4166, 21.4166))) +
  geom_path(color = zissou.blue, mapping = aes(x = c(-157.533, -157.533), y = c(21.4166, 21.2833))) +
  geom_path(color = zissou.blue, mapping = aes(x = c(-157.683, -157.533), y = c(21.2833, 21.2833))) +
  #geom_path(color = zissou.red, mapping = aes(x = c(-157.57, -157.57), y = c(21.308, 21.256))) +
  #geom_path(color = zissou.red, aes(x = c(-157.57, -157.69), y = c(21.256, 21.426))) + 
  #geom_path(color = zissou.red, aes(x = c(-157.69, -157.57), y = c(21.426, 21.308))) + 
  geom_point(color = zissou.gold, size = 3, data = receiver_pos_p_2,
             aes(x = c(lon_plot), y = c(lat_plot))) + 
  #geom_point(color = zissou.red, size = 2, 
  #aes(x = -157.57, y = 21.308))+
  labs(title = 'Phase 2 Acoustic Array')

####### Working until here 7 Feb 2015#### Working until here 7 Feb 2015



for (i in 1:length(tags_of_interest)){
indv_data = vue_data[vue_data$tag_id ==57459, ]
ll = cbind(indv_data$lon, indv_data$lat)
lon = unique(ll)[,1]
lat = unique(ll)[,2]
  }
map_title = toString(c('Detections of Tag ', toString(tags_of_interest[i])))
make_map(indv_data, lon, lat, map_title)
}

map_title = toString(c('Detections of Tag ', toString(tags_of_interest[i])))
  pdf(map_title)
  frame()
  make_map(indv_data)
  dev.off()
}



  
  ## For each/all fish:
  # How long tag active/time at liberty
  # Number of detections
  # Recovery rate
  # Date of last tag
  # Histogram of detections per time bin
  # Weekly
  # By time bin
  # Number of receivers detected at.
  # Determine receiver deployment configuration during those periods
  # Receiver name for each recorded movement along with dates.
  # Plot of receiver movements by date
  # Determine distance swam
  # Number of BRFA crossings
  # Number of channel crossings
  ## For all fish
  # Total study durration.
  # histogram of all detections by time
  # Remove last 2 weeks of tag records to account for predation
  # Determine which tags meet criteria for consideration
  # Export receiver deployment configurations for processing through web app
  # Import web app recovery rates
  # t-test tagged fish rates with Ho: recovery rate = model output
  # plot recovery rate vs model projections
