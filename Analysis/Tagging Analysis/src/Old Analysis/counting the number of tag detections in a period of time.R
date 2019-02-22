load("/Users/stephenscherrer/Google Drive/Weng Lab/Personal_Folders/Steve/dissertation work/Opakapaka Tagging/Opakapaka Tagging Analysis/workspace_image_updated")

p2_tags_good  = c(18260, 18259, 18256, 18253, 18252, 18251, 18249, 916, 915, 909, 905, 902)

vue_data = vue_data[vue_data$tag_id %in% p2_tags_good, ]

filter_false_detections = function(vue_data, ncores = 4){
  registerDoParallel(cores = ncores)
  fun_timer = proc.time()
  detection_status = foreach(i = 1:length(vue_data$datetime), .combine = c) %dopar%{
    ## If the difference in time between a detection under evaluation and the subset of all detections by that tag in a 24 hour window (12 hours on either side) is greater than one 1 (the detection under evaluation), the detection is flagged for removal
    return(length(which(abs(difftime(vue_data$datetime[vue_data$tag_id == vue_data$tag_id[i]], vue_data$datetime[i], units = "hours")) > 12)) > 1)
  }
  function_time = proc.time() - fun_timer # Stop the clock
  print(paste(length(which(detection_status == FALSE)),'of', length(detection_status), 'detections flagged FALSE. (', round(function_time[3])/60, 'minutes to process )'))
  return(detection_status)
}
vue_data = vue_data[filter_false_detections(vue_data), ]


# hour limit to group by (will crawl along data counting number of detections within this period)
hour_limit = 1
n_detections_in_hour_limit = rep(0, 100000)

for(i in 1:length(unique(vue_data$tag_id))){
  # print(i)
  # first separate by tag
  indv_data = vue_data[vue_data$tag_id == unique(vue_data$tag_id)[i], ]
  # then separate by station
  for(j in 1:length(unique(indv_data$station))){
    stat_data = indv_data[indv_data$station == unique(indv_data$station)[j], ]
    stat_data$index = 1:length(stat_data$datetime)
    stat_data$date = stat_data$datetime
    # for each detection at a receiver
    k = 1
    while(k < (length(stat_data$datetime))-1){
      end_k = max(which(abs(difftime(stat_data$datetime[k], stat_data$datetime, units = "hours")) <= hour_limit))
      if(end_k <= length(stat_data$datetime)){
        while(max(which(abs(difftime(stat_data$datetime[end_k], stat_data$datetime[end_k:length(stat_data$datetime)], units = "hours")) <= hour_limit)) > 1){
          # readline(prompt= paste("k:", k, "end_k:", end_k))
          end_k = end_k + max(which(abs(difftime(stat_data$datetime[end_k], stat_data$datetime[end_k:length(stat_data$datetime)], units = "hours")) <= hour_limit) -1 )
        }
      }
      # readline(prompt= paste("k:", k, "end_k:", end_k, "n_detections", (end_k - k) + 1))
      n_detections_in_hour_limit[(end_k - k) + 1] = n_detections_in_hour_limit[(end_k - k) + 1] + 1
      if(length(n_detections_in_hour_limit) > 100000){
        readline(prompt= paste("i", i, "j", j, "k", k))
      }
      print(end_k - k)
      k = end_k + 1
    }
  }
}
barplot(height = n_detections_in_hour_limit[1:21])




for(i in 1:length(unique(vue_data$tag_id))){
  print(i)
  # first separate by tag
  indv_data = vue_data[vue_data$tag_id == unique(vue_data$tag_id)[i], ]
  # each tag gets a counter
  bad_detection_counter = 0
  # total number of detections for that individual
  detections_total = length(indv_data$datetime)
  # for each station
  for(j in 1:length(unique(indv_data$station))){
    # data for station
    stat_data = indv_data[indv_data$station == unique(indv_data$station)[j], ]
    # difference in time between detections in seconds
    detection_diff = diff(stat_data$datetime) # in seconds
    # turn it to hours
    detection_diff = detection_diff / 3600 # turns this into hours
    # if there are 3 or more detections
    if(length(detection_diff) > n_detections){
      # for each difference in detection
      for(k in 1:(length(detection_diff)-1)){
        ## are there any detections within 1 hour of it?
        if(detection_diff[k] > 1 & detection_diff[k+1] > 1){
          # if not, it is a bad detection
          bad_detection_counter = bad_detection_counter+1
          #if there is only two detections, is the difference between them greater than one hour
        }
      }
    }else if(length(detection_diff) == 0){
      # it occurred in isolation and is bad.
      bad_detection_counter = bad_detection_counter+1
    }else if(length(detection_diff) == 1 & detection_diff[1] > 1){
      # if yes, then its a bad detection
      bad_detection_counter = bad_detection_counter+1
      # if there was only one detection
      
    }
  }
  print(paste('tag_id:', unique(vue_data$tag_id[i]), ' bad detections:', bad_detection_counter, "  total detections", detections_total))
  
  list_of_detection_differences = c(list_of_detection_differences, (bad_detection_counter / detections_total))
}




list_of_detection_differences = c()
for(i in 1:length(unique(vue_data$tag_id))){
  print(i)
  # first separate by tag
  indv_data = vue_data[vue_data$tag_id == unique(vue_data$tag_id)[i], ]
  # each tag gets a counter
  bad_detection_counter = 0
  # total number of detections for that individual
  detections_total = length(indv_data$datetime)
  # for each station
  for(j in 1:length(unique(indv_data$station))){
    # data for station
    stat_data = indv_data[indv_data$station == unique(indv_data$station)[j], ]
    # difference in time between detections in seconds
    detection_diff = diff(stat_data$datetime) # in seconds
    # turn it to hours
    detection_diff = detection_diff / 3600 # turns this into hours
    # if there are 3 or more detections
    if(length(detection_diff) > 2){
      # for each difference in detection
      for(k in 1:(length(detection_diff)-1)){
        ## are there any detections within 1 hour of it?
        if(detection_diff[k] > 1 & detection_diff[k+1] > 1){
          # if not, it is a bad detection
          bad_detection_counter = bad_detection_counter+1
          #if there is only two detections, is the difference between them greater than one hour
        }
      }
    }else if(length(detection_diff) == 1 & detection_diff[1] > 1){
      # if yes, then its a bad detection
      bad_detection_counter = bad_detection_counter+1
      # if there was only one detection
    }else if(length(detection_diff) == 0){
      # it occurred in isolation and is bad.
      bad_detection_counter = bad_detection_counter+1
    }
  }
  print(paste('tag_id:', unique(vue_data$tag_id[i]), ' bad detections:', bad_detection_counter, "  total detections", detections_total))
  
  list_of_detection_differences = c(list_of_detection_differences, (bad_detection_counter / detections_total))
}


