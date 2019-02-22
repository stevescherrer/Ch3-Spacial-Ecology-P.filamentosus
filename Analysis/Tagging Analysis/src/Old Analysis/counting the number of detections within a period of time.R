library('doParallel')
library('beepr')
load("/Users/stephenscherrer/Google Drive/Weng Lab/Personal_Folders/Steve/dissertation work/Opakapaka Tagging/Opakapaka Tagging Analysis/workspace_image_updated")

p2_tags_good  = c(18260, 18259, 18256, 18253, 18252, 18251, 18249, 916, 915, 909, 905, 902)
p2_tags_dead = c(36799, 18268, 18269, 18249, 18250, 18251, 18252, 18253, 18254, 922, 920, 919, 911, 909, 900, 899, 898)

vue_data = vue_data[vue_data$tag_id %in% p2_tags_dead, ]

filter_false_detections = function(vue_data, ncores = detectCores()){
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

false_detection_index = filter_false_detections(vue_data = vue_data, ncores = 4)

vue_data = vue_data[false_detection_index, ]


# hour limit to group by (will crawl along data counting number of detections within this period)
hour_limit = 1/15
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
      k = end_k + 1
    }
  }
}
barplot(height = n_detections_in_hour_limit[1:21], names.arg = as.character(0:20), xlab = "Length of Detection Run", ylab = "Number of Detection Runs")

n_detections_in_hour_limit[1:21]
n_detections_in_limit_4_min = n_detections_in_hour_limit[1:21]



