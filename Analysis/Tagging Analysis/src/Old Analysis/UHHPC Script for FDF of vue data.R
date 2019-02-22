#### A Script for performing false detection filtering on the UH HPC

#### Setting up project workspace
rm(list = ls())
data_directory = ""

#### Installing principle dependencies
install.packages('doParallel')
library('doParallel') #detectCores(), for_each()
source(vemcoUtilityFunctions.R)

#### Loading in datafiles
vue_raw = load_vemco(file.path(data_directory, 'VUE_Export_2017_June_25.csv'))

#### Flagging false detections
## based on Alfonso 2009
remove_false_detections = function(vue_data, transmitter_interval = 60, remove_detections = TRUE, ncores = 8){
  fun_timer = proc.time()
  tf_detection_index = c()
  registerDoParallel(cores = ncores)
  detection_status = foreach(i = 1:length(vue_data$datetime), .combine = c) %dopar%{
    ## If the difference in time between a detection under evaluation and the subset of all detections by that tag in a 24 hour window (12 hours on either side) is greater than one 1 (the detection under evaluation), the detection is flagged for removal
    return(length(which(abs(difftime(vue_data$datetime[vue_data$tag_id == vue_data$tag_id[i]], vue_data$datetime[i], units = "hours")) > 12)) > 1)
  }
  print(proc.time() - fun_timer) # Stop the clock
  if(remove_detections == TRUE){
    vue_data = vue_data[detection_status == TRUE, ]
    return(vue_data)
  }else{
    return(detection_status)
  }
  beep(2)
}

vue_processed = remove_false_detections(vue_data = vue_raw, transmitter_interval = 60, remove_detections = FALSE, ncores = detectCores())

write.csv(x = vue_processed, file = 'VUE_Export_2017_June_25_with_FDF.csv')

send_push(user = 'uGEHvA4hr37tsrCCtpSv4sUUxVuTqN', message = "FDF on UHHPC complete!")
