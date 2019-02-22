#### Turned off "make_detection_plot()" in
#### Turned off "movement_graph_standardized_by_distance calculations and output.
#### Modified run_analysis() incorperates movement_graph and brfa_movements_by_time_at_liberty

### Utility Functions Available
  # clean_vue_data()
  # clean_receiver_stations()
  # subset_time()
  # create_save_directory()
  # load_zissou_palette()
  # load_vemco()
  # convert_tz()
  # vue_col_names()
  # clean_tag_id()
  # clean_receiver()
  # load_receiver_data()
  # receiver_col_names()
  # load_tagging_data()
  # meta_data_col_names()
  # clean_vue_lat_lon()
  # remove_location()
  # clean_vue()
  # clean_tags()
  # convert_lat_lon
  # get_recovery_rate()
  # remove_detections_before_tagging()

### Analysis Functions Available
  # cluster_receivers()
  # get_graph()
  # build_detection_matrix()
  # calculate_time_at_liberty()
  # calculate_days_before_detection()
  # calculate_days_detected()
  # generate_tagging_detection()
  # generate_study_date()
  # list_stations_detected()
  # remove_false_detections()
  # tag_detection_histogram()
  # plot_receiver_map()
  # plot_depths()
  # plot_movements()
  # plot_path_use()
  # plot_tag_detections()
  # spatial_evenness()
  # days_present()
  # length_to_weight()
  # lldist()
  # distance_tracked()
  # tagging_histogram()
  # assign_color_palette()
  # generate_stripchart()
  # detection_stripchart()
  # in_brfa_e()
  # in_brfa_f()
  # brfa_movements()
  # brfa_movements_by_time_at_liberty()
  # n_movements()
  # max_movement()
  # get_fork_length()
  # get_tagging_date()
  # remove_only_tagging_date_entries()
  # run_analysis()
  # generate_analysis_report()
  # create_analysis_csv()
  # plot_receiver_maps()
  # create_day_night_plot()
  # plot_detections_by_receiver()
  # detection_stripcharts()
  # make_detection_plot()
  # generate_gif_images()
  # plot_clusters()
  # run()
  # distance_between_vue_receivers()
  # distance_between_receivers()
  # get_recovery_rates()
  # days_detected()
  # plot_receiver_histories()
  # stations_detected()
  # brfa_size()
  # make_detection_plot()
  # plot_station_map()


script_timer = proc.time()

#### TODO: 
####  plotting index as part of analysis (unfilled circles for uncertain detections etc...)
####  plot receivers detected by # of detections
####  % of days detected / days since tagging
####  Total days detected /days at liberty = fidelity index
####  Track length vs. Size? Good tracks vs. size
####  Residence behavior vs. Size?
####  Fix get_graph() function
####  Plot histogram for how long tags were heard after tagging. Both if tagged then heard 1 week, assume detection for entire week, and only for intervals detected.
####  Giant excel calendar of detections?
####  Cluster analysis of receivers?

#### Bottomfish Movement Analysis
#### Written: January 2016 by Stephen R. Scherrer

#### Code for the analysis of Acoustic Tags and VR2W 
#### Data applied to Opakapaka tagged in Oahu, Hawaii

##### Clearning Workspace and setting directories ------------------------------------
rm(list=ls()) # Clear workspace
project_dir = '/Users/stephenscherrer/Google Drive/Weng Lab/Personal_Folders/Steve/dissertation work/Opakapaka Tagging/Opakapaka Tagging Analysis'
data_dir = paste('/Users/stephenscherrer/Google Drive/Weng Lab/Data/Bottomfish/Oahu Receiver Data Files/')
results_dir = paste(project_dir, '/results/', sep = "")
# figure_dir = paste(project_dir, '/figures/', sep = "")
src_dir = paste(project_dir, '/src/', sep = "")
bin_dir = paste(project_dir, '/bin/', sep = "")
setwd(project_dir)
savehistory(file=paste(results_dir, 'results.txt'))
# load("/Users/stephenscherrer/Google Drive/Weng Lab/Steve/dissertation work/Opakapaka Tagging/Opakapaka Tagging Analysis/workspace_image")

##### Importing principle dependencies----------------------------------------------
#   install.packages('wesanderson') # color palett for plotting
#   install.packages('matlab')
#   install.packages('maps')
#   install.packages('mapdata')
#   install.packages('maptools')
#   install.packages('scales')
#   install.packages('ggmap')
#   install.packages('doParallel') # foreach(), detectCores(), registerDoParallel()
#   install.packages('marmap')
#   install.packages('lubridate') # floor_date()
#   install.packages('beepr') # beep()
#   install.packages('dplyr') # filter()
#   install.packages('wesanderson') # wes_palette
#   install.packages('useful') # compare.list()
#   install.packages('RAtmosphere')
#   install.packages('geosphere') # areaPolygon()
#   install.packages('igraph') # graph.adjacency()
#   install.packages('distgeo')
#   install.packages('notifyR')
# library("matlab", lib.loc) # ceil()
# library('maps')
# library('mapdata')
#library('maptools')  # for shapefiles
#library('scales')  # for transparency
#library('ggmap')
#source('/Users/stephenscherrer/Documents/Programming/R/utility_functions.R')
#library('reshape') # merge_all
library('marmap') # bathymetry()
# source('/Users/stephenscherrer/Documents/Work/UH/Projects/dissertation work/Spacial Ecology/Bottomfish Analysis Feb 2015/R Code/bf_analysis_functions.R')
# library('plotly')
library('lubridate') # floor_date(), ceil_date()
library('doParallel') # foreach()
library('beepr') # beep
library('dplyr') # filter()
library('wesanderson') #wes_palette
library('useful') # compare.list()
# library('animation') # NOTE: creating animations requires installation of ImageMagick. See http://www.imagemagick.org for details
library('RAtmosphere')
library('geosphere') # areaPolygon()
library('igraph') # graph.adjacency()
library('wesanderson')
library('geosphere') # distGeo() Note: wrapped in old lldist function
library('notifyR') # send_push()

##### Functions --------------------------------------------

#### Utility functions ----------

### Fixes station names, latitude, and longitude, for vue datafile
clean_vue_data = function(vue_data, receiver_data){
  for(i in 1:length(receiver_data$station_name)){
    vue_data$station[which(vue_data$receiver == receiver_data$vr2w_serial[i] & 
                             vue_data$datetime >= receiver_data$deployment_date[i] &
                             vue_data$datetime <= receiver_data$recovery_date[i])] = 
      as.character(receiver_data$station_name[i])
    vue_data$lat[which(vue_data$receiver == receiver_data$vr2w_serial[i] & 
                         vue_data$datetime >= receiver_data$deployment_date[i] &
                         vue_data$datetime <= receiver_data$recovery_date[i])] = 
      as.numeric(receiver_data$lat[i])
    vue_data$lon[which(vue_data$receiver == receiver_data$vr2w_serial[i] & 
                         vue_data$datetime >= receiver_data$deployment_date[i] &
                         vue_data$datetime <= receiver_data$recovery_date[i])] = 
      as.numeric(receiver_data$lon[i])
  }
  vue_data$lat = as.numeric(vue_data$lat)
  vue_data$lon = as.numeric(vue_data$lon)
  return(vue_data)
}

clean_receiver_stations = function(receiver_data, region = c('Oahu', 'PB'), remove = FALSE){
  index = c()
  for(i in 1:length(receiver_data$station_name)){
    if(strsplit(as.character(receiver_data$station_name[i]), split = " ")[[1]][1] %in% region == TRUE){
      index = c(index, i)
    }
  }
  if(remove == FALSE){
    receiver_data = receiver_data[index, ]
  }else if(remove == TRUE){
    receiver_data = receiver_data[!index, ]
  }
  receiver_data$station_name = as.character(receiver_data$station_name)
  return(receiver_data)
}

subset_time = function(vue_data, start = min(vue_data$datetime), end = max(vue_data$datetime)){
  new_vue_df = vue_data[which(vue_data$datetime >= as.POSIXct(start) & vue_data$datetime < as.POSIXct(end)), ]
  return (new_vue_df)
}

create_save_directory = function(new_directory){
  if(new_directory != FALSE){
#    main_directory = getwd() # stores old working directory. 
    if(new_directory == TRUE){
      dir.name = paste(getwd(), "/", Sys.time(), sep = "") # if Output Results is TRUE, creates a new directory for storing output files with system time
    }else{
      dir.name = paste(getwd(), "/", new_directory, sep = "") # if Output Results is not FALSE but not TRUE, creates a new directory for storing output files with contents of output_results argument
    }
    dir.create(dir.name) # creates new output directory
    # setwd(dir.name) # changes to newly created output directory
  }
  return(dir.name) # for setting working directory with
}

load_zissou_palette = function(){
  ### Plotting Colors -
  zissou = list()
  zissou$teal   = wes_palette("Zissou", 5)[1]
  zissou$blue   = wes_palette("Zissou", 5)[2]
  zissou$gold   = wes_palette("Zissou", 5)[3]
  zissou$yellow = wes_palette("Zissou", 5)[4]
  zissou$red    = wes_palette("Zissou", 5)[5]
  return(zissou)
}

load_vemco = function(filename, filepath = FALSE, format = '%Y-%m-%d %H:%M:%S'){
  proj_dir = getwd()
  if (isTRUE(filepath != FALSE)) {setwd(filepath)}
  vue_data_raw = read.csv(filename)
  vue_data_cleaned = vue_col_names(vue_data_raw)
  vue_data_cleaned$datetime = strptime(vue_data_cleaned$datetime, 
                                       format = format,
                                       tz = "GMT")
  vue_data_cleaned$datetime = convert_tz(vue_data_cleaned$datetime, new.tz = 'HST')
  vue_data_cleaned$tag_id = clean_tag_id(vue_data_cleaned$tag_id)
  vue_data_cleaned$receiver = clean_receiver(vue_data_cleaned$receiver)
  
  setwd(proj_dir)
  return (vue_data_cleaned)
}

convert_tz = function(datetime, new.tz = 'HST'){
  #Function to convert GMT/UTC times to HST time
  datetime.new.tz = strptime(datetime, format = '%Y-%m-%d %H:%M:%S', tz = new.tz)
  dateoffset = datetime-datetime.new.tz
  datetime.new.tz = datetime.new.tz + dateoffset
  return(datetime.new.tz)
}

vue_col_names = function(vue_data_raw){
  colnames(vue_data_raw)[1]  <- 'datetime'
  colnames(vue_data_raw)[2]  <- 'receiver'
  colnames(vue_data_raw)[3]  <- 'tag_id'
  colnames(vue_data_raw)[4]  <- 'name'
  colnames(vue_data_raw)[5]  <- 'tag_serial'
  colnames(vue_data_raw)[6]  <- 'depth'
  colnames(vue_data_raw)[7]  <- 'sensor.unit'
  colnames(vue_data_raw)[8]  <- 'station'
  colnames(vue_data_raw)[9]  <- 'lat'
  colnames(vue_data_raw)[10] <- 'lon'
  return (vue_data_raw)
}

clean_tag_id = function(tag_id){
  ## Returns tag ID number as a factor, removing the 'A69-####-' prefix
  cleaned_id = as.factor(substring(tag_id, 10, ))
  return (cleaned_id)
}

clean_receiver = function(receiver){
  ## Returns receiver number as a factor, remvoing the 'VR2W-' Prefix
  cleaned_receiver = as.factor(substring(receiver, 6))
  return (cleaned_receiver)
}

load_receiver_data = function(filename, filepath = FALSE){
  proj_dir = getwd()
  if (filepath != FALSE){
    setwd(filepath)
  }
  receiver_dates = receiver_col_names(read.csv(filename))
  receiver_dates$deployment_date = strptime(receiver_dates$deployment_date, format = '%m/%d/%y %H:%M', tz = 'HST')
  receiver_dates$recovery_date = strptime(receiver_dates$recovery_date, format = '%m/%d/%y %H:%M', tz = 'HST')
  receiver_dates$lat = convert_lat_lon(receiver_dates$lat_deg, receiver_dates$lat_min)
  receiver_dates$lon = convert_lat_lon(receiver_dates$lon_deg, receiver_dates$lon_min)
  setwd(proj_dir)
  return (receiver_dates)
}

receiver_col_names = function(receiver_file_raw){
  receiver_file = receiver_file_raw
  colnames(receiver_file)[1] = 'serviced'
  colnames(receiver_file)[2] = 'station_name'
  colnames(receiver_file)[3] = 'consecutive_deployment_number'
  colnames(receiver_file)[4] = 'deployment_date'
  colnames(receiver_file)[5] = 'recovery_date'
  colnames(receiver_file)[6] = 'recovered'
  colnames(receiver_file)[7] = 'in_data_set'
  colnames(receiver_file)[8] = 'lat_deg'
  colnames(receiver_file)[9] = 'lat_min'
  colnames(receiver_file)[10] = 'lon_deg'
  colnames(receiver_file)[11] = 'lon_min'
  colnames(receiver_file)[12] = 'depth'
  colnames(receiver_file)[13] = 'vr2w_serial'
  colnames(receiver_file)[14] = 'acoustic_release_serial'
  colnames(receiver_file)[15] = 'acoustic_release_battery_life'
  colnames(receiver_file)[16] = 'acoustic_release_voltage_at_deployment'
  colnames(receiver_file)[17] = 'acoustic_release_serial_code'
  colnames(receiver_file)[18] = 'temperature_logger_serial'
  colnames(receiver_file)[19] = 'location_code'
  colnames(receiver_file)[20] = 'deployed_by'
  colnames(receiver_file)[21] = 'recovered_by'
  colnames(receiver_file)[22] = 'comments_deployment'
  colnames(receiver_file)[23] = 'comments_recovery'
  return (receiver_file)
}

load_tagging_data = function(filename, filepath = FALSE){
  proj_dir = getwd()
  if (isTRUE(filepath != FALSE)){
    setwd(filepath)
  }
  tagging_data_raw = read.csv(filename)
  tagging_meta_data = meta_data_col_names(tagging_data_raw)
  tagging_meta_data$datetime = strptime(tagging_meta_data$datetime, format = '%m/%d/%y %H:%M', tz = 'HST')
  tagging_meta_data$vem_tag_id = as.factor(tagging_meta_data$vem_tag_id)
  tagging_meta_data$lat = convert_lat_lon(tagging_meta_data$lat_deg, tagging_meta_data$lat_min)
  tagging_meta_data$lon = convert_lat_lon(tagging_meta_data$lon_deg, tagging_meta_data$lon_min)
  setwd(proj_dir)
  return (tagging_meta_data)
}

meta_data_col_names = function(data_frame){
  colnames(data_frame)[1]  <- 'unique_id'
  colnames(data_frame)[2]  <- 'datetime'
  colnames(data_frame)[3]  <- 'species'
  colnames(data_frame)[4]  <- 'conventional_tag_id'
  colnames(data_frame)[5]  <- 'vem_tag_type'
  colnames(data_frame)[6]  <- 'vem_tag_serial'
  colnames(data_frame)[7]  <- 'vem_tag_id'
  colnames(data_frame)[8]  <- 'fork_length(cm)'
  colnames(data_frame)[9]  <- 'precaudal_length(cm)'
  colnames(data_frame)[10] <- 'cohort'
  colnames(data_frame)[11] <- 'area_of_capture'
  colnames(data_frame)[12] <- 'depth_of_capture'
  colnames(data_frame)[13] <- 'lat_deg'
  colnames(data_frame)[14] <- 'lat_min'
  colnames(data_frame)[15] <- 'lon_deg'
  colnames(data_frame)[16] <- 'lon_min'
  colnames(data_frame)[17] <- 'lat'
  colnames(data_frame)[18] <- 'lon'
  colnames(data_frame)[19] <- 'stomach_everted'
  colnames(data_frame)[20] <- 'eyes_popped'
  colnames(data_frame)[21] <- 'bladder_vented'
  colnames(data_frame)[22] <- 'point_of_incision'
  colnames(data_frame)[23] <- 'dna_clip'
  colnames(data_frame)[24] <- 'cannulation'
  colnames(data_frame)[25] <- 'sex'
  colnames(data_frame)[26] <- 'video'
  colnames(data_frame)[27] <- 'photo'
  colnames(data_frame)[28] <- 'photo_name'
  colnames(data_frame)[29] <- 'audio_log_file'
  colnames(data_frame)[30] <- 'dropshot'
  colnames(data_frame)[31] <- 'tissue_sample'
  colnames(data_frame)[32] <- 'gut_sample'
  colnames(data_frame)[33] <- 'tagger'
  colnames(data_frame)[34] <- 'notes'
  colnames(data_frame)[35] <- 'recaptured'
  colnames(data_frame)[36] <- 'detections'
  colnames(data_frame)[37] <- 'comments'
  return(data_frame)
}

clean_vue_lat_lon = function(vue_data_df, receiver_data_df){
  station = rep(NA, times = length(vue_data_df$datetime))
  for (i in 1:length(receiver_data_df$station_name)){
    receiver_subset_index = which(vue_data_df$receiver == receiver_data_df$vr2w_serial[i])
    deploy_subset_index = which(vue_data_df$datetime >= receiver_data_df$deployment_date[i])
    recover_subset_index = which(vue_data_df$datetime < na.omit(receiver_data_df$recovery_date[i]))
    ind = Reduce(intersect, list(receiver_subset_index, deploy_subset_index, recover_subset_index))
    vue_data_df$lat[ind] = receiver_data_df$lat[i]
    vue_data_df$lon[ind] = receiver_data_df$lon[i]
    station[ind] = as.character(receiver_data_df$station_name[i])
  }
  vue_data_df$station = as.factor(station)
  return(vue_data_df)
}

### remove_location -> Removing station location from vue dataframe
remove_location = function(vue_data, location_to_remove = FALSE){
  if (location_to_remove == FALSE){
    return (vue_data)
  }else{
    keep_data = vue_data[vue_data$station != location_to_remove, ]
    return (keep_data)}
}

clean_vue = function(vue_data, tag_ids = FALSE, exclude = FALSE){
  ##Function for removing all tags from a vue DB not explicitly kept by the tag_ids input.
  if (tag_ids[1] == FALSE){
    keep_data = vue_data
  }else{
    if (exclude == FALSE){
      keep_data = vue_data[vue_data$tag_id %in% tag_ids, ]
    } else if (exclude == TRUE){
      keep_data = vue_data[!(vue_data$tag_id %in% tag_ids), ]
    }
    keep_data$tag_id = as.factor(as.character(keep_data$tag_id))
  }
  return(keep_data)
}

clean_tags = function(tag_ids, vue_data){
  ## function to remove tags from tag id list that are not in vue database and report which tags those are
  keep_tags = tag_ids[tag_ids %in% vue_data$tag_id]
  print (paste('The Following Tags Were Not In the VUE Data:', as.character(tag_ids[!(tag_ids %in% vue_data$tag_id)]),sep = ' '))
  return(keep_tags)
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

get_recovery_rate = function(vue_data, 
                             start_date, 
                             end_date, 
                             tag_id, 
                             ping_interval){
  ## Function to determine recovery rate from a vue dataset
  vd_time = vue_data[vue_data$study_date > start_date & 
                       vue_data$study_date < end_date, ]
  vd_tag  = vd_time[as.numeric(as.character(vd_time$tag_id)) == tag_id, ]
  # subtract extra 1 because not including start or end dates. 
  # See Critereons for Analysis
  elapsed_days = (end_date - start_date) - 1 
  theoretical_pings_sent = (elapsed_days * 24 * 60 * 60) / ping_interval
  ind_recovery_rate = length(vd_tag$study_date) / theoretical_pings_sent
  recovered = c(ind_recovery_rate, theoretical_pings_sent, length(vd_tag$study_date))
  names(recovered) = c('recovery_rate', 'pings_sent', 'pings_recovered')
  return(recovered)
}

remove_detections_before_tagging = function(vue_data, tagging_data, ncores = 8){
  ## function to remove all tags not in tagging data as well as removing all detections of tags
  ## occurring before the tag is deployed. 
  ## This might occur when a tag was previously deployed for range testing purposes.
  tagging_data$vem_tag_id = as.numeric(as.character(tagging_data$vem_tag_id))
  vue_data$tag_id = as.numeric(as.character(vue_data$tag_id))
  vue_data.filtered = matrix()
  registerDoParallel(cores = ncores)
  vue_data.filtered = foreach(i = 1:length(which(is.na(tagging_data$vem_tag_id) == FALSE)), .combine = rbind) %dopar%{
    return(filter(vue_data, tag_id == tagging_data$vem_tag_id[is.na(tagging_data$vem_tag_id) == FALSE][i], datetime >= tagging_data$datetime[is.na(tagging_data$vem_tag_id) == FALSE][i]))
  }
  return(vue_data.filtered)
}

#### Use Functions -----------------
cluster_receivers = function(receiver_data, n_clusters = 5, lon = TRUE){
  if(lon == FALSE){
    fit = kmeans(receiver_data$lat, centers = n_clusters, iter.max = 100)
  }else{
  fit = kmeans(cbind(receiver_data$lat, receiver_data$lon), centers = n_clusters, iter.max = 100)
  }
}

get_graph = function(vue_df, 
                     tag_id = FALSE, 
                     time_period = FALSE,
                     igraph=TRUE,
                     binary=FALSE,
                     removeLoops=FALSE,
                     start=FALSE,
                     end=FALSE,
                     numstations = FALSE,
                     remove_zeros = TRUE){
  ### A function to create an adjacency matrix from a vue dataset
  ### Arguments: 
  ### vue_df = a dataframe containing a vue export with 
  ### columns for datetime, station, and tag_id. Imported
  ### with load_vemco function from 'R_Utility_Functions.R' 
  ### script
  ### tag_id = a tag id (or multiple ids) corrosponding to the
  ### unique transmitter number for a vemco tag (or tags)
  ### time_period = a list containing two POSIXct objects 
  ### corrosponding to the begining of the time for analysis
  ### and the up to but not including the end time for analysis
  
  ### TO DO: Add functionality to determine which stations had 
  ### receivers during a given time_period. Replace vue_df$station
  ### with these stations. 
  if (tag_id[1] == FALSE){ # if no tag ids specified, all tag ids used
    tag_id = unique(vue_df$tag_id)
  }
  if (time_period[1] == FALSE){ # if no dates specified, all dates used
    time_period = c(min(vue_df$datetime), max(vue_df$datetime))
  }else {
    if (start == FALSE) {
      start = min(vue_df$datetime)
    }
    if (end == FALSE) {
      end = max(vue_df$datetime)
    }
    vue_df = vue_df[which(vue_df$datetime >= start & vue_df$datetime <= end), ]
    }

  
  # Pulling out detections for a specific tag
  vue_df = clean_vue(vue_data, tag_id, exclude = FALSE)
  # Order vue_df by tag id
  vue_df = vue_df[order(vue_df$tag_id, vue_df$datetime), ]
  
  numstations = length(unique(vue_df$station))
  
  ## Assigning a station number
  vue_df$station_number = 0
  for(i in 1:length(unique(vue_df$station))){
    vue_df$station_number[vue_df$station == sort(as.character(unique(vue_df$station)))[i]] = i
  }
  
  # To Do: pull out receivers present during a specific time period
  
  # Build adjacency matrix, with receivers as nodes and fish 
  # movements as edges
  # Rows indicate movement from a receiver
  # Columns indicate movement to a receiver
  adj_matrix = matrix(0, numstations,numstations)
  # If station changes, increase adjacency matrix value by one

  for (i in 2:length(vue_df$station_number)){
    if(vue_df$tag_id[i] == vue_df$tag_id[i-1]){
      prevLoc = as.numeric(vue_df$station_number[i-1])
      newLoc = as.numeric(vue_df$station_number[i])
      if(binary) {
        adj_matrix[prevLoc, newLoc] = 1
      } else {
        adj_matrix[prevLoc, newLoc] =  adj_matrix[prevLoc, newLoc] + 1
      }
    }
  }
  
  if (removeLoops) {
    for (i in 1:numstations) {
      adj_matrix[i,i]=0
    }
  }
  #station_map = station_ids_map(vue_df)
  colnames(adj_matrix) = sort(unique(vue_df$station))
  rownames(adj_matrix) = sort(unique(vue_df$station))
  #colnames(adj_matrix) = station_map$'Station Name'
  #rownames(adj_matrix) = station_map$'Station Name'

if(remove_zeros == TRUE){
    adj_matrix = adj_matrix[which(rowSums(adj_matrix) != 0), ]
    adj_matrix = adj_matrix[ ,which(colSums(adj_matrix) != 0)]
  }

  if(igraph) {
    # Convert adjacency matrix into a graph object
    vemco_graph = graph.adjacency(adj_matrix, mode = 'directed', 
                                  weighted = TRUE)
    return (vemco_graph)
  }
  # Return graph object
  return(adj_matrix)   
} 


### Building a matrix of tags by receiver where numbers corrospond to the number 
### of detections for each tag at a given station
build_detection_matrix = function(vue_df){
  ## Outputs a matrix where each row corrosponds to a unique tag in the vue database 
  ## and each column corrosponds to a unique receiver. Values of each index are the number
  ## of detections for each tag at a particular location.
  detection_matrix = matrix(nrow = length(unique(as.numeric(as.character(vue_df$tag_id)))), 
                            ncol = length(unique(vue_df$station)))
  # dim(detection_matrix)
  for(i in 1:dim(detection_matrix)[1]){
    for(j in 1:dim(detection_matrix)[2]){
      detection_matrix[i,j] = dim(vue_df[which(vue_df$tag_id == unique(vue_df$tag_id)[i] & 
                                                 vue_df$station == unique(vue_df$station)[j]), ])[1]
    }
  }
  rownames(detection_matrix) = unique(as.character(vue_df$tag_id))
  colnames(detection_matrix) = unique(as.character(vue_df$station))
  return(detection_matrix)
}

## Determining days at liberty for each tag
calculate_time_at_liberty = function(vue_df){
  time_at_liberty_matrix =  matrix(ncol = 1, nrow = length(unique(vue_df$tag_id)))
  for(i in 1:length(unique(vue_df$tag_id))){
    indv_data = vue_df[vue_df$tag_id == unique(vue_df$tag_id)[i], ]
    time_at_liberty_matrix[i] = round(difftime(indv_data$datetime[length(indv_data$datetime)], indv_data$datetime[1], units = "days"), digits = 2)
  }
  return(time_at_liberty_matrix)
}

## Determining days between tagging and first apearance on array
calculate_days_before_detection = function(vue_df){
  days_before_detection =  c()
  for(i in 1:length(unique(vue_df$tag_id))){
    indv_data = vue_df[vue_df$tag_id == unique(vue_df$tag_id)[i], ]
    days_before_detection = c(days_before_detection, round(difftime(time1 = min(indv_data$datetime[which(indv_data$station != 'Tagging Location')]), time2 = indv_data$datetime[which(indv_data$station == 'Tagging Location')], units = "days"), digits = 2))
  }
  return(days_before_detection)
}

## Determing unique days detected
calculate_days_detected = function(vue_df){
  days_detected = matrix(ncol = 1, nrow = length(unique(vue_df$tag_id)))
  for(i in 1:length(unique(vue_df$tag_id))){
    indv_data = vue_df[vue_df$tag_id == unique(vue_df$tag_id)[i], ]
    days_detected[i] = length(unique(floor_date(indv_data$datetime, unit = 'day')))
  }
  return(days_detected)
}

#### Appending position and date record for each tagging
generate_tagging_detection = function(tagging_data, vue_data){
  ### generate_tagging_detection -> for each tag in dataset
  ###   adds a detection corrosponding to the tagging location data
  tagging_data$vem_tag_id = as.numeric(as.character(tagging_data$vem_tag_id))
  vue_data$tag_id = as.numeric(as.character(vue_data$tag_id))
  tag_ids = na.exclude(tagging_data$vem_tag_id[tagging_data$vem_tag_id %in% vue_data$tag_id])
  for (i in 1:length(tag_ids)){
    false_record     = vue_data[1, ]
    false_record[1]  = as.POSIXct(as.character(tagging_data$datetime[which(tagging_data$vem_tag_id == tag_ids[i])]))
    false_record[2]  = 0
    false_record[3]  = tag_ids[i]
    false_record[4]  = NA
    false_record[5]  = NA
    false_record[6]  = NA
    false_record[7]  = 1
    false_record[8]  = 'Tagging Location'
    false_record[9]  = convert_lat_lon(tagging_data$lat_deg[which(tagging_data$vem_tag_id == tag_ids[i])], tagging_data$lat_min[which(tagging_data$vem_tag_id == tag_ids[i])])
    false_record[10] = convert_lat_lon(tagging_data$lon_deg[which(tagging_data$vem_tag_id == tag_ids[i])], tagging_data$lon_min[which(tagging_data$vem_tag_id == tag_ids[i])])
    vue_data = rbind(false_record, vue_data)
  }
  # Ordering data by date, then tag, then receiver
  vue_data = vue_data[order(vue_data$datetime, vue_data$tag_id, vue_data$receiver), ]
  return (vue_data)
}

#### Assigning Study Date
generate_study_date = function(vue_df, start_date = FALSE){
  if(start_date == FALSE){
    start_date = floor_date(min(vue_df$datetime), unit = "day")
  }
  vue_df$study_date = as.numeric(difftime(vue_df$datetime, as.POSIXct(start_date), units = 'days'))
  return(vue_df)
}

#### Listing all stations a tag was detected on
list_stations_detected = function(vue_data){
  detected_list = list()
  detected_list$unique = list()
  detected_list$all = list()
  for(i in 1:length(unique(vue_data$tag_id))){
    indv_data = vue_data[vue_data$tag_id == unique(vue_data$tag_id)[i], ]
    detected_list$unique[i] = list(unique(indv_data$station))
    detected_char_vec = c(indv_data$station[1])
    for(n in 2:length(indv_data$station)){
      if(indv_data$station[n-1] != indv_data$station[n]){
        detected_char_vec = c(detected_char_vec, indv_data$station[n])
      }
      detected_list$all[i] = list(detected_char_vec)
    }
  }
  return(detected_list)
}


#### Function for determining area size of BRFAs
brfa_size = function(North, South, East, West, Additional_Point = FALSE){
  poly.mat = rbind(c(West,North), c(East, North), c(East, South), c(West, South))
  if(Additional_Point[1] != FALSE){
    poly.mat = rbind(poly.mat, c(Additional_Point[2], Additional_Point[1]))
  }
  brfa_area = areaPolygon(poly.mat) / 1000000
  return(brfa_area)
}

#### Removing false detections
## based on Pincock 2012
remove_false_detections = function(vue_data, transmitter_interval = 60, remove_detections = TRUE, ncores = 8){
  fun_timer = proc.time()
  tf_detection_index = c()
  registerDoParallel(cores = ncores)
  tf_detection_index = foreach(i = c(1:length(vue_data$datetime)), .combine = c) %dopar%{
    return(any(which(vue_data$tag_id[i] == vue_data$tag_id     & 
                       vue_data$receiver[i] == vue_data$receiver &
                       (abs(difftime(time1 = rep(vue_data$datetime[i], length(vue_data$datetime)),  
                                     time2 = vue_data$datetime, units = "secs")) <= (30 * transmitter_interval)) == TRUE) != i))
  }
  print(proc.time() - fun_timer) # Stop the clock
  if(remove_detections == TRUE){
    vue_data = vue_data[tf_detection_index == TRUE, ]
    return(vue_data)
  }else{
    return(tf_detection_index)
  }
  beep(2)
}

#### Creating a histogram of detections per day
tag_detection_histogram = function(vue_data, collate = FALSE, print = TRUE){
  if(collate == TRUE){
    if(print == TRUE){
      png('All Tags Daily Detections Histogram.png', height = 800, width = 1000)
    }
    par(mfrow = c(5, 5), mar=c(1,3,3,1), oma=c(0,0,2,0)) # setting up plotting grid
    hist(floor(vue_data$study_date), right = FALSE, breaks = 0:ceiling(max(vue_data$study_date)),
         xlab = 'Study Date', 
         ylab = 'Detections',
         main = 'All Tags')
    for(i in 1:length(unique(vue_data$tag_id))){
      indv_data = vue_data[vue_data$tag_id == unique(vue_data$tag_id)[i], ]
      hist(floor(indv_data$study_date), right = FALSE, breaks = 0:ceiling(max(vue_data$study_date)),
           main = unique(vue_data$tag_id)[i], # individual plot label
           xlab = 'Study Date', # x axis label
           ylab = 'Detections') # y axis label
      abline(v = indv_data$study_date[indv_data$station == 'Tagging Location'], col = 'red')
    }
    title("Daily Detections of Each Tag", outer=TRUE)
    dev.off()
  }else{
    if(print == TRUE){
      png('All Tags - Daily Histogram.png', height = 800, width = 1000)
    }
    par(mfrow = c(1, 1), mar=c(3,3,3,3), oma=c(2,2,2,2)) # setting up plotting grid
    hist(floor(vue_data$study_date), right = FALSE, breaks = 0:ceiling(max(vue_data$study_date)),
         xlab = 'Study Date', 
         ylab = 'Detections',
         main = 'All Tags')
    dev.off()
    for(i in 1:length(unique(vue_data$tag_id))){
      if(print == TRUE){
        png(paste(unique(vue_data$tag_id)[i], 'Daily Histogram.png'), height = 800, width = 1000)
      }
      par(mfrow = c(1, 1), mar=c(3,3,3,3), oma=c(2,2,2,2)) # setting up plotting grid
      indv_data = vue_data[vue_data$tag_id == unique(vue_data$tag_id)[i], ]
      hist(floor(indv_data$study_date), right = TRUE, breaks = 0:ceiling(max(vue_data$study_date)),
           main = unique(vue_data$tag_id)[i], # individual plot label
           xlab = 'Study Date', # x axis label
           ylab = 'Detections', # y axis label
           col = 'black') # fills in histogram bars
      abline(v = floor(indv_data$study_date[indv_data$station == 'Tagging Location'])-1, col = 'red')
      dev.off()
    }
  }
}

#### Plotting Map of Receivers in Study
plot_receiver_map = function(receiver_data, filename = FALSE, region = 'Makapuu', rec_col = FALSE){
  ### Plots map of receiver coordinates
  ## Use rec_col = 'station' to map stations to colors
  ## Available regions include: 'Makapuu" (default), and "Oahu"
  if(exists('filename') == TRUE){ 
    filename = paste(filename, '.png', sep = "")
    png(filename)}
  if(exists('bathymetry') == FALSE){
    if(region == 'Makapuu'){
      bathymetry = getNOAA.bathy(lon1 = -157.8, 
                                 lon2 = -157.5, 
                                 lat1 = 21.2, 
                                 lat2 = 21.5,
                                 resolution = .75)
    }else if(region == 'Oahu'){
      bathymetry = getNOAA.bathy(lon1 = -158.5, 
                                 lon2 = -157.5, 
                                 lat1 = 21.18, 
                                 lat2 = 21.82,
                                 resolution = 1)
    }else if(region == 'Oahu and Penguin Banks'){
      bathymetry = getNOAA.bathy(lon1 = -158.5, 
                                 lon2 = -157, 
                                 lat1 = 20.75, 
                                 lat2 = 21.82,
                                 resolution = 1)
    }
  }
  ## Importing Zissou Color palette
  zissou = load_zissou_palette()
  ## Plotting basemap
  plot.bathy(bathymetry, land = TRUE, image=TRUE, bpal = list(c(-400, -1, zissou$blue)), deepest.isobath = c(-10000), shallowest.isobath = c(0), step = c(100), drawlabels = TRUE)
  ## Adding scale legend
  scaleBathy(bathymetry, deg = .48, cex = .5)
  #scaleBathy(bathymetry, deg = .48, cex = .5)
  ## Adding receiver locations
  if(exists('rec_col') == FALSE){
    rec_col = 'red'
  }
  receiver_plot_colors = rec_col
  if(class(rec_col) == 'list'){
    color_palette = rec_col
    receiver_plot_colors = rep('black', length(receiver_data$station_name))
    for(i in 1:length(receiver_data$lat)){
      if(receiver_data$station_name[i] %in% color_palette$station){
        receiver_plot_colors[i] = color_palette$colors[which(color_palette$station == receiver_data$station_name[i])]
      }
    }
  }
  points(lat~lon, data = receiver_data, pch = 19, col = receiver_plot_colors,cex = 1)
  #text(lat~lon, data = receiver_data, labels = receiver_data$number)
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
  
  lines(lat~lon, data = brfa_e, pch = 19, col = 'purple', lwd = 3, cex = .6)
  lines(lat~lon, data = brfa_f, pch = 19, col = 'purple', cex = .6)
  dev.off()
  return(bathymetry)
}

plot_depths = function(vue_data, individual_tags = FALSE, print = TRUE){
  depth_data = vue_data[is.na(vue_data$depth) == FALSE, ]
  if(dim(depth_data)[1]>0){
  if(individual_tags == TRUE){
    for(i in 1:length(unique(depth_data$tag_id))){
      indv_data = depth_data[depth_data$tag_id == unique(depth_data$tag_id)[i], ]
      if(print == TRUE){
        png(paste(unique(depth_data$tag_id)[i], 'Depth History.png', sep = ' '))
      }
      par(mfrow = c(2, 1), mar=c(3,3,3,3), oma=c(2,2,2,2)) # setting up plotting grid
      plot(indv_data$depth ~ indv_data$study_date, type = 'l',
           ylim = c(300, 0),
           xlim = c(min(indv_data$study_date), max(indv_data$study_date)),
           xlab = 'Study Date',
           ylab = 'Depth (m)',
           main = 'Depth History Across Study')
      points(indv_data$depth ~ indv_data$study_date, pch = 19)
      plot(indv_data$depth ~ hour(indv_data$datetime),
           ylim = c(300, 0),
           xlim = c(0, 23),
           xlab = 'Hour',
           ylab = 'Depth (m)',
           main = 'Hourly Depth Distribution')
      title(paste(unique(depth_data$tag_id)[i], 'Depth History'), outer = TRUE)
      dev.off()
    } 
  }else if(individual_tags == FALSE){
    if(print == TRUE){
      png('All Tags Depth History.png')
    }
    par(mfrow = c(2, 1),  mar=c(3,3,3,3), oma=c(2,2,2,2)) # setting up plotting grid
    plot(depth_data$depth ~ depth_data$study_date, type = 'l',
         ylim = c(300, 0),
         xlim = c(min(vue_data$study_date), max(vue_data$study_date)),
         xlab = 'Study Date',
         ylab = 'Depth (m)',
         main = 'Depth History Across Study')
    points(depth_data$depth ~ depth_data$study_date, pch = 19)
    plot(depth_data$depth ~ hour(depth_data$datetime),
         ylim = c(300, 0),
         xlim = c(0, 23),
         xlab = 'Hour',
         ylab = 'Depth (m)',
         main = 'Hourly Depth Distribution')
    title('All Tags Depth History', outer = TRUE)
    dev.off()
  }
}
}

#### Plotting Movement Maps for Each Fish
plot_movements = function(vue_data, receiver_data, region = 'Makapuu', tag_ids = FALSE, rec_col = FALSE, plot_title = FALSE){
  if (tag_ids[1] == FALSE){
    tag_ids = as.numeric(as.character(unique(vue_data$tag_id)))
  }
  for (id in tag_ids){
    indv_data = vue_data[vue_data$tag_id == id, ]
    
    ### Removing receivers not present while transmitter was active
    receiver_data_to_plot = receiver_data[which(receiver_data$deployment_date <= min(indv_data$datetime) &
                                                  (receiver_data$recovery_date >= min(indv_data$datetime) |
                                                     is.na(receiver_data$recovery_date) == TRUE)), ]
    receiver_data_to_plot$station_name = as.character(receiver_data_to_plot$station_name)
    
    if(plot_title == FALSE){
      plot_title = paste(id, 'Movement Map.png')
    }
    png(plot_title)
    
    if(exists('bathymetry') == FALSE){
      if(region == 'Makapuu'){
        bathymetry = getNOAA.bathy(lon1 = -157.8, 
                                   lon2 = -157.5, 
                                   lat1 = 21.2, 
                                   lat2 = 21.5,
                                   resolution = .75)
      }else if(region == 'Oahu'){
        bathymetry = getNOAA.bathy(lon1 = -158.5, 
                                   lon2 = -157.5, 
                                   lat1 = 21.18, 
                                   lat2 = 21.82,
                                   resolution = 1)
      }else if(region == 'Oahu and Penguin Banks'){
        bathymetry = getNOAA.bathy(lon1 = -158.5, 
                                   lon2 = -157, 
                                   lat1 = 20.75, 
                                   lat2 = 21.82,
                                   resolution = 1)
      }
    }
    
    ## Importing Zissou Color palette
    zissou = load_zissou_palette()
    ## Plotting basemap
    plot.bathy(bathymetry, land = TRUE, image=TRUE, bpal = gray.colors(10), deepest.isobath = c(-500), shallowest.isobath = c(0), step = c(100), drawlabels = TRUE)
    ## Adding scale legend
    scaleBathy(bathymetry, deg = .1, cex = .5)
    #scaleBathy(bathymetry, deg = .48, cex = .5)
    
    ## Adding receiver locations
    if(exists('rec_col') == FALSE){
      rec_col = 'red'
    }
    receiver_plot_colors = rec_col
    if(class(rec_col) == 'list'){
      color_palette = rec_col
      receiver_plot_colors = rep('black', length(receiver_data_to_plot$station_name))
      for(i in 1:length(receiver_data_to_plot$station_name)){
        if(receiver_data_to_plot$station_name[i] %in% color_palette$station){
          receiver_plot_colors[i] = color_palette$colors[which(color_palette$station == receiver_data_to_plot$station_name[i])]
        }
      }
    }
    ## Plotting fish movements
    lines(lat~lon, data = indv_data, col = 'blue', lty = 1, lwd = 2)
    # points(lat~lon, data = indv_data, col = 'blue',cex = 0.6, pch = 19)
    
    points(lat~lon, data = receiver_data_to_plot, pch = 19, col = receiver_plot_colors, cex = 1)
    
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
    dev.off()
    #dev.off()
  }
  return(bathymetry)
}

plot_tag_detections = function(vue_data, receiver_data, start_date = FALSE, end_date = FALSE, date_format = "%Y-%m-%d %H:%M:%S", plot_lost = FALSE, remove_station = FALSE, region = "Makapuu", tag_ids = FALSE, rec_col = station_palette, title = FALSE, plot_receivers = "all"){
  ### Types of plots (plot_receivers) to make include:
  # all = plots all receivers regardless of time
  # start = plots only receivers that were deployed at the begning of the time interval given by start_date
  # end = plots only receivers that were deployed at the end of the time interval given by end_date
  # study = plots all receivers that were deployed at some point during the time interval given by start_date and end_date
  # start tag = plots only receivers that were deployed at the time the animal was tagged
  # end tag = plots only receivers that were deployed at the last detection of a tag
  # study tag = plots all receivers that were deployed during the time the tag was out
  if(plot_lost == FALSE){
    receiver_data = receiver_data[receiver_data$recovered == "", ]
  } 
  receiver_data = receiver_data[is.na(receiver_data$deployment_date) == FALSE, ]
  if(start_date == FALSE){
    start_date = min(vue_data$datetime)
  }else{
    start_date = as.POSIXct(start_date, format = date_format)
  }
  if(end_date == FALSE){
    end_date = max(vue_data$datetime)
  }else{
    end_date = as.POSIXct(end_date, format = date_format)
  }
  for(i in 1:length(unique(vue_data$tag_id))){
    indv_data = vue_data[vue_data$tag_id == unique(vue_data$tag_id)[i], ]
    if(plot_receivers == "all"){
      plot_receiver_data = receiver_data
    }else if(plot_receivers == "start"){
      plot_receiver_data = receiver_data[which(receiver_data$deployment_date <= start_date & (receiver_data$recovery_date > start_date | is.na(receiver_data$recovery_date) == TRUE)), ]
    }else if(plot_receivers == "end"){
      plot_receiver_data = receiver_data[which(receiver_data$deployment_date < end_date & (receiver_data$recovery_date > end_date | is.na(receiver_data$recovery_date) == TRUE)), ]
    }else if(plot_receivers == "study"){
      plot_receiver_data = receiver_data[which(receiver_data$deployment_date < end_date & (receiver_data$recovery_date > start_date | is.na(receiver_data$recovery_date) == TRUE)), ]
    }else if(plot_receivers == "start tag"){
      plot_receiver_data = receiver_data[which(receiver_data$deployment_date <= min(indv_data$datetime) & (receiver_data$recovery_date > min(indv_data$datetime) | is.na(receiver_data$recovery_date) == TRUE)), ]
    }else if(plot_receivers == "end tag"){
      plot_receiver_data = receiver_data[which(receiver_data$deployment_date <= max(indv_data$datetime) & (receiver_data$recovery_date > max(indv_data$datetime) | is.na(receiver_data$recovery_date) == TRUE)), ]
    }else if(plot_receivers == "study tag"){
      plot_receiver_data = receiver_data[which(receiver_data$deployment_date < max(indv_data$datetime) & (receiver_data$recovery_date > min(indv_data$datetime) | is.na(receiver_data$recovery_date) == TRUE)), ]
    }
    plot_movements(vue_data = indv_data, receiver_data = plot_receiver_data, region = region, rec_col = rec_col)
  }
}

#### Calculating spatial evenness
spatial_evenness = function(vue_data, receiver_data){
  ### function to calculate spacitail eveness based on Pielou 1966 from TinHan 2014
  ## outputs a dataframe first column is tag id, second column is spatial evenness index
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
    #print(spatial_sum)
    spatial_evenness_df[i, 2] = (-1 * sum((spatial_sum[spatial_sum != 'NaN']))) / log(R)
  }
  return(spatial_evenness_df)
}

#### Determining days present vs. Absent
days_present = function(vue_data, print = TRUE, ncores = 8){
  emptyout = c()
  registerDoParallel(cores = ncores)
  emptyout = foreach(i = 1:length(unique(vue_data$tag_id))) %dopar% {
    indv_data = vue_data[vue_data$tag_id == unique(vue_data$tag_id)[i], ]
    dates_detected = floor(indv_data$study_date) - min(floor(indv_data$study_date)) + 1
    present_absent = rep(FALSE, range(dates_detected)[2]-range(dates_detected)[1]+1)
    present_absent[dates_detected] = TRUE
    present = c()
    absent = c()
    present_counter = 1
    absent_counter = 1
    if(length(present_absent) > 1){
      for(r in 2:length(present_absent)){
        if(present_absent[r-1] == present_absent[r] & present_absent[r-1] == TRUE){
          present_counter = present_counter + 1
        }else if(present_absent[r-1] != present_absent[r] & present_absent[r-1] == TRUE){
          present = c(present, present_counter)
          present_counter = 1
          absent_counter = 1
        }else if(present_absent[r-1] == present_absent[r] & present_absent[r-1] == FALSE){
          absent_counter = absent_counter + 1
        }else if(present_absent[r-1] != present_absent[r] & present_absent[r-1] == FALSE){
          absent = c(absent, absent_counter)
          absent_counter = 1
          present_counter = 1
        }
        if(r == length(present_absent)){
          present = c(present, present_counter)
        }
      }
    }else{
      present = 1
    }
    if(print == TRUE){
      png(paste(unique(vue_data$tag_id)[i], 'presence-absence histogram.png', sep = ' '))
    }
    par(mfrow = c(1, 2))
    hist(present, breaks = 300,
         xlim = c(0, 300),
         xlab = 'Days',
         main = 'Consecutive Days Present',
         col  = 'black')
    if(is.null(absent) == FALSE){
      hist(absent, breaks = 300,
           xlim = c(0, 300),
           xlab = 'Days',
           main = 'Consecutive Days Absent',
           col  = 'black')
    }
    dev.off()
  }
  # beep(3)
}

#### Converting opakapaka FL to weight based on Uchiyama and Kazama 2003
length_to_weight = function(FL, a = 0.000381465, b = 2.79567){
  # Based on the formula Weight = a*FL^b
  # Default coefficients for pooled sex opakapaka data
  weight = a * (FL^b)
  return(weight)
}

#### Getting distance (km) between two consecutive points 
# lldist <- function(point1, point2){
#   #The following program computes the distance on the surface of the earth 
#   # between two points point1 and point2 in KM
#   # Both the points are of the form (Longitude, Latitude)
#   #From: http://www.biostat.umn.edu/~sudiptob/Software/distonearth.R
#   R <- 6371
#   p1rad <- point1 * pi/180
#   p2rad <- point2 * pi/180
#   ## modified function by adding the following if statment. will no longer throw random error if location doesnt change. 
#   d = 0
#   if(p1rad[1] != p2rad[1] || p1rad[2] != p2rad[2]){
#     d <- sin(p1rad[2])*sin(p2rad[2])+cos(p1rad[2])*cos(p2rad[2])*cos(abs(p1rad[1]-p2rad[1]))
#   }
#   return(R*d)
# }

lldist = function(point1, point2){
  distance = distGeo(p1 = point1, p2 = point2) / 1000
  return(distance)
}

#### Distance each tag was tracked 
distance_tracked = function(vue_data){
  ## Function to measure the total distance in km between subsequent receivers
  ## that a fish was detected
  individual_distance = matrix(0, length(unique(vue_data$tag_id)), 1)
  for (i in 1:length(unique(vue_data$tag_id))){
    subset = vue_data[vue_data$tag_id == unique(vue_data$tag_id)[i],]
    for (a in 2:length(subset$lon)){
      individual_distance[i] = individual_distance[i] + lldist(point1 = c(subset$lon[a-1], subset$lat[a-1]), point2 = c(subset$lon[a], subset$lat[a]))
    }
    individual_distance[i] = round(individual_distance[i], digits = 2)
  }
  total_distance_tracked = sum(individual_distance)
  print (sprintf("All fish were tracked a collective distance of %s km", total_distance_tracked))
  return(individual_distance)
}

#### Generating histogram of detections for each fish
tagging_histogram = function(vue_data){
  for(i in 1:length(unique(vue_data$tag_id))){
    indv_data = vue_data[unique(vue_data$tag_id)[i], ]
    title = sprintf('%s Study Date Histogram.png', unique(vue_data$tag_id)[i])
    png(title)
    par(mfrow = c(1,1))
    plot_title = sprintf('Detections of Tag %s', unique(vue_data$tag_id)[i])
    indv_data = vue_data[vue_data$tag_id == unique(vue_data$tag_id)[i], ]
    hist(indv_data$study_date,  
         breaks = max(vue_data$study_date), 
         main = plot_title, xlab = 'Study Date', 
         ylab = 'Transmissions Detected', ylim = c(0,30), 
         xlim = c(0, max(indv_data$study_date)))
    abline(v = indv_data$study_date[indv_data$station == "Tagging Location"]-.5, col = 'red')
    for(n in 3:length(indv_data$station)){
      if(indv_data$station[n] != indv_data$station[n-1]){
        abline(v = indv_data$study_date[n]-.5, col = 'blue')
      }
    }
    dev.off()
  }
}

#### Assigning a unique color to each Station
assign_color_palette = function(vue_data){
  color_palette = list()
  color_palette$colors = rainbow(length(unique(vue_data$station)))
  color_palette$station = unique(vue_data$station)
  return(color_palette)
}

### Generating Stripchart of detections for all individuals
generate_stripchart = function(vue_data, color_palette = "black"){
  if(class(color_palette) != "list"){
    palette = list()
    palette$colors = rep(color_palette, length(unique(vue_data$station)))
    palette$station = unique(vue_data$station)
    color_palette = palette
  }else{
    color_palette = color_palette
  }
  for(i in 1:length(unique(vue_data$tag_id))){
    indv_data = vue_data[vue_data$tag_id == unique(vue_data$tag_id)[i], ]
    title = sprintf('%s Stripchart.png', unique(vue_data$tag_id)[i])
    png(title, width = 625, height = 150)
    par(mfrow = c(1,1))
    plot_title = sprintf('Detections of Tag %s', unique(vue_data$tag_id)[i])
    stripchart(indv_data$study_date[1], xlim = c(min(indv_data$study_date), max(indv_data$study_date)), main = plot_title)  
    changing_station_index = 1 # Index includes first detection since first detection is always "Tagging Location"
    for(n in 2:length(indv_data$study_date)){
      if(indv_data$station[n] != indv_data$station[n-1]){
        changing_station_index = c(changing_station_index, n)
      }
    }
    changing_station_index = c(changing_station_index, length(indv_data$study_date))
    for(k in 2:length(changing_station_index)){
      stripchart(indv_data$study_date[changing_station_index[k-1]:changing_station_index[k]], 
                 col = color_palette$colors[color_palette$station == indv_data$station[changing_station_index[k-1]]], 
                 pch = 19, add = TRUE)
    }
  dev.off()
  }
}

detection_stripchart = function(vue_data, tag_ids = FALSE, start_date = FALSE, end_date = FALSE, battery_life = 365){
  if(tag_ids[1] != FALSE){
    vue_data = vue_data[vue_data$tag_id %in% tag_ids, ]
  }
  if(start_date != FALSE){
    vue_data = vue_data[vue_data$datetime >= as.POSIXct(start_date), ]
  }
  if(end_date != FALSE){
    vue_data = vue_data[vue_data$datetime <= as.POSIXct(end_date), ]
  }
  sc_dates = floor(range(vue_data$study)[1]):ceiling(range(vue_data$study)[2])
  sc_tags = unique(vue_data$tag_id)
  all_tag_all_date_df = c()
  for(i in 1:length(sc_tags)){
    all_tag_all_date_df = rbind(all_tag_all_date_df, cbind(sc_tags[i], sc_dates))
  }
  
  vue_daily = vue_data
  vue_daily$study_date = floor(vue_daily$study_date)
  vue_daily$datetime = floor_date(vue_daily$datetime, unit = 'day')
  vue_daily = unique(vue_daily)
  setwd(figure_dir)
  png('Detection Stripchart.png')
  dev.new(width = 863, height = 1200)
  ## Create a stirpchart with horizontal lines for clairty
  stripchart(all_tag_all_date_df[,2] ~ all_tag_all_date_df[,1], pch = '-', cex = .5, xaxt = 'n', las = 2,  xlab = "")
  ## Adding dates detected
  stripchart(vue_daily$study_date ~ vue_daily$tag_id, pch = '|', add = TRUE, subset = vue_daily$station != "Tagging Location")
  ## Adding tagging date
  stripchart(vue_daily$study_date ~ vue_daily$tag_id,  pch = 19, add = TRUE, subset = vue_daily$station == "Tagging Location")
  ## Adding tag expiration date
  stripchart((vue_daily$study_date + battery_life) ~ vue_daily$tag_id,  pch = 1, add = TRUE, subset = vue_daily$station == "Tagging Location")
  date_labels = as.character(strptime(seq(min(vue_daily$datetime), max(vue_daily$datetime), length.out = 10), format = '%Y-%m-%d'))
  axis(side = 1, at = seq(min(vue_daily$study_date), max(vue_daily$study_date), length.out = 10), labels = date_labels, las = 2, cex = .25)
  dev.off()
}

### Movements in and out of BRFA
in_brfa_e = function(vue_data){
     (vue_data$lat > 21.28333333 & vue_data$lat < 21.4166666) & # BRFA E
     (vue_data$lon > -157.6833333 & vue_data$lon < -157.533333) # west and east
     }
in_brfa_f = function(vue_data){
     (vue_data$lat > 20.9166666 & vue_data$lat < 21.03333333) & # BRFA F # south and north
     (vue_data$lon > -157.566666 & vue_data$lon < -157.3666666) # west and east
     } 

brfa_movements = function(vue_data){
  # brfa_out_receivers = paste('Oahu - Makapuu BRFA ', c(1:5, 22:34), sep = "")
  brfa_crossings_in_to_out = matrix(data = 0, nrow = length(unique(vue_data$tag_id)), ncol = 1)
  brfa_crossings_out_to_in = matrix(data = 0, nrow = length(unique(vue_data$tag_id)), ncol = 1)
  time_tracked_in = matrix(data = 0, nrow = length(unique(vue_data$tag_id)), ncol = 1)
  time_tracked_out = matrix(data = 0, nrow = length(unique(vue_data$tag_id)), ncol = 1)
  vue_data$in_brfa = FALSE
  vue_data$in_brfa[in_brfa_e(vue_data)] = TRUE
  vue_data$in_brfa[in_brfa_f(vue_data)] = TRUE
  for(i in 1:length(unique(vue_data$tag_id))){
    indv_data = vue_data[vue_data$tag_id == unique(vue_data$tag_id)[i], ]
    if(indv_data$in_brfa[1] == FALSE){
      out_start_index = 1
      in_start_index = c()
    }else{
      out_start_index = c()
      in_start_index = 1
    }
    for(k in 2:length(indv_data$in_brfa)){
      if(indv_data$in_brfa[k] == TRUE & indv_data$in_brfa[k-1] == FALSE){ # if it was in and is now out
        brfa_crossings_in_to_out[i] = brfa_crossings_in_to_out[i] + 1 # moved in to out
        in_start_index = c(in_start_index, k) # note detection out
      }else if(indv_data$in_brfa[k] == FALSE & indv_data$in_brfa[k-1] == TRUE){ # if is now in and started out
        brfa_crossings_out_to_in[i] = brfa_crossings_out_to_in[i] + 1 # moved out to in
        out_start_index = c(out_start_index, k) # note detection in
      }
    }
      if(indv_data$in_brfa[length(indv_data$in_brfa)] == TRUE){ # if the last detection was in brfa
        in_start_index = c(in_start_index, length(indv_data$in_brfa)) #increment in
      }else if(indv_data$in_brfa[length(indv_data$in_brfa)] == FALSE){
        out_start_index = c(out_start_index, length(indv_data$in_brfa))
      }
    crossing_times = unique(sort(c(in_start_index, out_start_index)))
    time_vector = rep(0, length(crossing_times)-1)
    for(r in 2:length(crossing_times)){
      time_vector[r-1] = difftime(indv_data$datetime[crossing_times[r]], indv_data$datetime[crossing_times[r-1]], units = 'days')
    }
    #### Calculating time spent in and out of BRFAs
    time_tracked_1 = 0
    time_tracked_2 = 0
        for(c in 1:length(time_vector)){
          if(c%%2 == 1){ # if time vector position is odd (ie: time 1, 3, 5... n)
            time_tracked_1 = time_tracked_1 + time_vector[c]
          }else{
            time_tracked_2 = time_tracked_2 + time_vector[c]
          }
        }
        if(is.null(in_start_index[1]) == FALSE){
          if(in_start_index[1] == 1){
            time_tracked_in[i] = round(time_tracked_1, digits = 2)
            time_tracked_out[i] = round(time_tracked_2, digits = 2)
          }else{
            time_tracked_in[i] = round(time_tracked_2, digits = 2)
            time_tracked_out[i] = round(time_tracked_1, digits = 2)
        }
        }else{
          time_tracked_out[i] = round(time_tracked_1, digits = 2)
        }
  }
  brfa_crossings = as.data.frame(cbind(brfa_crossings_in_to_out, brfa_crossings_out_to_in, time_tracked_in, time_tracked_out))
  colnames(brfa_crossings) = c('in_to_out', 'out_to_in', 'time_tracked_in', 'time_tracked_out')
  return(brfa_crossings)
}

brfa_movements_by_time_at_liberty = function(brfa_movements, time_at_liberty){
  brfa_crossings_std = list()
  brfa_crossings_std$total = (brfa_movements[ ,1] + brfa_movements[ ,2]) / time_at_liberty
  brfa_crossings_std$mean = mean(brfa_crossings_std$total)
  brfa_crossings_std$stdev = sd(brfa_crossings_std$total)
  brfa_crossings_std$fivenum = fivenum(brfa_crossings_std$total)
  return(brfa_crossings_std)
}

n_movements = function(vue_data){
  movements = matrix(nrow = length(unique(vue_data$tag_id)), ncol = 1, data = 0)
  for(i in 1:length(unique(vue_data$tag_id))){
    indv_data = vue_data[vue_data$tag_id == unique(vue_data$tag_id)[i], ]
    if(length(indv_data$datetime) >= 2){
      for(r in 2:length(indv_data$datetime)){
        if(indv_data$station[r] != indv_data$station[r-1]){
          movements[i] = movements[i] + 1
        }
      }
    }
  }
  return(movements)
}

max_movement = function(vue_data){
  max_movement = matrix(ncol = 2, nrow = length(unique(vue_data$tag_id)), data = 0)
  for(i in 1:length(unique(vue_data$tag_id))){
    indv_data = vue_data[vue_data$tag_id == unique(vue_data$tag_id)[i], ]
    max_lat = c(indv_data$lon[which.max(indv_data$lat)], indv_data$lat[which.max(indv_data$lat)])
    max_lon = c(indv_data$lon[which.max(indv_data$lon)], indv_data$lat[which.max(indv_data$lon)])
    min_lat = c(indv_data$lon[which.min(indv_data$lat)], indv_data$lat[which.min(indv_data$lat)])
    min_lon = c(indv_data$lon[which.min(indv_data$lon)], indv_data$lat[which.min(indv_data$lon)])
    max_min_coordinates = rbind(max_lat, max_lon, min_lat, min_lon)
    if(dim(unique(max_min_coordinates))[1] > 2){
      # square km distance of polygon with conversion to km
      max_movement[i,1] = areaPolygon(max_min_coordinates) * 1*10^-6
    }
      # linear distance in km between two points with conversion to km
    max_linear = 0
    for(k in 1:dim(unique(max_min_coordinates))[1]){
      for(r in 1:dim(unique(max_min_coordinates))[1]){
        max_linear_dist = distGeo(p1 = unique(max_min_coordinates)[k, ], p2 = unique(max_min_coordinates)[r, ]) / 1000
        # print(max_linear_dist)
        if(max_linear_dist > max_linear){
          max_linear = max_linear_dist
        }
        max_movement[i,2] = max_linear
      }
    }
  }
  colnames(max_movement) = c('max polygon area', 'max linear area')
  return(max_movement)
}

get_fork_length = function(vue_data, tagging_data){
  fork_length = c()
  for(i in 1:length(unique(vue_data$tag_id))){
    fork_length = c(fork_length, as.character(tagging_data$"fork_length(cm)"[which(tagging_data$vem_tag_id == unique(vue_data$tag_id)[i])]))
  }
  return(fork_length)
}

get_tagging_date = function(vue_data, tagging_data){
  tagging_date = c()
  for(i in 1:length(unique(vue_data$tag_id))){
    tagging_date = c(tagging_date, as.character(tagging_data$datetime[which(tagging_data$vem_tag_id == unique(vue_data$tag_id)[i])]))
  }
  return(tagging_date)
}

## Remove entries with just tagging date
remove_only_tagging_date_entries = function(vue_data){
  rm_tags = c()
  for(i in 1:length(unique(vue_data$tag_id))){
    # print(c(i, unique(vue_data$tag_id)[i]))
    indv_data = vue_data[vue_data$tag_id == unique(vue_data$tag_id)[i], ]
    if(all(indv_data$station == 'Tagging Location')){
      rm_tags = c(rm_tags, indv_data$tag_id[1])
    }
  }
  return(vue_data[!(vue_data$tag_id %in% rm_tags), ])
}

distance_between_vue_receivers = function(vue_df){
  ### Pulling out all unique station?
  unique_stations = sort(unique(vue_df$station[vue_df$station != "Tagging Location"]))
  unique_lon = c()
  unique_lat = c()
  ### getting out associated lat and lon values
  for(i in 1:length(unique_stations)){
    unique_lon = c(unique_lon, vue_df$lon[vue_df$station == unique_stations[i]][1])
    unique_lat = c(unique_lat, vue_df$lat[vue_df$station == unique_stations[i]][1])
  }
  dist_matrix = matrix(data = 0, nrow = length(unique_stations), ncol = length(unique_stations))
  for(r in 1:length(unique_stations)){
    for(c in 1:length(unique_stations)){
      dist_matrix[r, c] = lldist(point1 = c(unique_lon[r], unique_lat[r]), point2 = c(unique_lon[c], unique_lat[c]))
    }
  }
  colnames(dist_matrix) = unique_stations
  rownames(dist_matrix) = unique_stations
  return(dist_matrix)
}  


distance_between_receivers = function(receiver_data, start_date = FALSE, end_date = FALSE, include_lost = FALSE){
  if(start_date != FALSE){
    receiver_data = receiver_data[receiver_data$deployment_date <= start_date, ]
  }
  if(end_date != FALSE){
    receiver_data = receiver_data[which(receiver_data$recovery_date >= end_date | is.na(receiver_data$recovery_date)), ]
  }else{
    receiver_data = receiver_data[which(receiver_data$recovery_date > start_date | is.na(receiver_data$recovery_date)), ]
  }
  if(include_lost == FALSE){
    receiver_data = receiver_data[which(receiver_data$recovered == ""), ]
  }
  distances_between_receivers = list()
  distances_between_receivers$matrix = matrix(0, length(receiver_data$lat), length(receiver_data$lat))
  for(i in 1:length(receiver_data$lat)){
    for (a in 1:length(receiver_data$lat)){
      if(a != i){ # Added because when i = 17 and a = 17, lldist was producing NaNs.
        distances_between_receivers$matrix[i, a] = lldist(point1 = c(receiver_data$lon[i], receiver_data$lat[i]), point2 = c(receiver_data$lon[a], receiver_data$lat[a]))
      }
    }
  }
  distances_between_receivers$mean = mean(distances_between_receivers$matrix[which(distances_between_receivers$matrix > 0 & is.na(distances_between_receivers$matrix)==FALSE)])
  distances_between_receivers$sd = sd(distances_between_receivers$matrix[which(distances_between_receivers$matrix > 0 & is.na(distances_between_receivers$matrix)==FALSE)])
  distances_between_receivers$iqr = fivenum(distances_between_receivers$matrix[which(distances_between_receivers$matrix > 0 & is.na(distances_between_receivers$matrix)==FALSE)])[c(2,4)]
  return(distances_between_receivers)
}


run_analysis = function(vue_data, tagging_data, start_date = FALSE, end_date = FALSE){
  vue_df = vue_data
  ### Adding in date and location of tagging
  movement_graph = get_graph(vue_df = vue_data, start = start_date, end = end_date, igraph = FALSE)
  receiver_distance_graph = distance_between_vue_receivers(vue_df = vue_data)
  #movement_graph_standardized_by_distance = (movement_graph * receiver_distance_graph)/max((movement_graph * receiver_distance_graph))
  vue_df = generate_tagging_detection(tagging_data = tagging_data, vue_data = vue_data)
  if(start_date != FALSE){
    # vue_data = filter(vue_data, datetime >= start_date)
    vue_df = vue_df[vue_df$datetime >= start_date, ]
  }
  if(end_date != FALSE){
    # vue_data = filter(vue_data, datetime <= end_date)
    vue_df = vue_df[vue_df$datetime <= end_date, ]
  }
  vue_df = remove_only_tagging_date_entries(vue_data = vue_df)
  ### Adding in study date - adjusts all date times relative to start of study
  vue_df = generate_study_date(vue_df) 
  ### Making detection matrix - Each row is a Tag ID, each station in a column. 
  ### matrix values are number of transmissions for a tag at a given station
  ## Note: Excludes tagging location
  tag_detection_matrix = build_detection_matrix(vue_df[vue_df$station != 'Tagging Location', ])  
  ### Determining how many times a single tag was detected
  ## Note: Excludes tagging location
  n_detections_by_tag = rowSums(tag_detection_matrix)  
  ## How many tags were detected at 2 or more stations?
  ## Note: Excludes tagging location
  n_stations_by_tag = rowSums(tag_detection_matrix / tag_detection_matrix, na.rm = TRUE)  
  ## How many unique days were tags detected?
  ## Note: Excludes tagging
  unique_days_detected = calculate_days_detected(vue_df[vue_df$station != 'Tagging Location', ])  
  ## How many days was an individual tracked?
  time_at_liberty = calculate_time_at_liberty(vue_df)  
  ## How many transmissions were detected per day on average?
  transmissions_per_day = list()
  # for entire time at liberty
  transmissions_per_day$at_liberty = n_detections_by_tag / time_at_liberty
  # only for days a tag was detected
  transmissions_per_day$days_detected = n_detections_by_tag / unique_days_detected  
  ## How many days after tracking until individual appears on array?
  days_before_detection = calculate_days_before_detection(vue_df)  
  ## What percentage of days at liberty was a tag detected?
  detected_by_liberty = calculate_days_detected(vue_df) / ceiling(calculate_time_at_liberty(vue_df))
  ## How far was a tag tracked?
  track_distance = distance_tracked(vue_df)
  ## How many times was a tag detected changing station?
  movements = n_movements(vue_df)
  ## What stations was a tag detected on?
  detected_stations = list_stations_detected(vue_df)
  #### Movements into / out of BRFA for each fish
  brfa_stats = brfa_movements(vue_df)
  #### Standardized movements
  brfa_movements_standardized_by_time_at_liberty = brfa_movements_by_time_at_liberty(brfa_stats, time_at_liberty)
  #### Number of fish tagged greater than L50 
  n_mature = length(which(as.numeric(as.character(tagging_data$"fork_length(cm)"[tagging_data$vem_tag_id %in% unique(vue_df$tag_id)])) >= 43))
  proportion_mature = round(n_mature / length(unique(vue_df$tag_id)), digits = 2)
  #### Homerange size
  homerange = max_movement(vue_df)
  #### Creating analysis list
  analysis_summary = list()
  analysis_summary$data = vue_df
  analysis_summary$tag_ids = unique(vue_df$tag_id)
  analysis_summary$tagging_date = get_tagging_date(vue_data = vue_data, tagging_data = tagging_data)
  analysis_summary$fork_length = get_fork_length(vue_data = vue_data, tagging_data = tagging_data)
  analysis_summary$tag_detection_matrix = tag_detection_matrix
  analysis_summary$n_detections_by_tag = n_detections_by_tag
  analysis_summary$n_stations_by_tag = n_stations_by_tag
  analysis_summary$unique_days_detected = unique_days_detected
  analysis_summary$time_at_liberty = time_at_liberty
  analysis_summary$transmissions_per_day = transmissions_per_day
  analysis_summary$days_before_detection = days_before_detection
  analysis_summary$percentage_of_days_at_liberty_detected = detected_by_liberty
  analysis_summary$track_distance = track_distance
  analysis_summary$movements = movements
  analysis_summary$detected_stations = detected_stations
  analysis_summary$brfa_stats = brfa_stats
  analysis_summary$n_mature = n_mature
  analysis_summary$proportion_mature = proportion_mature
  analysis_summary$homerange = homerange
  analysis_summary$brfa_movements_standardized_by_time_at_liberty = brfa_movements_standardized_by_time_at_liberty
  analysis_summary$movement_graph = movement_graph
  analysis_summary$receiver_distance_graph = receiver_distance_graph
  #analysis_summary$movement_graph_standardized_by_distance = movement_graph_standardized_by_distance
  return(analysis_summary)
}

generate_analysis_report = function(vue_data, tagging_data, start_date, end_date){
  analysis_summary = run_analysis(vue_data = vue_data, tagging_data = tagging_data, start_date = start_date, end_date = end_date)
  sink("analysis_output.txt")
  for(i in 1:length(unique(analysis_summary$data$tag_id))){
    cat(paste("Transmitter ID: ", unique(analysis_summary$data$tag_id)[i], "\n"))
    cat(paste("Species: ", tagging_data$species[which(tagging_data$vem_tag_id == unique(analysis_summary$data$tag_id)[i])], "\n"))
    cat(paste("Fork Length: ", tagging_data$"fork_length(cm)"[which(tagging_data$vem_tag_id == unique(analysis_summary$data$tag_id)[i])], " cm", "\n"))
    cat(paste("Tagging Date: ", tagging_data$datetime[which(tagging_data$vem_tag_id == unique(analysis_summary$data$tag_id)[i])], "\n"))
    cat(paste("Tagging Location: ", tagging_data$area_of_capture[which(tagging_data$vem_tag_id == unique(analysis_summary$data$tag_id)[i])], "\n"))  
    cat(paste("Day of First Detected: ", analysis_summary$data$datetime[which(analysis_summary$data$tag_id == unique(analysis_summary$data$tag_id)[i] & analysis_summary$data$station != "Tagging Location")][1], "\n"))
    cat(paste("Day of Last Detection: ", analysis_summary$data$datetime[which(analysis_summary$data$tag_id == unique(analysis_summary$data$tag_id)[i])][length(which(analysis_summary$data$tag_id == unique(analysis_summary$data$tag_id)[i] & analysis_summary$data$station != "Tagging Location"))], "\n"))
    cat(paste("Days at Liberty: ", ceiling(analysis_summary$time_at_liberty[i]), "\n"))
    cat(paste("Unique Days Detected: ", analysis_summary$unique_days_detected[i], "\n"))
    cat(paste("Number of Detections (Total): ", length(which(analysis_summary$data$tag_id == unique(analysis_summary$data$tag_id)[i] & analysis_summary$data$station != "Tagging Location")), "\n"))
    cat(paste("Number of Detections / Day at Liberty: ", analysis_summary$transmissions_per_day$at_liberty[i], "\n"))
    cat(paste("Number of Detections / Day Detected: ", analysis_summary$transmissions_per_day$days_detected[i], "\n"))
    cat(paste("Number of Receivers Detected (Total): ", analysis_summary$n_stations_by_tag[i], "\n"))
    cat(paste("Number of receivers Detected / Days at Liberty: ", analysis_summary$n_stations_by_tag[i] / analysis_summary$time_at_liberty[i], "\n"))
    cat(paste("Movements from inside to outside BRFA (Total): ", analysis_summary$brfa_stats[i,1], "\n"))
    cat(paste("Movements from inside to outside BRFA / Day: ", analysis_summary$brfa_stats[i,1] / analysis_summary$time_at_liberty[i], "\n"))
    cat(paste("Movements from outside to inside BRFA (Total): ", analysis_summary$brfa_stats[i,2], "\n"))
    cat(paste("Movements from outside to inside BRFA / Day: ", analysis_summary$brfa_stats[i,2]/analysis_summary$time_at_liberty[i], "\n"))
    cat(paste("Time in BRFA: ", analysis_summary$brfa_stats[i,3], "\n"))
    cat(paste("Time out of BRFA: ", analysis_summary$brfa_stats[i,4], "\n"))
    cat(paste("Approximate Distance Tracked (Total): ", analysis_summary$track_distance[i], "km","\n"))
    cat(paste("Approximate Distance Tracked / Day: ", analysis_summary$track_distance[i] / analysis_summary$time_at_liberty[i],"km", "\n")) 
    cat(paste("Unique Stations_Detected", analysis_summary$detected_stations$unique[i], "\n"))
    if(analysis_summary$homerange[i,1] != 0){
      cat(paste("Area of detected homerange", round(analysis_summary$homerange[i,1], digits = 2), 'km^2'))
    }else if(analysis_summary$homerange[i,2] != 0){
      cat(paste("Maximum linear Distance", round(analysis_summary$homerange[i,2], digits = 2), 'km'))
    }
    cat("\n")
    cat("\n")
  }
  sink()
  return(analysis_summary)
}

create_analysis_csv = function(vue_data, tagging_data = tagging_data, start_date, end_date){
  analysis_out = run_analysis(vue_data = vue_data, tagging_data = tagging_data, start_date = start_date, end_date = end_date)
  tag_id = as.vector(analysis_out$tag_id)
  tagging_date = analysis_out$tagging_date
  fork_length = analysis_out$fork_length
  time_at_liberty = analysis_out$time_at_liberty
  n_detections = analysis_out$n_detections
  percent_of_detections = analysis_out$n_detections/sum(analysis_out$n_detections)
  detections_per_day = n_detections/time_at_liberty
  unique_days_detected = analysis_out$unique_days_detected
  distance_tracked = analysis_out$track_distance
  distance_per_day = distance_tracked / time_at_liberty
  receivers_detected = analysis_out$n_stations_by_tag
  movements = analysis_out$movements
  movements_by_day = movements/time_at_liberty
  brfa_crossings = rowSums(analysis_out$brfa_stats[, 1:2])
  time_tracked_in = analysis_out$brfa_stats[ ,3]
  time_tracked_out = analysis_out$brfa_stats[ ,4]
  brfa_crossings_per_day = brfa_crossings/time_at_liberty
  homerange_polygon = analysis_out$homerange[ ,1]
  homerange_linear = analysis_out$homerange[ ,2]
  analysis_df = data.frame(cbind(tag_id, tagging_date, time_at_liberty,n_detections, percent_of_detections, detections_per_day, unique_days_detected, distance_tracked, distance_per_day, receivers_detected, movements, movements_by_day, brfa_crossings, time_tracked_in, time_tracked_out, brfa_crossings_per_day, homerange_polygon, homerange_linear))
  write.csv(analysis_df, file = "analysis.csv")
}

#### Minimum average size of fish to tag ---------------------
#### Determining size minimums for tagging paka based on McCleave and Stred, 1975, and Adams et al. 1998 
## (see Parrish et al 2015 for sources). Tag weight should not exceed 2% of fish weight
paka_lengths = 1:100 ## hypothetical size distribution between 1cm and 100cm FL
paka_weights.kg = length_to_weight(paka_lengths) ## Convert sizes to KG
paka_weights.g = paka_weights.kg*1000
paka_weights_.02.g = paka_weights.g*.02
v13_weight = 10.2024 # grams
v13p_weight = 12.7698 # grams
## have to add 1 to each of these as they are under, not over estimates. 
## figured this out by looking at values prior to absoluting them, but probably 
## theres an automated way
which.min(abs(paka_weights_.02.g - v13_weight)) 
# minimum size of fish tagged with V13 = 14cm
which.min(abs(paka_weights_.02.g - v13p_weight))
# minimum size of fish tagged with V13p = 15cm

#### Generating Plots -----------------------------------------------------------------------
plot_receiver_maps = function(receiver_data, plot_lost = FALSE, start_date = FALSE, end_date = FALSE, date_format = "%Y-%m-%d %H:%M:%S", daily = TRUE, remove_station = FALSE, region = "Makapuu", rec_col = "red", print = TRUE){
  if(plot_lost == FALSE){
    receiver_data = receiver_data[receiver_data$recovered == "",]
  }
  if(remove_station != FALSE){
    receiver_data = receiver_data[!(receiver_data$station_name %in% remove_station), ]
  }
  receiver_data = receiver_data[is.na(receiver_data$deployment_date) == FALSE, ]
  activity_dates = sort(unique(c(as.character(receiver_data$deployment_date[is.na(receiver_data$deployment_date) == FALSE]), as.character(receiver_data$recovery_date[is.na(receiver_data$recovery_date) == FALSE]))))
    if(start_date != FALSE){
      # start_date = as.POSIXct(start_date, format = date_format)
      # receiver_data = receiver_data[which(receiver_data$deployment_date <= start_date), ]
      activity_dates = c(activity_dates, as.character(start_date))
      activity_dates = activity_dates[as.POSIXct(activity_dates, format = "%Y-%m-%d %H:%M:%S") >= start_date]
    }
  if(end_date != FALSE){
    # end_date = as.POSIXct(end_date, format = date_format)
    # receiver_data = receiver_data[which(receiver_data$recovery_date > end_date | is.na(receiver_data$recovery_date)), ]
    activity_dates = c(activity_dates, as.character(end_date))
    activity_dates = activity_dates[as.POSIXct(activity_dates, format = "%Y-%m-%d %H:%M:%S") <= end_date]
  }
  activity_dates = as.POSIXct(sort(activity_dates), format = "%Y-%m-%d %H:%M:%S")
  if(daily == TRUE){
    activity_dates = unique(floor_date(activity_dates, unit = "day"))
  }
  for(i in 1:length(activity_dates)){
    receivers_to_plot = receiver_data[which(receiver_data$deployment_date <= activity_dates[i] & 
                                              (is.na(receiver_data$recovery_date) == TRUE | receiver_data$recovery_date > activity_dates[i])), ]
    plot_receiver_map(receivers_to_plot, filename = paste(activity_dates[i], 'Receiver Map - Current Stations - Red', sep = " "), rec_col = "red", region = region)
    # dev.off()
    plot_receiver_map(receivers_to_plot, filename = paste(activity_dates[i], 'Receiver Map - Current Stations - Station Colors', sep = " "), rec_col = rec_col, region = region)
    # dev.off()
  }
  plot_receiver_map(receiver_data, filename = paste(min(activity_dates), "-", max(activity_dates), 'Receiver Map - All Stations - Red', sep = " "), rec_col = 'red', region = region)
  # dev.off()
  plot_receiver_map(receiver_data, filename = paste(min(activity_dates), "-", max(activity_dates), 'Receiver Map - All Stations - Station Colors', sep = " "), rec_col = rec_col, region = region)
  # dev.off()
}

#### Creating day night plots
create_day_night_plot = function(vue_data, color_palette = FALSE, start_date = FALSE, end_date = FALSE, date_format = "%Y-%m-%d %H:%M:%S"){
  if(start_date == FALSE){
    start_date = min(vue_data$datetime)
  }else if(start_date != FALSE){
    start_date = as.POSIXct(start_date, date_format)
  }
  if(end_date == FALSE){
    end_date = max(vue_data$datetime)
  }else if(end_date != FALSE){
    end_date = as.POSIXct(end_date, date_format)
  }
  vue_data = vue_data[vue_data$datetime >= start_date, ]
  vue_data = vue_data[vue_data$datetime <= end_date, ]
  
  if(class(color_palette) == 'logical'){
    vue_data$plot_color = "black"
  }else if(class(color_palette$colors) == "character"){
    vue_data$plot_color = "black"
    for(i in 1:length(color_palette$station)){
      vue_data$plot_color[vue_data$station == color_palette$station[i]] = color_palette$color[i]
    }
  }else{
    vue_data$plot_color = color_palette
  }
  
  # Creating plot_date and plot_time columns
  vue_data$plot_date  = as.POSIXlt(as.character(vue_data$datetime),format = "%Y-%m-%d")
  vue_data$plot_time = as.numeric(format(as.POSIXlt(vue_data$datetime,format= "%H:%M:%S"),'%H'))+as.numeric(format(as.POSIXlt(vue_data$datetime,format= "%H:%M:%S"),'%M'))/60+as.numeric(format(as.POSIXlt(vue_data$datetime,format= "%H:%M:%S"),'%S'))/3600
  
  for(i in 1:length(unique(vue_data$tag_id))){
    ### Setting up graphics window
    title = paste(unique(vue_data$tag_id)[i], "Day Night Detection Plot.png")
    png(title)
    par(mfcol=c(1,1))
    
    ### Subsetting data
    indv_data = vue_data[vue_data$tag_id == unique(vue_data$tag_id)[i],]
    with(vue_data, plot(plot_date,plot_time,col=plot_color,main=unique(vue_data$tag_id)[i],pch=19,cex=1,ylim=c(0,24),type="n",xaxs="i",yaxs="i",xlab="Date", ylab="Time"))
    
     #if (nrow(indv_data)>1){
      
      sundate<-seq(min(vue_data$plot_date),max(vue_data$plot_date), by="day")
      
      sundateJ<-as.numeric(format(seq(min(vue_data$plot_date),max(vue_data$plot_date), by="day"),"%j"))
      
      
      
      sun<-suncalc(sundateJ,20.63229,-156.49693,UTC=TRUE)
      lines(sundate,sun$sunrise,col="326")
      lines(sundate,sun$sunset,col="326") 
      
      
      cord.x <- c(min(sundate),sundate,max(sundate)) 
      cord.y <- c(24, sun$sunset,24)
      polygon(cord.x,cord.y,col='grey')
      # with(indv_data, points(plot_date,plot_time,col=zone,pch=16,cex=.75))
      
      cord.x <- c(min(sundate),sundate,max(sundate)) 
      cord.y <- c(0, sun$sunrise,0)
      polygon(cord.x,cord.y,col='grey')
      with(indv_data, points(plot_date, plot_time, col = plot_color, pch = 19, cex = 1))
    dev.off()
  }
  }

#### Looking at receiver performance
## Detection history per receiver
plot_detections_by_receiver = function(vue_data){
  png('Detections By Receiver Stripchart.png')
stripchart(vue_data$study_date ~ as.factor(as.character(vue_data$receiver)), pch = 15,
           main = "Detections by Receiver")          
## Do detections have an hourly pattern?
hist(hour(vue_data$datetime), right = FALSE, breaks = 0:24, main = "Detections per Hour")
## Do detections have any observable daily patterns
hist(day(vue_data$datetime), right = False, breaks = min(vue_data$datetime:max(vue_data$datetime)))
hist(floor(vue_data$study_date), right = FALSE, breaks = 0:ceiling(max(vue_data$study_date)),
     main = 'Daily Detections at Each Receiver', xlab = 'Study Date', ylab = 'Detections')

par(mfrow = c(3, 4),oma=c(0,0,2,0)) # setting up plotting grid
for(i in 2:length(unique(vue_data$station))){
  indv_data = vue_data[vue_data$station == unique(vue_data$station)[i], ]
  hist(floor(indv_data$study_date), right = FALSE, breaks = 0:ceiling(max(vue_data$study_date)),
       main = unique(vue_data$station)[i], # individual plot label
       xlab = 'Study Date', # x axis label
       ylab = 'Detections') # y axis label
}
title("Detections Per Day", outer=TRUE)
dev.off()
}

#### History of detections
detection_stripcharts = function(vue_data, false_detection_index = FALSE, aggregate = FALSE){
  if(aggregate == TRUE){
## Looking at history of all detections
png('detection_stripchart_all.png')
par(mfrow = c(1, 1))
stripchart(vue_data$study_date ~ as.factor(vue_data$tag_id),
           main = "All Detections", las = 2)
dev.off()
png('detection_stripchart_valid.png')
par(mfrow = c(1,1))
## Looking at history of "Valid" detections (Not questionable)
stripchart(vue_data$study_date[false_detection_index == TRUE] ~ as.factor(vue_data$tag_id[false_detection_index == TRUE]),
           main = "Valid Detections", col = 'darkgreen', pch = 15, add = FALSE)
dev.off()
## Looking at history of just questionable detections
png('detection_stripchart_questionable.png')
stripchart(vue_data$study_date[false_detection_index == FALSE] ~ as.factor(vue_data$tag_id[false_detection_index == FALSE]),
           main = "Questionable Detections", col = 'darkred', cex = .5, pch = 16, add = TRUE)
  }else if(aggregate == FALSE){
    png('detection_stripchart_all.png')
    par(mfrow = c(1, 3))
    stripchart(vue_data$study_date ~ as.factor(vue_data$tag_id),
               main = "All Detections")
    ## Looking at history of "Valid" detections (Not questionable)
    stripchart(vue_data$study_date[false_detection_index == TRUE] ~ as.factor(vue_data$tag_id[false_detection_index == TRUE]),
               main = "Valid Detections", col = 'darkgreen', pch = 15, add = FALSE)
    ## Looking at history of just questionable detections
    stripchart(vue_data$study_date[false_detection_index == FALSE] ~ as.factor(vue_data$tag_id[false_detection_index == FALSE]),
               main = "Questionable Detections", col = 'darkred', cex = .5, pch = 16, add = TRUE)
  }
  dev.off()
}

make_detection_plot = function(vue_data){
  png('detection_stripchart.png')
  dev.new(width = 863, height = 1000)
  date_labels = as.character(strptime(seq(min(vue_data$datetime), max(vue_data$datetime), length.out = 10), format = '%Y-%m-%d'))
  par(mfrow = c(2,1))
  stripchart(vue_data$study_date ~ as.factor(vue_data$tag_id), xaxt = 'n', las = 2,  xlab = "")
  axis(side = 1, at = seq(min(vue_data$study_date), max(vue_data$study_date), length.out = 10), labels = date_labels, las = 2, cex = .25)
  dev.off()
}

generate_gif_images = function(vue_data, receiver_data = receiver_data, tag_ids = FALSE, start_date = FALSE, end_date = FALSE, individuals = TRUE, region = 'Oahu and Penguin Banks'){
  if(tag_ids[1] != FALSE){
    vue_data = vue_data[vue_data$tag_id %in% tag_ids, ]
  }
  if(start_date != FALSE){
    vue_data = vue_data[vue_data$datetime >= as.POSIXct(start_date), ]
  }
  if(end_date != FALSE){
    vue_data = vue_data[vue_data$datetime <= as.POSIXct(end_date), ]
  }
  if(exists('bathymetry') == FALSE){
    if(region == 'Makapuu'){
      bathymetry = getNOAA.bathy(lon1 = -157.8, 
                                 lon2 = -157.5, 
                                 lat1 = 21.2, 
                                 lat2 = 21.5,
                                 resolution = .75)
    }else if(region == 'Oahu'){
      bathymetry = getNOAA.bathy(lon1 = -158.5, 
                                 lon2 = -157.5, 
                                 lat1 = 21.18, 
                                 lat2 = 21.82,
                                 resolution = 1)
    }else if(region == 'Oahu and Penguin Banks'){
      bathymetry = getNOAA.bathy(lon1 = -158.5, 
                                 lon2 = -157, 
                                 lat1 = 20.75, 
                                 lat2 = 21.82,
                                 resolution = 1)
    }
  }
  parent_dir = getwd()
  for(i in 1:length(unique(vue_data$tag_id))){
    indv_data = vue_data[vue_data$tag_id == unique(vue_data$tag_id)[i], ]
    # creating a subdirectory to dump all images created in
    dir.name = paste(unique(vue_data$tag_id)[i], 'GIF image dump', sep = " ")
    gif.dir = create_save_directory(dir.name)
    setwd(gif.dir)
    # a range of dates to plot
    date_range = seq.POSIXt(from = floor_date(min(indv_data$datetime), unit = 'day'), to = ceiling_date(max(indv_data$datetime), unit = 'day'), by = "day")
    for(r in 1:length(date_range)){
      subset_receiver_data = receiver_data[which(receiver_data$deployment_date <= date_range[r] & receiver_data$recovery_date >= date_range[r]), ]
      subset_indv_data = indv_data[which(indv_data$datetime >= date_range[r] & indv_data$datetime <= (date_range[r] + 60*60*24)), ]
      png(paste(indv_data$tag_id[1], ' ', date_range[r], '.png', sep = ""), width = 718, height = 403)
      plot.bathy(bathymetry, land = TRUE, image=TRUE, deepest.isobath = c(-10000), shallowest.isobath = c(0), step = c(100), drawlabels = TRUE, main = paste('Tag ID: ',indv_data$tag_id[1], " Date: ", date_range[r], sep = ""))
      ## Plotting the BRFA Boundaries
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
      lines(lat~lon, data = brfa_e, pch = 19, col = 'purple', lwd = 3, cex = .6)
      lines(lat~lon, data = brfa_f, pch = 19, col = 'purple', cex = .6)
      ## Plotting receiver locations
      if(dim(subset_receiver_data)[1] != 0){
        points(lat~lon, data = subset_receiver_data, pch = 19, col = 'red', cex = 1)
        ## Plotting tag detections
        if(dim(subset_indv_data)[1] == 1){
          points(lat~lon, data = subset_indv_data, pch = 19, col = 'green', cex = 1)
        }else if(dim(subset_indv_data)[1] >1){
          points(lat~lon, data = subset_indv_data, pch = 19, col = 'green', cex = 1)
          lines(lat~lon, data = subset_indv_data, col = 'green', cex = 1)
        }
      }
      dev.off()
    }
    command = paste("convert -delay .5 -loop 1 *.png ", unique(vue_data$tag_id)[i], ".gif", sep = "")
    system(command)
    file.remove(list.files(pattern=".png"))
    setwd(parent_dir)
  }
}

plot_path_use = function(movement_graph, vue_df, region = 'Makapuu'){
  ## Clearing out loops if they remain
  n_stations = dim(movement_graph)
  rev_identity = diag(x = 2, nrow = n_stations, ncol = n_stations)
  rev_identity[rev_identity == 0] = 1;   rev_identity[rev_identity == 2] = 0
  movement_graph = (movement_graph * rev_identity) / max(movement_graph)
if(exists('bathymetry') == FALSE){
  if(region == 'Makapuu'){
    bathymetry = getNOAA.bathy(lon1 = -157.8, 
                               lon2 = -157.5, 
                               lat1 = 21.2, 
                               lat2 = 21.5,
                               resolution = .75)
  }else if(region == 'Oahu'){
    bathymetry = getNOAA.bathy(lon1 = -158.5, 
                               lon2 = -157.5, 
                               lat1 = 21.18, 
                               lat2 = 21.82,
                               resolution = 1)
  }else if(region == 'Oahu and Penguin Banks'){
    bathymetry = getNOAA.bathy(lon1 = -158.5, 
                               lon2 = -157, 
                               lat1 = 20.75, 
                               lat2 = 21.82,
                               resolution = 1)
  }
}
# setwd(figure_dir)
  # a range of dates to plot
    png('relative ingress and egress map.png', width = 850, height = 800)
    plot.bathy(bathymetry, bpal = FALSE, land = TRUE, image=TRUE, deepest.isobath = c(-10000), shallowest.isobath = c(0), step = c(100), drawlabels = TRUE, main = 'Relative Ingress/Egress Rates')
    ## Plotting the BRFA Boundaries
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
    lines(lat~lon, data = brfa_e, pch = 19, col = 'purple', lwd = 3, cex = .6)
    lines(lat~lon, data = brfa_f, pch = 19, col = 'purple', lwd = 3, cex = .6)
    ## Plotting receiver locations
    subset_receiver_data = receiver_data[which((receiver_data$recovery_date >= min(vue_df$datetime) | is.na(receiver_data$recovery_date)) & receiver_data$recovered == ""), ]
    if(dim(subset_receiver_data)[1] != 0){
      points(lat~lon, data = subset_receiver_data, pch = 19, col = 'red', cex = .25)
    }
### Getting lat lon indexes for receivers
    unique_stations = sort(unique(vue_df$station[vue_df$station != "Tagging Location"]))
    unique_lon = c()
    unique_lat = c()
    ### getting out associated lat and lon values
    for(i in 1:length(unique_stations)){
      unique_lon = c(unique_lon, vue_df$lon[vue_df$station == unique_stations[i]][1])
      unique_lat = c(unique_lat, vue_df$lat[vue_df$station == unique_stations[i]][1])
    }
    ### Plotting ingress rates
    scaled_graph = ((exp((movement_graph))^(1/2) - 1) * 9) ^ (1/2)
    scaled_graph[scaled_graph == Inf] = 0
    for(r in 1:length(unique_stations)){
      for(c in 1:length(unique_stations)){
        move_line = as.data.frame(rbind(c(unique_lon[r], unique_lat[r]), 
                                        c(unique_lon[c], unique_lat[c])))
        colnames(move_line) = c('lon', 'lat')
        lines(lat~lon, data = move_line, col = 'blue', lwd = scaled_graph[r,c])
      }
    }
    dev.off()
}


#### Plotting cluster analysis
plot_clusters = function(receiver_data, start_date = FALSE, end_date = FALSE, region = "Makapuu"){
    setwd(figure_dir)
    # if(tag_ids[1] != FALSE){
    #   vue_data = vue_data[vue_data$tag_id %in% tag_ids, ]
    # }
      subset_receiver_data = receiver_data
    if(start_date != FALSE){
      subset_receiver_data = receiver_data[which(subset_receiver_data$deployment_date <= as.POSIXct(start_date) &
                                            subset_receiver_data$recovery_date > as.POSIXct(start_date)), ]
    }
    if(end_date != FALSE){
      subset_receiver_data = receiver_data[which(subset_receiver_data$recovery_data >= as.POSIXct(end_date) &
                                            subset_receiver_data$deployment_date <= as.POSIXct(end_date)), ]
    }
    if(is.null(subset_receiver_data$cluster[1])){
      subset_receiver_data$cluster = cluster_receivers(subset_receiver_data, n_clusters = 5)$cluster
    }
    if(exists('bathymetry') == FALSE){
      if(region == 'Makapuu'){
        bathymetry = getNOAA.bathy(lon1 = -157.8, 
                                   lon2 = -157.5, 
                                   lat1 = 21.2, 
                                   lat2 = 21.5,
                                   resolution = .75)
      }else if(region == 'Oahu'){
        bathymetry = getNOAA.bathy(lon1 = -158.5, 
                                   lon2 = -157.5, 
                                   lat1 = 21.18, 
                                   lat2 = 21.82,
                                   resolution = 1)
      }else if(region == 'Oahu and Penguin Banks'){
        bathymetry = getNOAA.bathy(lon1 = -158.5, 
                                   lon2 = -157, 
                                   lat1 = 20.75, 
                                   lat2 = 21.82,
                                   resolution = 1)
      }
    }
    dir.name = 'clustered_receiver_map'
    dir.create(dir.name)
    setwd(dir.name)
    png('clustered_receiver_map.png', width = 718, height = 403)
    plot.bathy(bathymetry, land = TRUE, image=TRUE, deepest.isobath = c(-10000), shallowest.isobath = c(0), step = c(100), drawlabels = TRUE, main = 'Receiver Clusterings')
      points(lat~lon, data = subset_receiver_data, pch = 19, col = rainbow(length(unique(subset_receiver_data$cluster)))[subset_receiver_data$cluster], cex = 1)
    dev.off()
}

#### Plotting Map of Receivers in Study
plot_station_map = function(receiver_data, filename = 'receivers_deployed', date = "2016-12-08", region = 'Makapuu', color = 'black'){
  ### Plots map of receiver coordinates
  ## Removing receiver_data from stations not present on date
  if(date != FALSE){
    receiver_data = receiver_data[(receiver_data$deployment_date <= as.POSIXct(date) & (receiver_data$recovery_date >= as.POSIXct(date) | is.na(receiver_data$recovery_date))), ]
  }
  ## Removing any NA entries
  receiver_data = receiver_data[is.na(receiver_data$station) == FALSE,  ]
  ## Creating a list of station numbers from the station name
  receiver_data$station_number = NA
  for(i in 1:dim(receiver_data)[1]){
    receiver_data$station_number[i] = strsplit(as.character(receiver_data$station_name[i]), split = " ")[[1]][5]
  }
  
  ### Getting Bathymetry Region
  ## Available regions include: 'Makapuu" (default), and "Oahu"
  if(exists('filename') == TRUE){ 
    filename = paste(filename, '_', date, '.png', sep = "")
    png(filename, width = 620, height = 620)}
  if(exists('bathymetry') == FALSE){
    if(region == 'Makapuu'){
      bathymetry = getNOAA.bathy(lon1 = -157.8, 
                                 lon2 = -157.5, 
                                 lat1 = 21.2, 
                                 lat2 = 21.5,
                                 resolution = .75)
    }else if(region == 'Oahu'){
      bathymetry = getNOAA.bathy(lon1 = -158.5, 
                                 lon2 = -157.5, 
                                 lat1 = 21.18, 
                                 lat2 = 21.82,
                                 resolution = 1)
    }else if(region == 'Oahu and Penguin Banks'){
      bathymetry = getNOAA.bathy(lon1 = -158.5, 
                                 lon2 = -157, 
                                 lat1 = 20.75, 
                                 lat2 = 21.82,
                                 resolution = 1)
    }
  }
  ## Plotting basemap
  plot.bathy(bathymetry, land = TRUE, image=TRUE, bpal = list(c(-400, -400, 'blue')), deepest.isobath = c(-10000), shallowest.isobath = c(0), step = c(100), drawlabels = TRUE)
  ## Adding scale legend
  scaleBathy(bathymetry, deg = .048, cex = .5)
  #scaleBathy(bathymetry, deg = .48, cex = .5)
  ## Adding receiver locations
  text((lat-.005)~lon, data = receiver_data, labels = receiver_data$station_number, cex = .5)
  points(lat~lon, data = receiver_data, pch = 19, col = color, cex = 1)
  #text(lat~lon, data = receiver_data, labels = receiver_data$number)
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
  
  lines(lat~lon, data = brfa_e, pch = 19, col = 'red', lwd = 3, cex = .6)
  lines(lat~lon, data = brfa_f, pch = 19, col = 'red', cex = .6)
  dev.off()
  return(bathymetry)
}

####### Function to execute program  -------
run = function(description = "", vue_data, receiver_data, tagging_data, start_date = FALSE, end_date = FALSE, date_format = "%Y-%m-%d %H:%M%:S", tag_ids = FALSE, output_directory = FALSE, region = "Makapuu"){
  ## Creating individual directory
  run_time = Sys.time()
  setwd(results_dir)
  run.dir = create_save_directory(new_directory = paste(paste(run_time, description, sep = " - "), '/', sep = "")) # Creating new directory for run
  setwd(run.dir) # Creating new directory for run
  if(start_date != FALSE){ # Establishing start date
    start_date = as.POSIXct(start_date, format = date_format)
  }
  if(end_date != FALSE){ # Establishing end date
    end_date = as.POSIXct(end_date, format = date_format)
  }
  if(tag_ids[1] != FALSE){
    vue_data = vue_data[vue_data$tag_id %in% tag_ids, ]
  }
  
  #### Generating report
    report.dir = 'output/'
    setwd(create_save_directory(report.dir))
    #false_detection_index = remove_false_detections(vue_data, remove_detections = FALSE) # Removing false detections
    analysis_summary = generate_analysis_report(vue_data = vue_data, tagging_data = tagging_data, start_date = start_date, end_date = end_date)
    #analysis_summary$false_detection_index = false_detection_index
    create_analysis_csv(vue_data = analysis_summary$data, tagging_data = tagging_data, start_date = start_date, end_date = end_date)
    ### save analysis file
    save.image(file = "workspace_image_updated")

  #### Making Plots
    setwd(run.dir)
    fig.dir = create_save_directory('figures/')
    setwd(fig.dir)
    ## Assigning station colors
    station_palette = assign_color_palette(analysis_summary$data) # Assigning mapping colors
    ### Plotting receiver maps
    plot_receiver_maps(receiver_data = receiver_data, start_date = start_date, end_date = end_date, rec_col = station_palette, region = region)
    ### Plotting stripchart of all detections, output for pages
    # make_detection_plot(analysis_summary$data) ### Dev.off()???
    ### Plotting movements of each tag tags
    plot_tag_detections(analysis_summary$data, receiver_data, start_date = start_date, end_date = end_date, date_format = "%Y-%m-%d %H:%M:%S", plot_lost = FALSE, remove_station = FALSE, tag_ids = FALSE, rec_col = station_palette, plot_receivers = "study", region = region)
    ## plotting relative movements of all fish
    plot_path_use(movement_graph = analysis_summary$movement_graph, vue_df = analysis_summary$data, region = region)
    #### Detection stripchart for each tag with color corrosponding to stations
    generate_stripchart(vue_data = analysis_summary$data, color_palette = station_palette)
    create_day_night_plot(vue_data = analysis_summary$data, color_palette = station_palette)
    #### Detection histogram for each tag 
    tag_detection_histogram(vue_data = analysis_summary$data, collate = FALSE)
    tag_detection_histogram(vue_data = analysis_summary$data, collate = TRUE)
    png(paste(description, 'Detection Stripchart.png'))
    stripchart(analysis_summary$data$study_date ~ as.factor(analysis_summary$data$tag_id), xlab = NULL, ylab = NULL, las = 2)
    dev.off()
    # detection_stripcharts(vue_data = analysis_summary$data, false_detection_index = vue_df$false_detection_index, aggregate = TRUE)
    #### Plotting depths for tags with depth sensors
    plot_depths(vue_data = analysis_summary$data, individual_tags = TRUE, print = TRUE)
    plot_depths(vue_data = analysis_summary$data, individual_tags = FALSE, print = TRUE)
    generate_gif_images(vue_data = analysis_summary$data, receiver_data = receiver_data, region = region)
    setwd(project_dir) # resetting to main project directory 
    print('SUCCESS!')
    beep(8) # SUCCESS!
    return(analysis_summary)
}

#### USAGE----------

##### Analysis - Feb 2016 ----------------------------------------------------------
#### Importing Data Files -----
vue_data = load_vemco(filename = 'VUE_Export_6_Dec_2016.csv', filepath = data_dir)
receiver_data = load_receiver_data(filename = 'DEPLOYMENT_RECOVERY_LOG.csv', filepath = data_dir)
tagging_data = load_tagging_data(filename = 'Bottomfish_Tag_Master.csv', filepath = data_dir)
vue_data$tag_id = as.numeric(as.character(vue_data$tag_id))
vue_data$station = as.character(vue_data$station)
vue_data = clean_vue_data(vue_data = vue_data, receiver_data = receiver_data)
vue_data = vue_data[!(vue_data$station %in% "Steve and Brendan's Doughnut test"), ]
receiver_data = receiver_data[receiver_data$lat != 0, ]
tagging_data = tagging_data[is.na(tagging_data$vem_tag_id) == FALSE, ]
# tagging_data$"fork_length(cm)" 

#### Cleaning Data Files -----------------------------------
  ### Removing all tags not associated with study from vue
  tag_ids = c(898:927, 18249:18275, 57445:57466, 37965:37985)
  n_detections = list()
  n_detections$total = dim(vue_data)[1] # Total number of detections from all tags
#### Removing any detections not associated with the study or from tags previously deployed in range tests
  vue_data = remove_detections_before_tagging(vue_data, tagging_data)
#### Removing tags with issues
  dead_tags = c(37969, 57459)
  cross_tags = c(57451) # tags are at cross seamount. cannot find tags in hard copy logs. probably monchong tagged with BF tags.
  vue_data = vue_data[!(vue_data$tag_id %in% dead_tags), ]
  vue_data = vue_data[!(vue_data$tag_id %in% cross_tags), ]

#### Removing tags that aren't opakapaka
opakapaka_tag_ids = tagging_data$vem_tag_id[tagging_data$species == "Opakapaka"]
opakapaka_tag_ids = as.numeric(as.character(opakapaka_tag_ids[is.na(opakapaka_tag_ids) == FALSE]))

vue_data = vue_data[vue_data$tag_id %in% opakapaka_tag_ids, ]

station_palette = assign_color_palette(vue_data)

## Filtering for false detections 
vue_data$false_detection_index = remove_false_detections(vue_data, remove_detections = FALSE)
#false_detections = vue_data[false_detection_index == FALSE, ]

save.image(file = "/Users/stephenscherrer/Google Drive/Weng Lab/Personal_Folders/Steve/dissertation work/Opakapaka Tagging/Opakapaka Tagging Analysis/workspace_image_updated")
send_push(user = 'uGEHvA4hr37tsrCCtpSv4sUUxVuTqN', message = "False Detection Filtering Complete!")
###############################################################################

load("/Users/stephenscherrer/Google Drive/Weng Lab/Personal_Folders/Steve/dissertation work/Opakapaka Tagging/Opakapaka Tagging Analysis/workspace_image_updated")
  
  
  
  
#### Producing results for first phase of study
  phase1 = run(vue_data = vue_data, description = "Phase I - All Receivers", receiver_data = receiver_data, tagging_data = tagging_data,
               start_date = as.POSIXct("2012-04-13"), 
               end_date = as.POSIXct("2014-12-06"),
               region = "Oahu and Penguin Banks")
  # for(i in 1:1000){dev.off()}
#### Producing results for second phase of study
  phase2 = run(vue_data = vue_data, description = "Phase II - All Receivers", receiver_data = receiver_data, tagging_data = tagging_data,
               start_date = as.POSIXct("2014-12-07"),
               end_date = FALSE,
               region = "Makapuu")
  # for(i in 1:1000){dev.off()}
#### Rerunning analysis without botcam data. For plotting movement maps 
#### and distance tracked statistics
  vue_data = vue_data[!(vue_data$station %in% "With BotCam Crew"), ]
  vue_data = vue_data[!(vue_data$station %in% "Cross 1"), ]
  
## Removing cross seamount from receiver_data
  receiver_data = clean_receiver_stations(receiver_data, region = c('Oahu', 'PB'), remove = FALSE)
#### Rerunning phase 1 analysis without botcam detections
  phase1 = run(vue_data = vue_data, description = "Phase I - For Data", receiver_data = receiver_data, tagging_data = tagging_data,
               start_date = as.POSIXct("2012-04-13"), 
               end_date = as.POSIXct("2014-12-06"),
               region = "Oahu and Penguin Banks")

#### Rerunning phase 2 analyis without botcam detections
  phase2 = run(vue_data = vue_data, description = "Phase II - For Data",  receiver_data = receiver_data, tagging_data = tagging_data,
               start_date = as.POSIXct("2014-12-07"),
               end_date = as.POSIXct("2016-03-10 00:00:00"),
               region = "Makapuu")
  
  all_data = run(vue_data = vue_data, description = "All Project Data", receiver_data = receiver_data, tagging_data = tagging_data,
               start_date = FALSE, 
               end_date = FALSE,
               region = "Oahu and Penguin Banks")

#### Minimum distance across BRFA E - across south border
  brfa_e_min_dim = lldist(point1 = c(-157.6833333, 21.28333333), point2 = c(-157.533333, 21.28333333))
  brfa_e_north_south = lldist(point1 = c(-157.533333, 21.4166666), point2 = c(-157.6833333, 21.28333333))
#### Minimum distance across BRFA F
  brfa_f_min_dim = lldist(point1 = c(-157.3666666, 20.9166666), point2 = c(-157.3666666, 21.03333333))
  brfa_f_east_west = lldist(point1 = c(-157.566666, 21.03333333), point2 = c(-157.3666666, 21.03333333))
#### Maximum homerange size
  homerange = round(max_movement(vue_data), digits = 2)
  # View(homeranges)





####

#### Determining mean distance and sd for receivers in phase 2
  receiver_distance_phase_2 = distance_between_receivers(receiver_data, start_date = as.POSIXct("2015-02-28 00:00:00"), 
                                                                        end_date = as.POSIXct("2015-03-01 00:00:00"), include_lost = FALSE)
  # mean = 34.58549, sd = 21.73923, IQR = 15.61947 53.91185

#### Determining mean distance and sd for receivers in phase 2.5
  receiver_distance_phase_2.5 = distance_between_receivers(receiver_data, start_date = as.POSIXct("2015-12-28 00:00:00"), 
                                                                          end_date = as.POSIXct("2015-12-29 00:00:00"), include_lost = FALSE)
  # mean = 10.41 km sd = 6.85 km, IQR = 4.899075, 15.394144

  receiver_distance_phase_1 = distance_between_receivers(receiver_data, start_date = as.POSIXct("2013-12-06 00:00:00"),
                                                                        end_date = as.POSIXct("2013-12-07 00:00:00"), include_lost = FALSE)
  # mean = 51.88947 km, sd = 27.32509 km, IQR = 32.73332 69.45658


#### phase 1 dates
start_date = as.POSIXct('2012-04-13 16:05:30 HST')
end_date = as.POSIXct("2013-12-07 00:00:00")

#### Phase 2.1 dates
start_date = as.POSIXct('2015-03-16 16:05:30 HST')
end_date = as.POSIXct('2015-05-25 00:00:00')

#### Phase 2.2 dates
start_date = as.POSIXct('2015-05-30 00:00:00')
end_date = as.POSIXct('2015-11-29 00:00:00')

#### Phase 2.3 dates
start_date = as.POSIXct('2015-11-29 00:00:00')
end_date = as.POSIXct("2016-01-08 00:00:00")

#### Notes on tags 37960 moves between pinicle south and the mound

#### Phase I 
p1_tags_good  = c(57358, 57457, 57455, 37960, 37940)
phase1_good = run(vue_data = vue_data, 
                  description = "Phase I - Good Tags", 
                  receiver_data = receiver_data, 
                  tagging_data = tagging_data,
                  tag_ids = p1_tags_good,
                            start_date = as.POSIXct("2012-04-13"), 
                            end_date = as.POSIXct("2014-12-06"),
                            region = "Oahu and Penguin Banks")

p1_tags_quest = c(p1_tags_good, 57464, 37975, 37961)
phase1_quest = run(vue_data = vue_data, 
                  description = "Phase I - Questionable Tags", 
                  receiver_data = receiver_data, 
                  tagging_data = tagging_data,
                  tag_ids = p1_tags_quest,
                  start_date = as.POSIXct("2012-04-13"), 
                  end_date = as.POSIXct("2014-12-06"),
                  region = "Oahu and Penguin Banks")





#### PHASE II 
phase2_all = run(vue_data = vue_data, 
                 description = "Phase II - Good Tags", 
                 receiver_data = receiver_data, 
                 tagging_data = tagging_data,
                 start_date = as.POSIXct('2016-01-01'), 
                 end_date = FALSE,
                 region = "Makapuu")


p2_tags_good  = c(18260, 18259, 18256, 18253, 18252, 18251, 18249, 916, 915, 909, 905, 902)
phase2_good = run(vue_data = vue_data, 
                   description = "Phase II - Good Tags", 
                   receiver_data = receiver_data, 
                   tagging_data = tagging_data,
                   tag_ids = p2_tags_good,
                   start_date = FALSE, 
                   end_date = FALSE,
                   region = "Makapuu")

p2_tags_quest = c(p2_tags_good, 18266, 18265, 18263, 18262, 18257, 18255, 18254, 18250, 921, 919, 918, 913, 912, 911, 910, 906, 903, 898)
phase2_quest = run(vue_data = vue_data, 
                  description = "Phase II - Questionable Tags", 
                  receiver_data = receiver_data, 
                  tagging_data = tagging_data,
                  tag_ids = p2_tags_quest,
                  start_date = FALSE, 
                  end_date = FALSE,
                  region = "Makapuu")
 #good_tracks_p2 = c(18249, 18251, 18253, 18259, 18260, 916, 915, 905, 909, 902, 18252, 18256)
 #quest_tracks_p2 = c(18250, 18252, 18254, 18255, 18256, 18257, 918, 919, 916, 910)

phase2$good = list()
phase2$quest = list()
phase2$good$data = phase2$data[phase2$data$tag_id %in% good_tracks_p2, ]
phase2$quest$data = phase2$data[phase2$data$tag_id %in% c(good_tracks_p2, quest_tracks_p2), ]

phase2$good$homerange = cbind(unique(vue_data$tag_id), homerange)
phase2$good$homerange = phase2$good$homerange[phase2$good$homerange [ ,1] %in% good_tracks_p2, ]
phase2$quest$homerange= cbind(unique(vue_data$tag_id), homerange)
phase2$quest$homerange = phase2$quest$homerange[phase2$quest$homerange [ ,1] %in% quest_tracks_p2, ]

 ### Tags with tracks that persist at least a week after tagging
phase2$good$tag_ids_detected_7days = c()
for(i in 1:length(unique(phase2$good$data$tag_id))){
  indv_data = phase2$good$data[phase2$good$data$tag_id == unique(phase2$good$data$tag_id)[i], ]
  if(abs(difftime(time1 = min(indv_data$datetime), time2 = max(indv_data$datetime), units = "days")) >= 7){
    phase2$good$tag_ids_detected_7days = c(phase2$good$tag_ids_detected_7days, unique(phase2$good$data$tag_id)[i])
  }
}
phase2$quest$tag_ids_detected_7days = c()
for(i in 1:length(unique(phase2$quest$data$tag_id))){
  indv_data = phase2$quest$data[phase2$quest$data$tag_id == unique(phase2$quest$data$tag_id)[i], ]
  if(abs(difftime(time1 = min(indv_data$datetime), time2 = max(indv_data$datetime), units = "days")) >= 7){
    phase2$quest$tag_ids_detected_7days = c(phase2$quest$tag_ids_detected_7days, unique(phase2$quest$data$tag_id)[i])
  }
}

## Tags with more than 7 days of detection data
phase2$good$tag_ids_detected_7unique = c()
for(i in 1:length(unique(phase2$good$data$tag_id))){
  indv_data = phase2$good$data[phase2$good$data$tag_id == unique(phase2$good$data$tag_id)[i], ]
   days = calculate_days_detected(indv_data[indv_data$station != 'Tagging Location', ]) 
  if(days >=7){
    phase2$good$tag_ids_detected_7unique = c(phase2$good$tag_ids_detected_7unique, c(unique(phase2$good$data$tag_id)[i], days[1]))
    }
}

phase2$quest$tag_ids_detected_7unique = c()
for(i in 1:length(unique(phase2$quest$data$tag_id))){
  indv_data = phase2$quest$data[phase2$quest$data$tag_id == unique(phase2$quest$data$tag_id)[i], ]
  days = calculate_days_detected(indv_data[indv_data$station != 'Tagging Location', ]) 
  if(days >=7){
    phase2$quest$tag_ids_detected_7unique = c(phase2$quest$tag_ids_detected_7unique, c(unique(phase2$quest$data$tag_id)[i], days[1]))
    }
}


#### track lengths for good and bad tags
phase2$good$track_distance = list()
phase2$quest$track_distance = list()
#phase2$good$track_distance = cbind(phase2$tag_ids, phase2$track_distance)
phase2$good$track_distance = cbind(good_tracks_p2, phase2$track_distance[phase2$tag_ids %in% good_tracks_p2, ])
range(phase2$good$track_distance[,2])
fivenum(phase2$good$track_distance[,2])
phase2$quest$track_distance = cbind(c(quest_tracks_p2, good_tracks_p2), phase2$track_distance[phase2$tag_ids %in% c(quest_tracks_p2, good_tracks_p2), ])

phase2$quest$track_distance$quest = cbind(c(good_tracks_p2, quest_tracks_p2), phase2$track_distance[cbind(phase2$tag_ids, phase2$track_distance[ ,1]) %in% c(good_tracks_p2, quest_tracks_p2), 1])
range(phase2$quest$track_distance$quest[,2])
fivenum(phase2$quest$track_distance$quest[,2])

## How many fish changed locations
n_stations = cbind(phase2$tag_id, phase2$n_stations_by_tag)
n_stations.good = n_stations[n_stations[ ,1] %in% good_tracks_p2, ]
range(n_stations.good[ ,2])
fivenum(n_stations.good[ ,2])
n_stations.quest = n_stations[n_stations[ ,1] %in% c(good_tracks_p2, questionable_tracks_p2), ]
range(n_stations.quest[ ,2])
fivenum(n_stations.quest[ ,2])


#### February 26 2016 - Analyizing data from BRFA 38, receiver recovered on this date from a fisherman off kokohead
b38 = filter(vue_data, station == "Oahu - Makapuu BRFA 38")
unique(b38$tag_id) # list of tags detected at this receiver
b38 = generate_study_date(b38)
stripchart(b38$study_date ~ b38$tag_id)
### Appears tag 905 is alive!!!
t905 = vue_data[vue_data$tag_id == 905, ]
plot(t905$depth ~ t905$study_date)

stripchart(all_data$data$study_date~all_data$data$tag_id, las = 2)

stripchart(all_data$data$study_date[all_data$data$station == "Tagging Location"]~all_data$data$tag_id[all_data$data$station == "Tagging Location"], col = "red", add = TRUE)
#### Saving Workspace Image ----------------------------------------------------------------
# save.image(file = "workspace_image")
beep(2)

vd = vue_data[vue_data$datetime > as.POSIXct("2014-12-07"), ]
vd = generate_tagging_detection(tagging_data, vue_data)
vd = vd[vd$datetime > as.POSIXct("2014-12-07"), ]
vd = generate_study_date(vd)
stripchart(vd$study_date~vd$tag_id, las = 2)
stripchart(vd$study_date[vd$station == "Tagging Location"]~vd$tag_id[vd$station == "Tagging Location"], col = "red", add = TRUE)







#### Analysis for barotrauma paper. Which fish were heard from >7 days after tagging?

 ### First run up to phase 2 data
  ## Tags presumed dead
  dead_fish_p2 = c(920, 908, 901, 900, 917, 914)
  dead_fish_p1 = c(37969, 57459, 57451)
  p2_mortality = merge(x = phase2$data, y = tagging_data[ ,c("datetime", "vem_tag_id")], by.x = "tag_id", by.y = "vem_tag_id")
  p2_mortality = p2_mortality[!(p2_mortality$tag_id %in% dead_fish_p2), ] # removing dead fish
  tags_with_7plus_days = c()
  for(i in 1:length(unique(p2_mortality$tag_id))){
    indv_data = p2_mortality[p2_mortality$tag_id == unique(p2_mortality$tag_id)[i], ]
    a = (difftime(max(indv_data$datetime.x), min(indv_data$datetime.x), units = 'day'))
      if(a > 7){
      tags_with_7plus_days = c(tags_with_7plus_days, indv_data$tag_id[1])
    }
  }

#### Analysis for CBOGS 2016
  #### Metrics to score:
    # 1. Recovery Rate
    # 2. Time detected after tagging
    # 3. Average number of receivers detected
    # 4. Days detected
    # 5. Pre Analysis of homerange size
    # 6. Movements into/out of BRFA
  setwd(paste(results_dir, '/cbogs2016/', sep = ""))
  dead_fish = c(dead_fish_p1, dead_fish_p2) # Tag IDS for known dead fish go here
  
  ### CBOGS 2016 # 1. Recovery Rate

  get_recovery_rates = function(vue_data, rm_tags = FALSE, start_date = FALSE, end_date = FALSE, calculate_by = 'pings'){
    #### calculate_by argument has 2 options
      #1. 'pings' - calculates end date based on last transmission received
      #2. 'battery' - calculates end date based on when battery set to expire
    
    if(rm_tags[1] != FALSE){
      vue_data = vue_data[!(vue_data$tag_id %in% rm_tags), ]
    }
    if(start_date != FALSE){
      vue_data = vue_data[vue_data$datetime > start_date, ]
    }
    if(end_date != FALSE){
      vue_data = vue_data[vue_data$datetime < end_date, ]
    }
  rec_rates = matrix(nrow = 0, ncol = 4)
  colnames(rec_rates) = c('tag_id', 'theoretical transmissions', 'observed transmissions', 'recovery rate')
  for(i in 1:length(unique(vue_data$tag_id))){
    indv_data = vue_data[(vue_data$tag_id == unique(vue_data$tag_id)[i]), ]
    if(unique(vue_data$tag_id)[i] >= 898 && unique(vue_data$tag_id)[i] <= 927){
      transmission_interval = (30 + 90)/2 # seconds between transmissions
      battery_life = 257 # days
    }else if(unique(vue_data$tag_id)[i] >= 18236 && unique(vue_data$tag_id)[i] <= 18275){
      transmission_interval = (30 + 90) / 2 #seconds between transmissions
      battery_life = 362 # days
    }else if(unique(vue_data$tag_id)[i] >= 37935 && unique(vue_data$tag_id)[i] <= 37985){
      transmission_interval = (110 + 250) / 2 # 180 seconds between transmissions
      battery_life = 539 # days
    }else if(unique(vue_data$tag_id)[i] >= 52142 && unique(vue_data$tag_id)[i] <= 52161){
      transmission_interval = (100 + 200) / 2
        battery_life = 450 # days
    }else if(unique(vue_data$tag_id)[i] >= 57371 && unique(vue_data$tag_id)[i] <= 57470){
      transmission_interval = (200 + 450) / 2 # 325 seconds
      battery_life = 450 # days
    }
    if(calculate_by == 'battery'){
      ## Date tag should theoretically expire
      indv_battery_expire = min(indv_data$datetime) + (24 * 60 * 60) * battery_life
      ## This next bit solves the problem if a tag is still active by only calculating until last download
      if(end_date[1] != FALSE){
        # if a tag expires after the end date
        if(indv_battery_expire >= end_date){
          # calculate based on the end date
          theoretical_pings_sent = as.numeric(abs(difftime(time1 = min(indv_data$datetime), time2 = end_date, units = 'secs'))) / transmission_interval
        }else{
          indv_data = indv_data[indv_data$datetime <= min(indv_data$datetime) + battery_life*24*60*60, ]
          theoretical_pings_sent = as.numeric(abs(difftime(time1 = min(indv_data$datetime), time2 = indv_battery_expire, units = 'secs'))) / transmission_interval
        }
      }
    }else if(calculate_by == 'pings'){
      theoretical_pings_sent = as.numeric(abs(difftime(time1 = min(indv_data$datetime), time2 = max(indv_data$datetime), units = 'secs'))) / transmission_interval
    }else{
      print('calculate_by was not supplied with a valid argument. Arguments included: "pings", and "battery"')
    }
    observed_pings = length(indv_data$datetime) - 1 ## Acounts for tagging date which is not actually a logged transmission
    recovery_rate = observed_pings / theoretical_pings_sent
    rec_rates = rbind(rec_rates, c(unique(vue_data$tag_id)[i], theoretical_pings_sent, observed_pings, recovery_rate))
  }
  return(rec_rates)
  }
  
  ## Running get_recovery_rates function
  p1_rr = get_recovery_rates(vue_data = phase1$data, rm_tags = dead_fish_p1, start_date = FALSE, end_date = FALSE, calculate_by = 'battery')
    fivenum(p1_rr[,'recovery rate'])
    #  3.865182e-06 1.546073e-05 3.865182e-05 7.034632e-04 2.127396e-02
  p2_rr = get_recovery_rates(vue_data = phase2$data, rm_tags = dead_fish_p2, start_date = FALSE, end_date = max(phase2$data$datetime), calculate_by = 'battery')
    fivenum(p2_rr[,'recovery rate'])
    # 1.938275e-05 1.708087e-04 1.099823e-03 1.421844e-02 4.235259e-01
  rr_combined = rbind(cbind(p1_rr, 'phase1'), cbind(p2_rr, 'phase2'))
  colnames(rr_combined) = c(colnames(p1_rr), 'phase')
  boxplot(as.numeric(rr_combined[,'recovery rate'])~rr_combined[,'phase'], ylim = c(0, 0.01))
  wilcox.test(x = as.numeric(p1_rr[ ,'recovery rate']), y = as.numeric(p2_rr[ ,'recovery rate']), paired = FALSE)
    # p-value = 0.004699
  
  
  ### CBOGS 2016 # 2. Time detected after tagging
  days_detected = function(vue_data, tagging_data, start_date = FALSE, end_date = FALSE, remove_tags = FALSE){
    if(start_date != FALSE){
      vue_data = vue_data[vue_data$datetime >= start_date, ]
    }
    if(end_date != FALSE){
      vue_data = vue_data[vue_data$datetime <= end_date, ]
    }
    tagging_data = tagging_data[is.na(tagging_data$vem_tag_id) == FALSE, ]
    if(remove_tags != FALSE){
      tagging_data = tagging_data[!(tagging_data$vem_tag_id %in% remove_tags), ]
    }
    #print(unique(vue_data$tag_id))
    # constructing a matrix to populate for making histograms
    days_tracked = c()
    all_days_tracked = c()
    # Making a list of all tag ids that have been put out before last receiver download
      tag_data = tagging_data[which(tagging_data$species == "Opakapaka"), ]
      tag_data = tag_data[which(tag_data$datetime >= min(vue_data$datetime)), ]
      tag_data = tag_data[which(tag_data$datetime <= max(vue_data$datetime)), ]
      all_tag_ids = as.numeric(levels(na.exclude(unique(tag_data$vem_tag_id))))
    # Looping through all tag ids to find the number of days a tag was detected
    for(i in 1:length(unique(all_tag_ids))){
      if(all_tag_ids[i] %in% vue_data$tag_id){
        indv_data = vue_data[vue_data$tag_id == all_tag_ids[i], ]
        track_length = as.numeric(ceiling(abs(difftime(min(indv_data$datetime), max(indv_data$datetime), units = "days"))))
        days_tracked = c(days_tracked, track_length)
        all_days_tracked = c(all_days_tracked, track_length:0)
      }else{
        days_tracked = c(days_tracked, 0)
        all_days_tracked = c(all_days_tracked, 0)
      }
      days_tracked_proportion = days_tracked / length(all_tag_ids) # Standardizing this value
      all_days_tracked_proportion = all_days_tracked / length(all_tag_ids)
    }
    track_times = list()
    track_times$days_tracked = days_tracked
    track_times$days_tracked_proportion = days_tracked_proportion
    track_times$all_days_tracked = all_days_tracked
    track_times$all_days_tracked_proportion = all_days_tracked_proportion
    return(track_times)
  }
  phase_1_days_detected = days_detected(vue_data = vue_data, tagging_data = tagging_data, start_date = as.POSIXct('2012-04-13 16:05:30 HST'), end_date = as.POSIXct("2014-12-07 00:00:00"), remove_tags = FALSE)
  phase_2_days_detected = days_detected(vue_data = vue_data, tagging_data = tagging_data, start_date = as.POSIXct('2015-03-16 16:05:30 HST'), end_date = FALSE, remove_tags = FALSE)

  
  hist.scaled = function(histogram_formula){
    hist_counts = hist(histogram_formula, plot = FALSE)$counts
    labels = paste(seq(0, 100, 10), '%', sep = "") # x axis labels in groups of 10 from 0 to 100 (%)
    hist(histogram_formula, ylim = c(0, max(hist_counts)), yaxt = 'n', ylab = '', xlab = 'Days Since Tagging')
    axis(side = 2, labels = labels, at = seq(0, max(hist_counts), length.out = length(labels)))
  }
  
  
  png('days_detected_comparison.png')
  par(mfrow = c(1, 2))
  hist.scaled(phase_1_days_detected$all_days_tracked[phase_1_days_detected$all_days_tracked < 100])
  hist.scaled(phase_2_days_detected$all_days_tracked[phase_2_days_detected$all_days_tracked < 100])
 # hist.scaled(phase_1_days_detected$days_tracked)
 # hist.scaled(phase_2_days_detected$days_tracked)
  dev.off()
  
  #### CBOGS 2016 #3. Total number of receivers detected
  stations_detected = function(vue_data, tagging_data, start_date = FALSE, end_date = FALSE, remove_tags = FALSE){  
    if(start_date != FALSE){
      vue_data = vue_data[vue_data$datetime >= start_date, ]
    }
    if(end_date != FALSE){
      vue_data = vue_data[vue_data$datetime <= end_date]
    }
    if(remove_tags[1] != FALSE){
      vue_data = vue_data[!(vue_data$tag_id %in% remove_tags), ]
    }
    stations_detected = c()
    for(i in 1:length(unique(vue_data$tag_id))){
      stations_detected = c(stations_detected, length(unique(vue_data$station[vue_data$tag_id == unique(vue_data$tag_id)[i]])) - 1) #subtract 1 to account for "tagging location"
    }
    return(stations_detected)
  }
  
  ## Running stations detected
    phase_1_stations_detected = stations_detected(vue_data = phase1$data, tagging_data = tagging_data, remove_tags = dead_fish_p1)
    fivenum(phase_1_stations_detected)
    phase_2_stations_detected = stations_detected(vue_data = phase2$data, tagging_data = tagging_data, remove_tags = dead_fish_p2)
    fivenum(phase_2_stations_detected)
    
     ## Plotting stations detected
    png('p1_p2_stations_detected_hist.png')
    par(mfrow = c(1, 2))  
    hist(phase_1_stations_detected)
    hist(phase_2_stations_detected)
    dev.off()
    
    png('p1_p2_stations_detected_boxplot.png')
    par(mfrow = c(1, 1))  
    stations_detected_for_boxplot = rbind(cbind(phase_1_stations_detected, 'phase 1'), cbind(phase_2_stations_detected, 'phase 2'))
    boxplot(as.numeric(stations_detected_for_boxplot[,1]) ~ stations_detected_for_boxplot[,2], main = "# Stations Detected")
    dev.off()
    wilcox.test(as.numeric(phase_1_stations_detected),  as.numeric(phase_2_stations_detected))
    
    
  #### CBOGS 2016 #4. Days Detected
  days_for_boxplot = rbind(cbind(phase1$unique_days, "Phase I"), cbind(phase2$unique_days, 'Phase II'))
  boxplot(as.numeric(days_for_boxplot[,1]) ~ days_for_boxplot[ ,2], main = 'Unique Days Detected')
  wilcox.test(x = as.numeric(phase1$unique_days[which(!(phase1$tag_ids %in% dead_fish_p1))]), y = as.numeric(phase2$unique_days[which(!(phase2$tag_ids %in% dead_fish_p2))]), paired = FALSE)
    fivenum(phase1$unique_days)
    fivenum(phase2$unique_days)
  hist(phase1$unique_days)
  hist(phase2$unique_days)  

  
  #### CBOGS 2016 #5. Pre Analysis of homerange size for phase 2 data
  ## Rerunning max_movement for phase 2. Now all tags get a maximum linear distance. with more than 3 locations get max polygon
  p2_homerange_size = max_movement(phase2_quest$data)
  ## Square rooting max polygon to get a linear dimension
  p2_homerange_size[ ,1] = sqrt(p2_homerange_size[ ,1])
  ## Determining if linear or polygon is greater for each tag
  p2_homerange_max = c()
  for(i in 1:dim(p2_homerange_size)[1]){
    p2_homerange_max = c(p2_homerange_max, max(p2_homerange_size[i, ]))
  }
  ## Removing dead fish from analysis
  p2_homerange_max = p2_homerange_max[!(phase2$tag_ids %in% dead_fish_p2) ]
  ## pasting together tag id and max homerange
  p2_max_homerange = cbind(phase2$tag_ids[!(phase2$tag_ids %in% dead_fish_p2) ],p2_homerange_max)
  fivenum(p2_max_homerange[,2])
  # 0.6056681  2.3533339  3.9627153  5.8499004 14.9273110
  boxplot(p2_max_homerange[,2], main = "Linear Home Range (km)")

  
  brfa_areas = c()
  
  # #### Size of BRFAs
  # # BRFA A
  brfa_areas = c(brfa_areas,
                 brfa_size(
  North = convert_lat_lon(21, 44),
  South = convert_lat_lon(21, 41) ,
  West = convert_lat_lon(-160, 39) ,
  East = convert_lat_lon(-160, 30) ))
  
  
  # # BRFA B
  brfa_areas = c(brfa_areas,
                 brfa_size(  
    North = convert_lat_lon(21, 50.41666),
    South = convert_lat_lon(21, 46.50) ,
    West  = convert_lat_lon(-160, 07.50),
    East  = convert_lat_lon(-160, 04.50)))
    
  # BRFA C 
  brfa_areas = c(brfa_areas,
                 brfa_size(
   North  = convert_lat_lon(21, 52.50),
   South  = convert_lat_lon( 21, 50.50),
   West  = convert_lat_lon(-159, 32),
   East  = convert_lat_lon(-159, 23)))
   
  # BRFA D
  brfa_areas = c(brfa_areas,
                 brfa_size(
   North  = convert_lat_lon( 21, 39),
   South  = convert_lat_lon( 21, 36.3),
   West  = convert_lat_lon(-158, 24),
   East  = convert_lat_lon(-158, 15),
   Additional_Point  = c(convert_lat_lon(21, 36), convert_lat_lon(-158, 24))))

  # BRFA E
  brfa_areas = c(brfa_areas,
                 brfa_size(
   North  = convert_lat_lon(21, 25),
   South  = convert_lat_lon(21, 17),
   West  = convert_lat_lon(-157, 41),
   East  = convert_lat_lon(-157, 32),
   Additional_Point  = c(convert_lat_lon(21, 23.63333), convert_lat_lon(-157, 41))))

  # BRFA F
  brfa_areas = c(brfa_areas,
                 brfa_size(
  North  = convert_lat_lon( 21, 02), 
  South  = convert_lat_lon(20, 55),
  West  = convert_lat_lon(-157, 34),
  East  = convert_lat_lon(-157, 22)))

  # BRFA G
  brfa_areas = c(brfa_areas,
                 brfa_size(
  North  = convert_lat_lon(21, 14), 
  South  = convert_lat_lon(21, 10),
  West  = convert_lat_lon(-156, 58),
  East  = convert_lat_lon(-156, 52)))

   # BRFA H
  brfa_areas = c(brfa_areas,
                 brfa_size(
   North  = convert_lat_lon(21, 07),
   South  = convert_lat_lon(21, 03),
   West  = convert_lat_lon(-156, 42),
   East  = convert_lat_lon(-156, 38)))
   
  # BRFA J
  brfa_areas = c(brfa_areas,
                 brfa_size(
  North  = convert_lat_lon(20, 55), 
  South  = convert_lat_lon(20, 47),
  West  = convert_lat_lon(-156, 08),
  East  = convert_lat_lon(-155, 59)))

  # BRFA K
  brfa_areas = c(brfa_areas,
                 brfa_size(
  North  = convert_lat_lon(20, 21),
  South  = convert_lat_lon(20, 16),
  West  = convert_lat_lon(-155, 53),
  East  = convert_lat_lon(-155, 25),
  Additional_Point = c(convert_lat_lon(20, 05.5),convert_lat_lon(-155,25))))

  # BRFA L
  brfa_areas = c(brfa_areas,
                 brfa_size(
  North  = convert_lat_lon(19, 44),
  South  = convert_lat_lon(19, 35),
  West  = convert_lat_lon(-154, 59.8333),
  East  = convert_lat_lon(-154, 54)))
  
  # BRFA M
  brfa_areas = c(brfa_areas,
                 brfa_size(
  North  = convert_lat_lon( 18, 57),
  South  = convert_lat_lon( 18, 51),
  West  = convert_lat_lon(-155, 41),
  East  = convert_lat_lon(-155, 37)))

  
  
  
  #### CBOGS 2016 #6. Pre Analysis of BRFA Movements for phase 2 data
  
  
#### Making Stripcharts of Detections
    make_detection_plot = function(vue_data){
      png('detection_stripchart_quest.png')
      dev.new(width = 863, height = 3000)
      date_labels = as.character(strptime(seq(min(vue_data$datetime), max(vue_data$datetime), length.out = 10), format = '%Y-%m-%d'))
      par(mfrow = c(2,1))
      stripchart(vue_data$study_date ~ as.factor(vue_data$tag_id), xaxt = 'n', las = 2,  xlab = "")
      axis(side = 1, at = seq(min(vue_data$study_date), max(vue_data$study_date), length.out = 10), labels = date_labels, las = 2, cex = .25)
      dev.off()
     }

    fivenum(sqrt(brfa_areas))
    
    linear_homerange_and_brfa = rbind(cbind(p2_max_homerange[,2], ' P. filamentosus'), cbind(sqrt(brfa_areas), 'BRFA'))
    
    
    boxplot(as.numeric(linear_homerange_and_brfa[ ,1]) ~ linear_homerange_and_brfa[ ,2], main = "Linear Homerange vs. sqrt(BRFA Areas)", ylab = "km")
    
    
    #### CBOGS 2016 #7. Pre Analysis of BRFA Movements in and out for phase 2 data
    boxplot(phase2$brfa_stats[!(phase2$tag_ids %in% dead_fish_p2) ,1] + phase2$brfa_stats[!(phase2$tag_ids %in% dead_fish_p2) ,2], main = "Movements Across BRFA Boundaries By Individual")
    
    
    
    setwd(project_dir)
  
    
  #### PreAnalysis for Kevin May 2016
    ### Plotting individual detections by study date
    setwd(figure_dir)
    dir.create('individual plots by station')
    setwd('individual plots by station')
    for(i in 1:length(unique(analysis_summary$tag_id))){
      indv_subset = vue_data$tag_id == unique(analysis_summary$tag_id)[i]
      indv_data = vue_data[indv_subset, ]
      png(paste(unique(vue_data$tag_id)[i],'detections by station.png', sep = " "))
      par(mar=c(8,12,1,1))
      date_labels = as.character(strptime(seq(min(indv_data$datetime), max(indv_data$datetime), length.out = 10), format = '%Y-%m-%d'))
      stripchart(vue_data$datetime ~ vue_data$station, xlab = '', ylab = '', xaxt = 'n', las = 2, subset = indv_subset, main = paste(unique(vue_data$tag_id)[i],'Detections by Station', sep = " "), cex = .5)      
      axis(side = 1, at = seq(min(indv_data$datetime), max(indv_data$datetime), length.out = 10), labels = date_labels, las = 2, cex = .5)
      ## Adding rtation name back to plot if tag only detected in one place
      if(length(unique(indv_data$station)) == 1){
        mtext(side=2, text=unique(indv_data$station), las=1, line=2)
      }
      dev.off()
    }
    


    
    
    
#### Additional summaries for Annual Progress Report August 2016
    ## Total Tagged fish and total detected
      # Phase 1
        p1_tagged = unique(tagging_data$vem_tag_id[which(tagging_data$species == "Opakapaka" & tagging_data$datetime <= "2013-12-07 00:00:00")])
        n_tagged_p1 = length(p1_tagged)
          # 52
        p1_detected = unique(vue_data$tag_id[vue_data$tag_id %in% p1_tagged])
        n_detected_p1 = length(p1_detected)
          # 16
        n_alive_p1 = 
        n_quest_p1 = 
      # Phase 2
        p2_tagged = unique(tagging_data$vem_tag_id[which(tagging_data$species == "Opakapaka" & tagging_data$datetime > "2013-12-07 00:00:00" & tagging_data$datetime < max(vue_data$datetime))])
        n_tagged_p2 = length(p2_tagged)
          # 59
        p2_detected = unique(vue_data$tag_id[vue_data$tag_id %in% p2_tagged])
        n_detected_p2 = length(p2_detected)
          # 56
        n_alive_p2 = 
        n_quest_p2 =
      # Total
        n_tagged_total = n_tagged_p1 + n_tagged_p2
          # 111
        n_detected_total = n_detected_p1 + n_detected_p2
        n_alive_total = 
        n_quest_total =
    ## Average Size of fish tagged
        # Phase 1 - All fish
        fivenum(tagging_data$`fork_length(cm)`[tagging_data$vem_tag_id %in% p1_tagged])
          # 30 40 44 49 65
        # Phase 1 - valid tracks
        # Phase 1 - valid + questionable tracks
        # Phase 2 - All fish
        fivenum(tagging_data$`fork_length(cm)`[tagging_data$vem_tag_id %in% p2_tagged])
          # 30 42 47 60 76
        # Phase 2 - valid tracks
        # Phase 2 - valid + questionable tracks
        
    ## Tagged since last report
        date_of_last_report = as.POSIXct('2015-09-01')
        n_tagged_since_last_report = length(tagging_data$vem_tag_id[which(tagging_data$species == 'Opakapaka' & tagging_data$datetime >= date_of_last_report)])
          # 47  
    ## Mortality summary
      ## Basis for mortality judgement
      ## Mortality vs. Size
      ## Mortality vs. order tagged
    ## Network Summary
      ## Receivers lost
    ## Analysis
      ## Movements across BRFA boundaries
        # Number of total movements
        # Number of total movements per fish per day
          # Aka Fish crossed every ___ Days
      ## Spatial Structure
        # Zone Cluster by Receiver Analysis
          # Clustering receivers
          phase2_receiver_data = receiver_data[na.omit(receiver_data$deployment_date > as.POSIXct('2015-12-06')), ]
          cluster_fit = cluster_receivers(phase2_receiver_data, lon = FALSE)
          cluster_lat_lon = cluster_fit$centers
          phase2_receiver_data$cluster = cluster_fit$cluster
          plot_clusters(phase2_receiver_data)
        # Depth of receivers where fish were detected
          # Changes with time?
        # Area tracked
          # Linear Distance
          # Convex Polygon
        # Network Cluster Analysis
      ## General Stats
        # Average number of days tracked (Five num)
        # Total distance tracked
        # Average number of receivers detected on
      
    
### Plotting network histories
  ## Three frame graphics
    # 1. map of network in place when serviced
    # 2. map of stations recovered
    # 3. map of network in place after servicing
  
plot_receiver_histories = function(vue_data, receiver_data,  plot_region = 'Makapuu'){

  if(exists('bathymetry') == FALSE){
    if(plot_region == 'Makapuu'){
      bathymetry = getNOAA.bathy(lon1 = -158, 
                                 lon2 = -157.5, 
                                 lat1 = 21.2, 
                                 lat2 = 21.6,
                                 resolution = 1)
    }else if(plot_region == 'Oahu'){
      bathymetry = getNOAA.bathy(lon1 = -158.5, 
                                 lon2 = -157.5, 
                                 lat1 = 21.18, 
                                 lat2 = 21.82,
                                 resolution = 1)
    }else if(plot_region == 'Oahu and Penguin Banks'){
      bathymetry = getNOAA.bathy(lon1 = -158.5, 
                                 lon2 = -157, 
                                 lat1 = 20.75, 
                                 lat2 = 21.82,
                                 resolution = 1)
    }
  }
  
  receiver_dates = (c(unique(receiver_data$deployment_date), unique(receiver_data$recovery_date)))
  receiver_dates = as.POSIXct(unique(as.Date(receiver_dates[is.na(receiver_dates) == FALSE])))
  receiver_date_labels = receiver_dates[receiver_dates >= min(vue_data$datetime)] 
  receiver_dates = receiver_dates[receiver_dates >= min(vue_data$datetime)] + 4 * 60*60*24
  receiver_data$recovered = as.character(receiver_data$recovered)
  for(i in 1:length(receiver_dates)){
    temp_receiver_data = receiver_data[which(receiver_data$deployment_date < receiver_dates[i] &
                                         (receiver_data$recovery_date >= receiver_dates[i] | is.na(receiver_data$recovery_date))) , ]  # &
                                            # receiver_data$recovered == ""), ]
  
  
  png(paste('Receiver Map ', as.character(as.Date(receiver_date_labels[i])),'.png', sep = "" ))
  par(mfrow = c(1, 1), oma=c(0,0,2,0), pty = 's')
  
  ## Making Prior to Service Plot
  plot.bathy(bathymetry, land = TRUE, image=TRUE, bpal = list(c(-400, -1, 'grey')), deepest.isobath = c(-10000), shallowest.isobath = c(0), step = c(100), drawlabels = TRUE)
  
  ## Adding BRFA Boundaries to plot
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
  lines(lat~lon, data = brfa_e, pch = 19, col = 'purple', lwd = 3, cex = .6)
  lines(lat~lon, data = brfa_f, pch = 19, col = 'purple', cex = .6)
  
  ## Adding receiver locations to plot
 
  points(lat~lon, data = temp_receiver_data[temp_receiver_data$recovered == "",], pch = 19, col = 'red',cex = 1)
  points(lat~lon, data = temp_receiver_data[temp_receiver_data$recovered != "",], pch = 19, col = 'blue',cex = 1)
  
  title(paste('Receiver Network ', as.character(as.Date(receiver_date_labels[i])), sep = ""), outer = TRUE)
  dev.off()
  }
}

download_cruise_dates = c('2015-09-15', '2016-01-08', '2016-03-07', '2016-05-28')

plot_receiver_histories(vue_data = phase2_good$data, receiver_data = receiver_data, plot_region = 'Makapuu')
          
run_time = (proc.time() - fun_timer) # Stop the clock
print(run_time)
send_push(user = 'uGEHvA4hr37tsrCCtpSv4sUUxVuTqN', message = paste("Put down that beer, your run is complete! Total Run Time:", run_time))
  

#### Analysis for Annual Progress Report 2016 ---------------

p2_tags_good  = c(18260, 18259, 18256, 18253, 18252, 18251, 18249, 916, 915, 909, 905, 902)
phase2_good = run(vue_data = vue_data, 
                  description = "Phase II - Good Tags", 
                  receiver_data = receiver_data, 
                  tagging_data = tagging_data,
                  tag_ids = p2_tags_good,
                  start_date = FALSE, 
                  end_date = FALSE,
                  region = "Makapuu")


analysis_summary = phase2_good

std_error = function(x){
  sqrt(var(x)/length(x))
  }
## Hypothesis 1: Bottomfish routinely move across the borders of existing BRFAs.
    ## Hypothesis 1 was tested by determining if location coordinates for each tag 
    ## detection occurred within our outside BRFA boundaries. Movement across BRFA 
    ## boundaries was said to occur when a tag was detected outside of BRFA boundaries 
    ## followed by a detection within BRFA boundaries, or vice versa. The number of 
    ## movements across BRFA boundaries was then standardized by the time at liberty, 
    ## the number of days elapsed between tagging and the final detection of a tag.
  
  ## Number of fish moving across BRFA boundaries
    tags_across_brfas = length(which((analysis_summary$brfa_stats$in_to_out + analysis_summary$brfa_stats$out_to_in) > 0))
    print(paste('There were ', tags_across_brfas, ' of ', length(analysis_summary$brfa_stats$in_to_out),' tags that were detected moving across BRFA boundaries' ))
  ## Percentage of all fish tagged with any detections detected moving across boundaries
    percent_across_brfas = tags_across_brfas / length(analysis_summary$brfa_stats$in_to_out)
    print(paste('This represents ', round(percent_across_brfas, digits = 3), ' of the tags detected on the network' ))
    
  ## Number of movements across BRFA boundaries
    movements_into_brfa = sum(analysis_summary$brfa_stats$out_to_in)
    movements_out_of_brfa = sum(analysis_summary$brfa_stats$in_to_out)
    total_brfa_movements = sum(movements_into_brfa, movements_out_of_brfa)
    print(paste('The total number of observed movements into and out of the brfa was', total_brfa_movements,'There was ', movements_into_brfa, ' observed movements made into the BRFA and ', movements_out_of_brfa, ' observed movements made from inside to the outside of the brfa.'))
    
  ## Number of movements per fish - Average, Standard Error
    mean_brfa_movements_per_fish = mean(analysis_summary$brfa_stats$out_to_in + analysis_summary$brfa_stats$in_to_out)
    se_brfa_movements_per_fish = std_error(analysis_summary$brfa_stats$out_to_in + analysis_summary$brfa_stats$in_to_out)
    print(paste('The mean number of movements across brfa boundaries per fish was ', round(mean_brfa_movements_per_fish, digits = 3), ' movements per fish (standard error ', round(se_brfa_movements_per_fish, digits = 3), ')', sep = ""))
    
  ## Number of movements per fish - Median, Min, 1st Quantile, 3rd Quantile, Max
    five_num_brfa_movements_per_fish = fivenum(analysis_summary$brfa_stats$out_to_in + analysis_summary$brfa_stats$in_to_out)
    print(paste('The median number of movements across brfa boundaries per fish was ', round(five_num_brfa_movements_per_fish[3], digits = 3), ' movements per fish (min = ', round(five_num_brfa_movements_per_fish[1], digits = 3), ', 1st Quantile = ', round(five_num_brfa_movements_per_fish[2], digits = 3), ', 3rd Quantile = ', round(five_num_brfa_movements_per_fish[4], digits = 3), ', max = ', round(five_num_brfa_movements_per_fish[5], digits = 3), ')', sep = ""))
    
  ## Number of movements per fish standardized by liberty - Average, Standard Error
    mean_brfa_movements_per_fish_by_tol = mean((analysis_summary$brfa_stats$out_to_in + analysis_summary$brfa_stats$in_to_out) / analysis_summary$time_at_liberty)
    se_brfa_movements_per_fish_by_tol = std_error((analysis_summary$brfa_stats$out_to_in + analysis_summary$brfa_stats$in_to_out) / analysis_summary$time_at_liberty)
    print(paste('The mean number of movements across brfa boundaries per fish, standardized by time at liberty, was ', round(mean_brfa_movements_per_fish_by_tol, digits = 3), ' movements per fish (standard error ', round(se_brfa_movements_per_fish_by_tol, digits = 3), ')', sep = ""))
    
  ## One crossing for ____ days.
    days_per_crossing = which.min(abs((mean_brfa_movements_per_fish_by_tol * seq(from = 1, to = 1000)) - 1))
    print(paste('This represents a crossing every ', days_per_crossing, ' days', sep = ""))
    
  ## Number of movements per fish standardized by liberty -  Median, Min, 1st Quantile, 3rd Quantile, Max
    five_num_brfa_movements_per_fish_by_tol = fivenum((analysis_summary$brfa_stats$out_to_in + analysis_summary$brfa_stats$in_to_out) / analysis_summary$time_at_liberty)
    print(paste('The median number of movements across brfa boundaries per fish, standardized by time at liberty, was ', round(five_num_brfa_movements_per_fish_by_tol[3], digits = 3), ' movements per fish (min = ', round(five_num_brfa_movements_per_fish_by_tol[1], digits = 3), ', 1st Quantile = ', round(five_num_brfa_movements_per_fish_by_tol[2], digits = 3), ', 3rd Quantile = ', round(five_num_brfa_movements_per_fish_by_tol[4], digits = 3), ', max = ', round(five_num_brfa_movements_per_fish_by_tol[5], digits = 3), ')', sep = ""))
    
  ## Fish tagged inside, moving outside and vice versa
    for(i in 1:length(unique(analysis_summary$data$tag_id))){
      indv_data = analysis_summary$data[analysis_summary$data$tag_id == unique(analysis_summary$data$tag_id)[i], ]
      brfa_status = in_brfa_e(indv_data)
      brfa_status[brfa_status == TRUE]  = "inside"
      brfa_status[brfa_status == FALSE] = "outside"
      status_string = paste('Tag ', indv_data$tag_id[1], ' was tagged ', brfa_status[1], ' the brfa')
      for(r in 2:length(brfa_status)){
        if(brfa_status[r] != brfa_status[r-1]){
          status_string = paste(status_string, 'then moved ', brfa_status[r])
        }
      }
      print(status_string)
    }

## Hypothesis 2. Bottomfish movements exceed the scale of individual fishery closed areas (BRFAs). 
    ## Hypothesis 2 was tested by calculating the home range size for tags that 
    ## were detected at 3 or more locations, including the location the individual 
    ## was tagged. Home range size was determined by fitting a polygon to the most 
    ## northern, southern, eastern, and western coordinates an individual was detected. 
    ## The average length of this polygon, estimated by the square root of the home 
    ## range size was compared the average length dimensions of the BRFAs Median 
    ## value 9.04 km (Min = 6.11, 1st Quantile = 8.75, 3rd Quantile = 14.98, 
    ## Max = 16.39). 
    ## For tags that were detected at only two locations, a linear geodesic 
    ## distance was calculated between the two points and compared to the average 
    ## BRFA length dimension.

  ## Number of tags with 3 or more positions for max polygon
  ## Area for tags wtih 3 or more positions from max polygon (km^2) - Mean and Standard Error
  ## Area for tags wtih 3 or more positions from max polygon (km^2) - Median, Min, 1st Quantile, 3rd Quantile, Max
  ## Square Rooted Area for tags wtih 3 or more positions from max polygon (km^2) - Mean and Standard Error
  ## Square Rooted Area for tags wtih 3 or more positions from max polygon (km^2) - Median, Min, 1st Quantile, 3rd Quantile, Max
  ## Maximum Linear Distance for all tags - Mean, Standard Error
  ## Maximum Linear Distance for all tags - Median, Min, 1st Quantile, 3rd Quantile, Max

## Hypothesis 3. Bottomfish do not utilize habitat uniformly.
    ## Hypothesis 3 was tested by comparing detection of tags moving between two 
    ## stations. A two dimensional matrix was constructed with the number of rows 
    ## and columns equal to the number of total stations tags were detected. Each 
    ## station was assigned a unique row and column. Each detection for each fish 
    ## was compared to the one previous. The for each detection pair, the matrix 
    ## values were incremented by one with rows representing the location of the 
    ## previous detection and columns representing the present location. The 
    ## diagonal values of the matrix identify subsequent detections at the same 
    ## station. A fish may be more likely to be detected at adjacent stations than 
    ## stations with further physical separation. Attempting to account for this, 
    ## row and column value was multiplied by the calculated distance between their 
    ## representative locations and then divided by the maximum value to standardize 
    ## flow rates relative to a maximum value of 1. 




    
