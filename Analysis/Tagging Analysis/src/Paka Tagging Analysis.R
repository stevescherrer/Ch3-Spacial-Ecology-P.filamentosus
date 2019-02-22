Early attempts to tag and track fish were limited by the scale and scope of the receiver tracking array. Deepwater acoustic tracking is relatively novel and presents a number of unique challenges compared to similar shallow water studies. During this study, the operational depths of array hardware often exceeded maximum scuba depths, requiring receiver stations to be deployed and recovered from a suitable vessel with the assistance of acoustic release mechanisms. 
Logistical challenges involved in the recovery and redeployment process limited maintenance and turnover of receivers to one or two times per year. It was not uncommon that during maintenance, a receiver station was unable to be recovered. Events that limited recovery include failure of the acoustic release, stations that broke free of their moorings, and entanglement with derelict fishing gear or benthic substrate. Failure of a VR2 receiver unit or corruption of the receiver’s data file also resulted in data loss. Of 220 total station deployments, 18 stations were unable to be recovered, or suffered equipment failure resulting in a loss of data. 
####TODO:
### Cull depreciated functions
### Debug n-stations detected
### Fix depth sensor for tag 32

### Do detections change with day night dusk dawn? - Individual and in total
### Does depth of receivers detected change with day night dusk dawn? - Individual and in total
### Do detections coorrelate wtih moonrise moonset?


### Note: All analysis results should be ordered by tag ID number. always use sort(unique(vue_data$tag_id)) if indexing mannually.
### Work to remove clean_vue() function as it's only used once

#### Incorperate some other random analysis currently above run function.
#### Incorperate changes in receiver placement to analysis plots.
#### Sink Hypothesis reports

#### Turned off "make_detection_plot()" in
#### Turned off "movement_graph_standardized_by_distance calculations and output.
#### Modified run_analysis() incorperates movement_graph and brfa_movements_by_track_length

### Utility Functions Available
# clean_vue_data()
# clean_receiver_stations()
# create_save_directory()
# load_vemco()
# load_receiver_data()
# receiver_col_names()
# load_tagging_data()
# remove_location()
# clean_vue()
# convert_lat_lon
# remove_detections_before_tagging()

### Analysis Functions Available
# cluster_receivers()
# get_graph()
# build_detection_matrix()
# calculate_time_at_liberty()
# calculate_track_length
# calculate_days_before_detection()
# calculate_days_detected()
# generate_tagging_detection()
# generate_study_date()
# list_stations_detected()
# remove_false_detections()
# spatial_evenness()
# days_present()
# length_to_weight()
# lldist()
# distance_tracked()
# in_brfa_e()
# in_brfa_f()
# brfa_movements()
# brfa_movements_by_track_length()
# n_movements()
# max_movement()
# get_fork_length()
# get_tagging_date()
# remove_only_tagging_date_entries()
# run_analysis()
# generate_analysis_report()
# create_analysis_csv()
# run()
# distance_between_vue_receivers()
# distance_between_receivers()
# get_recovery_rates()
# days_detected()
# stations_detected()
# brfa_size()

### Plotting Functions
# tag_detection_histogram()
# plot_receiver_map()
# plot_depths()
# plot_movements()
# plot_path_use()
# plot_tag_detections()
# tagging_histogram()
# assign_color_palette()
# generate_stripchart()
# detection_stripchart()
# indv_difftimes_hist
# plot_receiver_maps()
# create_day_night_plot()
# plot_detections_by_receiver()
# generate_gif_images()
# make_detection_plot()
# detection_stripcharts()
# plot_receiver_histories()
# make_detection_plot()
# plot_station_map()
# plot_clusters()


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

##### Clearning Workspace and Setting Directories #####
rm(list=ls()) # Clear workspace
gc()
script_timer = proc.time()
project_dir = getwd()
data_dir = '/Volumes/GoogleDrive/My Drive/Weng Lab/Data/Bottomfish/Oahu Receiver Data Files'
results_dir = paste(project_dir, '/results', sep = "")
# fig.dir = paste(project_dir, '/figures', sep = "")
src_dir = paste(project_dir, '/src', sep = "")
bin_dir = paste(project_dir, '/bin', sep = "")
setwd(project_dir)
savehistory(file=paste(results_dir, 'history.txt'))
# load("/Users/stephenscherrer/Google Drive/Weng Lab/Steve/dissertation work/Opakapaka Tagging/Opakapaka Tagging Analysis/workspace_image")

###### Importing principle dependencies #####
# install.packages('marmap')
library('marmap') # bathymetry()
# install.packages('lubridate')
library('lubridate') # floor_date(), ceil_date()
# install.packages('doParallel')
library('doParallel') # foreach()
# install.packages('beepr')
library('beepr') # beep
# install.packages('dplyr')
library('dplyr') # filter()
# install.packages('useful')
library('useful') # compare.list()
# install.packages('RAtmosphere')
library('RAtmosphere')
# install.packages('geosphere')
library('geosphere') # areaPolygon(), # distGeo() Note: wrapped in old lldist function
# install.packages('igraph')
library('igraph') # graph.adjacency()
# install.packages('notifyR')
library('notifyR') # send_push()
# install.packages('png')
library('png')
# install.packages('lunar')
library('lunar') # lunar.phase()
# install.packages('suncalc')
library('suncalc') # getSunlightTimes
# install.packages('data.table')
library('data.table') # uniqueN
# install.packages('lattice')
library('lattice')
# install.packages('ncdf4')
library('ncdf4') #nc_open()
# install.packages('xts')
library('xts')
# install.packages('lme4')
library('lme4')
# install.packages('lmtest')
library('lmtest')
# install.packages('ggplot2')
library('ggplot2')

registerDoParallel(cores = detectCores()-1)



##### Declaration of Functions #####

#### Utility Functions
convert_lat_lon = function(ll_deg, ll_min = FALSE){
  #### Converts latitude and longitude between ll minutes and ll decimal degrees
  ### 2 usages:
  ## 1. Convert decimal degrees to degree minutes
  # 1 argument
  # ll_pref is a single argument of latitude or longitude in decimal degrees
  # Returns a prefix and decimal for that argument
  ## 2. Convert degree minutes to decimal degrees
  # 2 arguments
  # ll_pref is the latitude or longitude's degree
  # ll_min is the degree minutes
  # returns a single float of ll in decimal degrees
  ## Use Case 1.
  if (ll_min[1] == FALSE){
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
    ## Use Case 2.
  }else{
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

create_save_directory = function(directory_name = NULL){
  #### Creates a new file directory. If directory_name argument is not provided, new directory name defaults to the current system time
  if(is.null(directory_name)){
    dir.name = paste(getwd(), "/", Sys.time(), sep = "")
  }else{
    dir.name = paste(getwd(), "/", directory_name, sep = "") 
  }
  dir.create(dir.name) # creates new output directory
  return(dir.name) # for setting working directory with
}

convert_fath_to_m = function(fath){
  fath * 1.8288
}

get_bathymetry = function(region, resolution = 'low'){
  # Region choices are "Makapuu", "Oahu", "Oahu and Penguin Banks"  
  # resolution choices are 'high', 'medium', 'low'
  
  if(tolower(resolution) == 'low'){
    if(file.exists('/Volumes/GoogleDrive/My Drive/Weng Lab/Data/Hawaii Bathymetry/Hawaii 1 km/Oahu and Penguin Banks Bathymetry')){
      bathymetry = read.csv('/Volumes/GoogleDrive/My Drive/Weng Lab/Data/Hawaii Bathymetry/Hawaii 1 km/Oahu and Penguin Banks Bathymetry', sep = ",")
      bathymetry = as.bathy(bathymetry)
    }else{
      bathymetry = getNOAA.bathy(lon1 = -161, lon2 = -154, lat1 = 18.4, lat2 = 22.44, resolution = 1)
    }
  } else if (tolower(resolution) == 'medium'){
    ## Load in ncdf of 50m bathy dataset and convert to bathy class
    oahu_50m_bathy = nc_open('/Volumes/GoogleDrive/My Drive/Weng Lab/Data/Hawaii Bathymetry/Hawaii 50m Bathymetry/himbsyn.bathy.v19.grd')
    bathymetry = ncvar_get(oahu_50m_bathy, varid = 'z')
    rownames(bathymetry) = ncvar_get(oahu_50m_bathy, varid = 'lon')
    colnames(bathymetry) = ncvar_get(oahu_50m_bathy, varid = 'lat')
    class(bathymetry) <- "bathy"
    
  } else if (tolower(resolution) == 'high'){
    bathymetry = read.bathy('/Volumes/GoogleDrive/My Drive/Weng Lab/Data/Hawaii Bathymetry/Hawaii 5m Bathymetry/oahu_5m_bathy_lat_lon.xyz')
  }
  
  ## Subset by region
  if(tolower(region) == 'makapuu'){
    bathymetry = subsetBathy(mat = bathymetry, x = c(-157.75, -157.5), y = c(21.25, 21.45), locator = FALSE)
  }else if(tolower(region) == 'oahu'){
    bathymetry = subsetBathy(bathymetry, x = c(-158.49,-157.5015), y = c( 21.175, 21.81), locator = FALSE)
  }else if(tolower(region) == 'oahu and penguin banks'){
    bathymetry = subsetBathy(mat = bathymetry, x = c(-158.45, -157.24), y = c(20.85, 21.8), locator = FALSE)
  }else if(tolower(region) %in% c('mhi', 'main hawaiian islands')){
    bathymetry = subsetBathy(mat = bathymetry, x = c(-160.98, -154.1), y = c(18.42, 22.43), locator = FALSE)
  }
  
  return(bathymetry)
}

get_brfa_areas = function(target_depth = -120, target_depth_contour = c(-100, -400), save_plot = TRUE){
  
  bathy_50m = get_bathymetry('mhi', 'medium')
  
  ## Somewhere for plots to dump out
  if(save_plot){
    pdf('BRFA Habitat Areas.pdf', width = 11, height = 8.5)
    par(mfrow = c(3, 4))
  }
  
  ### BRFA A
  brfa = subsetBathy(mat = bathy_50m, x = c(convert_lat_lon(-160, 39), convert_lat_lon(-160, 30)), y = c( convert_lat_lon(21, 44), convert_lat_lon(21, 41)), locator = FALSE)
  plot.bathy(brfa, land = TRUE, image=TRUE, bpal = list(c(max(target_depth_contour), min(target_depth_contour), "lightblue")), deepest.isobath = c(-10000), shallowest.isobath = c(-1), step = c(100), drawlabels = TRUE, main = 'BRFA A')
  
  # South to south
  start = as.numeric(c(rownames(brfa[1:round(nrow(brfa)/3), ])[which.min(abs(brfa[1:round(nrow(brfa)/3 ) ,1] - target_depth))], min(colnames(brfa))))
  end = as.numeric(c(rownames(brfa[round(nrow(brfa)/2):nrow(brfa), ])[which.min(abs(brfa[round(nrow(brfa)/2):nrow(brfa), 1] - target_depth))], min(colnames(brfa))))
  locations = as.data.frame(rbind(start, end), row.names = FALSE)
  
  points(start[1], start[2], col = 'red', pch = 19)
  points(end[1], end[2], col = 'red', pch = 19)
  get.depth(brfa, start[1], start[2], locator = F)
  get.depth(brfa, end[1], end[2], locator = F)
  
  trans1 = trans.mat(brfa, min.depth = max(target_depth_contour) , max.depth = min(target_depth_contour))
  out1 = lc.dist(trans1, locations, res = "path")
  lapply(out1, lines, col = "red", lwd = 1, lty = 1)
  brfa_a_path = lc.dist(trans1, locations, res = "dist")
  
  
  ### BRFA B
  brfa = subsetBathy(mat = bathy_50m, x = c(convert_lat_lon(-160, 04.50), convert_lat_lon(-160, 07.50)), y = c( convert_lat_lon(21, 50.41666),  convert_lat_lon(21, 46.50)), locator = FALSE)
  plot.bathy(brfa, land = TRUE, image=TRUE, bpal = list(c(min(target_depth_contour), max(target_depth_contour), "lightblue")), deepest.isobath = c(-10000), shallowest.isobath = c(-1), step = c(100), drawlabels = TRUE, main = 'BRFA B')
  ## All fucked up
  ## In three parts (but really in two)
  ## First we need to get the length of the strip across the northern side of niihau - Runs East West
  start = as.numeric(c(max(rownames(brfa)), names(brfa[nrow(brfa), 75:ncol(brfa)])[which.min(abs(brfa[1, 75:ncol(brfa)] - target_depth))]) )# West
  end = as.numeric(c(names(which.min(abs(brfa[ ,ncol(brfa)] - target_depth))), max(colnames(brfa)))) # North
  
  locations = as.data.frame(rbind(start, end))
  
  points(start[1], start[2], col = 'red', pch = 19)
  points(end[1], end[2], col = 'red', pch = 19)
  get.depth(brfa, start[1], start[2], locator = F)
  get.depth(brfa, end[1], end[2], locator = F)
  
  points(locations, col = 'red', pch = 19)
  get.depth(brfa, locations, locator = F)
  
  trans1 = trans.mat(brfa, min.depth = target_depth_contour[1] , max.depth = target_depth_contour[2] )
  out1 = lc.dist(trans1, locations, res = "path")
  lapply(out1, lines, col = "red", lwd = 1, lty = 1)
  brfa_b_path_north = lc.dist(trans1, locations, res = c("dist"))
  
  
  ### Now we need to find a start and end point around the mound. This is probably
  if(target_depth <= min(brfa[which(rownames(brfa) <= -160.1155 & rownames(brfa) >= -160.0798) , which(colnames(brfa) >= 21.77928 & colnames(brfa) <= 21.81752)])){
    
    row_m1 = which.min(abs(as.numeric(rownames(brfa)) - -160.1005))
    row_m2 = which.min(abs(as.numeric(rownames(brfa)) - -160.1055))
    row_m3 = which.min(abs(as.numeric(rownames(brfa)) - -160.093))
    
    mound_1  = as.numeric(c(rownames(brfa)[row_m1], names(which.min(abs(brfa[row_m1, which(colnames(brfa) > 21.805)] - target_depth)))))
    mound_2  = as.numeric(c(rownames(brfa)[row_m2], names(which.min(abs(brfa[row_m2, which(colnames(brfa) <= 21.805)] - target_depth)))))
    mound_3  = as.numeric(c(rownames(brfa)[row_m3], names(which.min(abs(brfa[row_m3, which(colnames(brfa) <= 21.805)] - target_depth)))))
    
    get.depth(brfa, mound_1[1], mound_1[2], locator = F)
    get.depth(brfa, mound_2[1], mound_2[2], locator = F)
    get.depth(brfa, mound_3[1], mound_3[2], locator = F)
    
    text(mound_1[1], mound_1[2], labels = 1, col = 'yellow')
    text(mound_2[1], mound_2[2], labels = 2, col = 'yellow')
    text(mound_3[1], mound_3[2], labels = 3, col = 'yellow')
    
    locations = as.data.frame(rbind(mound_1, mound_2))
    trans1 = trans.mat(brfa, min.depth = target_depth_contour[1] , max.depth = target_depth_contour[2] )
    out1 = lc.dist(trans1, locations, res = "path")
    lapply(out1, lines, col = "red", lwd = 1, lty = 1)
    
    brfa_b_path_12 = lc.dist(trans1, locations, res = c("dist"))
    locations = as.data.frame(rbind(mound_2, mound_3))
    trans1 = trans.mat(brfa, min.depth = target_depth_contour[1] , max.depth = target_depth_contour[2] )
    out1 = lc.dist(trans1, locations, res = "path")
    lapply(out1, lines, col = "red", lwd = 1, lty = 1)
    
    brfa_b_path_23 = lc.dist(trans1, locations, res = c("dist"))
    locations = as.data.frame(rbind(mound_3, mound_1))
    trans1 = trans.mat(brfa, min.depth = target_depth_contour[1] , max.depth = target_depth_contour[2] )
    out1 = lc.dist(trans1, locations, res = "path")
    lapply(out1, lines, col = "red", lwd = 1, lty = 1)
    
    brfa_b_path_31 = lc.dist(trans1, locations, res = c("dist"))
    brfa_mound = sum(brfa_b_path_12, brfa_b_path_23, brfa_b_path_31)
  } else {
    brfa_mound = 0
  }
  brfa_b_path = sum(brfa_b_path_north, brfa_mound)
  
  ### BRFA C
  brfa = subsetBathy(mat = bathy_50m, x = c(convert_lat_lon(-159, 32), convert_lat_lon(-159, 23)), y = c( convert_lat_lon(21, 52.50), convert_lat_lon( 21, 50.50)), locator = FALSE)
  plot.bathy(brfa, land = TRUE, image=TRUE, bpal = list(c(min(target_depth_contour), max(target_depth_contour), "lightblue")), deepest.isobath = c(-10000), shallowest.isobath = c(-1), step = c(100), drawlabels = TRUE, main = 'BRFA C')
  # East to west
  start = as.numeric(c(max(rownames(brfa)), names(brfa[nrow(brfa), ])[which.min(abs(brfa[1, ] - target_depth))]) )# West
  end = as.numeric(c(min(rownames(brfa)), names(brfa[1, ])[which.min(abs(brfa[nrow(brfa), ] - target_depth))])) # East
  
  locations = as.data.frame(rbind(start, end))
  points(locations, col = 'red', pch = 19)
  get.depth(brfa, locations, locator = F)
  
  trans1 = trans.mat(brfa, min.depth = target_depth_contour[1] , max.depth = target_depth_contour[2] )
  out1 = lc.dist(trans1, locations, res = "path")
  lapply(out1, lines, col = "red", lwd = 1, lty = 1)
  
  brfa_c_path = lc.dist(trans1, locations, res = c("dist"))
  
  ### BRFA D
  brfa = subsetBathy(mat = bathy_50m, x = c(convert_lat_lon(-158, 24), convert_lat_lon(-158, 15)), y = c( North  = convert_lat_lon( 21, 39), North  = convert_lat_lon( 21, 36)), locator = FALSE)
  plot.bathy(brfa, land = TRUE, image=TRUE, bpal = list(c(min(target_depth_contour), max(target_depth_contour), "lightblue")), deepest.isobath = c(-10000), shallowest.isobath = c(-1), step = c(100), drawlabels = TRUE, main = 'BRFA D')
  # South to east
  start = as.numeric(c(names(which.min(abs(brfa[ ,1] - target_depth))), min(colnames(brfa)))) # South
  end = as.numeric(c(min(rownames(brfa)), names(brfa[1, ])[which.min(abs(brfa[nrow(brfa), ] - target_depth))])) # East
  
  locations = as.data.frame(rbind(start, end))
  points(locations, col = 'red', pch = 19)
  get.depth(brfa, locations, locator = F)
  
  trans1 = trans.mat(brfa, min.depth = target_depth_contour[1] , max.depth = target_depth_contour[2] )
  out1 = lc.dist(trans1, locations, res = "path")
  lapply(out1, lines, col = "red", lwd = 1, lty = 1)
  
  brfa_d_path = lc.dist(trans1, locations, res = c("dist"))
  
  ## BRFA E
  brfa = subsetBathy(mat = bathy_50m, x = c(convert_lat_lon(-157, 41), convert_lat_lon(-157, 32)), y = c( convert_lat_lon(21, 25), convert_lat_lon(21, 17)), locator = FALSE)
  plot.bathy(brfa, land = TRUE, image=TRUE, bpal = list(c(min(target_depth_contour), max(target_depth_contour), "lightblue")), deepest.isobath = c(-10000), shallowest.isobath = c(-1), step = c(100), drawlabels = TRUE, main = 'BRFA E')
  # North to south
  start = as.numeric(c(names(which.min(abs(brfa[ ,ncol(brfa)] - target_depth))), max(colnames(brfa)))) # North
  end = as.numeric(c(names(which.min(abs(brfa[ ,1] - target_depth))), min(colnames(brfa)))) # South
  
  locations = as.data.frame(rbind(start, end))
  points(locations, col = 'red', pch = 19)
  get.depth(brfa, locations, locator = F)
  
  trans1 = trans.mat(brfa, min.depth = target_depth_contour[1] , max.depth = target_depth_contour[2] )
  out1 = lc.dist(trans1, locations, res = "path")
  lapply(out1, lines, col = "red", lwd = 1, lty = 1)
  brfa_e_path = lc.dist(trans1, locations, res = c("dist"))
  
  
  ### BRFA F
  brfa = subsetBathy(mat = bathy_50m, x = c(convert_lat_lon(-157, 34), convert_lat_lon(-157, 22)), y = c( convert_lat_lon( 21, 02), convert_lat_lon(20, 55)), locator = FALSE)
  plot.bathy(brfa, land = TRUE, image=TRUE, bpal = list(c(min(target_depth_contour), max(target_depth_contour), "lightblue")), deepest.isobath = c(-10000), shallowest.isobath = c(-1), step = c(100), drawlabels = TRUE, main = 'BRFA F')
  # West to East
  start = as.numeric(c(max(rownames(brfa)), names(brfa[nrow(brfa), ])[which.min(abs(brfa[1, ] - target_depth))]) )# West
  end = as.numeric(c(min(rownames(brfa)), names(brfa[1, ])[which.min(abs(brfa[nrow(brfa), ] - target_depth))])) # East
  
  locations = as.data.frame(rbind(start, end))
  points(locations, col = 'red', pch = 19)
  get.depth(brfa, locations, locator = F)
  
  trans1 = trans.mat(brfa, min.depth = target_depth_contour[1] , max.depth = target_depth_contour[2] )
  out1 = lc.dist(trans1, locations, res = "path")
  lapply(out1, lines, col = "red", lwd = 1, lty = 1)
  brfa_f_path = lc.dist(trans1, locations, res = c("dist"))
  
  ### BRFA G
  brfa = subsetBathy(mat = bathy_50m, x = c(convert_lat_lon(-156, 58), convert_lat_lon(-156, 52)), y = c( convert_lat_lon(21, 14), convert_lat_lon(21, 10)), locator = FALSE)
  plot.bathy(brfa, land = TRUE, image=TRUE, bpal = list(c(min(target_depth_contour), max(target_depth_contour), "lightblue")), deepest.isobath = c(-10000), shallowest.isobath = c(-1), step = c(100), drawlabels = TRUE, main = 'BRFA G')
  # West to East
  start = as.numeric(c(max(rownames(brfa)), names(brfa[nrow(brfa), ])[which.min(abs(brfa[1, ] - target_depth))]) )# West
  end = as.numeric(c(min(rownames(brfa)), names(brfa[1, ])[which.min(abs(brfa[nrow(brfa), ] - target_depth))])) # East
  
  locations = as.data.frame(rbind(start, end))
  points(locations, col = 'red', pch = 19)
  get.depth(brfa, locations, locator = F)
  
  trans1 = trans.mat(brfa, min.depth = target_depth_contour[1] , max.depth = target_depth_contour[2] )
  out1 = lc.dist(trans1, locations, res = "path")
  lapply(out1, lines, col = "red", lwd = 1, lty = 1)
  
  brfa_g_path = lc.dist(trans1, locations, res = c("dist"))
  
  ###  BRFA H
  brfa = subsetBathy(mat = bathy_50m, x = c(convert_lat_lon(-156, 42), convert_lat_lon(-156, 38)), y = c( convert_lat_lon(21, 07), convert_lat_lon(21, 03)), locator = FALSE)
  
  if(min(brfa) >= min(target_depth_contour) & max(brfa) <= max(target_depth_contour)){
    plot.bathy(brfa, land = TRUE, image=TRUE, bpal =  "lightblue", deepest.isobath = c(-10000), shallowest.isobath = c(-1), step = c(100), drawlabels = TRUE, main = 'BRFA H')
    ### THIS WHOLE BRFA IS FUCKING PROTECTED HABITAT.
    start = as.numeric(c(min(rownames(brfa)), min(colnames(brfa))))
    end = as.numeric(c(max(rownames(brfa)), max(colnames(brfa))))
    
    
    locations = as.data.frame(rbind(start, end))
    points(locations, col = 'red', pch = 19)
    
    trans1 = trans.mat(brfa, min.depth = target_depth_contour[1] , max.depth = target_depth_contour[2] )
    out1 = lc.dist(trans1, locations, res = "path")
    lapply(out1, lines, col = "red", lwd = 1, lty = 1)
    
    brfa_h_path = lc.dist(trans1, locations, res = c("dist"))
    
    
  }else{
    print('YOURE GOING TO NEED TO RECALCULATE AREA FOR BRFA H BECAUSE THE WHOLE THING IS NOT HABITAT')
    plot.bathy(brfa, land = TRUE, image=TRUE, bpal = list(c(-100, -120, "red")), deepest.isobath = c(-10000), shallowest.isobath = c(-1), step = c(25), drawlabels = TRUE, main = 'BRFA H')
    
    
    
    ## Or... west to north
    west = as.numeric(c(max(rownames(brfa)), names(brfa[nrow(brfa), ])[which.min(abs(brfa[1, ] - target_depth))]) )# West
    north = as.numeric(c(names(which.min(abs(brfa[ ,ncol(brfa)] - target_depth))), max(colnames(brfa)))) # North
    
    locations = as.data.frame(rbind(west, north))
    points(locations, col = 'red', pch = 19)
    get.depth(brfa, locations, locator = F)
    
    locations = as.data.frame(rbind(west, north))
    trans1 = trans.mat(brfa, min.depth = target_depth_contour[1] , max.depth = target_depth_contour[2] )
    out1 = lc.dist(trans1, locations, res = "path")
    lapply(out1, lines, col = "red", lwd = 1, lty = 1)
    
    brfa_path_north_west = lc.dist(trans1, locations, res = c("dist"))
    
    
    ## South to east
    south = as.numeric(c(names(which.min(abs(brfa[ ,1] - target_depth))), min(colnames(brfa)))) # South
    east = as.numeric(c(min(rownames(brfa)), names(brfa[1, ])[which.min(abs(brfa[nrow(brfa), ] - target_depth))])) # East
    
    locations = as.data.frame(rbind(south, east))
    points(locations, col = 'red', pch = 19)
    get.depth(brfa, locations, locator = F)
    
    locations = as.data.frame(rbind(south, east))
    trans1 = trans.mat(brfa, min.depth = target_depth_contour[1] , max.depth = target_depth_contour[2] )
    out1 = lc.dist(trans1, locations, res = "path")
    lapply(out1, lines, col = "red", lwd = 1, lty = 1)
    
    brfa_path_south_east = lc.dist(trans1, locations, res = c("dist"))
    
    ## Two mounds.
    # south west mound
    if(target_depth <= min(brfa[which(rownames(brfa) <= -156.69 & rownames(brfa) >= -156.6727) , which(colnames(brfa) >= 21.06 & colnames(brfa) <= 21.075)])){
      ### Now we need to find a start and end point around the mound. This is probably
      row_m1 = which.min(abs(as.numeric(rownames(brfa)) - -156.6845))
      row_m2 = which.min(abs(as.numeric(rownames(brfa)) - -156.6805))
      row_m3 = which.min(abs(as.numeric(rownames(brfa)) - -156.678))
      mound_1  = as.numeric(c(rownames(brfa)[row_m1], names(which.min(abs(brfa[row_m1, which(colnames(brfa) < 21.07457)] - target_depth)))))
      mound_2  = as.numeric(c(rownames(brfa)[row_m2], names(which.min(abs(brfa[row_m2, which(colnames(brfa) <= 21.07457 & colnames(brfa) >= 21.06703)] - target_depth)))))
      mound_3  = as.numeric(c(rownames(brfa)[row_m3], names(which.min(abs(brfa[row_m3, which(colnames(brfa) <= 21.07 & colnames(brfa) >= 21.05859)] - target_depth)))))
      get.depth(brfa, mound_1[1], mound_1[2], locator = F)
      get.depth(brfa, mound_2[1], mound_2[2], locator = F)
      get.depth(brfa, mound_3[1], mound_3[2], locator = F)
      
      text(mound_1[1], mound_1[2], labels = 1, col = 'blue')
      text(mound_2[1], mound_2[2], labels = 2, col = 'blue')
      text(mound_3[1], mound_3[2], labels = 3, col = 'blue')
      
      
      locations = as.data.frame(rbind(mound_1, mound_2))
      trans1 = trans.mat(brfa, min.depth = target_depth_contour[1] , max.depth = target_depth_contour[2] )
      out1 = lc.dist(trans1, locations, res = "path")
      lapply(out1, lines, col = "red", lwd = 1, lty = 1)
      
      brfa_path_12 = lc.dist(trans1, locations, res = c("dist"))
      locations = as.data.frame(rbind(mound_2, mound_3))
      trans1 = trans.mat(brfa, min.depth = target_depth_contour[1] , max.depth = target_depth_contour[2] )
      out1 = lc.dist(trans1, locations, res = "path")
      lapply(out1, lines, col = "red", lwd = 1, lty = 1)
      
      brfa_path_23 = lc.dist(trans1, locations, res = c("dist"))
      locations = as.data.frame(rbind(mound_3, mound_1))
      trans1 = trans.mat(brfa, min.depth = target_depth_contour[1] , max.depth = target_depth_contour[2] )
      out1 = lc.dist(trans1, locations, res = "path")
      lapply(out1, lines, col = "red", lwd = 1, lty = 1)
      
      brfa_path_31 = lc.dist(trans1, locations, res = c("dist"))
      mound_1_length = sum(brfa_path_12, brfa_path_23, brfa_path_31)
    } else {
      mound_1_length = 0
    }
    
    ### Mound 2
    if(target_depth <= min(brfa[which(rownames(brfa) <= -156.6782 & rownames(brfa) >= -156.6544) , which(colnames(brfa) >= 21.07653 & colnames(brfa) <= 21.09154)])){
      
      row_m1 = which.min(abs(as.numeric(rownames(brfa)) - -156.675))
      row_m2 = which.min(abs(as.numeric(rownames(brfa)) - -156.6655))
      row_m3 = which.min(abs(as.numeric(rownames(brfa)) - -156.658))
      
      mound_1  = as.numeric(c(rownames(brfa)[row_m1], names(which.min(abs(brfa[row_m1, which(colnames(brfa) > 21.07371 & colnames(brfa) < 21.09)] - target_depth)))))
      mound_2  = as.numeric(c(rownames(brfa)[row_m2], names(which.min(abs(brfa[row_m2, which(colnames(brfa) <= 21.1 & colnames(brfa) >= 21.085)] - target_depth)))))
      mound_3  = as.numeric(c(rownames(brfa)[row_m3], names(which.min(abs(brfa[row_m3, which(colnames(brfa) <= 21.084 & colnames(brfa) >= 21.075)] - target_depth)))))
      get.depth(brfa, mound_1[1], mound_1[2], locator = F)
      get.depth(brfa, mound_2[1], mound_2[2], locator = F)
      get.depth(brfa, mound_3[1], mound_3[2], locator = F)
      
      points(mound_1[1], mound_1[2], col = 'blue', cex = 2)
      points(mound_2[1], mound_2[2], cex = 2, col = 'blue')
      points(mound_3[1], mound_3[2], cex = 2, col = 'blue')
      
      locations = as.data.frame(rbind(mound_1, mound_2))
      trans1 = trans.mat(brfa, min.depth = target_depth_contour[1] , max.depth = target_depth_contour[2] )
      out1 = lc.dist(trans1, locations, res = "path")
      lapply(out1, lines, col = "red", lwd = 1, lty = 1)
      brfa_path_12 = lc.dist(trans1, locations, res = c("dist"))
      
      locations = as.data.frame(rbind(mound_2, mound_3))
      trans1 = trans.mat(brfa, min.depth = target_depth_contour[1] , max.depth = target_depth_contour[2] )
      out1 = lc.dist(trans1, locations, res = "path")
      lapply(out1, lines, col = "red", lwd = 1, lty = 1)
      brfa_path_23 = lc.dist(trans1, locations, res = c("dist"))
      
      locations = as.data.frame(rbind(mound_3, mound_1))
      trans1 = trans.mat(brfa, min.depth = target_depth_contour[1] , max.depth = target_depth_contour[2] )
      out1 = lc.dist(trans1, locations, res = "path")
      lapply(out1, lines, col = "red", lwd = 1, lty = 1)
      brfa_path_31 = lc.dist(trans1, locations, res = c("dist"))
      
      mound_2_length = sum(brfa_path_12, brfa_path_23, brfa_path_31)
    } else {
      mound_2_length = 0
    }
    
    brfa_h_path = sum(brfa_path_south_east, brfa_path_north_west, mound_1_length, mound_2_length)
  }
  
  ### BRFA J
  brfa = subsetBathy(mat = bathy_50m, x = c(convert_lat_lon(-156, 08), convert_lat_lon(-155, 59)), y = c( convert_lat_lon(20, 55), convert_lat_lon(20, 47)), locator = FALSE)
  plot.bathy(brfa, land = TRUE, image=TRUE, bpal = list(c(min(target_depth_contour), max(target_depth_contour), "lightblue")), deepest.isobath = c(-10000), shallowest.isobath = c(-1), step = c(100), drawlabels = TRUE, main = 'BRFA J')
  # West to East
  start = as.numeric(c(max(rownames(brfa)), names(brfa[nrow(brfa), ])[which.min(abs(brfa[1, ] - target_depth))]) )# West
  end = as.numeric(c(min(rownames(brfa)), names(brfa[1, ])[which.min(abs(brfa[nrow(brfa), ] - target_depth))])) # East
  
  locations = as.data.frame(rbind(start, end))
  points(locations, col = 'red', pch = 19)
  get.depth(brfa, locations, locator = F)
  
  trans1 = trans.mat(brfa, min.depth = target_depth_contour[1] , max.depth = target_depth_contour[2] )
  out1 = lc.dist(trans1, locations, res = "path")
  lapply(out1, lines, col = "red", lwd = 1, lty = 1)
  
  brfa_j_path = lc.dist(trans1, locations, res = c("dist"))
  
  ### BRFA K
  brfa = subsetBathy(mat = bathy_50m, x = c(convert_lat_lon(-155, 53), convert_lat_lon(-155, 25)), y = c( convert_lat_lon(20, 21), convert_lat_lon(20, 05.5)), locator = FALSE)
  plot.bathy(brfa, land = TRUE, image=TRUE, bpal = list(c(min(target_depth_contour), max(target_depth_contour), "lightblue")), deepest.isobath = c(-10000), shallowest.isobath = c(-1), step = c(100), drawlabels = TRUE, main = 'BRFA K')
  # West to East
  start = as.numeric(c(max(rownames(brfa)), names(brfa[nrow(brfa), ])[which.min(abs(brfa[1, ] - target_depth))]) )# West
  end = as.numeric(c(min(rownames(brfa)), names(brfa[1, ])[which.min(abs(brfa[nrow(brfa), ] - target_depth))])) # East
  
  locations = as.data.frame(rbind(start, end))
  
  points(locations, col = 'red', pch = 19)
  get.depth(brfa, locations, locator = F)
  
  trans1 = trans.mat(brfa, min.depth = target_depth_contour[1] , max.depth = target_depth_contour[2] )
  out1 = lc.dist(trans1, locations, res = "path")
  lapply(out1, lines, col = "red", lwd = 1, lty = 1)
  
  brfa_k_path = lc.dist(trans1, locations, res = c("dist"))
  
  ### BRFA L
  brfa = subsetBathy(mat = bathy_50m, x = c(convert_lat_lon(-154, 59.8333), convert_lat_lon(-154, 54)), y = c( convert_lat_lon(19, 35), convert_lat_lon(19, 44)), locator = FALSE)
  plot.bathy(brfa, land = TRUE, image=TRUE, bpal = list(c(min(target_depth_contour), max(target_depth_contour), "lightblue")), deepest.isobath = c(-10000), shallowest.isobath = c(-1), step = c(100), drawlabels = TRUE, main = 'BRFA L')
  # North to South
  start = as.numeric(c(names(which.min(abs(brfa[ ,ncol(brfa)] - target_depth))), max(colnames(brfa)))) # North
  end = as.numeric(c(names(which.min(abs(brfa[ ,1] - target_depth))), min(colnames(brfa)))) # South
  
  locations = as.data.frame(rbind(start, end))
  points(locations, col = 'red', pch = 19)
  get.depth(brfa, locations, locator = F)
  
  trans1 = trans.mat(brfa, min.depth = target_depth_contour[1] , max.depth = target_depth_contour[2] )
  out1 = lc.dist(trans1, locations, res = "path")
  lapply(out1, lines, col = "red", lwd = 1, lty = 1)
  brfa_l_path = lc.dist(trans1, locations, res = c("dist"))
  
  ### BRFA M
  brfa = subsetBathy(mat = bathy_50m, x = c(convert_lat_lon(-155, 37), convert_lat_lon(-155, 41)), y = c( convert_lat_lon(18, 57), convert_lat_lon(18, 51)), locator = FALSE)
  plot.bathy(brfa, land = TRUE, image=TRUE, bpal = list(c(min(target_depth_contour), max(target_depth_contour), "lightblue")), deepest.isobath = c(-10000), shallowest.isobath = c(-1), step = c(100), drawlabels = TRUE, main = 'BRFA M')
  # West to East
  start = as.numeric(c(max(rownames(brfa)), names(brfa[nrow(brfa), ])[which.min(abs(brfa[1, ] - target_depth))]) )# West
  end = as.numeric(c(min(rownames(brfa)), names(brfa[1, ])[which.min(abs(brfa[nrow(brfa), ] - target_depth))])) # East
  
  locations = as.data.frame(rbind(start, end))
  points(locations, col = 'red', pch = 19)
  get.depth(brfa, locations, locator = F)
  
  trans1 = trans.mat(brfa, min.depth = target_depth_contour[1] , max.depth = target_depth_contour[2] )
  out1 = lc.dist(trans1, locations, res = "path")
  lapply(out1, lines, col = "red", lwd = 1, lty = 1)
  brfa_m_path = lc.dist(trans1, locations, res = c("dist"))
  
  if(save_plot){
    dev.off()
  }
  
  ### ALL BRFA DIMENSIONS!!!!
  brfa_dimensions = c(brfa_a_path, brfa_b_path, brfa_c_path, brfa_d_path, brfa_e_path, brfa_f_path, brfa_g_path, brfa_h_path, brfa_j_path, brfa_k_path, brfa_l_path, brfa_m_path)
  return(brfa_dimensions)
}

lldist = function(point1, point2){
  ## Requires geosphere package for distGeo function
  distance = distGeo(p1 = point1, p2 = point2) / 1000
  return(distance)
}

mode = function(x){
  ## Function to return the mode of a list
  names(sort(x = table(x), decreasing = TRUE)[1])
}

std_error = function(x){
  #### Calculates standard error of set (x)
  sqrt(var(x)/length(x))
}

#### Loading and Cleaning Data
clean_receiver_stations = function(receiver_df, region = c('Oahu', 'PB'), remove = FALSE){
  #### Matches supplied region argument with location prefix for receiver station names.
  ### If remove argument is true, matching stations are removed, if false, non-matching stations are removed
  index = c()
  for(i in 1:length(receiver_df$station_name)){
    if(strsplit(as.character(receiver_df$station_name[i]), split = " ")[[1]][1] %in% region == TRUE){
      index = c(index, i)
    }
  }
  if(remove == FALSE){
    receiver_df = receiver_df[index, ]
  }else if(remove == TRUE){
    receiver_df = receiver_df[!index, ]
  }
  receiver_df$station_name = as.character(receiver_df$station_name)
  return(receiver_df)
}

clean_vue = function(vue_df, tag_ids = NULL, exclude = FALSE){
  ## Function for removing all tags from a vue DB not explicitly kept by the tag_ids input.
  if (exclude == FALSE){
    keep_data = vue_df[vue_df$tag_id %in% tag_ids, ]
  }else if (exclude == TRUE){
    keep_data = vue_df[!(vue_df$tag_id %in% tag_ids), ]
  }
  return(keep_data)
}

clean_vue_data = function(vue_df, receiver_df){
  #### Reassigns station names, latitude, and longitude for vue datafile by cross referencing detection dates and VR2 serial number with receiver deployment logs 
  for(i in 1:length(receiver_df$station_name)){
    vue_df$station[which(vue_df$receiver == receiver_df$vr2w_serial[i] & 
                           vue_df$datetime >= receiver_df$deployment_date[i] &
                           vue_df$datetime <= receiver_df$recovery_date[i])] = 
      as.character(receiver_df$station_name[i])
    vue_df$lat[which(vue_df$receiver == receiver_df$vr2w_serial[i] & 
                       vue_df$datetime >= receiver_df$deployment_date[i] &
                       vue_df$datetime <= receiver_df$recovery_date[i])] = 
      as.numeric(receiver_df$lat[i])
    vue_df$lon[which(vue_df$receiver == receiver_df$vr2w_serial[i] & 
                       vue_df$datetime >= receiver_df$deployment_date[i] &
                       vue_df$datetime <= receiver_df$recovery_date[i])] = 
      as.numeric(receiver_df$lon[i])
  }
  vue_df$lat = as.numeric(vue_df$lat)
  vue_df$lon = as.numeric(vue_df$lon)
  vue_df$depth[!is.na(vue_df$depth)] = -vue_df$depth[!is.na(vue_df$depth)]
  return(vue_df)
}

clean_vue_fdf = function(vue_df, fdf_report){
  ## This creates a the column detection_status on vue_df. 
  ## If detection_status == TRUE, then it is not a false detection
  ## if detection_status == FALSE, then it is believed to be a false detection
  ## Remove anything where the tag passed the check
  fdf_report = fdf_report[fdf_report$acceptance == "Questionable", ]
  ## Create a new column
  vue_df$detection_status = TRUE
  for(i in 1:length(fdf_report)){
    fd_index = which(vue_df$tag_id == as.numeric(fdf_report$tag_id[i]) & vue_df$receiver == as.numeric(fdf_report$receiver[i]) & vue_df$datetime >= fdf_report$first_detected[i] & vue_df$datetime <= fdf_report$last_detected[i])
    if(length(fd_index) > 0){
      vue_df$detection_status[fd_index] = FALSE
    }
  }
  return(vue_df)
}

generate_tagging_detection = function(tagging_df, vue_df){
  #### Constructs a detection corrosponding to the date and location the fish was tagged to the vue dataset
  ## Pulling out a list of all tag ids
  tag_ids = na.exclude(tagging_df$vem_tag_id[tagging_df$vem_tag_id %in% vue_df$tag_id])
  ## Looping through each individual tag
  for (i in 1:length(tag_ids)){
    ## Constructing record for tagging data
    false_record = vue_df[1, ]
    false_record$datetime  = as.POSIXct(tagging_df$datetime[which(tagging_df$vem_tag_id == tag_ids[i])])
    false_record$receiver  = 0
    false_record$tag_id  = tag_ids[i]
    false_record$name  = NA
    false_record$tag_serial  = NA
    false_record$depth = NA
    false_record$sensor.unit  = 1
    false_record$station  = 'Tagging Location'
    false_record$lat  = convert_lat_lon(tagging_df$lat_deg[which(tagging_df$vem_tag_id == tag_ids[i])], tagging_df$lat_min[which(tagging_df$vem_tag_id == tag_ids[i])])
    false_record$lon = convert_lat_lon(tagging_df$lon_deg[which(tagging_df$vem_tag_id == tag_ids[i])], tagging_df$lon_min[which(tagging_df$vem_tag_id == tag_ids[i])])
    false_record$full_tag_id = vue_df$full_tag_id[vue_df$tag_id == tag_ids[i]][1]
    vue_df = rbind(false_record, vue_df)
  }
  if(length(vue_df$date) > 0){
    vue_df$date = as.Date(vue_df$datetime, tz = "HST")
    vue_df$time = strftime(vue_df$datetime, format="%H:%M:%S", tz = "HST")
  }
  # Ordering data by date, then tag, then receiver
  vue_df = vue_df[order(vue_df$datetime, vue_df$tag_id, vue_df$receiver), ]
  return (vue_df)
}

generate_study_date = function(vue_df, receiver_df = NULL, start_date = NULL){
  #### Creating a study date, measured in days. Day zero is either the start date or the first detection in the vue file
  if(is.null(start_date)){
    start_date = floor_date(min(vue_df$datetime), unit = "day")
  }
  vue_df$study_date = as.numeric(difftime(vue_df$datetime, as.POSIXct(start_date), units = 'days'))
  return(vue_df)
}

generate_receiver_study_date = function(receiver_df, vue_df = NULL, first_date = NULL){
  #### Creating a study date, similar to the one constructed in generate_study_date, 
  # First look to see if start date assigned. If no start date, then start with first vue_df date, if no vue_df date, default to earlest deployment date of receivers
  
  #I dont know why we have to do this, but here we are...
  # receiver_df = receiver_df[-(which(is.na(receiver_df$deployment_date) | is.na(receiver_df$recovery_date))), ]
  
  if(is.null(first_date)){ # If there is no date to start from
    if(is.null(vue_df$study_date)){ # Is there a study date in VUE DF? If yes
      first_date = as.character(min(vue_df$datetime)) # if no, first date is the earliest tag detection in the data set
    }else{ # if there is a study date
      first_date = as.character(vue_df$datetime[which.min(vue_df$study_date)] - min(vue_df$study_date)) # first date is datetime with the smallest study date, minus the study date
    } 
  }else{ # If there is a date to start from
    first_date = as.character(first_date) # Then the first date is that date
  }
  receiver_df$deployment_study_date[!is.na(receiver_df$recovery_date)] = as.numeric(difftime(receiver_df$deployment_date[!is.na(receiver_df$recovery_date)], as.POSIXct(first_date, format = "%Y-%m-%d %H:%M:%S"), units = 'days'))
  receiver_df$recovery_study_date[!is.na(receiver_df$recovery_date)] = as.numeric(difftime(receiver_df$recovery_date[!is.na(receiver_df$recovery_date)], first_date, units = 'days'))
  return(receiver_df)
}

load_fdf_report = function(filename = '/Users/stephenscherrer/Desktop/FDF.csv', tag_specs = NULL){
  fdf_report = read.csv(filename, stringsAsFactors = FALSE)
  colnames(fdf_report) = c('tag_id', 'receiver', 'detections', 'min_interval', 'short_intervals', 'long_intervals', 'first_detected', 'last_detected', 'acceptance')
  ## Reclassing date objects
  fdf_report$first_detected = as.POSIXct(fdf_report$first_detected, tz = 'UTC')  
  fdf_report$last_detected = as.POSIXct(fdf_report$last_detected, tz = 'UTC') 
  # Converting to local time
  fdf_report$first_detected = with_tz(fdf_report$first_detected, 'HST')
  fdf_report$last_detected = with_tz(fdf_report$last_detected, 'HST')
  ## Removing any tags that aren't ours.
  if(!is.null(tag_specs)){
    fdf_report = fdf_report[fdf_report$tag_id %in% tag_specs$vue_tag_id, ]
  }
  ## Changing tag format
  for(i in 1:length(fdf_report$tag_id)){
    fdf_report$tag_id[i] = strsplit(fdf_report$tag_id[i], split = "-")[[1]][3]
  }
  ## Changing reciever format
  fdf_report$receiver = gsub(pattern = "VR2AR-", replacement = "", x = fdf_report$receiver)
  fdf_report$receiver = gsub(pattern = "VR2W-", replacement = "", x = fdf_report$receiver)
  ## Changing datetime to local time (HST)
  fdf_report$first_detected
  return(fdf_report)
}

load_receiver_data = function(filename, format = '%m/%d/%y %H:%M', tz = 'HST'){
  #### Loads in .csv file containing receiver deployment and recovery data and cleans up file as appropriate
  ### Loading in datafile
  receiver_df = read.csv(filename, stringsAsFactors = FALSE)
  ### Adjusting Column Names
  colnames(receiver_df)[1] = 'station_name'
  colnames(receiver_df)[2] = 'consecutive_deployment_number'
  colnames(receiver_df)[3] = 'deployment_date'
  colnames(receiver_df)[4] = 'recovery_date'
  colnames(receiver_df)[5] = 'recovered'
  colnames(receiver_df)[6] = 'lat_deg'
  colnames(receiver_df)[7] = 'lat_min'
  colnames(receiver_df)[8] = 'lon_deg'
  colnames(receiver_df)[9] = 'lon_min'
  colnames(receiver_df)[10] = 'depth'
  colnames(receiver_df)[11] = 'vr2w_serial'
  colnames(receiver_df)[12] = 'acoustic_release_serial'
  colnames(receiver_df)[13] = 'acoustic_release_battery_life'
  colnames(receiver_df)[14] = 'acoustic_release_voltage_at_deployment'
  colnames(receiver_df)[15] = 'acoustic_release_serial_code'
  colnames(receiver_df)[16] = 'temperature_logger_serial'
  colnames(receiver_df)[17] = 'deployed_by'
  colnames(receiver_df)[18] = 'recovered_by'
  colnames(receiver_df)[19] = 'comments_deployment'
  colnames(receiver_df)[20] = 'comments_recovery'
  ### Converting deployment and recovery dates to POSIX objects
  receiver_df$deployment_date = as.POSIXct(receiver_df$deployment_date, format = '%m/%d/%y %H:%M', tz = 'HST')
  receiver_df$recovery_date = as.POSIXct(receiver_df$recovery_date, format = '%m/%d/%y %H:%M', tz = 'HST')
  ## Converting latitude and longitude from degree minutes to decimal degrees
  receiver_df$lat = convert_lat_lon(receiver_df$lat_deg, receiver_df$lat_min)
  receiver_df$lon = convert_lat_lon(receiver_df$lon_deg, receiver_df$lon_min)
  ## Converting station depth recorded in fathoms to m. 1 fathom = 1.8288 m
  station_fath_to_m = as.numeric(sapply(strsplit(receiver_df$depth, " "), "[", 1)) * 1.8288
  fath_indicies = which(tolower(sapply(strsplit(receiver_df$depth, " "), "[", 2)) %in% c('fathoms', 'fath'))
  receiver_df$depth[fath_indicies] = station_fath_to_m[fath_indicies]
  receiver_df$depth = as.numeric(gsub(pattern = " m", replacement = "", x = receiver_df$depth))
  return (receiver_df)
}

load_receiver_event_data = function(filename, receiver_df = NULL){
  
  ### Loading in receiver event file
  raw_event_data = read.csv(filename, stringsAsFactors = FALSE)
  
  ### Cleaning event data object
  ## Renaming columns
  colnames(raw_event_data) = c('datetime', 'receiver', 'description', 'data', 'units')
  ## classing datetime as POSIX object
  raw_event_data$datetime = as.POSIXct(raw_event_data$datetime, format = "%Y-%m-%d %H:%M:%S", tz = "HST") - (10 * 60 * 60) # Convert from GMT to HST
  ## Removing VR2W and VR2AR prefixes from receiver IDs
  raw_event_data$receiver = gsub(pattern = "VR2W-", replacement = "", x = raw_event_data$receiver)
  raw_event_data$receiver = gsub(pattern = "VR2AR-", replacement = "", x = raw_event_data$receiver)
  cleaned_event_data = list()
  for(i in 1:length(unique(raw_event_data$description))) {
    cleaned_event_data[[i]] = raw_event_data[raw_event_data$description == unique(raw_event_data$description)[i], ]
  }
  names(cleaned_event_data) = gsub(" ", replacement = "_", tolower(unique(raw_event_data$description)))
  
  ### If we also pass in receiver_df to the receiver_df argument, we clean out any data from before or after a given receiver was in the water
  if(!is.null(receiver_df)){
    ## Loop through each event
    for(k in names(cleaned_event_data)){
      new_receiver_df = c()
      ## For each receiver, subset just the receiver data that matches
      for(j in unique(cleaned_event_data[[k]]$receiver)){
        receiver_df_subset = receiver_df[receiver_df$vr2w_serial == j & (!is.na(receiver_df$recovery_date) | receiver_df$recovered != ""), ]
        ## Only if there is reciever data that matches the receiver in the event data file
        if(dim(receiver_df_subset)[1] > 0){
          ## Loop through each deployment, subset out event data between deployment and recovery for a given receiver, and rbind it back into the dataframe
          for(i in 1:length(receiver_df_subset$deployment_date)){
            deployment_data = receiver_df_subset[i, ]
            new_receiver_df = rbind(new_receiver_df, cleaned_event_data[[k]][which(cleaned_event_data[[k]]$receiver == deployment_data$vr2w_serial, cleaned_event_data[[k]]$datetime > deployment_data$deployment_date & cleaned_event_data[[k]]$datetime < deployment_data$recovery_date), ])
          }
        }
      }
      new_receiver_df$data = as.numeric(new_receiver_df$data)
      ## Replace the original event data with cleaned event data
      cleaned_event_data[[k]] = new_receiver_df
    }
  }
  
  return(cleaned_event_data)
}

load_tagging_data = function(filename){
  #### Loads in .csv file containing fish tagging data and cleans it up as appropriate
  tagging_df = read.csv(filename, stringsAsFactors = FALSE)
  ### Adjusting Column names
  colnames(tagging_df)[1]  <- 'unique_id'
  colnames(tagging_df)[2]  <- 'datetime'
  colnames(tagging_df)[3]  <- 'species'
  colnames(tagging_df)[4]  <- 'conventional_tag_id'
  colnames(tagging_df)[5]  <- 'vem_tag_type'
  colnames(tagging_df)[6]  <- 'vem_tag_serial'
  colnames(tagging_df)[7]  <- 'vem_tag_id'
  colnames(tagging_df)[8]  <- 'fork_length(cm)'
  colnames(tagging_df)[9]  <- 'precaudal_length(cm)'
  colnames(tagging_df)[10] <- 'cohort'
  colnames(tagging_df)[11] <- 'area_of_capture'
  colnames(tagging_df)[12] <- 'depth_of_capture'
  colnames(tagging_df)[13] <- 'lat_deg'
  colnames(tagging_df)[14] <- 'lat_min'
  colnames(tagging_df)[15] <- 'lon_deg'
  colnames(tagging_df)[16] <- 'lon_min'
  colnames(tagging_df)[17] <- 'lat'
  colnames(tagging_df)[18] <- 'lon'
  colnames(tagging_df)[19] <- 'stomach_everted'
  colnames(tagging_df)[20] <- 'eyes_popped'
  colnames(tagging_df)[21] <- 'bladder_vented'
  colnames(tagging_df)[22] <- 'point_of_incision'
  colnames(tagging_df)[23] <- 'dna_clip'
  colnames(tagging_df)[24] <- 'cannulation'
  colnames(tagging_df)[25] <- 'sex'
  colnames(tagging_df)[26] <- 'video'
  colnames(tagging_df)[27] <- 'photo'
  colnames(tagging_df)[28] <- 'photo_name'
  colnames(tagging_df)[29] <- 'audio_log_file'
  colnames(tagging_df)[30] <- 'dropshot'
  colnames(tagging_df)[31] <- 'tissue_sample'
  colnames(tagging_df)[32] <- 'gut_sample'
  colnames(tagging_df)[33] <- 'tagger'
  colnames(tagging_df)[34] <- 'notes'
  colnames(tagging_df)[35] <- 'recaptured'
  colnames(tagging_df)[36] <- 'detections'
  colnames(tagging_df)[37] <- 'comments'
  colnames(tagging_df)[38] <- 'release_method'
  #### Converting tagging date to POSIX object
  tagging_df$datetime = as.POSIXct(tagging_df$datetime, format = '%m/%d/%y %H:%M', tz = 'HST')
  #### Converting lat lon data from degree minutes to decimal degrees
  tagging_df$lat = convert_lat_lon(tagging_df$lat_deg, tagging_df$lat_min)
  tagging_df$lon = convert_lat_lon(tagging_df$lon_deg, tagging_df$lon_min)
  return (tagging_df)
}

load_tag_specs = function(filename){
  #### Loadsin .csv file containing vemco tag specifications of all weng lab tags 
  tag_specs = data.frame(read.csv(filename, stringsAsFactors = FALSE))
  colnames(tag_specs) = c('tag_family', 'tag_serial', 'tag_id', "vue_tag_id", "freq_khz", "est_tag_life_days", "power", 'fixed_delay', 'min_interval_sec', 'max_interval_sec', 'type', 'sync', 'bin', 'sensor_type', 'range', 'units', 'slope', 'intercept', 'calibration', 'rec_blanking_interval')
  return(tag_specs)
}

load_vemco_data = function(filename, format = '%Y-%m-%d %H:%M:%S', tz = 'HST'){
  #### Loads in a csv datafile exported from VUE and cleans up file for further analysis
  ### Loading in data file
  vue_df = read.csv(filename, stringsAsFactors = FALSE)
  ### Adjusting Column names
  colnames(vue_df)[1]  <- 'datetime'
  colnames(vue_df)[2]  <- 'receiver'
  colnames(vue_df)[3]  <- 'tag_id'
  colnames(vue_df)[4]  <- 'name'
  colnames(vue_df)[5]  <- 'tag_serial'
  colnames(vue_df)[6]  <- 'depth'
  colnames(vue_df)[7]  <- 'sensor.unit'
  colnames(vue_df)[8]  <- 'station'
  colnames(vue_df)[9]  <- 'lat'
  colnames(vue_df)[10] <- 'lon'
  ### Converting datetime to POSIXct Object
  vue_df$datetime = as.POSIXct(vue_df$datetime, format = format, tz = 'UTC')
  ## Convert datetime from GMT to HST
  vue_df$datetime = with_tz(vue_df$datetime, 'HST') # Note: HST is 10 Hours behind GMT
  ## Pulling out just date and time components
  vue_df$date = as.Date(vue_df$datetime)
  vue_df$time = strftime(vue_df$datetime, format="%H:%M:%S", tz = "HST")
  ### Cleaning up tag ids - removing the 'A69-####-' prefix
  vue_df$full_tag_id = vue_df$tag_id
  vue_df$tag_id = substring(vue_df$tag_id, 10)
  ### Cleaning up receiver - remvoing the 'VR2W-' Prefix
  if(substr(vue_df$receiver[1], 1, 5) == 'VR2W-'){
    vue_df$receiver = substring(vue_df$receiver, 6)
  }
  vue_df$receiver = abs(as.numeric(vue_df$receiver))
  return (vue_df)
}

remove_detections_before_tagging = function(vue_df, tagging_df, ncores = 8){
  #### Removes all tags not in tagging data as well as removing all detections of tags occurring before the tag is deployed. 
  ### This might occur when a tag was previously deployed for range testing purposes.
  tagging_df$vem_tag_id = as.numeric(as.character(tagging_df$vem_tag_id))
  vue_df$tag_id = as.numeric(as.character(vue_df$tag_id))
  vue_df.filtered = matrix()
  registerDoParallel(cores = ncores)
  vue_df.filtered = foreach(i = 1:length(which(is.na(tagging_df$vem_tag_id) == FALSE)), .combine = rbind) %dopar%{
    return(filter(vue_df, tag_id == tagging_df$vem_tag_id[is.na(tagging_df$vem_tag_id) == FALSE][i], datetime >= tagging_df$datetime[is.na(tagging_df$vem_tag_id) == FALSE][i]))
  }
  return(vue_df.filtered)
}

remove_location_from_vue = function(vue_df, location_to_remove = FALSE){
  #### Removes station(s) provided by location_to_remove argument from vue data
  vue_df = vue_df[-(which(vue_df$station %in% location_to_remove)), ]
}

remove_fish_with_only_tagging_date = function(vue_df){
  rm_tags = c()
  for(i in 1:length(unique(vue_df$tag_id))){
    # print(c(i, sort(unique(vue_df$tag_id))[i]))
    indv_data = vue_df[vue_df$tag_id == unique(vue_df$tag_id)[i], ]
    if(all(indv_data$station == 'Tagging Location')){
      rm_tags = c(rm_tags, indv_data$tag_id[1])
    }
  }
  return(vue_df[!(vue_df$tag_id %in% rm_tags), ])
}


#### Analysis Functions ####
assign_lunar_phase = function(vue_df){
  ### Assigning Lunar Phase
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
  
  ## Assigning lunar phase and week to vue_df stored within the run obj
  vue_df = merge(x = vue_df, y = lunar_weeks, by.x = 'date', by.y = 'day')
  
  return(vue_df)
}

assign_receiver_clusters = function(receiver_df){
  receiver_df$group = NA
  ## Subsetting just to makapuu receivers
  receiver_subset = receiver_df[receiver_df$lat >= 21.25 & receiver_df$lat < 21.46 & receiver_df$lon > -157.7 & receiver_df$lon < -157.5, ]
  receiver_subset$group[receiver_subset$lat > 21.416666] = 'Out North'
  receiver_subset$group[receiver_subset$lat < 21.416666 & receiver_subset$lat >= 21.37] = 'In North'
  receiver_subset$group[receiver_subset$lat < 21.37 & receiver_subset$lat >= 21.32] = 'In Central'
  receiver_subset$group[receiver_subset$lat < 21.32 & receiver_subset$lat >= 21.28333333] = 'In South'
  receiver_subset$group[receiver_subset$lat < 21.28333333] = 'Out South'
  
  ## Assigning these groupings back to receiver data
  for(i in 1:length(unique(receiver_subset$station_name))){
    receiver_df$group[receiver_df$station_name == unique(receiver_subset$station_name)[i]] = receiver_subset$group[receiver_subset$station_name == unique(receiver_subset$station_name)[i]][1]
  }
  ## Print a summary of what went where
  # print(aggregate(receiver_subset$station_name, by = list(receiver_subset$group), FUN = uniqueN))
  return(receiver_df)
}

analyze_diurnal_depth_by_tag = function(vue_dfz){
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

analyze_diurnal_depth_by_tag_by_station = function(vue_df){
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

calculate_brfa_size = function(North, South, East, West, Additional_Point = FALSE){
  #### Function for determining area size of BRFAs by drawing fitting a polygon to the coordinate points  
  poly.mat = rbind(c(West,North), c(East, North), c(East, South), c(West, South))
  if(Additional_Point[1] != FALSE){
    poly.mat = rbind(poly.mat, c(Additional_Point[2], Additional_Point[1]))
  }
  brfa_area = areaPolygon(poly.mat) / 1000000
  return(brfa_area)
}

calculate_brfa_movements_by_track_length = function(brfa_movements, track_length){
  ## Calculates brfa_movements standardized by track length using output from brfa_movements() and time_at_liberty()$days_at_liberty
  brfa_crossings_std = list()
  brfa_crossings_std$total = (brfa_movements$in_to_out + brfa_movements$out_to_in) / track_length
  brfa_crossings_std$mean = mean(brfa_crossings_std$total)
  brfa_crossings_std$stdev = sd(brfa_crossings_std$total)
  brfa_crossings_std$fivenum = fivenum(brfa_crossings_std$total)
  return(brfa_crossings_std)
}

calculate_distance_tracked = function(vue_df){
  ## Function to measure the total distance in km between subsequent receivers
  ## that a fish was detected
  individual_distance = data.frame('tag_id' = sort(unique(vue_df$tag_id)), 'distance_tracked' = 0)
  for (i in 1:length(individual_distance$tag_id)){
    indv_data = vue_df[vue_df$tag_id == individual_distance$tag_id[i], ]
    if(dim(indv_data)[1] > 1){
      for (a in 2:length(indv_data$lon)){
        if (any(indv_data$lat[a] != indv_data$lat[a-1] | indv_data$lon[a] != indv_data$lon[a-1])){
          individual_distance$distance_tracked[i] = individual_distance$distance_tracked[i] + lldist(point1 = c(indv_data$lon[a-1], indv_data$lat[a-1]), point2 = c(indv_data$lon[a], indv_data$lat[a]))
        }
      }
      individual_distance$distance_tracked[i] = round(individual_distance$distance_tracked[i], digits = 2)
    }
  }
  total_distance_tracked = sum(individual_distance$distance_tracked)
  print (sprintf("All fish were tracked a collective distance of %s km", total_distance_tracked))
  return(individual_distance)
}

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

calculate_presence_absence = function(vue_df, print = TRUE, ncores = 8){
  #### Determining days present vs. Absent
  
  emptyout = c()
  registerDoParallel(cores = ncores)
  emptyout = foreach(i = 1:length(sort(unique(vue_df$tag_id)))) %dopar% {
    indv_data = vue_df[vue_df$tag_id == sort(unique(vue_df$tag_id))[i], ]
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
      png(paste(sort(unique(vue_df$tag_id))[i], 'presence-absence histogram.png', sep = ' '))
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

calculate_spatial_evenness = function(vue_df, receiver_df){
  ### function to calculate spacitail eveness based on Pielou 1966 from TinHan 2014
  ## outputs a dataframe first column is tag id, second column is spatial evenness index
  vue_df = vue_df[vue_df$station != 'Tagging Location', ]
  spatial_evenness_df = as.data.frame(matrix(data = 0, nrow = length(sort(unique(vue_df$tag_id))), ncol = 2))
  colnames(spatial_evenness_df) = c('tag_id', 'spatial_evenness_metric')
  spatial_evenness_df$tag_id = sort(unique(vue_df$tag_id))
  R = length(unique(receiver_df$station_name)) #could replaces "station_name" with "zone"
  for(i in 1:length(sort(unique(vue_df$tag_id)))){
    indv_data = vue_df[which(vue_df$tag_id == sort(unique(vue_df$tag_id))[i]), ]
    spatial_sum = c()
    for(a in 1:length(unique(receiver_df$station_name))){
      rho_i = length(indv_data$datetime[which(as.character(indv_data$station) == as.character(receiver_df$station_name[a]))]) / length(indv_data$station)
      spatial_sum = c(spatial_sum, (rho_i * log(rho_i)))
    }
    #print(spatial_sum)
    spatial_evenness_df[i, 2] = (-1 * sum((spatial_sum[spatial_sum != 'NaN']))) / log(R)
  }
  return(spatial_evenness_df)
}

calculate_time_of_day = function(vue_df){
  ## Calculating sunrise, sunset and dawn for each detection using coordinates of Station Makapuu BRFA 15
  sun_times = getSunlightTimes(date = unique(vue_df$date), lat = mean(vue_df$lat, na.rm = T), lon = mean(vue_df$lon, na.rm = T), keep = c("sunrise", "sunset", "nauticalDawn", "nauticalDusk"), tz = "HST")
  sun_times$date = as.Date(sun_times$date, tz = "HST")
  ## Assigning time of day to each detection
  sun_times_by_vue_df = sun_times[match(vue_df$date, sun_times$date), c("nauticalDawn", "sunrise", "sunset", "nauticalDusk")]
  sun_times_by_vue_df$datetime = vue_df$datetime
  suntimes =  as.matrix(sun_times_by_vue_df)
  ## Assigning each detection a time of day by comparing the datetime element to all the other elements. This returns an integer (1-5) which corrosponds to time of day
  # index = apply(suntimes, MARGIN = 1, function(x) length(which(x["datetime"] >= x)))
  vue_df$time_of_day = c('night', 'dawn', 'day', 'dusk', 'night')[apply(suntimes, MARGIN = 1, function(x) length(which(x["datetime"] >= x)))]
  return(vue_df)
}

calculate_time_at_liberty = function(vue_df){
  #### Calculating the number of days between the first and last detection for each tag in the data set
  ## Aggregating to get min and max dates
  time_at_liberty = aggregate(vue_df$date, by = list(vue_df$tag_id), FUN = min)
  colnames(time_at_liberty) = c("tag_id", "min_date")
  time_at_liberty$max_date = aggregate(vue_df$date, by = list(vue_df$tag_id), FUN = max)$'x'
  ## Calculating difference between them. Add 1 to this to get difference inclusive of all dates. ex: Fish tagged 1/1/18 last detection 1/1/18 should have time at liberty = 1 not 0
  time_at_liberty$days_at_liberty = as.numeric(difftime(time_at_liberty$max_date, time_at_liberty$min_date, units = "days") + 1)
  return(time_at_liberty)
}

calculate_track_length = function(vue_df){
  #### Calculating the number of days between the first and last detection for each tag in the data set
  ## Aggregating to get min and max dates
  if('Tagging Location' %in% vue_df$station){
    vue_df = vue_df[vue_df$station != 'Tagging Location', ]
  }
  track_length = aggregate(vue_df$date, by = list(vue_df$tag_id), FUN = min)
  colnames(track_length) = c("tag_id", "min_date")
  track_length$max_date = aggregate(vue_df$date, by = list(vue_df$tag_id), FUN = max)$'x'
  ## Calculating difference between them. Add 1 to this to get difference inclusive of all dates. ex: Fish tagged 1/1/18 last detection 1/1/18 should have time at liberty = 1 not 0
  track_length$days_tracked = as.numeric(difftime(track_length$max_date, track_length$min_date, units = "days") + 1)
  return(track_length)
}

calculate_depth_range_constraint = function(vue_df, bathymetry = NULL){
  ## Calculating depth constraints by looping through each tag ID and finding their max and min depth
  depth_constraints = foreach(i = 1:length(unique(vue_df$tag_id)), .combine = c) %dopar% {
    # Subset each tag
    indv_data = vue_df[vue_df$tag_id == unique(vue_df$tag_id)[i], ]
    # remove first 14 days of detection
    indv_data = indv_data[indv_data$datetime >= (24*60*60) * 14, ]
    # if there is still data after removing first 14 days
    if(dim(indv_data)[1] > 0){
      # Return the extrapolated station depth as well as the 's depth from sensor tags
      return = c(range(indv_data$station_depth, na.rm = TRUE), range(indv_data$depth, na.rm = TRUE))
    }
  }
  # Remove any values that are NA or infinite (these come from bad station info or tags without pressure sensors)
  depth_constraints = depth_constraints[which(!is.infinite(depth_constraints) & !is.na(depth_constraints))]
  # Then make sure we're using negative numbers to represent depth
  depth_constraints = -1*abs(range(vue_df$station_depth, na.rm = TRUE))
  # If there's any tags with pressure sensors, now would be a good time to use that data too
  if(any(!is.na(vue_df$depth))){
    depth_constraints = c(depth_constraints, -1*abs(range(vue_df$depth, na.rm = TRUE)))
  }
  
  ## If bathymetry provided check it against our depth and adjust accordingly
  if(!is.null(bathymetry)){
    depths = get.depth(bathymetry, x = as.matrix(unique(vue_df[, c('lon','lat')])), locator = FALSE)
    if(max(depths$depth) > max(depth_constraints)){
      depth_constraints[which.max(depth_constraints)] = max(depths$depth)
    }
    if(min(depths$depth) < min(depth_constraints)){
      depth_constraints[which.min(depth_constraints)] = min(depths$depth)
    }
  }
  
  print(paste('Depth constrained between', ceiling(max(depth_constraints, na.rm = TRUE)), 'and', floor(min(depth_constraints, na.rm = TRUE)), 'meters'))
  return(c(ceiling(max(depth_constraints, na.rm = TRUE)), floor(min(depth_constraints, na.rm = TRUE))))
}

calculate_distance_between_vue_receivers = function(vue_df){
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

calculate_distance_between_receivers = function(receiver_df, start_date = NULL, end_date = NULL, include_lost = FALSE){
  receiver_df = receiver_df[order(receiver_df$station_name), ]
  
  if(!is.null(start_date)){
    receiver_df = receiver_df[receiver_df$deployment_date <= start_date, ]
  }
  if(!is.null(end_date)){
    receiver_df = receiver_df[which(receiver_df$recovery_date >= end_date | is.na(receiver_df$recovery_date)), ]
  }else{
    receiver_df = receiver_df[which(receiver_df$recovery_date > start_date | is.na(receiver_df$recovery_date)), ]
  }
  if(!include_lost){
    receiver_df = receiver_df[which(receiver_df$recovered == ""), ]
  }
  
  distances_between_receivers = list()
  distances_between_receivers$matrix = matrix(0, length(receiver_df$lat), length(receiver_df$lat))
  for(i in 1:length(receiver_df$lat)){
    for (a in 1:length(receiver_df$lat)){
      if(a != i){ # Added because when i = 17 and a = 17, lldist was producing NaNs.
        distances_between_receivers$matrix[i, a] = lldist(point1 = c(receiver_df$lon[i], receiver_df$lat[i]), point2 = c(receiver_df$lon[a], receiver_df$lat[a]))
      } else {
        distances_between_receivers$matrix[i, a] = NA
      }
    }
  }
  
  colnames(distances_between_receivers$matrix) = receiver_df$station_name
  rownames(distances_between_receivers$matrix) = receiver_df$station_name
  distances_between_receivers$mean = mean(distances_between_receivers$matrix[which(distances_between_receivers$matrix > 0 & is.na(distances_between_receivers$matrix)==FALSE)])
  distances_between_receivers$sd = sd(distances_between_receivers$matrix[which(distances_between_receivers$matrix > 0 & is.na(distances_between_receivers$matrix)==FALSE)])
  distances_between_receivers$iqr = fivenum(distances_between_receivers$matrix[which(distances_between_receivers$matrix > 0 & is.na(distances_between_receivers$matrix)==FALSE)])[c(2,4)]
  return(distances_between_receivers)
}

calculate_maximum_movement = function(vue_df, bathymetry = NULL, plot_results = FALSE){
  # calculate_maximum_movement_df = data.frame()
  ## First determine depth constraints for least cost path analysis
  depth_constraints = calculate_depth_range_constraint(vue_df, bathymetry = bathymetry)
  polygon_area = foreach(i = 1:length(unique(vue_df$tag_id)), .combine = c) %dopar% {
    # print(i)
    indv_data = vue_df[vue_df$tag_id == sort(unique(vue_df$tag_id))[i], ]
    
    ## If thre are more than 3 points, getting the area of the max polygon in square km
    max_polygon_area = NA
    if(length(unique(indv_data$station)) >= 3){
      # square km distance of polygon with conversion to km
      max_polygon_area = areaPolygon(cbind(indv_data$lon, indv_data$lat)) * 1*10^-6
    }
    return(max_polygon_area)
  }
  
  linear_dist = foreach(i = 1:length(unique(vue_df$tag_id)), .combine = c) %dopar% {
    indv_data = vue_df[vue_df$tag_id == sort(unique(vue_df$tag_id))[i], ]
    
    ## Calculate the maximum distance traveled for everyone!
    max_lat = c(indv_data$lon[which.max(indv_data$lat)], indv_data$lat[which.max(indv_data$lat)])
    max_lon = c(indv_data$lon[which.max(indv_data$lon)], indv_data$lat[which.max(indv_data$lon)])
    min_lat = c(indv_data$lon[which.min(indv_data$lat)], indv_data$lat[which.min(indv_data$lat)])
    min_lon = c(indv_data$lon[which.min(indv_data$lon)], indv_data$lat[which.min(indv_data$lon)])
    max_min_coordinates = rbind(max_lat, max_lon, min_lat, min_lon)
    
    # linear distance in km between two points with conversion to km
    max_linear = 0
    for(k in 1:dim(unique(max_min_coordinates))[1]){
      for(r in 1:dim(unique(max_min_coordinates))[1]){
        max_linear_dist = distGeo(p1 = unique(max_min_coordinates)[k, ], p2 = unique(max_min_coordinates)[r, ]) / 1000
        if(max_linear_dist > max_linear){
          max_linear = max_linear_dist
        }
      }
    }
    return(max_linear)
  }
  
  # Z Constrained Least Cost Path Analysis
  z_constrained_dist = rep(NA, length(unique(vue_df$tag_id)))
  
  if(!is.null(bathymetry)){
    #z_constrained_dist = foreach(i = 1:length(unique(vue_df$tag_id)), .combine = c) %dopar% {
    z_constrained_dist = c()
    for(i in 1:length(unique(vue_df$tag_id))){
      # print(i)
      indv_data = vue_df[vue_df$tag_id == sort(unique(vue_df$tag_id))[i], ]
      locations = as.matrix(unique(indv_data[ ,c("lon", "lat")]))
      trans1 = trans.mat(bathymetry, min.depth = max(depth_constraints), max.depth = min(depth_constraints))
      all_constrained_paths = lc.dist(trans1, locations, res = "dist")
      z_constrained_dist =  c(z_constrained_dist, max(all_constrained_paths))
      #return(max(all_constrained_paths))
      
      
      if(plot_results == TRUE){
        
        pdf(paste(indv_data$tag_id[1], 'constrained path.pdf'), height = 8, width = 8)
        plot(bathymetry, main = indv_data$tag_id[1])
        
        path_to_plot = lc.dist(trans1, as.data.frame(locations), res = c("path"))
        all_constrained_paths = lc.dist(trans1, locations, res = "dist")
        
        lines(path_to_plot[[which.max(all_constrained_paths)]], col = 'red', lwd = 3)
        
        brfa_e = as.data.frame(rbind(c(-157.68333333, 21.28333333), 
                                     c(-157.53333333, 21.28333333), 
                                     c(-157.53333333, 21.4166666), 
                                     c(-157.68333333, 21.4166666)))
        colnames(brfa_e) = c('lon', 'lat')
        lines(lat~lon, data = brfa_e, pch = 19, col = 'purple', lwd = 3, cex = .6)
        
        dev.off()
      }
    }
  }
  
  calculate_maximum_movement_df = as.data.frame(cbind(sort(unique(vue_df$tag_id)), polygon_area, linear_dist, z_constrained_dist), stringsAsFactors = FALSE)
  colnames(calculate_maximum_movement_df) = c("tag_id", "max_polygon_area", "max_linear_distance", "z_constrained_path_distance")
  calculate_maximum_movement_df$max_polygon_area = as.numeric(calculate_maximum_movement_df$max_polygon_area)
  calculate_maximum_movement_df$max_linear_distance = as.numeric(calculate_maximum_movement_df$max_linear_distance)
  calculate_maximum_movement_df$z_constrained_path_distance = as.numeric(calculate_maximum_movement_df$z_constrained_path_distance)
  return(calculate_maximum_movement_df)
}

count_brfa_movements = function(vue_df){
  vue_df$in_brfa = in_brfa_e(vue_df$lat, vue_df$lon)
  time_in_out = data.frame('tag_id' = unique(vue_df$tag_id), 'total_brfa_crossings' = 0, 'in_to_out' = 0, 'out_to_in' = 0, 'days_tracked_in' = 0, 'days_tracked_out' = 0, stringsAsFactors = FALSE)
  for(i in 1:length(unique(vue_df$tag_id))){
    indv_data = vue_df[vue_df$tag_id == unique(vue_df$tag_id)[i], ]
    for(r in 1:length(indv_data$datetime)){
      ## Dealing with time tracked in
      if(r == 1){ 
        start_time = indv_data$datetime[1]
      }else if(r == length(indv_data$datetime)){
        elapsed_time = as.numeric(difftime(indv_data$datetime[r], start_time, units = 'days'))
        if(indv_data$in_brfa[r] == TRUE){
          time_in_out[i, "days_tracked_in"] = time_in_out[i, "days_tracked_in"] + elapsed_time
        }else if(indv_data$in_brfa[r] == FALSE){
          time_in_out[i, "days_tracked_out"] = time_in_out[i, "days_tracked_out"] + elapsed_time
        }
      }else{
        ## If you are now in the BRFA, and you weren't before
        if(indv_data$in_brfa[r] != indv_data$in_brfa[r-1]){
          elapsed_time = as.numeric(difftime(indv_data$datetime[r], start_time, units = 'days'))
          # Set a new start time
          start_time = indv_data$datetime[r]
          ## If you have moved from inside to outside
          if(indv_data$in_brfa[r-1] == TRUE){
            ## End time tracked in and add results
            time_in_out[i, "days_tracked_in"] = time_in_out[i, "days_tracked_in"] + elapsed_time
            ## Increment move counter for in to out
            time_in_out[i, "in_to_out"] =   time_in_out[i, "in_to_out"]  + 1
            ## If you have moved from outside to inside
          }else if(indv_data$in_brfa[r-1] == FALSE){
            ## End time tracked out and add results
            time_in_out[i, "days_tracked_out"] = time_in_out[i, "days_tracked_out"] + elapsed_time
            ## Increment move counter for out to in
            time_in_out[i, "out_to_in"] =   time_in_out[i, "out_to_in"]  + 1
          }
        }
      }
    }
  }
  time_in_out$days_tracked_in = round(time_in_out$days_tracked_in, digits = 2)
  time_in_out$days_tracked_out = round(time_in_out$days_tracked_out, digits = 2)
  time_in_out$total_brfa_crossings = time_in_out$in_to_out + time_in_out$out_to_in
  return(time_in_out)
}

count_days_before_detection = function(vue_df){
  #### Calculating the number of days between when the fish was tagged and when it was first detected on the array
  days_before_detection = aggregate(vue_df$datetime[vue_df$station == "Tagging Location"], by = list(vue_df$tag_id[vue_df$station == "Tagging Location"]), FUN = unique)
  colnames(days_before_detection) = c('tag_id', 'tagging_date')
  vue_df = vue_df[vue_df$tag_id %in% days_before_detection$tag_id, ]
  days_before_detection$first_detection = aggregate(vue_df$datetime[vue_df$station != "Tagging Location"], by = list(vue_df$tag_id[vue_df$station != "Tagging Location"]), FUN = min)$x
  days_before_detection$days_before_detection = as.numeric(difftime(days_before_detection$first_detection, days_before_detection$tagging_date, units = 'days'))
  return(days_before_detection)
}

count_days_detected = function(vue_df){
  #### Calculating the number of unique days a tag was detected
  days_detected = aggregate(vue_df$date, by = list(vue_df$tag_id), FUN = uniqueN)
  colnames(days_detected) = c("tag_id", "unique_days")
  return(days_detected)
}

count_movements = function(vue_df){
  movements = data.frame("tag_id" = sort(unique(vue_df$tag_id)), "movements_detected" = 0)
  for(i in 1:length(movements$tag_id)){
    indv_data = vue_df[vue_df$tag_id == movements$tag_id[i], ]
    movements$movements_detected[i] = length(which(indv_data$station[2:length(indv_data$station)] != indv_data$station[1:(length(indv_data$station)-1)]))
  }
  return(movements)
}

count_unique_detections = function(vue_df){
  unique_detections = aggregate(vue_df$datetime, by = list(vue_df$tag_id), FUN = uniqueN)
  colnames(unique_detections) = c("tag_id", "n_detections")
  return(unique_detections)
}

count_unique_stations = function(vue_df){
  unique_stations = aggregate(vue_df$station, by = list(vue_df$tag_id), FUN = uniqueN)
  colnames(unique_stations) = c('tag_id', 'n_stations')
  return(unique_stations)
}

determine_track_status = function(vue_df, bathymetry = get_bathymetry('mhi', 'low')){
  ## Loop
  mortality_status = foreach(i = 1:length(unique(vue_df$tag_id)), .combine = rbind) %dopar% {
    rationalle = 'No Tag Detections'
    indv_data = vue_df[vue_df$tag_id == unique(vue_df$tag_id)[i], ]
    ## Is the tag detected after 14 days?
    after_index = difftime(indv_data$datetime, min(indv_data$datetime)) > (14*24*60*60)
    if(any(after_index == TRUE)){
      rationalle = paste(rationalle, 'Tag Detected after 14 days')
      ## Is the tag detected moving a distance > 2 km after 14 days?
      locations = as.matrix(unique(indv_data[ after_index,c("lon", "lat")]))
      trans1 = trans.mat(bathymetry, min.depth = -1, max.depth = -10000)
      if(any(lc.dist(trans1, locations, res = "dist") > 2.2)){
        status = 'Alive'
        rationalle = paste(rationalle, 'Observed Movement More Than 2km')
      } else {
        rationalle = paste(rationalle, 'Observed Movement Less Than 2 km')
        ### Does the tag have a depth transmitter?
        absolute_range = max(range(indv_data$depth[after_index])) - min(range(indv_data$depth[after_index])) 
        if (!is.na(absolute_range)){ # If yes...
          ## Does the absolute range after 14 days differ by more than 10 m?
          if(absolute_range > 10) {
            status = 'Alive'
            rationalle = paste(rationalle, 'Depth range after 14 days greater than 10 m')
          } else {
            status = 'Dead'
            rationalle = paste(rationalle, 'Depth range after 14 days less than 10 m', sep = ',')
            if (length(unique(indv_data$station[-after_index])) > 3 + ("Tagging Location" %in% indv_data$station)) {
              rationalle = paste(rationalle, 'Tag detected at 4 or more stations in first 14 days', sep = ',')
            } else {
              rationalle = paste(rationalle, 'Tag detected at < 4 stations in first 14 days', sep = ',')
            }
          }
          ## If the tag does not have a depth transmitter
          ### Is the tag detected at 4 or more stations in the first 14 days?
        } else if (length(unique(indv_data$station[-after_index])) > 3 + ("Tagging Location" %in% indv_data$station)) {
          status = 'Dead'
          rationalle = paste(rationalle, 'Tag detected at 4 or more stations in first 14 days', sep = ',')
        } else {
          status = 'Unknown'
          rationalle = paste(rationalle, 'Tag detected at < 4 stations in first 14 days', sep = ',')
        }
      }
    } else {
      status = 'Excluded From Analysis'
      rationalle = paste(rationalle, 'Tag Detected < 14 days', sep = ',')
      if (length(unique(indv_data$station)) > 3 + ("Tagging Location" %in% indv_data$station)) {
        rationalle = paste(rationalle, 'Tag detected at 4 or more stations in first 14 days', sep = ',')
      } else {
        rationalle = paste(rationalle, 'Tag detected at < 4 stations in first 14 days', sep = ',')
      }
      if (in_brfa_e(indv_data$lat[length(indv_data$lat)], indv_data$lon[length(indv_data$lon)])){
        rationalle = paste(rationalle, 'Tag last detected in BRFA E', sep = ',')
      } else {
        rationalle = paste(rationalle, 'Tag last detected outside BRFA E', sep = ',')
      }
    }
    write_df_line = data.frame('tag_id' = unique(vue_df$tag_id)[i], 'status' = status, 'rationalle' = rationalle )
    return(write_df_line)
  }
  
  mortality_status = mortality_status[order(mortality_status$tag_id), ]
  write.csv(mortality_status, file.path(results_dir, 'tag_mortality_status.csv'))
  
  track_status = list()
  track_status$status_df = mortality_status
  track_status$valid_tracks = mortality_status$tag_id[mortality_status$status == 'Alive']
  track_status$unknown_tracks = mortality_status$tag_id[mortality_status$status == 'Unknown']
  track_status$dead_tracks = mortality_status$tag_id[mortality_status$status == 'Dead']
  track_status$excluded_tracks = mortality_status$tag_id[mortality_status$status == 'Excluded From Analysis']
  return(track_status)
}

extrapolate_detection_depth_from_receiver_df = function(vue_df, receiver_df){
  vue_df$station_depth = NA
  for(i in 1:length(receiver_df$deployment_date)){
    if(!is.na(as.numeric(receiver_df$depth[i]))){
      vue_df$station_depth[which(vue_df$receiver == receiver_df$vr2w_serial[i]  & vue_df$datetime >= receiver_df$deployment_date[i] & vue_df$datetime <= receiver_df$recovery_date[i])] = -as.numeric(receiver_df$depth[i])
    }
  }
  return(vue_df)
}

generate_detection_matrix = function(vue_df){
  #### Building a matrix of tags by receiver where numbers corrospond to the number of detections for each tag at a given station
  ## Outputs a matrix where each row corrosponds to a unique tag in the vue database 
  ## and each column corrosponds to a unique receiver. Values of each index are the number
  ## of detections for each tag at a particular location.
  detection_data_frame = aggregate(vue_df$datetime, by = list(vue_df$tag_id, vue_df$station), FUN  = length)
  colnames(detection_data_frame) = c('tag_id', 'station', 'detections')
  detection_matrix = xtabs(detection_data_frame$detections ~ detection_data_frame$tag_id + detection_data_frame$station)
  return(detection_matrix)
}

generate_graph = function(vue_df, receiver_df, tag_ids = NULL, start_date = NULL, end_date = NULL, igraph=TRUE, binary=FALSE, removeLoops=FALSE, remove_zeros = FALSE, remove_tagging_location = TRUE){
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
  if (!is.null(tag_ids[1])){ # if no tag ids specified, all tag ids used
    vue_df = vue_df[vue_df$tag_id %in% tag_ids, ]
  }
  
  if(!is.null(start_date)) {
    vue_df = vue_df[which(vue_df$datetime >= start_date), ]
    receiver_df = receiver_df[receiver_df$deployment_date <= start_date & receiver_df$recovery_date > start_date, ]
  }
  
  if(!is.null(end_date)) {
    vue_df = vue_df[which(vue_df$datetime < end_date), ]
    receiver_df = receiver_df[receiver_df$deployment_date <= end_date & receiver_df$recovery_date > end_date, ]
  }
  
  # Removing tagging location if it is present
  if(remove_tagging_location == TRUE){
    vue_df = vue_df[which(vue_df$station != 'Tagging Location'), ]
  }
  
  # Order vue_df by tag id
  vue_df = vue_df[order(vue_df$tag_id, vue_df$datetime), ]
  
  # Build adjacency matrix, with receivers as nodes and fish 
  # movements as edges
  # Rows indicate movement from a receiver
  # Columns indicate movement to a receiver
  adj_matrix = matrix(data = 0, nrow = length(unique(vue_df$station)), ncol = length(unique(vue_df$station)))
  colnames(adj_matrix) = sort(unique(vue_df$station))
  rownames(adj_matrix) = sort(unique(vue_df$station))
  # If station changes, increase adjacency matrix value by one
  
  for (i in 2:length(vue_df$station)){
    # print(i)
    if(vue_df$tag_id[i] == vue_df$tag_id[i-1]){
      prevLoc = vue_df$station[i-1]
      newLoc = vue_df$station[i]
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

get_fork_length = function(vue_df, tagging_df){
  fork_length = data.frame('tag_id' = sort(unique(vue_df$tag_id)), 'fork_length_cm' = NA, stringsAsFactors = FALSE)
  for(i in 1:length(fork_length$tag_id)){
    fork_length$"fork_length_cm"[i] = tagging_df$"fork_length(cm)"[which(tagging_df$vem_tag_id == fork_length$tag_id[i])]
  }
  return(fork_length)
}

get_tagging_date = function(vue_df){
  tagging_dates = aggregate(vue_df$datetime[vue_df$station == "Tagging Location"], by = list(vue_df$tag_id[vue_df$station == "Tagging Location"]), FUN = unique)
  colnames(tagging_dates) = c("tag_id", "tagging_date")
  return(tagging_dates)
}

in_brfa_e = function(lat, lon){
  ## Returns T/F indicating if detection occurred within the boundaries of BRFA E
  (lat > 21.28333333 & lat < 21.4166666) & # BRFA E
    (lon > -157.6833333 & lon < -157.533333) # west and east
}

in_brfa_f = function(lat, lon){
  ## Returns T/F indicating if detection occurred within the boundaries of BRFA F
  (lat > 20.9166666 & lat < 21.03333333) & # BRFA F # south and north
    (lon > -157.566666 & lon < -157.3666666) # west and east
} 

lc.dist <- function(trans,loc,res=c("dist","path")) {
  ### A modification of the lc.dist function from marmap to avoid rounding
  
  require(gdistance)
  require(sp)
  
  min.depth <- trans@history[[1]]
  max.depth <- trans@history[[2]]
  bathymetry <- trans@history[[3]]
  trans@history <- list()
  
  loc.depth <- get.depth(bathymetry, x = loc, locator = FALSE)
  
  if (any(loc.depth[,3] > min.depth) | any(loc.depth[,3] < max.depth)) print(loc.depth)
  if (any(loc.depth[,3] > min.depth) | any(loc.depth[,3] < max.depth)) warning(paste("One or more points are located outside of the [",min.depth, ";", max.depth,"] depth range. You will get unrealistically huge distances unless you either increase the range of possible depths in trans.mat() or you move the problematic points in a spot where their depths fall within the [",min.depth, ";", max.depth,"] depth range.\nYou can use get.depth() to determine the depth of any point on a bathymetric map", sep=""))
  
  if (res=="dist") {
    cost <- gdistance::costDistance(trans,as.matrix(loc))/1000
    return(round(cost, digits = 3))
  }
  
  if (res=="path") {
    nb.loc <- nrow(loc)
    path <- list()
    comb <- combn(1:nb.loc,2)
    pb <- txtProgressBar(min = 0, max = ncol(comb), style = 3)
    
    for (i in 1:ncol(comb)) {
      origin <- sp::SpatialPoints(loc[comb[1,i],])
      goal <- sp::SpatialPoints(loc[comb[2,i],])
      
      temp <- gdistance::shortestPath(trans,origin,goal,output="SpatialLines")
      path[[i]] <- temp@lines[[1]]@Lines[[1]]@coords
      
      setTxtProgressBar(pb, i)
    }
    
    close(pb)
    return(path)
  }
  
}

list_stations_detected = function(vue_df){
  #### Creates lists of stations each tag was detected at. One list is the unique stations detected, the second is a list of each subsequent station
  detected_list = list()
  detected_list$unique = list()
  detected_list$all = list()
  for(i in 1:length(sort(unique(vue_df$tag_id)))){
    indv_data = vue_df[vue_df$tag_id == sort(unique(vue_df$tag_id))[i], ]
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

#### Plotting Functions
assign_color_palette = function(vue_df){
  #### Assigning a unique color to each Station
  
  color_palette = list()
  color_palette$colors = rainbow(length(unique(vue_df$station)))
  color_palette$station = unique(vue_df$station)
  return(color_palette)
}

generate_gif_images = function(vue_df, receiver_df = receiver_df, tag_ids = NULL, start_date = NULL, end_date = NULL, individuals = TRUE, region = 'Oahu and Penguin Banks', bathymetry = NULL){
  if(!is.null(tag_ids[1])){
    vue_df = vue_df[vue_df$tag_id %in% tag_ids, ]
  }
  if(!is.null(start_date)){
    vue_df = vue_df[vue_df$datetime >= as.POSIXct(start_date), ]
  }
  if(!is.null(end_date)){
    vue_df = vue_df[vue_df$datetime <= as.POSIXct(end_date), ]
  }
  if(is.null('bathymetry')){
    if(region == 'Makapuu'){
      bathymetry = getNOAA.bathy(lon1 = -157.8, 
                                 lon2 = -157.5, 
                                 lat1 = 21.2, 
                                 lat2 = 21.5,
                                 resolution = 1)
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
  paka_raster = readPNG(file.path(src_dir, 'Opakapaka.png'))
  parent_dir = getwd()
  for(i in 1:length(unique(vue_df$tag_id))){
    indv_data = vue_df[vue_df$tag_id == unique(vue_df$tag_id)[i], ]
    # creating a subdirectory to dump all images created in
    dir.name = paste(unique(vue_df$tag_id)[i], 'GIF image dump', sep = " ")
    gif.dir = create_save_directory(dir.name)
    setwd(gif.dir)
    # a range of dates to plot
    date_range = seq.POSIXt(from = floor_date(min(indv_data$datetime), unit = 'day'), to = ceiling_date(max(indv_data$datetime), unit = 'day'), by = "day")
    for(r in 1:length(date_range)){
      subset_receiver_df = receiver_df[which(receiver_df$deployment_date <= date_range[r] & receiver_df$recovery_date >= date_range[r]), ]
      subset_indv_data = indv_data[which(indv_data$datetime >= date_range[r] & indv_data$datetime <= (date_range[r] + 60*60*24)), ]
      png(paste(indv_data$tag_id[1], ' ', date_range[r], '.png', sep = ""), width = 800, height = 800)
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
      ## Plotting locations of receivers present that day
      if(dim(subset_receiver_df)[1] != 0){
        points(lat~lon, data = subset_receiver_df, pch = 19, col = 'red', cex = 1)
        ## Getting lat lon coordinates of fish on that day
        lat_lons = unique(data.frame(x = subset_indv_data$lon, y = subset_indv_data$lat))
        # if the fish was present, plot the raster image of a paka at the positions the fish was
        if(dim(subset_indv_data)[1] >= 1){
          rasterImage(image = paka_raster, xleft =lat_lons$x - .02, xright = lat_lons$x + .02, ybottom = lat_lons$y - .01, ytop = lat_lons$y + .01)
        }
        # If the fish was in more than one place, draw a green line to indicate this
        if(dim(subset_indv_data)[1] >1){
          lines(lat~lon, data = subset_indv_data, col = 'lightpink2', cex = 2)
        }
      }
      dev.off()
    }
    command = paste("convert -delay .5 -loop 1 *.png ", unique(vue_df$tag_id)[i], ".gif", sep = "")
    system(command)
    file.remove(list.files(pattern=".png"))
    setwd(parent_dir)
  }
}

plot_stripchart = function(vue_df, color_palette = "black"){
  if(class(color_palette) != "list"){
    palette = list()
    palette$colors = rep(color_palette, length(unique(vue_df$station)))
    palette$station = unique(vue_df$station)
    color_palette = palette
  }else{
    color_palette = color_palette
  }
  for(i in 1:length(unique(vue_df$tag_id))){
    indv_data = vue_df[vue_df$tag_id == unique(vue_df$tag_id)[i], ]
    title = sprintf('%s Stripchart.png', unique(vue_df$tag_id)[i])
    pdf(title, width = 22, height = 17)
    par(mfrow = c(1,1))
    plot_title = sprintf('Detections of Tag %s', unique(vue_df$tag_id)[i])
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

plot_clusters = function(receiver_data, start_date = NULL, end_date = NULL, region = "Makapuu"){
  #### Plotting cluster analysis
  
  
  # setwd(fig.dir)
  # if(tag_ids[1] != FALSE){
  #   vue_data = vue_data[vue_data$tag_id %in% tag_ids, ]
  # }
  subset_receiver_data = receiver_data
  if(!is.null(start_date)){
    subset_receiver_data = receiver_data[which(subset_receiver_data$deployment_date <= as.POSIXct(start_date) &
                                                 subset_receiver_data$recovery_date > as.POSIXct(start_date)), ]
  }
  if(!is.null(end_date)){
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
                                 resolution = 1)
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

plot_day_night = function(vue_df, receiver_df = NULL, color_palette = FALSE, start_date = NULL, end_date = NULL, date_format = "%Y-%m-%d %H:%M:%S", save_plot = TRUE, plot_title = FALSE){
  ### Function to produce day night plots modified from code provided by Alex Filous with individual days on X axis and time of day on Y axis. Shading represents night/day
  ## If provided, receiver_df argument will take deployment and recovery dates and score plot vertically with a bar on dates that equipment was serviced
  if(is.null(start_date)){
    start_date = min(vue_df$datetime)
  }else if(!is.null(start_date)){
    start_date = as.POSIXct(start_date, date_format)
  }
  if(is.null(end_date)){
    end_date = max(vue_df$datetime)
  }else if(!is.null(end_date)){
    end_date = as.POSIXct(end_date, date_format)
  }
  vue_df = vue_df[vue_df$datetime >= start_date, ]
  vue_df = vue_df[vue_df$datetime <= end_date, ]
  
  ### If receiver_df argument is present, pull relevant dates for plotting vetical bars
  if(is.null(receiver_df) == FALSE){
    ### Getting dates when receivers in the study were rearranged.
    receiver_df_subset = receiver_df[receiver_df$station_name %in% unique(vue_df$station), ] # First subset only stations where a fish was detected
    receiver_dates = unique(c(trunc(receiver_df_subset$deployment_date, 'day'), trunc(receiver_df_subset$recovery_date, 'day'))) # Then pull out unique dates on which those stations were serviced
    receiver_dates = receiver_dates[which(receiver_dates >= floor_date(min(vue_df$datetime)) & receiver_dates <= ceiling_date(max(vue_df$datetime)))]
  }
  
  if(class(color_palette) == 'logical'){
    vue_df$plot_color = "black"
  }else if(class(color_palette$colors) == "character"){
    station_colors = as.data.frame(cbind(color_palette$colors, color_palette$station))
    colnames(station_colors) = c('plot_color', 'station')
    vue_df = merge(vue_df, station_colors)
  }
  
  # Creating plot_date and plot_time columns
  vue_df$plot_date  = as.POSIXlt(as.character(vue_df$datetime),format = "%Y-%m-%d")
  vue_df$plot_time = as.numeric(format(as.POSIXlt(vue_df$datetime,format= "%H:%M:%S"),'%H'))+as.numeric(format(as.POSIXlt(vue_df$datetime,format= "%H:%M:%S"),'%M'))/60+as.numeric(format(as.POSIXlt(vue_df$datetime,format= "%H:%M:%S"),'%S'))/3600
  
  for(i in 1:length(unique(vue_df$tag_id))){
    #print(i)
    ### Setting up graphics window
    if(save_plot == TRUE){
      title = paste(unique(vue_df$tag_id)[i], "Day Night Detection Plot.png")
      png(title)
      par(mfcol=c(1,1))
    }
    
    if(plot_title == FALSE){
      main_tag_id = NULL
    } else {
      main_tag_id = unique(vue_df$tag_id)[i]
    }
    
    ### Subsetting data
    indv_data = vue_df[vue_df$tag_id == unique(vue_df$tag_id)[i],]
    with(vue_df, plot(plot_date,plot_time,col=plot_color,main=main_tag_id,pch=19,cex=1,ylim=c(0,24),type="n", xaxt = 'n',xaxs="i",yaxs="i",xlab="", ylab="Time of Day (HST - 24 hr.)"))
    date_labels = as.character(strptime(seq(min(vue_df$datetime), max(vue_df$datetime), length.out = 10), format = '%Y-%m-%d'))
    axis(side = 1, at = seq(min(vue_df$plot_date), max(vue_df$plot_date), length.out = 10), labels = date_labels, las = 2, cex = .25)
    #if (nrow(indv_data)>1){
    
    sundate<-seq(min(vue_df$plot_date),max(vue_df$plot_date), by="day")
    
    sundateJ<-as.numeric(format(seq(min(vue_df$plot_date),max(vue_df$plot_date), by="day"),"%j"))
    
    
    
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
    if(is.null(receiver_df) == FALSE){
      abline(v = as.numeric(receiver_dates), col = 'blue')
    }
    if(save_plot == TRUE){
      dev.off()
    }
  }
}

plot_depths = function(vue_df, color_palette = FALSE, save_plot = TRUE, plot_title = TRUE){
  for(i in 1:length(unique(vue_df$tag_id))){
    indv_data = vue_df[vue_df$tag_id == unique(vue_df$tag_id)[i], ]
    indv_data = indv_data[order(indv_data$datetime), ]
    if(!is.na(indv_data$depth[length(indv_data$depth)])){
      if(save_plot == TRUE){
        png(paste(indv_data$tag_id[1], 'depth history.png'))
      }
      
      if(plot_title == FALSE){
        main_tag_id = NULL
      } else {
        main_tag_id = unique(vue_df$tag_id)[i]
      }
      
      plot(y = indv_data$depth,  x = indv_data$date, main = main_tag_id, ylim = c(min(indv_data$depth, na.rm = T), 0), col = 'black', xaxt = 'n', xlab = '', ylab = 'Depth (m)')
      axis(side = 1, at = seq(min(indv_data$date), max(indv_data$date), length.out = 10), labels = seq(min(indv_data$date), max(indv_data$date), length.out = 10), las = 2, cex = .25)
      lines(y = indv_data$depth,  x = indv_data$date, col = 'grey')
      
      if(is.list(color_palette)){
        station_pal = as.data.frame(cbind(color_palette$colors, color_palette$station))
        colnames(station_pal) = c('color', 'station')
        indv_data = merge(x = indv_data, y = station_pal)
        points(y = indv_data$depth,  x = indv_data$date, col = indv_data$color, pch = 19)
      }
      
      if(save_plot == TRUE){
        dev.off()
      }
    }
  }
}

plot_detection_records = function(vue_df, receiver_df = receiver_df, start_date = as.POSIXct("2017-06-26 HST"), end_date = NULL, color_palette = NULL){
  
  ### Plotting Depth by Time of Day Coded by Station
  
  if(is.null(start_date)){
    start_date = min(vue_df$datetime)
  }
  if(is.null(end_date)){
    end_date = max(vue_df$datetime) + .01
  }
  
  if(is.null(color_palette)){
    color_palette = assign_color_palette(vue_df)
  }
  
  vue_df = vue_df[which(vue_df$datetime >= start_date & vue_df$datetime < end_date), ]
  receiver_df = receiver_df[which(receiver_df$deployment_date <= end_date & receiver_df$recovery_date >= start_date & !is.na(receiver_df$recovery_date)), ]
  
  ## For tags without depth sensors, inferring depth from station deployment records
  vue_df = extrapolate_detection_depth_from_receiver_df(vue_df, receiver_df)
  
  ## Removing anything that now does not have a depth association
  vue_df = vue_df[-which(is.na(vue_df$station_depth)), ]
  
  
  vue_df = merge(vue_df, color_palette)
  receiver_df = merge(receiver_df, color_palette, by.x = 'station_name', by.y = 'station', all.x = TRUE)
  receiver_df$colors = as.character(receiver_df$colors)
  receiver_df$colors[is.na(receiver_df$colors)] = 'grey'
  
  receiver_df$station_number = sapply(strsplit(receiver_df$station_name, " "), "[[", 5)
  
  
  pdf('Map of Recovered Receiver Stations Color Coded.pdf', height = 8.5, width = 8.5)
  par(mfrow = c(1,1))
  if(is.null(bathymetry)){
    bathymetry = get_bathymetry(region = 'Makapuu', resolution = 'Medium')
  }
  
  plot(bathymetry)
  #   points(lat~lon, data = receiver_df[which(receiver_df$recovered != ""), ], pch = 1, col = 'grey', cex = 2)
  points(lat~lon, data = receiver_df[which(receiver_df$recovered == ""), ], pch = 19, col = receiver_df$colors, cex = 2)
  text(lat ~ lon, data = receiver_df, labels = receiver_df$station_number, cex = .5, col = 'white')
  #  text(lat ~ lon, data = receiver_df[which(receiver_df$recovered != ""), ], labels = receiver_df$station_number[which(receiver_df$recovered != "")], cex = .5, col = 'black')
  dev.off()
  
  for(i in 1:length(unique(vue_df$tag_id))){
    print(i)
    indv_data = vue_df[vue_df$tag_id == unique(vue_df$tag_id)[i], ]
    
    pdf(paste(unique(vue_df$tag_id)[i], 'Detection Records.pdf'))
    par(mfrow = c(3, 1), mar = c(5.5, 4.1, 4.1, 2.1))
    
    indv_data = indv_data[indv_data$station != 'Tagging Location', ]
    indv_data$station_number <- sapply(strsplit(indv_data$station, " "), "[[", 5)
    
    indv_data$hour = (hour(indv_data$datetime) + minute(indv_data$datetime)/60)
    
    ## Plotting Full detection Stripchart
    stripchart(indv_data$datetime ~ as.factor(indv_data$station_number),
               main = paste(indv_data$tag_id[1], ' Detection Record'),
               xlab = '', xaxt = 'n',
               ylab = 'Station', yaxt = 'n',
               col = 'white')
    points(y = as.factor(indv_data$station_number), x = indv_data$datetime, col = indv_data$colors)
    axis(side = 1, at = seq(min(indv_data$datetime), max(indv_data$datetime), length.out = 10), labels = as.Date(seq(min(indv_data$datetime), max(indv_data$datetime), length.out = 10)), las = 2, cex = .5)   
    axis(side = 2, at = 1:length(unique(as.factor(indv_data$station_number))), labels = unique(indv_data$station_number))
    
    ## Plotting Station by time of day
    stripchart(indv_data$hour ~ as.factor(indv_data$station_number),
               main = paste(indv_data$tag_id[1], ' Station by Time of Day'),
               xlab = 'Hour', xlim = c(0, 24), xaxt = 'n',
               ylab = 'Station', yaxt = 'n',
               col = 'white')
    points(y = as.factor(indv_data$station_number), x = indv_data$hour, col = indv_data$colors)
    axis(side = 1, at = 0:24, labels = paste(0:24, ':00'))
    axis(side = 2, at = 1:length(unique(as.factor(indv_data$station_number))), labels = unique(indv_data$station_number))
    
    ## Plot 24 hour depth history of a tag either from pressure sensor or from station depth
    if(!any(is.na(indv_data$depth))){
      plot(y = indv_data$depth, x = indv_data$hour,
           main = paste(indv_data$tag_id[1], ' Tag Depth by Time of Day'),
           xlab = 'Hour', xlim = c(0, 24), xaxt = 'n',
           ylab = 'Depth', ylim = range(indv_data$depth),
           col = indv_data$colors)
      axis(side = 1, at = 0:24, labels = paste(0:24, ':00'), las = 2)
    } else {
      plot(y = indv_data$station_depth, x = indv_data$hour,
           main = paste(indv_data$tag_id[1], ' Station Depth by Time of Day'),
           xlab = 'Hour', xlim = c(0, 24), xaxt = 'n',
           ylab = 'Depth', ylim = range(indv_data$station_depth),
           col = indv_data$colors)
      axis(side = 1, at = 0:24, labels = paste(0:24, ':00'), las = 2)
    }
    dev.off()
  }
}

plot_detection_stripchart = function(analysis_summary){
  png(paste('Detection Stripchart.png'))
  stripchart(analysis_summary$data$datetime ~ as.factor(analysis_summary$data$tag_id), xlab = NULL, ylab = NULL, las = 2)
  dev.off()
}

plot_detection_stripcharts = function(vue_data, false_detection_index = FALSE, aggregate = FALSE){
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

plot_indv_difftimes_hist = function(vue_df, max_diff_days = 30, ignore_same_day_detections = TRUE, output_dir = NULL){
  ## Creates a histogram for each individual indicating the frequency of breaks between each subsequent detection
  ## Inputs: vue_df - a data frame with detection data including datetimes in POSIXct format
  ##         max_diff_days - a value for the maximum number of day bins. Any differences larger than this are binned in the last catagory. If null, this defaults to the largest value of an indviduals data set
  ##         ignore_same_day_detections - wether histogram is time between unique days detected on array (TRUE) or time between each detection (FALSE). Defaults to TRUE
  ##         output_directory - Where plots should be stored
  vue_df = vue_df[order(vue_df$datetime), ]
  ## Loop through individuals
  for(i in 1:length(unique(vue_df$tag_id))){
    indv_data = vue_df[vue_df$tag_id == unique(vue_df$tag_id)[i], ]
    det_vec = as.Date(indv_data$datetime) # convert detections to just dates
    ## If ignore_same_day_detections argument is true, Remove duplicate dates 
    if(ignore_same_day_detections == TRUE){
      det_vec = unique(det_vec)
    }
    
    ## Get difference between each detection date
    diff_days = as.numeric(difftime(det_vec, units = "days"))
    
    ## if max_diff_days argument is not specified, default to the max
    if(is.null(max_diff_days)){
      max_diff_days = max(diff_days)
    }
    
    ## Setting upper limit for time between detections
    diff_days[diff_days >= max_diff_days] = max_diff_days
    
    ## Pre-running histogram function - Necessary to get max(indv_hist$counts) for scalling histogram sizes below 
    indv_hist = hist(diff_days, breaks = seq(0,max_diff_days), plot = FALSE) # Necessary to get max(indv_hist$counts) for scalling histogram sizes below
    
    ## Plotting indvidual histogram 
    png(paste(output_dir, unique(vue_df$tag_id)[i], ' Diff Between Detection Hist.png', sep = ""), height = 800, width = 1000)
    indv_hist = hist(diff_days, breaks = seq(0,max_diff_days), xaxt = 'n', ylim = c(0,ceiling(max(indv_hist$counts) * 1.05)), xlab = 'Days Between Subsequent Detections', main = unique(vue_df$tag_id)[i])
    axis(side = 1, at = seq(0,max_diff_days), labels = c(seq(0,max_diff_days - 1), paste(max_diff_days, '+', sep = '')))
    text(x = seq(0,max_diff_days) + 0.5, y = max(indv_hist$counts) * (indv_hist$counts/max(indv_hist$counts) + .05), labels = round(indv_hist$counts/max(indv_hist$counts) * 100, digits = 2), cex = 0.5)
    dev.off()
  }
}

plot_mortality_stripchart  = function(analysis_summary, valid_tags, questionable_tags, expired_tags){
  pdf('tag mortality chart.pdf', width = 8.5, height = 11)
  par(mai=c(1.5,0.82,0.42,0.42))
  analysis_summary$data$tag_id = as.numeric(as.character(analysis_summary$data$tag_id))
  #### First filtering out any tracks that might not be in the data set under analysis
  valid_tags = valid_tags[valid_tags %in% unique(analysis_summary$data$tag_id)]
  questionable_tags = questionable_tags[questionable_tags %in% unique(analysis_summary$data$tag_id)]
  expired_tags = expired_tags[expired_tags %in% unique(analysis_summary$data$tag_id)]
  
  #### Creating a matrix to store the tag ID number and it's corrosponding color status for plotting 
  tag_color = matrix(data = NA, ncol = 2, nrow = (length(valid_tags) + length(questionable_tags) + length(expired_tags)))
  tag_color[ ,1] = as.numeric(c(valid_tags, questionable_tags, expired_tags))
  tag_color[ ,2] = c(rep('green', length.out = length(valid_tags)), rep('yellow', length.out = length(questionable_tags)), rep('red', length.out = length(expired_tags)))
  tag_color = tag_color[order(as.numeric(tag_color[,1]), decreasing = FALSE), ]
  
  #### Create data frame of dummy data for plotting track status colors
  dummy_data = as.data.frame(matrix(NA, nrow = 0, ncol = 2))
  colnames(dummy_data) = c('dummy_date', 'tag_id')
  for(i in 1:length(tag_color[ ,1])){
    dummy_data = rbind(dummy_data, cbind(tag_color[i,1], as.character(seq.POSIXt(from = min(analysis_summary$data$datetime), to = max(analysis_summary$data$datetime), by = 'day'))))
  }
  
  #### Plotting status lines 
  stripchart(as.POSIXct(dummy_data[order(dummy_data[ ,1], decreasing = TRUE) ,2]) ~ as.numeric(as.character(dummy_data[order(dummy_data[ ,1], decreasing = TRUE),1])), col = tag_color[ ,2], xlab = '', xaxt = 'n',  pch = 15, cex = 1, las = 2, jitter = FALSE)
  #### Overlaying detections
  stripchart(analysis_summary$data$datetime[analysis_summary$data$station != 'Tagging Location'] ~ analysis_summary$data$tag_id[analysis_summary$data$station != 'Tagging Location'], add = TRUE, pch = 15, jitter = FALSE)
  ### Adding in tagging date as a triangle
  stripchart(analysis_summary$data$datetime[analysis_summary$data$station == 'Tagging Location'] ~ analysis_summary$data$tag_id[analysis_summary$data$station == 'Tagging Location'], add = TRUE, pch = 17, jitter = FALSE)
  
  #### Plotting dates on x axis
  date_labels = as.character(strptime(seq(min(analysis_summary$data$datetime), max(analysis_summary$data$datetime), length.out = 10), format = '%Y-%m-%d'))
  axis(side = 1, at = seq(min(analysis_summary$data$datetime), max(analysis_summary$data$datetime), length.out = 10), labels = date_labels, las = 2, cex.axis = 1)
  dev.off()
}

plot_movements = function(vue_df, receiver_df, region = 'Makapuu', tag_ids = FALSE, rec_col = FALSE, plot_title = FALSE, bathymetry = NULL){
  #### Plotting Movement Maps for Each Fish
  
  if(is.null('bathymetry')){
    if(region == 'Makapuu'){
      bathymetry = getNOAA.bathy(lon1 = -157.8, 
                                 lon2 = -157.5, 
                                 lat1 = 21.2, 
                                 lat2 = 21.5,
                                 resolution = 1)
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
  
  #### Filtering for specified tag_ids if they are specified in function argument
  if (tag_ids[1] != FALSE){
    vue_df = vue_df[vue_df$tag_id %in% tag_ids, ]
  }
  
  ### Stepping through individual ids
  for (i in 1:length(sort(unique(vue_df$tag_id)))){
    indv_data = vue_df[vue_df$tag_id == sort(unique(vue_df$tag_id))[i], ]
    
    ### Removing receivers not present while transmitter was active
    receiver_df_to_plot = receiver_df[which(receiver_df$deployment_date <= min(indv_data$datetime) &
                                              (receiver_df$recovery_date >= min(indv_data$datetime) |
                                                 is.na(receiver_df$recovery_date) == TRUE)), ]
    receiver_df_to_plot$station_name = as.character(receiver_df_to_plot$station_name)
    
    if(plot_title == FALSE){
      plot_title = paste(as.character(sort(unique(vue_df$tag_id))[i]), 'Movement Map.png')
    }
    png(plot_title)
    
    
    
    
    ## Plotting basemap
    plot.bathy(bathymetry, land = TRUE, image=TRUE, bpal = gray.colors(10), deepest.isobath = c(-500), shallowest.isobath = c(0), step = c(100), drawlabels = TRUE)
    ## Adding scale legend
    scaleBathy(bathymetry, deg = .1, cex = .5)
    #scaleBathy(bathymetry, deg = .48, cex = .5)
    
    ## Adding receiver locations.
    # If station colors are not assigned, stations default to red
    if('rec_col' == FALSE){
      rec_col = 'red'
    }
    receiver_plot_colors = rec_col
    # if a list of colors tied to station names is given, stations are plotted in that color
    if(class(rec_col) == 'list'){
      color_palette = rec_col
      receiver_plot_colors = rep('black', length(receiver_df_to_plot$station_name))
      for(r in 1:length(receiver_df_to_plot$station_name)){
        if(receiver_df_to_plot$station_name[r] %in% color_palette$station){
          receiver_plot_colors[r] = color_palette$colors[which(color_palette$station == receiver_df_to_plot$station_name[r])]
        }
      }
    }
    
    ## Plotting fish movements
    lines(lat~lon, data = indv_data, col = 'blue', lty = 1, lwd = 2)
    # points(lat~lon, data = indv_data, col = 'blue',cex = 0.6, pch = 19)
    
    points(lat~lon, data = receiver_df_to_plot, pch = 19, col = receiver_plot_colors, cex = 1)
    
    if(dim(indv_data[indv_data$station == "Tagging Location", ])[1] > 0){
      points(lat~lon, data = indv_data[indv_data$station == "Tagging Location", ], pch = 2, col = 'red', cex = 1)
    }
    
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
  # return(bathymetry)
}

plot_path_use = function(movement_graph, vue_df, receiver_df, region = 'Makapuu', bathymetry = NULL){
  ## Clearing out loops if they remain
  rev_identity = diag(x = 2, nrow = dim(movement_graph)[1], ncol = dim(movement_graph)[2])
  rev_identity[rev_identity == 0] = 1;   rev_identity[rev_identity == 2] = 0
  movement_graph = (movement_graph * rev_identity) / max(movement_graph)
  if(is.null('bathymetry')){
    if(region == 'Makapuu'){
      bathymetry = getNOAA.bathy(lon1 = -157.75, 
                                 lon2 = -157.5, 
                                 lat1 = 21.27, 
                                 lat2 = 21.46,
                                 resolution = 1)
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
  # setwd(fig.dir)
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
  lines(lat~lon, data = brfa_e, pch = 19, col = 'black', lwd = 3, lty = 2, cex = .6)
  lines(lat~lon, data = brfa_f, pch = 19, col = 'purple', lwd = 3, lty = 2, cex = .6)
  ## Plotting receiver locations
  subset_receiver_data = receiver_df[which((receiver_df$recovery_date >= min(vue_df$datetime) & !is.na(receiver_df$recovery_date)) & receiver_df$recovered == ""), ]
  if(dim(subset_receiver_data)[1] != 0){
    points(lat~lon, data = subset_receiver_data, col = 'black', cex = 1)
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
  scaled_graph = (movement_graph / (max(movement_graph) - min(movement_graph))) #standardize graph on range of 0-1
  color_scaled_graph = round(scaled_graph * 8) # standardize graph on range of 0-8 for plotting
  colors = rainbow(5)[5:1]
  sort(color_scaled_graph)
  for(col_value in unique(sort(color_scaled_graph))){
    plot_value = which(color_scaled_graph == col_value, arr.ind = TRUE)
    move_line = data.frame('lon' = unique_lon[plot_value[,1]], 'lat' = unique_lat[plot_value[,2]])
    lines(lat~lon, data = move_line, col = colors[col_value], lwd = 3)
  }
  dev.off()
}

plot_receiver_map = function(receiver_df = receiver_df, snapshot_date = NULL, bathymetry = NULL, region = 'Makapuu', resolution = 'Medium', save_plot = TRUE, receiver_plot_colors = 'red', receiver_numbers = FALSE){
  #### Plotting Map of Receivers in Study
  
  if(!is.null(snapshot_date)){
    receiver_df = receiver_df[receiver_df$deployment_date <= as.POSIXct(snapshot_date) & receiver_df$recovery_date >= as.POSIXct(snapshot_date), ]
  }
  
  if(is.null(bathymetry)){
    bathymetry = get_bathymetry(region, resolution)
  }
  
  if(save_plot){
    png(paste('Receiver Map ', as.character(snapshot_date), '.png', sep = ""))
  }  
  
  plot.bathy(bathymetry, land = TRUE, image=TRUE, bpal = list(c(-400, -100, "lightskyblue")), deepest.isobath = c(-10000), shallowest.isobath = c(0), step = c(100), drawlabels = TRUE)
  scaleBathy(bathymetry, deg = .48, cex = .5)
  
  # If receiver was recovered, it gets full dot
  points(lat~lon, data = receiver_df[receiver_df$recovered == "", ], pch = 19, col = receiver_plot_colors, cex = 2)
  # If receiver was lost, it gets empty dot
  points(lat~lon, data = receiver_df[which(receiver_df$recovered != ""), ], pch = 1, col = receiver_plot_colors, cex = 2)
  
  if(receiver_numbers == TRUE){
    receiver_df$station_number = NA
    for(i in 1:length(receiver_df$station_name)){
      receiver_df$station_number[i] = strsplit(receiver_df$station_name[i], split = 'Oahu - Makapuu BRFA ' )[[1]][2]
      if(isTRUE(receiver_df$station_number[i] == "15 (Makapuu In BRFA)")){
        receiver_df$station_number[i] = 15
      }
    }
  }
  text(lat~lon, labels = receiver_df$station_number,  col = 'white', data = receiver_df[receiver_df$recovered == "", ])
  text(lat~lon, labels = receiver_df$station_number,  col = 'dark red', data = receiver_df[receiver_df$recovered == "", ])
  
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
  lines(lat~lon, data = brfa_f, pch = 19, col = 'purple', lwd = 3, cex = .6)
  
  if(save_plot){
    dev.off()
  }
}

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
                                 resolution = 1)
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

plot_tag_detections = function(vue_df, receiver_df, start_date = NULL, end_date = NULL, date_format = "%Y-%m-%d %H:%M:%S", plot_lost = FALSE, remove_station = FALSE, region = "Makapuu", tag_ids = FALSE, rec_col = FALSE, title = FALSE, plot_receivers = "all", bathymetry = NULL){
  ### Types of plots (plot_receivers) to make include:
  # all = plots all receivers regardless of time
  # start = plots only receivers that were deployed at the begning of the time interval given by start_date
  # end = plots only receivers that were deployed at the end of the time interval given by end_date
  # study = plots all receivers that were deployed at some point during the time interval given by start_date and end_date
  # start tag = plots only receivers that were deployed at the time the animal was tagged
  # end tag = plots only receivers that were deployed at the last detection of a tag
  # study tag = plots all receivers that were deployed during the time the tag was out
  if(plot_lost == FALSE){
    receiver_df = receiver_df[receiver_df$recovered == "", ]
  } 
  receiver_df = receiver_df[is.na(receiver_df$deployment_date) == FALSE, ]
  if(is.null(start_date)){
    start_date = min(vue_df$datetime)
  }else{
    start_date = as.POSIXct(start_date, format = date_format)
  }
  if(is.null(end_date)){
    end_date = max(vue_df$datetime)
  }else{
    end_date = as.POSIXct(end_date, format = date_format)
  }
  
  for(i in 1:length(sort(unique(vue_df$tag_id)))){
    indv_data = vue_df[vue_df$tag_id == sort(unique(vue_df$tag_id))[i], ]
    if(plot_receivers == "all"){
      plot_receiver_df = receiver_df
    }else if(plot_receivers == "start"){
      plot_receiver_df = receiver_df[which(receiver_df$deployment_date <= start_date & (receiver_df$recovery_date > start_date | is.na(receiver_df$recovery_date) == TRUE)), ]
    }else if(plot_receivers == "end"){
      plot_receiver_df = receiver_df[which(receiver_df$deployment_date < end_date & (receiver_df$recovery_date > end_date | is.na(receiver_df$recovery_date) == TRUE)), ]
    }else if(plot_receivers == "study"){
      plot_receiver_df = receiver_df[which(receiver_df$deployment_date < end_date & (receiver_df$recovery_date > start_date | is.na(receiver_df$recovery_date) == TRUE)), ]
    }else if(plot_receivers == "start tag"){
      plot_receiver_df = receiver_df[which(receiver_df$deployment_date <= min(indv_data$datetime) & (receiver_df$recovery_date > min(indv_data$datetime) | is.na(receiver_df$recovery_date) == TRUE)), ]
    }else if(plot_receivers == "end tag"){
      plot_receiver_df = receiver_df[which(receiver_df$deployment_date <= max(indv_data$datetime) & (receiver_df$recovery_date > max(indv_data$datetime) | is.na(receiver_df$recovery_date) == TRUE)), ]
    }else if(plot_receivers == "study tag"){
      plot_receiver_df = receiver_df[which(receiver_df$deployment_date < max(indv_data$datetime) & (receiver_df$recovery_date > min(indv_data$datetime) | is.na(receiver_df$recovery_date) == TRUE)), ]
    }
    plot_movements(vue_df = indv_data, receiver_df = plot_receiver_df, region = region, rec_col = rec_col, bathymetry = bathymetry)
  }
}

plot_tagging_histogram = function(vue_df){
  #### Generating histogram of detections for each fish
  
  for(i in 1:length(sort(unique(vue_df$tag_id)))){
    indv_data = vue_df[sort(unique(vue_df$tag_id))[i], ]
    title = sprintf('%s Study Date Histogram.png', sort(unique(vue_df$tag_id))[i])
    png(title)
    par(mfrow = c(1,1))
    plot_title = sprintf('Detections of Tag %s', sort(unique(vue_df$tag_id))[i])
    indv_data = vue_df[vue_df$tag_id == sort(unique(vue_df$tag_id))[i], ]
    hist(indv_data$study_date,  
         breaks = max(vue_df$study_date), 
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

plot_tagging_locations = function(tagging_df, tag_ids = NULL, region = "Makapuu", bathymetry = NULL, description = NULL, jitter = FALSE, save_plot = TRUE){
  #### Plotting a map of tagging locations
  ## Outputs two images, one is a map with points indicating tagging locations, the second is a colorbar scale
  if(save_plot){
    png(paste('tagging locations', description, '.png', sep = ''), width = 1000, height = 860)
  }
  
  ## Plotting basemap
  if(is.null(bathymetry)){
    bathymetry = get_bathymetry(region = region, resolution = "medium")
  }
  plot.bathy(bathymetry, land = TRUE, image=TRUE, bpal = 'white', deepest.isobath = c(-10000), shallowest.isobath = c(0), step = c(100), drawlabels = TRUE, main = 'Tagging Locations')
  ## Adding scale legend
  scaleBathy(bathymetry, deg = .1, cex = .5)
  
  ## Adding BRFA boundaries
  brfa_e = as.data.frame(rbind(c(-157.68333333, 21.28333333), 
                               c(-157.53333333, 21.28333333), 
                               c(-157.53333333, 21.4166666), 
                               c(-157.68333333, 21.4166666)))
  colnames(brfa_e) = c('lon', 'lat')
  lines(lat~lon, data = brfa_e, pch = 19, col = 'purple', lwd = 3, cex = .6)
  if(save_plot){
    dev.off()
  }
  
  ## Getting a list of tagging locations and the number of individuals tagged at each
  tagging_locations = c()
  if(!is.null(tag_ids)){
    tagging_df = tagging_df[tagging_df$vem_tag_id %in% tag_ids, ]
  } else {
    tag_ids = unique(tagging_df$vem_tag_id)
  }
  
  if(!jitter){
    tagging_locations = as.data.frame(cbind(tagging_df$vem_tag_id, tagging_df$lat, tagging_df$lon, paste(tagging_df$lat, tagging_df$lon), tagging_df$species), stringsAsFactors = FALSE)
    colnames(tagging_locations) = c('tag_id', 'lat', 'lon', 'location', 'species')
    tagging_locations = tagging_locations[tagging_locations$tag_id %in% tag_ids, ]
    tagging_locations = tagging_locations[tagging_locations$species == "Opakapaka" | tagging_locations$species == "Opakapaka (Deceased)", ]
    tagging_locations = aggregate(tagging_locations$tag_id, by = list(tagging_locations$lat, tagging_locations$lon), FUN = uniqueN)
    colnames(tagging_locations) = c('lat', 'lon', 'n')
    
    
    ## Assigning a color palette based on the maximum number of fish that were tagged at a single spot
    plot_col_pal = rainbow(max(tagging_locations$n), start = 4/6, end = 0)
    
    
    ## Adding tagging locations
    for(i in min(tagging_locations$n):max(tagging_locations$n)){
      points(as.numeric(tagging_locations$lat[tagging_locations$n == i]) ~ as.numeric(tagging_locations$lon[tagging_locations$n  == i]), pch = 19, col = plot_col_pal[i], cex = 1)
    }
  } else if(jitter){
    
  }
  
  
  
  ## Plotting a separate color scale
  png('tagging locations colorbar scale.png', width = 430, height = 213)
  plot(rep(1, max(tagging_locations$n)) ~ c(1:max(tagging_locations$n)), col = plot_col_pal[1:max(tagging_locations$n)], pch = 15, cex = 5)
  if(save_plot){
    dev.off()
  }
}

plot_tagging_locations_and_receivers = function(tagging_df, receiver_df, bathymetry = NULL){
  ## Plotting tagging location with receiver location
  
  if(is.null(bathymetry)){
    bathymetry = get_bathymetry("Makapuu", resolution = 'low')
  }
  
  png('Tagging Locations with Receiver Footprints.png')
  plot.bathy(bathymetry, land = TRUE, image=TRUE, bpal = 'white', deepest.isobath = c(-10000), shallowest.isobath = c(0), step = c(100), drawlabels = TRUE, main = 'Tagging Locations')
  scaleBathy(bathymetry, deg = .02, cex = .5)
  points(as.numeric(tagging_df$lat) ~ as.numeric(tagging_df$lon), pch = 19, col = 'blue', cex = 1)
  points(as.numeric(receiver_df$lat[receiver_df$deployment_date <= max(tagging_df$datetime) & receiver_df$recovery_date >= min(tagging_df$datetime)]) ~ as.numeric(receiver_df$lon[receiver_df$deployment_date <= max(tagging_df$datetime) & receiver_df$recovery_date >= min(tagging_df$datetime)]), cex = 2, col = 'red')
  dev.off()
}

plot_receiver_use_individual_map = function(vue_df, receiver_df, region = "Makapuu", bathymetry = NULL){
  ####  Map of receivers colored by number of fish that hit on each
  
  ## Setting up the plot
  png('Receiver Use By Individuals.png', width = 1000, height = 860)
  
  ## Getting number of fish detected at each station
  receiver_use_by_individuals = aggregate(vue_df$tag_id, by = list(vue_df$station), FUN = uniqueN)
  colnames(receiver_use_by_individuals) = c('station', 'n_fish_detected')
  receiver_use_by_individuals = receiver_use_by_individuals[receiver_use_by_individuals$station != 'Tagging Location', ]
  
  ## Assigning a color plot based on the maximum number of fish detected at a single station
  plot_col_pal = rainbow(max(receiver_use_by_individuals$n_fish_detected), start = 4/6, end = 0)
  
  ## Pulling down bathy if it doesn't already exist
  if(is.null('bathymetry')){
    if(region == 'Makapuu'){
      bathymetry = getNOAA.bathy(lon1 = -157.8, 
                                 lon2 = -157.5, 
                                 lat1 = 21.2, 
                                 lat2 = 21.5,
                                 resolution = 1)
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
  plot.bathy(bathymetry, land = TRUE, image=TRUE, bpal = 'white', deepest.isobath = c(-10000), shallowest.isobath = c(0), step = c(100), drawlabels = TRUE, main = 'Tagging Locations')
  ## Adding scale legend
  scaleBathy(bathymetry, deg = .1, cex = .5)
  ## Getting a list of unique receivers and their positions
  unique_receiver_list = data.frame(stringsAsFactors = FALSE)
  receiver_df = receiver_df[which(receiver_df$deployment_date >= min(vue_df$datetime) & receiver_df$deployment_date < max(vue_df$datetime) & (receiver_df$recovered == "" | is.na(receiver_df$recovered))), ]
  unique_receiver_list = foreach(i = 1:length(unique(receiver_df$station_name)), .combine = rbind) %dopar%{
    return(receiver_df[which(receiver_df$station_name == unique(receiver_df$station_name)[i])[1], ])
  }
  ## Plotting those receivers as open circles
  points(unique_receiver_list$lat ~ unique_receiver_list$lon)
  ## Merging receiver use by individuals and lat lon positions of each receiver
  receiver_use_by_individuals = merge(x = receiver_use_by_individuals, y = unique_receiver_list[ ,c('station_name', 'lon', 'lat')], by.x = 'station', by.y = 'station_name')
  
  ## Plotting each receiver based on the number of fish detected
  for(i in min(receiver_use_by_individuals$n):max(receiver_use_by_individuals$n)){
    points(as.numeric(receiver_use_by_individuals$lat[receiver_use_by_individuals$n_fish_detected == i]) ~ as.numeric(receiver_use_by_individuals$lon[receiver_use_by_individuals$n_fish_detected  == i]), pch = 19, col = plot_col_pal[i], cex = 2)
  }
  
  ## Adding BRFA boundaries
  brfa_e = as.data.frame(rbind(c(-157.68333333, 21.28333333), 
                               c(-157.53333333, 21.28333333), 
                               c(-157.53333333, 21.4166666), 
                               c(-157.68333333, 21.4166666)))
  colnames(brfa_e) = c('lon', 'lat')
  lines(lat~lon, data = brfa_e, pch = 19, col = 'purple', lwd = 3, cex = .6)
  
  dev.off()
  
  ## Plotting color scale
  png('Receiver Use By Individuals colorbar scale.png', width = 430, height = 213)
  plot(rep(1, max(receiver_use_by_individuals$n_fish_detected)) ~ c(1:max(receiver_use_by_individuals$n_fish_detected)), col = plot_col_pal[1:max(receiver_use_by_individuals$n_fish_detected)], pch = 15, cex = 5)
  dev.off()
}

plot_lunar_phases = function(vue_df, receiver_df, region = "Makapuu", bathymetry = NULL){
  ## Plotting receiver use maps based on the lunar phase
  # Map for each of the 4 moon phases with colored by the number of fish detected during that phase
  ## Aggregating data by lunar phase. How many unique individuals detected at a station during each lunar phase?
  regolith = aggregate(vue_df$tag_id, by = list(vue_df$phase, vue_df$week, vue_df$station), FUN = uniqueN) ## Lunar aggregate
  colnames(regolith) = c('phase', 'week', 'station', 'n_tags')
  regolith = regolith[regolith$station != 'Tagging Location', ]
  ## Aggregating this data down again to sum across all weeks/same lunar phases
  compounded_regolith = aggregate(regolith$n_tags, by = list(regolith$phase, regolith$station), FUN = sum)
  colnames(compounded_regolith) = c('phase', 'station', 'n_sum_tags')
  
  
  ### Plotting this data
  png('n_fish_by_lunar_phase.png', width = 826, height = 437)
  par(mfrow = c(2,2))
  
  ## Creating a color palette
  plot_col_pal = rainbow(max(compounded_regolith$n_sum_tags), start = 4/6, end = 0)
  
  ## Pulling down bathy if it doesn't already exist
  if(is.null('bathymetry')){
    if(region == 'Makapuu'){
      bathymetry = getNOAA.bathy(lon1 = -157.8, 
                                 lon2 = -157.5, 
                                 lat1 = 21.2, 
                                 lat2 = 21.5,
                                 resolution = 1)
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
  
  ## Getting a list of coordinate positions for receivers - One location per station (ignores variability in deployment over time)
  unique_receiver_list = data.frame(stringsAsFactors = FALSE)
  receiver_df = receiver_df[which(receiver_df$deployment_date >= min(vue_df$datetime) & receiver_df$deployment_date < max(vue_df$datetime) & (receiver_df$recovered == "" | is.na(receiver_df$recovered))), ]
  
  unique_receiver_list = foreach(i = 1:length(unique(receiver_df$station_name)), .combine = rbind) %dopar% {
    return(receiver_df[which(receiver_df$station_name == unique(receiver_df$station_name)[i])[1], ])
  }
  
  ## Adding lat lon positions to fish counts at each station by phase
  receiver_use_by_lunar_phase = merge(x = compounded_regolith, y = unique_receiver_list[ ,c('station_name', 'lon', 'lat')], by.x = 'station', by.y = 'station_name')
  
  ## Sorting lunar phases for plotting
  lunar_phases = c("New", "Waxing", "Full", "Waning")
  
  ## plotting each lunar phase by looping through each individually
  for(i in lunar_phases){
    
    lunar_subset = receiver_use_by_lunar_phase[receiver_use_by_lunar_phase$phase == i, ]
    
    ## Plotting basemap
    plot.bathy(bathymetry, land = TRUE, image=TRUE, bpal = 'white', deepest.isobath = c(-10000), shallowest.isobath = c(0), step = c(100), drawlabels = TRUE, main = i)
    ## Adding scale legend
    scaleBathy(bathymetry, deg = .1, cex = .5)
    
    ## Adding receivers to map
    points(unique_receiver_list$lat~ unique_receiver_list$lon)
    
    ## Plotting receivers with data using colors from color palette defined above 
    for(i in lunar_subset$n_sum_tags){
      points(as.numeric(lunar_subset$lat[lunar_subset$n_sum_tags == i]) ~ as.numeric(lunar_subset$lon[lunar_subset$n_sum_tags  == i]), pch = 19, col = plot_col_pal[i], cex = 2)
    }
    
    ## Adding BRFA boundaries
    brfa_e = as.data.frame(rbind(c(-157.68333333, 21.28333333), 
                                 c(-157.53333333, 21.28333333), 
                                 c(-157.53333333, 21.4166666), 
                                 c(-157.68333333, 21.4166666)))
    colnames(brfa_e) = c('lon', 'lat')
    lines(lat~lon, data = brfa_e, pch = 19, col = 'purple', lwd = 3, cex = .6)
  }
  dev.off()
  
  ## Plotting a color scale
  png('n_fish_by_lunar_phase colorbar scale.png', width = 430, height = 213)
  par(mfrow = c(1, 1))
  plot(rep(1, max(receiver_use_by_lunar_phase$n_sum_tags)) ~ c(1:max(receiver_use_by_lunar_phase$n_sum_tags)), col = plot_col_pal[1:max(receiver_use_by_lunar_phase$n_sum_tags)], pch = 15, cex = 5)
  dev.off()
}

plot_tag_detection_histogram = function(vue_df, receiver_df = NULL, save_plot = TRUE, plot_title = TRUE){
  ### Revised function for plotting daily histogram of the number of detections.
  ## If receiver_df argument is not null, plot lines over histogram showing when receivers were in and out of water
  if(is.null(receiver_df) == FALSE){
    ### Getting dates when receivers in the study were rearranged.
    receiver_df_subset = receiver_df[receiver_df$station_name %in% unique(vue_df$station), ] # First subset only stations where a fish was detected
    receiver_study_dates = unique(c(as.Date(receiver_df_subset$deployment_date), as.Date(receiver_df_subset$recovery_date))) # Then pull out unique dates on which those stations were serviced
  }
  # ### Histogram for number of detections over of all study tags
  # png('All Tags - Daily Histogram.png', height = 800, width = 1000)
  # par(mfrow = c(1, 1), mar=c(3,3,3,3), oma=c(2,2,2,2)) # setting up plotting grid
  # hist(floor(vue_df$study_date), right = FALSE, breaks = 0:ceiling(max(vue_df$study_date)),
  #      xlab = '', 
  #      ylab = 'Detections',
  #      main = 'All Tags',
  #      xaxt = 'n')
  # date_labels = as.character(strptime(seq(min(vue_df$datetime), max(vue_df$datetime), length.out = 10), format = '%Y-%m-%d'))
  # axis(side = 1, at = seq(min(vue_df$study_date), max(vue_df$study_date), length.out = 10), labels = date_labels, las = 2, cex = .25)
  # if(is.null(receiver_df) == FALSE){
  #   abline(v = floor(receiver_dates), col = "blue")
  # }
  # dev.off()
  
  ### Then of individual tags
  for(i in 1:length(unique(vue_df$tag_id))){
    if(save_plot == TRUE){
      png(paste(unique(vue_df$tag_id)[i], 'Daily Histogram.png'), height = 800, width = 1000)
      par(oma=c(2,0,0,0)) # setting up plotting grid
    }
    
    if(plot_title == FALSE){
      main_tag_id = NULL
    } else {
      main_tag_id = unique(vue_df$tag_id)[i]
    }
    
    indv_data = vue_df[vue_df$tag_id == unique(vue_df$tag_id)[i], ]
    hist_plot = hist(indv_data$date, right = TRUE, breaks = seq.Date(min(vue_df$date),max(vue_df$date), by = 1), freq = TRUE,
                     main = main_tag_id, # individual plot label
                     xlab = '', # x axis label
                     ylab = 'Detections', # y axis label
                     col = 'black',
                     xaxt='n')
    date_labels = seq.Date(from = min(vue_df$date), to = max(vue_df$date), length.out = 10)
    axis(side = 1, at = seq.Date(from = min(vue_df$date), to = max(vue_df$date), length.out = 10), labels = date_labels, las = 2, cex = .25)
    abline(v = indv_data$date[indv_data$station == 'Tagging Location']-1, col = 'red', lwd = 2)
    if(is.null(receiver_df) == FALSE){
      abline(v = receiver_study_dates, col = "blue")
    }
    if(save_plot == TRUE){
      dev.off()
    }
  }
}

plot_vemco_stripplot = function(vue_df, save_plot = TRUE){
  for(i in 1:length(unique(vue_df$tag_id))){
    indv_data = vue_df[vue_df$tag_id == unique(vue_df$tag_id)[i], ]
    if(save_plot == TRUE){
      pdf(paste(unique(vue_df$tag_id)[i], 'stripchart.pdf'), width = 11, height = 8.5)
    }
    par(las = 1, mar=c(5.5,5,1,1))
    stripchart(indv_data$datetime ~ indv_data$station, xaxt = 'n', xlab = '')
    date_labels = seq(min(indv_data$datetime), max(indv_data$datetime), length.out = 10)
    axis(side = 1, at = seq(min(indv_data$datetime), max(indv_data$datetime), length.out = 10), labels = as.Date(date_labels), las = 2, cex = .25)
    if(save_plot == TRUE){
      dev.off()
    }
  }
}

six_pannel_plot = function(analysis_summary, vue_df = NULL, start_date = NULL, end_date = NULL, color_palette = NULL){
  
  if(is.null(vue_df)){
    vue_df = analysis_summary$data
  }
  
  if(is.null(start_date)){
    start_date = min(vue_df$datetime)
  }
  
  if(is.null(end_date)){
    end_date = max(vue_df$datetime)
  }
  
  if(is.null(color_palette)){
    color_palette = assign_color_palette(vue_df)
  }
  
  makapuu_bathy = get_bathymetry(region = "Makapuu", resolution = 'Medium')
  oahu_bathy = get_bathymetry(region = "Oahu", resolution = 'Medium')
  pb_bathy = get_bathymetry(region = "Oahu and Penguin Banks", resolution = 'Medium')
  
  # trans_makapuu = trans.mat(makapuu_bathy, min.depth = -100 , max.depth = -400)
  # trans_oahu = trans.mat(oahu_bathy, min.depth = -100 , max.depth = -400)
  # trans_pb = trans.mat(analysis_summary$bathymetry, min.depth = -100 , max.depth = -400)
  # 
  receiver_df = analysis_summary$receiver_df[analysis_summary$receiver_df$station_name %in% vue_df$station, ]
  
  receiver_df$station_number = NA
  for(j in 1:length(receiver_df$station_name)){
    receiver_df$station_number[j] = strsplit(receiver_df$station_name, split = ' ')[[j]][5]
  }
  
  for(i in 1:length(unique(vue_df$tag_id))){
    print(paste(i, unique(vue_df$tag_id)[i]))
    print('')
    print(paste('Plot', i, 'of', length(unique(vue_df$tag_id))))
    print(unique(vue_df$tag_id)[i])
    
    pdf(paste(unique(vue_df$tag_id)[i], '6 pannel plot.pdf'), height = 11, width = 8.5)
    
    layout(matrix(c(1, 1, 1, 2, 2, 2,
                    3, 3, 3, 4, 4, 4,
                    5, 5, 5, 6, 6, 6), nrow = 3, byrow = TRUE))
    
    
    indv_data = vue_df[vue_df$tag_id == unique(vue_df$tag_id)[i], ]
    
    # if(analysis_summary$summary_df$tagging_date[analysis_summary$summary_df$tag_id == unique(vue_df$tag_id)[i]] <= as.POSIXct('2015-03-17 0:00:00 HST')){
    #   trans1 = trans_pb
    bathy = pb_bathy
    # } else if (analysis_summary$summary_df$tagging_date[analysis_summary$summary_df$tag_id == unique(vue_df$tag_id)[i]] < ('2015-05-30')) {
    #   trans1 = trans_oahu
    bathy = oahu_bathy
    # } else {
    #   trans1 = trans_makapuu
    bathy = makapuu_bathy
    # }
    
    ### 1. Print Vital Stats
    print('1. Vital Stats')
    par(las = 1, mar=c(0,0,0,0))
    plot.new()
    # Tag ID
    text(x = 0, y = .95, paste('Tag ID:', unique(indv_data$tag_id)), pos = 4, cex = 1.5)
    
    text(x = 0.5, y = .95, paste('Species:', tagging_data$species[tagging_data$vem_tag_id == unique(indv_data$tag_id)]), pos = 4, cex = 1.5)
    # Track Status
    text(x = 0, y = .85, paste('Track Status:', analysis_summary$track_status$status_df$status[analysis_summary$track_status$status_df$tag_id == unique(indv_data$tag_id)]), pos = 4, cex = 1.5)
    text(x = 0, y = .65, paste('Track Status:', analysis_summary$track_status$status_df$rationalle[analysis_summary$track_status$status_df$tag_id == unique(indv_data$tag_id)]), pos = 4, cex = .25)
    
    # Tagging Date
    text(x = 0, y = .75, paste('Tagging Date:', analysis_summary$summary_df$tagging_date[analysis_summary$summary_df$tag_id == unique(indv_data$tag_id)]), pos = 4, cex = 1.5)
    
    # Data Density
    text(x = 0, y = .65, paste('Data Density:', round(as.numeric(analysis_summary$summary_df$data_density[analysis_summary$summary_df$tag_id == unique(indv_data$tag_id)]), digits = 2)), pos = 4, cex = 1.5)
    
    # Total detections
    text(x = 0, y = 0.55, paste('Tag Detections:', analysis_summary$summary_df$n_detections[analysis_summary$summary_df$tag_id == unique(indv_data$tag_id)]), pos = 4, cex = 1.5)
    
    # Track length
    text(x = 0, y = 0.45, paste('Track Length:', analysis_summary$summary_df$days_at_liberty[analysis_summary$summary_df$tag_id == unique(indv_data$tag_id)], 'Days'), pos = 4, cex = 1.5)
    
    # Unique Stations Detected
    text(x = 0, y = 0.35, paste('Unique Locations Detected:', analysis_summary$summary_df$n_stations[analysis_summary$summary_df$tag_id == unique(indv_data$tag_id)]), pos = 4, cex = 1.5)
    
    # Movements Detected
    text(x = 0, y = 0.25, paste('Movements Observed:', analysis_summary$summary_df$movements_detected[analysis_summary$summary_df$tag_id == unique(indv_data$tag_id)]), pos = 4, cex = 1.5)
    
    # BRFA Crossings
    text(x = 0, y = 0.15, paste('BRFA Crossings Observed:', analysis_summary$summary_df$total_brfa_crossings[analysis_summary$summary_df$tag_id == unique(indv_data$tag_id)]), pos = 4, cex = 1.5)
    
    # maximum observed movement
    text(x = 0, y = 0.05, paste('Maximum Linear Movement:', round(as.numeric(analysis_summary$summary_df$z_constrained_path_distance[analysis_summary$summary_df$tag_id == unique(indv_data$tag_id)], 'km'), digits = 2)), pos = 4, cex = 1.5)
    
    ### 2. Detection Stripchart
    print('2. Detection Strip Chart')
    
    plot_vemco_stripplot(vue_df = indv_data, save_plot = F)
    
    ### 3. Constrained movement path
    print('3. Constrained Movement Path')
    ## Plotting basemap with brfa boundary
    plot(bathy, main = NULL, land = TRUE, image=TRUE, bpal = list(c(-100, -400, "lightblue")), deepest.isobath = c(-10000), shallowest.isobath = c(-1), step = c(100), drawlabels = TRUE)
    
    brfa_e = as.data.frame(rbind(c(-157.68333333, 21.28333333), 
                                 c(-157.53333333, 21.28333333), 
                                 c(-157.53333333, 21.4166666), 
                                 c(-157.68333333, 21.4166666)))
    colnames(brfa_e) = c('lon', 'lat')
    lines(lat~lon, data = brfa_e, pch = 19, col = 'purple', lwd = 3, cex = .6)
    
    ## Pairing plotting colors with receivers
    plot_colors = as.data.frame(cbind(color_palette$station, color_palette$colors))
    colnames(plot_colors) = c('station_name', 'plot_color')
    
    ## Add points representing receivers
    receivers_to_plot = receiver_df[receiver_df$recovery_date >= min(indv_data$datetime) & receiver_df$deployment_date <= max(indv_data$datetime) & !is.na(receiver_df$station_name), ]
    receivers_to_plot = merge(receivers_to_plot, plot_colors, all.x = TRUE)
    
    ## Filled points for stations where individuals were detected
    points(lat~lon, data = receivers_to_plot[receivers_to_plot$recovered == "", ], col = 'white', pch = 19, cex = 1.5)
    points(lat~lon, data = receivers_to_plot[receivers_to_plot$recovered == "" & receivers_to_plot$station_name %in% indv_data$station, ], col = receivers_to_plot$plot_color[receivers_to_plot$recovered == "" & receivers_to_plot$station_name %in% indv_data$station ], pch = 19, cex = 1.5)
    # Hollow points for stations without detection
    points(lat~lon, data = receivers_to_plot[receivers_to_plot$recovered != "", ], col = 'red', pch = 1, cex = 1.5)
    
    ### Adding movement paths to map
    points(lat ~ lon, data = tagging_df[tagging_df$vem_tag_id == unique(indv_data$tag_id), ], col = 'yellow', pch = 17, cex = 1.5)
    indv_locations = as.data.frame(rbind(cbind(tagging_df$lon[tagging_df$vem_tag_id == unique(indv_data$tag_id)], tagging_df$lat[tagging_df$vem_tag_id == unique(indv_data$tag_id)]), cbind(indv_data$lon, indv_data$lat)))
    
    ## Removing multiple successive detections at same station
    rm_index = c()
    for(j in 2:dim(indv_locations)[1]){
      if(all(indv_locations[j, ] == indv_locations[j-1, ])){
        rm_index = c(rm_index, j)
      }
    }
    if(length(rm_index) > 0){
      indv_locations = indv_locations[-rm_index, ]
    }
    
    # ## Removing successive detection pairs
    # rm_index = c()
    # if(dim(indv_locations)[1] >= 4){
    #   for(j in seq(3, dim(indv_locations)[1], 2)){
    #     if(j+1 <= dim(indv_locations)[1]){
    #       if(all(indv_locations[c(j, j+1), ] == indv_locations[c(j-2, j-1), ])){
    #         rm_index = c(rm_index, c(j, j+1))
    #       }
    #     }
    #   }
    # }
    # if(length(rm_index) > 0){
    #   indv_locations = indv_locations[-rm_index, ]
    # }
    # 
    # path_to_plot = lc.dist(trans1, indv_locations, res = c("path"))
    # # all_constrained_paths = lc.dist(trans1, indv_locations, res = "dist")
    # for(j in 1:length(path_to_plot)){
    #   lines(path_to_plot[[j]], col = 'yellow', lwd = 2)
    # }
    
    points(lat ~ lon, data = tagging_df[tagging_df$vem_tag_id == unique(indv_data$tag_id), ], col = 'yellow', pch = 17, cex = 1.5)
    lines(lat ~ lon, data = indv_data, col = 'yellow', lwd = 2)
    
    ## Add station numbers
    receivers_to_plot$plot_color = as.character(receivers_to_plot$plot_color)
    receivers_to_plot$plot_color[is.na(receivers_to_plot$plot_color)] = 'red'
    # text(lat ~ lon, labels = station_number, data = receivers_to_plot, pos = 1, cex = .5, col = plot_color)
    
    ### 4. Day Night Plot
    print('4. Day Night Plot')
    
    par(las = 1, mar=c(5.5,4,1,1))
    plot_day_night(vue_df = indv_data, save_plot = F, color_palette = color_palette, plot_title = FALSE)
    
    ### 5. Daily Detection Plot
    print('5. Daily Detection Histogram')
    plot_tag_detection_histogram(indv_data, analysis_summary$receiver_df, save_plot = F, plot_title = FALSE)
    
    ### 6. Depth History
    print('6. Depth History')
    
    if(!is.na(indv_data$depth[2])){
      par(las = 1, mar=c(5.5,4,1,1))
      plot_depths(vue_df = indv_data, color_palette = color_palette, save_plot = F, plot_title = FALSE)
    } else {
      plot.new()
    }
    dev.off()
  }
}

#### Functions for Testing Disseration Specific Hypotheses
dissertation_H1 = function(analysis_summary, save_plot = TRUE){
  ## Hypothesis 1: Bottomfish routinely move across the borders of existing BRFAs.
  ## Hypothesis 1 was tested by determining if location coordinates for each tag 
  ## detection occurred within our outside BRFA boundaries. Movement across BRFA 
  ## boundaries was said to occur when a tag was detected outside of BRFA boundaries 
  ## followed by a detection within BRFA boundaries, or vice versa. The number of 
  ## movements across BRFA boundaries was then standardized by the track_length, 
  ## the number of days elapsed between tagging and the final detection of a tag.

  a = paste(length(which(analysis_summary$brfa_stats$total_brfa_crossings > 0)), ' of the ', length(analysis_summary$brfa_stats$tag_id), ' tracks were detected crossing the BRFA boundaries a combined total of ', sum(analysis_summary$brfa_stats$total_brfa_crossings),' times. The mean number of BRFA crossings detected per fish was ', round(mean(analysis_summary$brfa_stats$total_brfa_crossings), digits = 2), ' (s.d. = ', round(sd(analysis_summary$brfa_stats$total_brfa_crossings), digits = 2), ') over a mean track duration of ', round(mean(analysis_summary$track_length$days_tracked), digits = 2), ' days (s.d. = ', round(sd(analysis_summary$track_length$days_tracked)),' days). Standardized by time at liberty, the mean crossings into or out of the BRFA detected per fish per day was approximately ', round(analysis_summary$brfa_movements_standardized_by_track_length$mean, digits = 2), ' (Standard Deviation = ', round(analysis_summary$brfa_movements_standardized_by_track_length$stdev, digits = 2), ') or once every ', round(1 / analysis_summary$brfa_movements_standardized_by_track_length$mean, digits = 2), ' days.', sep = "")
  b = paste('However individual rates were as high as ', round(max(analysis_summary$brfa_movements_standardized_by_track_length$total), digits = 3), ' crossings per day, equivalent to crossing once every ',  round((1/max(analysis_summary$brfa_movements_standardized_by_track_length$total)), digits = 2), ' days.', sep = "")
  c = paste('The median number of crossings observed per fish per day at liberty was ', round(analysis_summary$brfa_movements_standardized_by_track_length$fivenum[3], digits = 2), ' (Min = ', round(analysis_summary$brfa_movements_standardized_by_track_length$fivenum[1], digits = 2),', 1st Quantile = ', round(analysis_summary$brfa_movements_standardized_by_track_length$fivenum[2], digits = 2), ', 3rd Quantile = ',round(analysis_summary$brfa_movements_standardized_by_track_length$fivenum[4], digits = 2),  ', Max =', round(analysis_summary$brfa_movements_standardized_by_track_length$fivenum[5], digits = 2),') or once per ', round((1/median(analysis_summary$brfa_movements_standardized_by_track_length$total)), digits = 2), ' days.', sep = "")
  print(paste(a, b, c))
  
  if(save_plot){
  ## Average BRFA crossings per day
  # Make the plot
  brfa_crossing_hist = ggplot(data = analysis_summary$summary_df, aes(x = as.numeric(mean_brfa_movements_per_day))) +
    geom_histogram(bins = 10) + 
    labs(x = 'Average Time Between BRFA Crossing (days)')
  # Save plot
  pdf(file = 'H1 - Valid Mean BRFA Crossings per Day Hist.pdf')
  print(crossings_per_day_hist)
  dev.off()
  
  ## Number of days between corssing events
  # Calculate crossing rates and bind them with tag ids
  relative_crossing_rate = data.frame(cbind(analysis_summary$tag_id, 1/calculate_brfa_movements_by_track_length(analysis_summary$brfa_stats, analysis_summary$track_length$days_tracked)$total), stringsAsFactors = FALSE)
    colnames(relative_crossing_rate) = c('tag_id', 'days_between_crossing')
  # Make the plot
  crossings_per_day_hist = ggplot(data = relative_crossing_rate, aes(x = as.numeric(days_between_crossing))) + 
    geom_histogram(bins = 10) +
    labs(x = 'Days Between Boundary Crossings', y = 'Count')
  # Save plot
  pdf(file = 'H1 - Valid Mean Days Between Crossings Hist.pdf')
    print(crossings_per_day_hist)
  dev.off()
  }

}

dissertation_H2 = function(analysis_summary, save_plot = TRUE){
  ## Hypothesis 2. Bottomfish movements exceed the scale of individual fishery closed areas (BRFAs). 
  ## Hypothesis 2 was tested by calculating the home range size for tags that 
  ## were detected at 3 or more locations, including the location the individual 
  ## was tagged. Home range size was determined by fitting a polygon to the most 
  ## northern, southern, eastern, and western coordinates an individual was detected. 
  ## The average length of this polygon, estimated by the square root of the home 
  ## range size was compared the average length dimensions of the BRFAs Median 
  ## value 9.04 km (Min = 6.11, 1st Quantile = 8.75, 3rd Quantile = 14.98, 
  ## Max = 16.39). 
  
  brfa_areas_km2 = get_brfa_areas() ### Gets area of each brfa in km^2
  fivenum(sqrt(brfa_areas_km2)) ### Five Nubmer stats on the sqrt of the brfa area (km)
  
  ## For tags that were detected at only two locations, a linear geodesic 
  ## distance was calculated between the two points and compared to the average 
  ## BRFA length dimension.
  
  ## Number of tags with 3 or more positions for max polygon
  n_tags_with_3_plus_positions = length(which(!is.na(analysis_summary$homerange$max_polygon_area)))
  ## Area for tags with 3 or more positions from max polygon (km^2) - Mean and Standard Error
  print(paste("There are",  n_tags_with_3_plus_positions, "tags with 3 or more known positions to compare to BRFA areas using max polygon. They have a mean area of", round(mean(analysis_summary$homerange$max_polygon_area[!is.na(analysis_summary$homerange$max_polygon_area)]), digits = 2), 'km^2 with a standard error of',  round(std_error(analysis_summary$homerange$max_polygon_area[!is.na(analysis_summary$homerange$max_polygon_area)])[1], digits = 2)))
  ## Area for tags with 3 or more positions from max polygon (km^2) - Median, Min, 1st Quantile, 3rd Quantile, Max
  print(paste("They have a median area of" , round(fivenum(analysis_summary$homerange$max_polygon_area[!is.na(analysis_summary$homerange$max_polygon_area)])[3], digits = 2), "km^2 (min =", round(fivenum(analysis_summary$homerange$max_polygon_area[!is.na(analysis_summary$homerange$max_polygon_area)])[1], digits = 2), ", first quantile =", round(fivenum(analysis_summary$homerange$max_polygon_area[!is.na(analysis_summary$homerange$max_polygon_area)])[2], digits = 2), ", third quantile =", round(fivenum(analysis_summary$homerange$max_polygon_area[!is.na(analysis_summary$homerange[ ,1])])[4], digits = 2), ", max =", round(fivenum(analysis_summary$homerange$max_polygon_area[!is.na(analysis_summary$homerange$max_polygon_area)])[5], digits = 2), ")"))
  ## Square Rooted Area for tags wtih 3 or more positions from max polygon (km^2) - Mean and Standard Error
  print(paste("Their mean square-rooted area is", round(mean(sqrt(analysis_summary$homerange$max_polygon_area[!is.na(analysis_summary$homerange$max_polygon_area)])), digits = 2), ", standard deviation =", round(sd(sqrt(analysis_summary$homerange$max_polygon_area[!is.na(analysis_summary$homerange$max_polygon_area)])), digits = 2)))
  ## Square Rooted Area for tags wtih 3 or more positions from max polygon (km^2) - Median, Min, 1st Quantile, 3rd Quantile, Max
  print(paste("They have a median sqrted length of" , round(fivenum(sqrt(analysis_summary$homerange$max_polygon_area[!is.na(analysis_summary$homerange$max_polygon_area)]))[3], digits = 2), "(min =", round(fivenum(sqrt(analysis_summary$homerange$max_polygon_area[ !is.na(analysis_summary$homerange$max_polygon_area)]))[1], digits = 2), ", first quantile =", round(fivenum(sqrt(analysis_summary$homerange$max_polygon_area[ !is.na(analysis_summary$homerange$max_polygon_area)]))[2], digits = 2), ", third quantile =", round(fivenum(sqrt(analysis_summary$homerange$max_polygon_area[ !is.na(analysis_summary$homerange$max_polygon_area)]))[4], digits = 2), ", max =", round(fivenum(sqrt(analysis_summary$homerange$max_polygon_area[!is.na(analysis_summary$homerange$max_polygon_area)]))[5], digits = 2), ")"))
  
  ## Maximum Linear Distance for all tags - Mean, Standard Error
  print(paste("The mean linear distance for all", length(analysis_summary$homerange$max_linear_distance), "tags was", round(mean(analysis_summary$homerange$max_linear_distance), digits = 2), "km. Standard deviation =", round(sd(analysis_summary$homerange$max_linear_distance), digits = 2)))
  ## Maximum Linear Distance for all tags - Median, Min, 1st Quantile, 3rd 
  print(paste("The median linear distance for the", length(analysis_summary$homerange$max_linear_distance), "tags was", round(fivenum(analysis_summary$homerange$max_linear_distance)[3], digits = 2), "(Min =", round(fivenum(analysis_summary$homerange$max_linear_distance)[1], digits = 2), "First quantile =", round(fivenum(analysis_summary$homerange$max_linear_distance)[2], digits = 2), "Third Quantile =", round(fivenum(analysis_summary$homerange$max_linear_distance)[4], digits = 2), "Max =", round(fivenum(analysis_summary$homerange$max_linear_distance)[5], digits = 2), ")"))
  
  ## Maximum z-constrained least cost distance for all tags - Mean, Standard Error
  print(paste("The mean z-constrained distance for all", length(analysis_summary$homerange$z_constrained_path_distance), "tags was", round(mean(analysis_summary$homerange$z_constrained_path_distance), digits = 2), "km. Standard deviation =", round(sd(analysis_summary$homerange$z_constrained_path_distance), digits = 2)))
  ## Maximum z-constrained least cost Distance for all tags - Median, Min, 1st Quantile, 3rd 
  print(paste("The median z-constrained distance for the", length(analysis_summary$homerange$z_constrained_path_distance), "tags was", round(fivenum(analysis_summary$homerange$z_constrained_path_distance)[3], digits = 2), "(Min =", round(fivenum(analysis_summary$homerange$z_constrained_path_distance)[1], digits = 2), "First quantile =", round(fivenum(analysis_summary$homerange$z_constrained_path_distance)[2], digits = 2), "Third Quantile =", round(fivenum(analysis_summary$homerange$z_constrained_path_distance)[4], digits = 2), "Max =", round(fivenum(analysis_summary$homerange$z_constrained_path_distance)[5], digits = 2), ")"))
  
  #### Making boxplots Comparing the two:
  ### Boxplot Comapring KM^2 Areas
  
  ## Creating a dataframe to store areas of both BRFAs and Fish
  if(length(analysis_summary$homerange$max_polygon_area[!is.na(analysis_summary$homerange$max_polygon_area)]) > 0){
    h2_areas_sqr = as.data.frame(rbind(cbind(analysis_summary$homerange$max_polygon_area[!is.na(analysis_summary$homerange$max_polygon_area)], "P. filamentosus"), cbind(brfa_areas_km2, "BRFA")), stringsAsFactors = FALSE)
    colnames(h2_areas_sqr) = c("area", "source")
    rownames(h2_areas_sqr) = NULL
    h2_areas_sqr$area = as.numeric(h2_areas_sqr$area)
    
    h2_area_plot = ggplot(data = h2_areas_sqr, aes(y = area, x= source)) + 
      geom_boxplot() +
      labs(y = 'km^2', x = 'Source')
    
    if(save_plot){
      pdf(file = "H2 - Boxplot - Homerange vs BRFA Area KM2.pdf")
    }
      print(h2_area_plot)
    if(save_plot){
      dev.off()
    }
  }
  
  ### Boxplot Comparing linear distance
  ## Creating a dataframe to store linear distances for BRFAs and Fish
  h2_areas_linear = as.data.frame(rbind(cbind(sqrt(analysis_summary$homerange$max_linear_distance), "P. filamentosus"), cbind(sqrt(brfa_areas_km2), "BRFA")), stringsAsFactors = FALSE)
  colnames(h2_areas_linear) = c("area", "source")
  rownames(h2_areas_linear) = NULL
  h2_areas_linear$area = as.numeric(h2_areas_linear$area)
  
  h2_linear_plot = ggplot(data = h2_areas_linear, aes(y = area, x= source)) + 
    geom_boxplot() +
    labs(y = 'km', x = 'Source')
  
  if(save_plot){
    pdf(file = "H2 - Max Linear Homerange vs. sqrt BRFA Area.pdf")
  }
  print(h2_linear_plot)
  if(save_plot){
    dev.off()
  }
  ### Boxplot Comparing least z constrained least cost distance
  ## Creating a dataframe to store z constrained least cost distances for BRFAs and Fish
  h2_areas_constrained = as.data.frame(rbind(cbind(analysis_summary$homerange$z_constrained_path_distance, "P. filamentosus"), cbind(brfa_areas_km2, "BRFA")), stringsAsFactors = FALSE)
  colnames(h2_areas_constrained) = c("area", "source")
  rownames(h2_areas_constrained) = NULL
  h2_areas_constrained$area = as.numeric(h2_areas_constrained$area)
  
  h2_areas_linear$area = as.numeric(h2_areas_linear$area)
  
  h2_constrained_plot = ggplot(data = h2_areas_constrained, aes(y = area, x= source)) + 
    geom_boxplot() +
    labs(y = 'km', x = 'Source')
  
  if(save_plot){
    pdf(file = "H2 - Z-Constrained Homerange vs. Z-Constrained BRFA Area.pdf")
  }
  print(h2_constrained_plot)
  if(save_plot){
    dev.off()
  }
  print(paste('The mean maximum observed movement distance for the ', length(analysis_summary$tag_ids), ' tags was ', round(mean(analysis_summary$homerange$z_constrained_path_distance), digits = 2) ,' km (s.d. = ', round(sd(analysis_summary$homerange$z_constrained_path_distance), digits = 2), ' km). The median maximum observed movement distance was ', round(fivenum(analysis_summary$homerange$z_constrained_path_distance)[3], digits = 2), ' km (Min = ',round(fivenum(analysis_summary$homerange$z_constrained_path_distance)[1], digits = 2),', 1st Quantile = ', round(fivenum(analysis_summary$homerange$z_constrained_path_distance)[2], digits = 2), ', 3rd Quantile = ', round(fivenum(analysis_summary$homerange$z_constrained_path_distance)[4], digits = 2), ', Max = ', round(fivenum(analysis_summary$homerange$z_constrained_path_distance)[5], digits = 2),').', sep = ""))
  
}

dissertation_H3 = function(analysis_summary){
  ## Hypothesis 3. Bottomfish do not utilize essential fish habitat uniformly.
  ## Hypothesis 3 was tested by comparing detection of tags moving between two 
  ## stations. A two dimensional matrix was constructed with the number of rows 
  ## and columns equal to the number of total stations tags were detected. Each 
  ## station was assigned a unique row and column. Each detection for each fish 
  ## was compared to the one previous. The for each detection pair, the matrix 
  ## values were incremented by one with rows representing the location of the 
  ## previous detection and columns representing the present location. The 
  ## diagonal values of the matrix identify subsequent detections at the same 
  ## station. 
  detection_matrix = analysis_summary$movement_graph
  
  ## A fish may be more likely to be detected at adjacent stations than 
  ## stations with further physical separation. Attempting to account for this, 
  ## row and column value was multiplied by the calculated distance between their 
  ## representative locations and then divided by the maximum value to standardize 
  ## flow rates relative to a maximum value of 1. 
  standardized_detection_matrix = analysis_summary$movement_graph_standardized_by_distance
  
}

##### Run Function #####
run = function(vue_df, tagging_df, receiver_df, start_date = NULL, end_date = NULL, tag_ids = NULL,  run_description = FALSE, region = "Makapuu", valid_tracks = NULL, questionable_tracks = NULL, expired_tracks = NULL, plot = TRUE, report = TRUE){
  print("Let's Do This.")
  #### Function to run analysis
  function_timer = proc.time()
  
  #### Creating run folder and directory heirarchy
  run_time = Sys.time()
  setwd(results_dir)
  if(run_description != FALSE){
    run.dir = create_save_directory(directory_name = paste(paste(run_time, run_description, sep = " - "), '/', sep = "")) # Creating new directory for run
  }else{
    run.dir = create_save_directory(directory_name = paste(paste(run_time, sep = " - "), '/', sep = "")) # Creating new directory for run
  }
  setwd(run.dir) # Creating new directory for run
  fig.dir = create_save_directory('figures/') ## Creating a directory for saving 
  report.dir = create_save_directory('reports/')
  
  #### Creating a list to serve as a repository for all analysis
  analysis_summary = list()
  
  #### Removing dates and tags if function arguments pass them
  if(!is.null(start_date)){
    # vue_df = filter(vue_df, datetime >= start_date)
    vue_df = vue_df[vue_df$datetime >= start_date, ]
  }
  if(!is.null(end_date)){
    # vue_df = filter(vue_df, datetime <= end_date)
    vue_df = vue_df[vue_df$datetime <= end_date, ]
  }
  if(!is.null(tag_ids[1])){
    vue_df = vue_df[vue_df$tag_id %in% tag_ids, ]
  }
  
  vue_df = extrapolate_detection_depth_from_receiver_df(vue_df = vue_df, receiver_df = receiver_df)
  vue_df = calculate_time_of_day(vue_df)
  
  if(dim(vue_df)[1] > 0){
    ##### Filtering for false detections
    ## False detection filtering is now handled by the UHHPC due to runtime constraints. 
    
    ### Assigning receivers to cluster groups
    receiver_df = assign_receiver_clusters(receiver_df)
    
    ### Reading in analysis bathymetry
    analysis_summary$bathymetry = get_bathymetry(region, resolution = 'low')
    
    ############ Analysis #################
    # print("You can't spell analysis without anal")
    ### Classifying tag status (if not explicitly done)
    if(is.null(valid_tracks)){
      analysis_summary$track_status = determine_track_status(vue_df = vue_df, bathymetry = analysis_summary$bathymetry) # , tagging_df = tagging_df, receiver_df = receiver_df)
      valid_tracks = analysis_summary$track_status$status_df$tag_id[analysis_summary$track_status$status_df$status == 'Valid']
      questionable_tracks = analysis_summary$track_status$status_df$tag_id[analysis_summary$track_status$status_df$status == 'Uncertain']
      expired_tracks = analysis_summary$track_status$status_df$tag_id[analysis_summary$track_status$status_df$status == 'Expired']
    } else {
      ## Adding track statuses to summary df 
      analysis_summary$track_status = list()
      analysis_summary$track_status$status_df = data.frame('tag_id' = sort(unique(vue_df$tag_id)), status = NA, stringsAsFactors = FALSE)
      analysis_summary$track_status$status_df[which(analysis_summary$track_status$status_df$tag_id %in% valid_tracks)] = 'Valid'
      analysis_summary$track_status$status_df[which(analysis_summary$track_status$status_df$tag_id %in% questionable_tracks)] = 'Uncertain'
      analysis_summary$track_status$status_df[which(analysis_summary$track_status$status_df$tag_id %in% expired_tracks)] = 'Expired'
    }
    ## Write out summary
    write.csv(analysis_summary$track_status$status_df, 'Track Statuses.csv')
    ### Creating a adjacency matrix graph of all tag movements. Shows movement from one station to another
    analysis_summary$movement_graph = generate_graph(vue_df = vue_df, receiver_df = receiver_df, start_date = start_date, end_date = end_date, igraph = FALSE, remove_zeros = FALSE)
    ### Creating a matrix populated by the distance in km between each station in space
    analysis_summary$receiver_distance_graph = calculate_distance_between_vue_receivers(vue_df = vue_df)
    ### Standardizing the number of movements by distance between receivers. 
    analysis_summary$movement_graph_standardized_by_distance = (analysis_summary$movement_graph * analysis_summary$receiver_distance_graph)/max((analysis_summary$movement_graph * analysis_summary$receiver_distance_graph))
    ### Creating a "detection" for when and where the animal was tagged
    vue_df = generate_tagging_detection(tagging_df = tagging_df, vue_df = vue_df)
    ### Removing any fish that were tagged but don't show up in the data
    vue_df = remove_fish_with_only_tagging_date(vue_df = vue_df)
    ### Creating study date, the number of days relative to the start of  the study - adjusts all date times relative to start of study
    vue_df = generate_study_date(vue_df) 
    analysis_summary$receiver_df = generate_receiver_study_date(receiver_df = receiver_df, vue_df = vue_df, first_date = NULL)
    analysis_summary$receiver_df = analysis_summary$receiver_df[analysis_summary$receiver_df$deployment_date <= end_date & analysis_summary$receiver_df$recovery_date >= start_date, ]
    ### Getting tagging data for only the fish in our dataset
    analysis_summary$tagging_df = tagging_df[tagging_df$vem_tag_id %in% unique(vue_df$tag_id), ]
    ### Assigning Lunar Phase to Data
    vue_df = assign_lunar_phase(vue_df)
    ### Making detection matrix - Each row is a Tag ID, each station in a column. 
    ### matrix values are number of transmissions for a tag at a given station
    ## Note: Excludes tagging location
    analysis_summary$tag_detection_matrix = generate_detection_matrix(vue_df[vue_df$station != 'Tagging Location', ])  
    
    ### Determining how many times a single tag was detected
    ## Note: Excludes tagging location
    analysis_summary$n_detections_by_tag = count_unique_detections(vue_df = vue_df[vue_df$station != "Tagging Location", ])
    ## How many tags were detected at 2 or more stations?
    ## Note: Excludes tagging location
    analysis_summary$n_stations_by_tag = count_unique_stations(vue_df = vue_df[vue_df$station != "Tagging Location", ])
    ## How many unique days were tags detected?
    ## Note: Excludes tagging
    analysis_summary$unique_days_detected = count_days_detected(vue_df[vue_df$station != 'Tagging Location', ])  
    ## How many days was an individual at liberty?
    analysis_summary$time_at_liberty = calculate_time_at_liberty(vue_df) 
    ## How many days was an individual tracked?
    analysis_summary$track_length = calculate_track_length(vue_df) 
    ## How many transmissions were detected per day on average?
    analysis_summary$transmissions_per_day = list()
    # for entire time at liberty
    analysis_summary$transmissions_per_day$at_liberty = analysis_summary$n_detections_by_tag$n_detections / analysis_summary$track_length$days_tracked
    # only for days a tag was detected
    analysis_summary$transmissions_per_day$days_detected = analysis_summary$n_detections_by_tag$n_detections / analysis_summary$unique_days_detected$unique_days  
    ## How many days after tracking until individual appears on array?
    analysis_summary$days_before_detection = count_days_before_detection(vue_df)  
    ## What percentage of days at liberty was a tag detected?
    analysis_summary$percentage_of_days_at_liberty_detected = analysis_summary$unique_days_detected$unique_days / analysis_summary$track_length$days_tracked
    ## How far was a tag tracked?
    analysis_summary$track_distance = calculate_distance_tracked(vue_df)
    ## How many times was a tag detected changing station?
    analysis_summary$movements = count_movements(vue_df)
    ## What stations was a tag detected on?
    analysis_summary$detected_stations = list_stations_detected(vue_df)
    #### Movements into / out of BRFA for each fish
    analysis_summary$brfa_stats = count_brfa_movements(vue_df)
    #### Standardized movements
    analysis_summary$brfa_movements_standardized_by_track_length = calculate_brfa_movements_by_track_length(analysis_summary$brfa_stats, analysis_summary$track_length$days_tracked)
    #### Number of fish tagged greater than L50 
    analysis_summary$n_mature = length(which(as.numeric(as.character(tagging_df$"fork_length(cm)"[tagging_df$vem_tag_id %in% unique(vue_df$tag_id)])) >= 43))
    analysis_summary$proportion_mature = round(analysis_summary$n_mature / length(unique(vue_df$tag_id)), digits = 2)
    #### Homerange size
    analysis_summary$homerange = calculate_maximum_movement(vue_df, bathymetry = analysis_summary$bathymetry)
    #### Data Density - 0:1 - Unique Days Detected / Days at Liberty
    analysis_summary$data_density = data.frame('tag_id' = analysis_summary$unique_days_detected$tag_id, 'data_density' = c(analysis_summary$unique_days_detected$unique_days / analysis_summary$track_length$days_tracked))
    #### Writing out data for results object
    analysis_summary$data = vue_df
    analysis_summary$tag_ids = unique(vue_df$tag_id)
    analysis_summary$tagging_date = get_tagging_date(vue_df = vue_df)
    analysis_summary$fork_length = get_fork_length(vue_df = vue_df, tagging_df = tagging_df)
    ############# Summarizing results into a table
    print('LETS MAKE THIS SICK ASS SUMMARY TABLE!')
    analysis_summary$summary_df = get_tagging_date(vue_df)
    analysis_summary$summary_df = merge(x = analysis_summary$summary_df, y = analysis_summary$fork_length, by.x = 'tag_id', by.y = 'tag_id')
    analysis_summary$summary_df = merge(x = analysis_summary$summary_df, y = analysis_summary$track_status$status_df[ ,c("tag_id", "status")], by.x = 'tag_id', by.y = 'tag_id')
    analysis_summary$summary_df = merge(x = analysis_summary$summary_df, y = analysis_summary$time_at_liberty[ ,c("tag_id", "days_at_liberty")], by.x = 'tag_id', by.y = 'tag_id')
    analysis_summary$summary_df = merge(x = analysis_summary$summary_df, y = analysis_summary$track_length[ ,c("tag_id", "days_tracked")], by.x = 'tag_id', by.y = 'tag_id')
    analysis_summary$summary_df = merge(x = analysis_summary$summary_df, y = analysis_summary$n_detections_by_tag, by.x = 'tag_id', by.y = 'tag_id')
    analysis_summary$summary_df$mean_detections_per_day = analysis_summary$transmissions_per_day$at_liberty
    analysis_summary$summary_df = merge(x = analysis_summary$summary_df, y = analysis_summary$unique_days_detected, by.x = 'tag_id', by.y = 'tag_id')
    analysis_summary$summary_df = merge(x = analysis_summary$summary_df, y = analysis_summary$track_distance, by.x = 'tag_id', by.y = 'tag_id')
    analysis_summary$summary_df$mean_distance_per_day = analysis_summary$summary_df$distance_tracked / analysis_summary$summary_df$days_at_liberty
    analysis_summary$summary_df = merge(x = analysis_summary$summary_df, y = analysis_summary$n_stations_by_tag, by.x = 'tag_id', by.y = 'tag_id')
    analysis_summary$summary_df = merge(x = analysis_summary$summary_df, y = analysis_summary$movements, by.x = 'tag_id', by.y = 'tag_id')
    analysis_summary$summary_df$meacount_movements_per_day = analysis_summary$summary_df$movements_detected / analysis_summary$summary_df$days_at_liberty
    analysis_summary$summary_df = merge(x = analysis_summary$summary_df, y = analysis_summary$brfa_stats, by.x = 'tag_id', by.y = 'tag_id')
    analysis_summary$summary_df$mean_brfa_movements_per_day = (analysis_summary$summary_df$in_to_out + analysis_summary$summary_df$out_to_in) / analysis_summary$summary_df$days_at_liberty
    analysis_summary$summary_df = merge(x = analysis_summary$summary_df, y = analysis_summary$homerange, by.x = 'tag_id', by.y = 'tag_id')
    analysis_summary$summary_df = merge(x = analysis_summary$summary_df, y = analysis_summary$data_density, by.x = 'tag_id', by.y = 'tag_id')
    
    ################## Answering dissertation Hypotheses #################
    print('UGH... DISSERTATION QUESTIONS?!')
    analysis_summary$h1_results = dissertation_H1(analysis_summary, save_plot = F)
    analysis_summary$h2_results = dissertation_H2(analysis_summary)
    analysis_summary$h3_results = dissertation_H3(analysis_summary)
    
    ########### Plotting, Charting, and Other Pretty Things ##############
    if(plot == TRUE){
      print('ONWARDS TO PLOTTING!')
      setwd(run.dir)
      setwd(fig.dir)
      
      ## Assigning station colors
      print('assigning station pallet')
      color_palette = assign_color_palette(analysis_summary$data) # Assigning mapping colors
      
      
      ### Plotting Depth by Time of Day Coded by Station
      plot_detection_records(vue_df = analysis_summary$data, receiver_df = analysis_summary$receiver_df, start_date = start_date, end_date = end_date, color_palette = color_palette)
      
      ## Plotting Six pannel plots  
      six_pannel_plot(analysis_summary, start_date = start_date, end_date = end_date, color_palette = color_palette)
      
      ### Plotting receiver maps
      print('plotting receiver maps')
      # plot_receiver_maps(receiver_df = analysis_summary$receiver_df, start_date = start_date, end_date = end_date, rec_col = color_palette, region = region, bathymetry = analysis_summary$bathymetry)
      
      ### Plotting the location where fish were tagged
      print('plotting tagging locations')
      plot_tagging_locations(tagging_df = tagging_df, tag_ids = analysis_summary$track_status$status_df$tag_id, bathymetry = analysis_summary$bathymetry)
      plot_tagging_locations_and_receivers(tagging_df = analysis_summary$tagging_df, receiver_df = analysis_summary$receiver_df, bathymetry = analysis_summary$bathymetry)
      
      ## Plotting receiver use by individuals
      print('plotting receiver use by individual')
      # plot_receiver_use_individual_map(vue_df = vue_df, receiver_df = receiver_df, bathymetry = analysis_summary$bathymetry)
      
      ## Plotting receiver use as a function of the moon phase
      print('plotting lunar phases')
      # plot_lunar_phases(vue_df = vue_df, receiver_df = receiver_df, bathymetry = analysis_summary$bathymetry)
      
      ### Plotting stripchart of all detections, with mortality
      if(length(valid_tracks) > 0 & length(questionable_tracks) > 0 & length(expired_tracks) > 0){
        print('plotting mortality stripchart')
        # plot_mortality_stripchart(analysis_summary, valid_tags = analysis_summary$track_status$valid_tracks, questionable_tags = questionable_tracks, expired_tags = expired_tracks)
      }
      
      ### Plotting movements of each tag tags
      print('plotting tag detections')
      plot_tag_detections(analysis_summary$data, receiver_df, start_date = start_date, end_date = end_date, date_format = "%Y-%m-%d %H:%M:%S", plot_lost = FALSE, remove_station = FALSE, tag_ids = FALSE, rec_col = FALSE, plot_receivers = "study", region = region, bathymetry = analysis_summary$bathymetry)
      
      ## plotting relative movements of all fish
      print('plotting path use')
      plot_path_use(movement_graph = analysis_summary$movement_graph, vue_df = vue_df, receiver_df = receiver_df, region = region, bathymetry = analysis_summary$bathymetry)
      
      #### Detection stripchart for each tag with color corrosponding to stations
      print('generating individual stripcharts by station')
      plot_vemco_stripplot(vue_df = analysis_summary$data)
      print('generating day night plots')
      plot_day_night(vue_df = analysis_summary$data, receiver_df = analysis_summary$receiver_df, color_palette = color_palette, save_plot = TRUE) # Note: Omitting the receiver_df argument will produce plots without the blue horizontal bars
      
      #### Detection histogram for each tag
      print('generating individual deteciton histograms')
      plot_tag_detection_histogram(vue_df = analysis_summary$data, receiver_df = analysis_summary$receiver_df) # Note: Ommitting the receiver_df argument will produce plots without the blue horizontal bars
      
      #### Difference in time between detections histogram - Plotting number of span of days a fish was absent from array. Note = 1 means fish detected the next day.
      print('generating histogram of days absent on the array')
      # plot_indv_difftimes_hist(vue_df = analysis_summary$data, max_diff_days = 30, ignore_same_day_detections = TRUE, output_dir = NULL)
      
      #### Plotting all tag detections for mortality chart
      print('plotting all tag detections for mortality chart')
      plot_detection_stripchart(analysis_summary = analysis_summary)
      
      ### Plotting z constrained least cost homerange movements
      calculate_maximum_movement(analysis_summary$data, bathymetry = analysis_summary$bathymetry, plot_results = TRUE)
      
      #### Plotting depths for tags with depth sensors
      print('plotting depths')
      plot_depths(vue_df = analysis_summary$data)
      
      #### Performing depth analysis
      print('performing depth analysis')
      #analyze_diurnal_depth_by_tag(vue_df = analysis_summary$data)
      #analyze_diurnal_depth_by_tag_by_station(vue_df = vue_df)
      
      ## Making Gif images
      print('making individual GIF files.')
      # generate_gif_images(vue_df = analysis_summary$data, receiver_df = analysis_summary$receiver_df, region = region, bathymetry = analysis_summary$bathymetry)
      
      ### Histograms of various metrics
      setwd(fig.dir)
      ## Fork Length
      print('plotting fork length histogram')
      hist_breaks = seq(min(as.numeric(analysis_summary$summary_df$fork_length))-5, max(as.numeric(analysis_summary$summary_df$fork_length))+5, by = 5)
      pre_hist = hist(as.numeric(analysis_summary$summary_df$fork_length), breaks = hist_breaks, plot = FALSE)
      png('fl hist.png', width = 826, height = 437)
      hist(as.numeric(analysis_summary$summary_df$fork_length), xlab = 'Fork Length (cm)', main = NULL, breaks = hist_breaks, xlim = c(min(hist_breaks), max(hist_breaks)), ylim = c(0, max(pre_hist$counts)*1.2))
      text(x = hist_breaks + 2.5, y = pre_hist$counts + max(pre_hist$count) / 10 , labels = pre_hist$counts)
      legend('topright', legend = c(paste('Median =',  fivenum(as.numeric(analysis_summary$summary_df$fork_length), na.rm = TRUE)[3], '\n IQR =', fivenum(as.numeric(analysis_summary$summary_df$fork_length), na.rm = TRUE)[2], '-', fivenum(as.numeric(analysis_summary$summary_df$fork_length), na.rm = TRUE)[4])))
      dev.off()
      
      ## Days at Liberty
      print('plotting time at liberty histogram')
      hist_breaks = seq(min(as.numeric(analysis_summary$summary_df$days_at_liberty))-5, max(as.numeric(analysis_summary$summary_df$days_at_liberty))+5, by = 5)
      pre_hist = hist(as.numeric(analysis_summary$summary_df$days_at_liberty), breaks = hist_breaks, plot = FALSE)
      png('tal hist.png', width = 826, height = 437)
      hist(as.numeric(analysis_summary$summary_df$days_at_liberty), xlab = 'Time at Liberty (Days)', main = NULL, breaks = hist_breaks, xlim = c(min(hist_breaks), max(hist_breaks)), ylim = c(0, max(pre_hist$counts)*1.2))
      # text(x = hist_breaks + 2.5, y = pre_hist$counts + max(pre_hist$count) / 10 , labels = pre_hist$counts)
      legend('topright', legend = c(paste('Median =',  fivenum(as.numeric(analysis_summary$summary_df$days_at_liberty), na.rm = TRUE)[3], '\n IQR =', fivenum(as.numeric(analysis_summary$summary_df$days_at_liberty), na.rm = TRUE)[2], '-', fivenum(as.numeric(analysis_summary$summary_df$days_at_liberty), na.rm = TRUE)[4])))
      dev.off()
      
      ## Number of Detections
      print('plotting n detections histogram')
      hist_breaks = seq(0, max(as.numeric(analysis_summary$summary_df$n_detections))+500, by = 500)
      pre_hist = hist(as.numeric(analysis_summary$summary_df$n_detections), breaks = hist_breaks, plot = FALSE)
      png('n detections.png', width = 826, height = 437)
      hist(as.numeric(analysis_summary$summary_df$n_detections), xlab = '# of detections', main = NULL, breaks = hist_breaks, xlim = c(min(hist_breaks), max(hist_breaks)), ylim = c(0, max(pre_hist$counts)*1.2))
      # text(x = hist_breaks + 2.5, y = pre_hist$counts + max(pre_hist$count) / 10 , labels = pre_hist$counts)
      legend('topright', legend = c(paste('Median =',  fivenum(as.numeric(analysis_summary$summary_df$n_detections), na.rm = TRUE)[3], '\n IQR =', fivenum(as.numeric(analysis_summary$summary_df$n_detections), na.rm = TRUE)[2], '-', fivenum(as.numeric(analysis_summary$summary_df$n_detections), na.rm = TRUE)[4])))
      dev.off()
      
      ## Detections Per Day
      print('plotting detections per day histogram')
      hist_breaks = seq(min(as.numeric(analysis_summary$summary_df$mean_detections_per_day))-5, max(as.numeric(analysis_summary$summary_df$mean_detections_per_day))+5, by = 5)
      pre_hist = hist(as.numeric(analysis_summary$summary_df$mean_detections_per_day), breaks = hist_breaks, plot = FALSE)
      png('detections per day hist.png', width = 826, height = 437)
      hist(as.numeric(analysis_summary$summary_df$mean_detections_per_day), xlab = 'Mean Detections Per Day', main = NULL, breaks = hist_breaks, xlim = c(min(hist_breaks), max(hist_breaks)), ylim = c(0, max(pre_hist$counts)*1.2))
      # text(x = hist_breaks + 2.5, y = pre_hist$counts + max(pre_hist$count) / 10 , labels = pre_hist$counts)
      legend('topright', legend = c(paste('Median =',  round(fivenum(as.numeric(analysis_summary$summary_df$mean_detections_per_day), na.rm = TRUE)[3], digits = 2), '\n IQR =', round(fivenum(as.numeric(analysis_summary$summary_df$mean_detections_per_day), na.rm = TRUE)[2], digits = 2), '-', round(fivenum(as.numeric(analysis_summary$summary_df$mean_detections_per_day), na.rm = TRUE)[4], digits = 2))))
      dev.off()
      
      ## Unique Days Detected
      print('plotting unique days detected histogram')
      hist_breaks = seq(0, max(as.numeric(analysis_summary$summary_df$unique_days))+5, by = 5)
      pre_hist = hist(as.numeric(analysis_summary$summary_df$unique_days), breaks = hist_breaks, plot = FALSE)
      png('unique days hist.png', width = 826, height = 437)
      hist(as.numeric(analysis_summary$summary_df$unique_days), xlab = 'Unique Days Detected', main = NULL, breaks = hist_breaks, xlim = c(min(hist_breaks), max(hist_breaks)), ylim = c(0, max(pre_hist$counts)*1.2))
      # text(x = hist_breaks + 2.5, y = pre_hist$counts + max(pre_hist$count) / 10 , labels = pre_hist$counts)
      legend('topright', legend = c(paste('Median =',  round(fivenum(as.numeric(analysis_summary$summary_df$unique_days), na.rm = TRUE)[3], digits = 2), '\n IQR =', round(fivenum(as.numeric(analysis_summary$summary_df$unique_days), na.rm = TRUE)[2], digits = 2), '-', round(fivenum(as.numeric(analysis_summary$summary_df$unique_days), na.rm = TRUE)[4], digits = 2))))
      dev.off()
      
      ## Distance Tracked
      print('plotting distance tracked histogram')
      hist_breaks = seq(0, max(as.numeric(analysis_summary$summary_df$distance_tracked))+5, by = 5)
      pre_hist = hist(as.numeric(analysis_summary$summary_df$distance_tracked), breaks = hist_breaks, plot = FALSE)
      png('distance tracked hist.png', width = 826, height = 437)
      hist(as.numeric(analysis_summary$summary_df$distance_tracked), xlab = 'Distance Tracked (km)', main = NULL, breaks = hist_breaks, xlim = c(min(hist_breaks), max(hist_breaks)), ylim = c(0, max(pre_hist$counts)*1.2))
      # text(x = hist_breaks + 2.5, y = pre_hist$counts + max(pre_hist$count) / 10 , labels = pre_hist$counts)
      legend('topright', legend = c(paste('Median =',  round(fivenum(as.numeric(analysis_summary$summary_df$distance_tracked), na.rm = TRUE)[3], digits = 2), '\n IQR =', round(fivenum(as.numeric(analysis_summary$summary_df$distance_tracked), na.rm = TRUE)[2], digits = 2), '-', round(fivenum(as.numeric(analysis_summary$summary_df$distance_tracked), na.rm = TRUE)[4], digits = 2))))
      dev.off()
      
      ## Distance Per Day
      print('plotting distance per day histogram')
      hist_breaks = seq(0, max(as.numeric(analysis_summary$summary_df$mean_distance_per_day))+.05, by = .05)
      pre_hist = hist(as.numeric(analysis_summary$summary_df$mean_distance_per_day), breaks = hist_breaks, plot = FALSE)
      png('distance per day hist.png', width = 826, height = 437)
      hist(as.numeric(analysis_summary$summary_df$mean_distance_per_day), xlab = 'Mean Distance Tracked Per Day (km)', main = NULL, breaks = hist_breaks, xlim = c(min(hist_breaks), max(hist_breaks)), ylim = c(0, max(pre_hist$counts)*1.2))
      # text(x = hist_breaks + 2.5, y = pre_hist$counts + max(pre_hist$count) / 10 , labels = pre_hist$counts)
      legend('topright', legend = c(paste('Median =',  round(fivenum(as.numeric(analysis_summary$summary_df$mean_distance_per_day), na.rm = TRUE)[3], digits = 2), '\n IQR =', round(fivenum(as.numeric(analysis_summary$summary_df$mean_distance_per_day), na.rm = TRUE)[2], digits = 2), '-', round(fivenum(as.numeric(analysis_summary$summary_df$mean_distance_per_day), na.rm = TRUE)[4], digits = 2))))
      dev.off()
      
      ## Receivers Detected
      print('plotting receivers detected histogram')
      hist_breaks = seq(0, max(as.numeric(analysis_summary$summary_df$n_stations))+5, by = 1)
      pre_hist = hist(as.numeric(analysis_summary$summary_df$n_stations), breaks = hist_breaks, plot = FALSE)
      png('n receivers detected.png', width = 826, height = 437)
      hist(as.numeric(analysis_summary$summary_df$n_stations), xlab = 'n Receivers Detected', main = NULL, breaks = hist_breaks, xlim = c(min(hist_breaks), max(hist_breaks)), ylim = c(0, max(pre_hist$counts)*1.2))
      # text(x = hist_breaks + 2.5, y = pre_hist$counts + max(pre_hist$count) / 10 , labels = pre_hist$counts)
      legend('topright', legend = c(paste('Median =',  round(fivenum(as.numeric(analysis_summary$summary_df$n_stations), na.rm = TRUE)[3], digits = 2), '\n IQR =', round(fivenum(as.numeric(analysis_summary$summary_df$n_stations), na.rm = TRUE)[2], digits = 2), '-', round(fivenum(as.numeric(analysis_summary$summary_df$n_stations), na.rm = TRUE)[4], digits = 2))))
      dev.off()
      
      ## Movements Detected
      print('plotting movements detected histogram')
      hist_breaks = seq(0, max(as.numeric(analysis_summary$summary_df$movements))+1, by = 1)
      pre_hist = hist(as.numeric(analysis_summary$summary_df$movements), breaks = hist_breaks, plot = FALSE)
      png('n movements detected.png', width = 826, height = 437)
      hist(as.numeric(analysis_summary$summary_df$movements), xlab = 'n Movements Detected', main = NULL, breaks = hist_breaks, xlim = c(min(hist_breaks), max(hist_breaks)), ylim = c(0, max(pre_hist$counts)*1.2))
      # text(x = hist_breaks + 2.5, y = pre_hist$counts + max(pre_hist$count) / 10 , labels = pre_hist$counts)
      legend('topright', legend = c(paste('Median =',  round(fivenum(as.numeric(analysis_summary$summary_df$movements), na.rm = TRUE)[3], digits = 2), '\n IQR =', round(fivenum(as.numeric(analysis_summary$summary_df$movements), na.rm = TRUE)[2], digits = 2), '-', round(fivenum(as.numeric(analysis_summary$summary_df$movements), na.rm = TRUE)[4], digits = 2))))
      dev.off()
      
      ## Mean Movements Per Day
      print('plotting movements per day histogram')
      hist_breaks = seq(0, max(analysis_summary$summary_df$meacount_movements_per_day)+1, by = 1)
      pre_hist = hist(analysis_summary$summary_df$meacount_movements_per_day, breaks = hist_breaks, plot = FALSE)
      png('mean movements by tal.png',  width = 826, height = 437)
      hist(analysis_summary$summary_df$meacount_movements_per_day, xlab = 'Mean Movements Per Day at Liberty', main = NULL, breaks = hist_breaks, xlim = c(min(hist_breaks), max(hist_breaks)), ylim = c(0, max(pre_hist$counts)*1.2))
      # text(x = hist_breaks + 2.5, y = pre_hist$counts + max(pre_hist$count) / 10 , labels = pre_hist$counts)
      legend('topright', legend = c(paste('Median =',  round(fivenum(analysis_summary$summary_df$meacount_movements_per_day, na.rm = TRUE)[3], digits = 2), '\n IQR =', round(fivenum(analysis_summary$summary_df$meacount_movements_per_day, na.rm = TRUE)[2], digits = 2), '-', round(fivenum(analysis_summary$summary_df$meacount_movements_per_day, na.rm = TRUE)[4], digits = 2))))
      dev.off()
      
      ## BRFA Crossings Detected
      print('plotting BRFA crossings detected histogram')
      hist_breaks = seq(0, max(as.numeric(analysis_summary$summary_df$total_brfa_crossings))+1, by = 1)
      pre_hist = hist(as.numeric(analysis_summary$summary_df$total_brfa_crossings), breaks = hist_breaks, plot = FALSE)
      png('BRFA Crossings Detected.png', width = 826, height = 437)
      hist(as.numeric(analysis_summary$summary_df$total_brfa_crossings), xlab = 'Movements Across BRFA', main = NULL, breaks = hist_breaks, xlim = c(min(hist_breaks), max(hist_breaks)), ylim = c(0, max(pre_hist$counts)*1.2))
      # text(x = hist_breaks + 2.5, y = pre_hist$counts + max(pre_hist$count) / 10 , labels = pre_hist$counts)
      legend('topright', legend = c(paste('Median =',  round(fivenum(as.numeric(analysis_summary$summary_df$total_brfa_crossings), na.rm = TRUE)[3], digits = 2), '\n IQR =', round(fivenum(as.numeric(analysis_summary$summary_df$total_brfa_crossings), na.rm = TRUE)[2], digits = 2), '-', round(fivenum(as.numeric(analysis_summary$summary_df$total_brfa_crossings), na.rm = TRUE)[4], digits = 2))))
      dev.off()
      
      ## Mean BRFA Crossings Detected Per Day
      print('plotting BRFA crossings per day histogram')
      hist_breaks = seq(0, max(as.numeric(analysis_summary$summary_df$mean_brfa_movements_per_day))+.1, by = .01)
      pre_hist = hist(as.numeric(analysis_summary$summary_df$mean_brfa_movements_per_day), breaks = hist_breaks, plot = FALSE)
      png('Mean BRFA Crossings Per Day.png', width = 826, height = 437)
      hist(as.numeric(analysis_summary$summary_df$mean_brfa_movements_per_day), xlab = 'Movements Across BRFA / Time At Liberty', main = NULL, breaks = hist_breaks, xlim = c(min(hist_breaks), max(hist_breaks)), ylim = c(0, max(pre_hist$counts)*1.2))
      # text(x = hist_breaks + 2.5, y = pre_hist$counts + max(pre_hist$count) / 10 , labels = pre_hist$counts)
      legend('topright', legend = c(paste('Median =',  round(fivenum(as.numeric(analysis_summary$summary_df$mean_brfa_movements_per_day), na.rm = TRUE)[3], digits = 2), '\n IQR =', round(fivenum(as.numeric(analysis_summary$summary_df$mean_brfa_movements_per_day), na.rm = TRUE)[2], digits = 2), '-', round(fivenum(as.numeric(analysis_summary$summary_df$mean_brfa_movements_per_day), na.rm = TRUE)[4], digits = 2))))
      dev.off()
      
      ## Time Tracked In and out
      print('plotting time in and time out histogram')
      png('in and out hist.png', width = 826, height = 700)
      par(mfrow = c(2, 1))
      hist(analysis_summary$brfa_stats$days_tracked_in / (analysis_summary$brfa_stats$days_tracked_in + analysis_summary$brfa_stats$days_tracked_out), xlim = c(0, 1), main = '% of Time In BRFA', xlab = '')
      hist(analysis_summary$brfa_stats$days_tracked_out / (analysis_summary$brfa_stats$days_tracked_in + analysis_summary$brfa_stats$days_tracked_out), xlim = c(0, 1), main = '% of Time Outside BRFA', xlab = '')
      dev.off()
      
      ## Home Range - Max Polygon Area
      print('plotting home range - polygon histogram')
      if(any(!is.na(analysis_summary$summary_df$max_polygon_area))){
        hist_breaks = seq(0, max(as.numeric(analysis_summary$summary_df$max_polygon_area), na.rm = TRUE)+1, by = 1)
        pre_hist = hist(as.numeric(analysis_summary$summary_df$max_polygon_area), breaks = hist_breaks, plot = FALSE)
        png('Polygonal Homerange.png', width = 826, height = 437)
        hist(as.numeric(analysis_summary$summary_df$max_polygon_area), xlab = 'Polygonal Homerange (km)', main = NULL, breaks = hist_breaks, xlim = c(min(hist_breaks), max(hist_breaks)), ylim = c(0, max(pre_hist$counts)*1.2))
        # text(x = hist_breaks + 2.5, y = pre_hist$counts + max(pre_hist$count) / 10 , labels = pre_hist$counts)
        legend('topright', legend = c(paste('Median =',  round(fivenum(as.numeric(analysis_summary$summary_df$max_polygon_area), na.rm = TRUE)[3], digits = 2), '\n IQR =', round(fivenum(as.numeric(analysis_summary$summary_df$max_polygon_area), na.rm = TRUE)[2], digits = 2), '-', round(fivenum(as.numeric(analysis_summary$summary_df$max_polygon_area), na.rm = TRUE)[4], digits = 2))))
        dev.off()
      }
      
      ## Home Range - Linear Distance
      print('plotting home range - linear distance histogram')
      hist_breaks = seq(0, max(as.numeric(analysis_summary$summary_df$max_linear_distance))+1, by = 1)
      pre_hist = hist(as.numeric(analysis_summary$summary_df$max_linear_distance), breaks = hist_breaks, plot = FALSE)
      png('Linear Homerange.png', width = 826, height = 437)
      hist(as.numeric(analysis_summary$summary_df$max_linear_distance), xlab = 'Linear Homerange (km)', main = NULL, breaks = hist_breaks, xlim = c(min(hist_breaks), max(hist_breaks)), ylim = c(0, max(pre_hist$counts)*1.2))
      # text(x = hist_breaks + 2.5, y = pre_hist$counts + max(pre_hist$count) / 10 , labels = pre_hist$counts)
      legend('topright', legend = c(paste('Median =',  round(fivenum(as.numeric(analysis_summary$summary_df$max_linear_distance), na.rm = TRUE)[3], digits = 2), '\n IQR =', round(fivenum(as.numeric(analysis_summary$summary_df$max_linear_distance), na.rm = TRUE)[2], digits = 2), '-', round(fivenum(as.numeric(analysis_summary$summary_df$max_linear_distance), na.rm = TRUE)[4], digits = 2))))
      dev.off()
      
      ### Data Density - Unique Days Detected / Days at Liberty
      print('plotting data density histogram')
      hist_breaks = seq(0, max(as.numeric(analysis_summary$summary_df$data_density))+0.1, by = .1)
      pre_hist = hist(analysis_summary$summary_df$data_density, breaks = hist_breaks, plot = FALSE)
      png('data density - days_detected per day at liberty.png', width = 826, height = 437)
      hist(analysis_summary$summary_df$data_density, xlab = 'Unique Days / Time at Liberty', main = NULL, breaks = hist_breaks, xlim = c(min(hist_breaks), max(hist_breaks)), ylim = c(0, max(pre_hist$counts)*1.2))
      # text(x = hist_breaks + 2.5, y = pre_hist$counts + max(pre_hist$count) / 10 , labels = pre_hist$counts)
      legend('topright', legend = c(paste('Median =',  round(fivenum(analysis_summary$summary_df$data_density, na.rm = TRUE)[3], digits = 2), '\n IQR =', round(fivenum(analysis_summary$summary_df$data_density, na.rm = TRUE)[2], digits = 2), '-', round(fivenum(analysis_summary$summary_df$data_density, na.rm = TRUE)[4], digits = 2))))
      dev.off()
      
      print('plotting brfa crossings per day at liberty histogram')
      png('brfa crossings per day at liberty.png', width = 826, height = 437)
      hist(analysis_summary$brfa_movements_standardized_by_track_length$total)
      dev.off()
      
      ### Frequency of fish movements detected.
      print('plotting homeranges barplot')
      analysis_summary$homerange$distance_bin = NA
      analysis_summary$homerange$distance_bin[analysis_summary$homerange$z_constrained_path_distance < 1] = '< 1 km'
      analysis_summary$homerange$distance_bin[analysis_summary$homerange$z_constrained_path_distance >= 1 & analysis_summary$homerange$z_constrained_path_distance < 5] = '1-5 km'
      analysis_summary$homerange$distance_bin[analysis_summary$homerange$z_constrained_path_distance >= 5 & analysis_summary$homerange$z_constrained_path_distance < 10] = '5-10 km'
      analysis_summary$homerange$distance_bin[analysis_summary$homerange$z_constrained_path_distance >= 10 & analysis_summary$homerange$z_constrained_path_distance < 20] = '10-20 km'
      analysis_summary$homerange$distance_bin[analysis_summary$homerange$z_constrained_path_distance > 20] = '> 20 km'
      homeranges_for_barchart = aggregate(analysis_summary$homerange$tag_id, by = list(analysis_summary$homerange$distance_bin), FUN = length)
      colnames(homeranges_for_barchart) = c('distance', 'n')
      ## Converting counts to percentages
      homeranges_for_barchart$percent = homeranges_for_barchart$n / sum(homeranges_for_barchart$n)
      ## Adding in columns that may not exist
      
      if(!"< 1 km" %in% homeranges_for_barchart$distance){
        homeranges_for_barchart = rbind(homeranges_for_barchart, data.frame('distance' = "< 1 km", 'n' = 0, 'percent' = 0))
      }
      if(!"1-5 km" %in% homeranges_for_barchart$distance){
        homeranges_for_barchart = rbind(homeranges_for_barchart, data.frame('distance' = "1-5 km", 'n' = 0, 'percent' = 0))
      }
      if(!"5-10 km" %in% homeranges_for_barchart$distance){
        homeranges_for_barchart = rbind(homeranges_for_barchart, data.frame('distance' = "5-10 km", 'n' = 0, 'percent' = 0))
      }
      if(!"10-20 km" %in% homeranges_for_barchart$distance){
        homeranges_for_barchart = rbind(homeranges_for_barchart, data.frame('distance' = "10-20 km", 'n' = 0, 'percent' = 0))
      }
      if(!"> 20 km" %in% homeranges_for_barchart$distance){
        homeranges_for_barchart = rbind(homeranges_for_barchart, data.frame('distance' = "> 20 km", 'n' = 0, 'percent' = 0))
      }
      ## Reordering for barcharting
      homeranges_for_barchart = homeranges_for_barchart[c(which(homeranges_for_barchart$distance == "< 1 km"), which(homeranges_for_barchart$distance == "1-5 km"), which(homeranges_for_barchart$distance == "5-10 km"), which(homeranges_for_barchart$distance == "10-20 km"), which(homeranges_for_barchart$distance == "> 20 km")), ]
      
      
      png('movement_bar_Scherrer.png',  width = 826, height = 216)
      barplot(height = homeranges_for_barchart$percent * 100, names.arg = homeranges_for_barchart$distance,
              main = 'Acoustic Tagging Data',
              ylab = '% of Fish',
              xlab = 'Z-constrained Home Range Distance')
      dev.off()
      
      # Fake a barchart for O'malley Data
      print("faking barchart for o'malley data")
      png('movement_bar_Omalley2015.png', width = 826, height = 216)
      pifg = list()
      pifg$distance = c('< 1 km', '1-5 km', '5-10 km', '10-20 km', '> 20 km')
      pifg$percent = c(53, 33, 5, 4, 6)
      barplot(height = pifg$percent, names.arg = pifg$distance,
              main = 'PIFG Mark Recapture Data',
              ylab = '% of Recaptures',
              xlab = 'Distance from Tagging Location (km)')
      dev.off()
      
      ### Histogram of movement by catagory
      print('plotting histogram of individuals by movement catagory')
      analysis_summary$brfa_stats$catagory = NA
      ## Locals never leave
      analysis_summary$brfa_stats$catagory[which(analysis_summary$brfa_stats$in_to_out == 0 & analysis_summary$brfa_stats$out_to_in == 0 &  analysis_summary$brfa_stats$days_tracked_in > 0)] = 'local'
      ## Commuters move across boundaries
      analysis_summary$brfa_stats$catagory[which(analysis_summary$brfa_stats$in_to_out > 0 & analysis_summary$brfa_stats$out_to_in > 0)] = 'commuter'
      ## Tourists were tagged in, and then left
      analysis_summary$brfa_stats$catagory[which( analysis_summary$brfa_stats$in_to_out > 0 &  analysis_summary$brfa_stats$out_to_in == 0)] = 'tourist'
      ## Transplants are tagged outside and then move in, never leave
      analysis_summary$brfa_stats$catagory[which( analysis_summary$brfa_stats$in_to_out == 0 & analysis_summary$brfa_stats$out_to_in > 0)] = 'transplant'
      ## Outsiders are never detected in the BRFA
      analysis_summary$brfa_stats$catagory[which( analysis_summary$brfa_stats$in_to_out == 0 &  analysis_summary$brfa_stats$out_to_in == 0 &  analysis_summary$brfa_stats$time_tracked_out > 0)] = 'outsider'
      ## Aggregating classification data
      brfa_catagory = aggregate( analysis_summary$brfa_stats$tag_id, by = list( analysis_summary$brfa_stats$catagory), FUN = uniqueN)
      colnames(brfa_catagory) = c('classification', 'n')
      ## Plotting
      png('Residency Status of Tags.png', width = 826, height = 437)
      brfa_catagory = brfa_catagory[c(which(brfa_catagory$classification == 'local'), which(brfa_catagory$classification == 'transplant'), which(brfa_catagory$classification == 'commuter'), which(brfa_catagory$classification == 'tourist'), which(brfa_catagory$classification == 'outsider')), ]
      barplot(height = brfa_catagory$n, names.arg = brfa_catagory$classification, ylim = c(0, max(brfa_catagory$n) * 1.1), main = 'Residency Status')
      text(x = c(.75, 1.9, 3.1, 4.3, 5.45), y = c(brfa_catagory$n + max(brfa_catagory$n) * 0.05), labels = brfa_catagory$n)
      dev.off()
      
      ### Histogram of mortalitiy decision tree sorting outcomes
      print('plotting mortality status of tags')
      status_counts = aggregate(analysis_summary$track_status$status_df$tag_id, by = list(analysis_summary$track_status$status_df$status), FUN = uniqueN)
      
      colnames(status_counts) = c('classification', 'n')
      #png('Mortality Status of Tags.png', width = 826, height = 437)
      #status_counts = status_counts[c(which(status_counts$classification == 'Valid'), which(status_counts$classification == 'Uncertain'), which(status_counts$classification == 'Expired')), ]
      #barplot(height = status_counts$n, names.arg = status_counts$classification, col = c('Green', 'Yellow', 'Red'), ylim = c(0, max(status_counts$n) * 1.2), main = 'Tag Status')
      #text(x = c(.75,1.9,3.1), y = c(status_counts$n + max(status_counts$n) * 0.1), labels = status_counts$n)
      #dev.off()
      
      setwd(run.dir)
      
    } # End if(plot == TRUE){
    
    ################ Reports ##############
    if(report == TRUE){
      print('lets make some reports!')
      ## Setting report directory
      setwd(report.dir)
      
      ## Writing out Analysis Summary as a .csv file
      write.csv(analysis_summary$summary_df, file = "analysis.csv", row.names = FALSE)
      
      ###### Individual reports for each Tag ID
      #### Outputting analysis summary data for each tag ID into analysis_output.txt
      sink("Individual Reports.txt")
      for(i in 1:length(unique(analysis_summary$summary_df$tag_id))){
        cat(paste("Transmitter ID: ", unique(analysis_summary$data$tag_id)[i], "\n"))
        cat(paste("Species: ", tagging_df$species[which(tagging_df$vem_tag_id == unique(analysis_summary$data$tag_id)[i])], "\n"))
        cat(paste("Fork Length: ", analysis_summary$summary_df$fork_length_cm[i], " cm", "\n"))
        cat(paste("Tagging Date: ", analysis_summary$summary_df$tagging_date, "\n"))
        cat(paste("Tagging Location: ", tagging_df$area_of_capture[which(tagging_df$vem_tag_id == unique(analysis_summary$summary_df$tag_id)[i])], "\n"))  
        cat(paste("Day of First Detected: ", analysis_summary$data$datetime[which(analysis_summary$data$tag_id == unique(analysis_summary$data$tag_id)[i] & analysis_summary$data$station != "Tagging Location")][1], "\n"))
        cat(paste("Day of Last Detection: ", analysis_summary$time_at_liberty$max_date[i], "\n"))
        cat(paste("Days at Liberty: ", ceiling(analysis_summary$summary_df$days_at_liberty[i]), "\n"))
        cat(paste("Track Length: ", analysis_summary$track_length$max_date[i], "\n"))
        cat(paste("Unique Days Detected: ", analysis_summary$summary_df$unique_days[i], "\n"))
        cat(paste("Number of Detections (Total): ", analysis_summary$n_detections_by_tag$n_detections[i], "\n"))
        cat(paste("Number of Detections / Day at Liberty: ", analysis_summary$transmissions_per_day$at_liberty[i], "\n"))
        cat(paste("Number of Detections / Day Detected: ", analysis_summary$transmissions_per_day$days_detected[i], "\n"))
        cat(paste("Number of Receivers Detected (Total): ", analysis_summary$n_stations_by_tag$n_stations[i], "\n"))
        cat(paste("Number of receivers Detected / Days at Liberty: ", analysis_summary$n_stations_by_tag$n_stations[i] / analysis_summary$summary_df$days_at_liberty[i], "\n"))
        cat(paste("Movements from inside to outside BRFA (Total): ", analysis_summary$summary_df$in_to_out[i], "\n"))
        cat(paste("Movements from inside to outside BRFA / Day: ", analysis_summary$summary_df$in_to_out[i] / analysis_summary$summary_df$days_at_liberty[i], "\n"))
        cat(paste("Movements from outside to inside BRFA (Total): ", analysis_summary$summary_df$out_to_in[i], "\n"))
        cat(paste("Movements from outside to inside BRFA / Day: ", analysis_summary$summary_df$out_to_in[i]/analysis_summary$summary_df$days_at_liberty[i], "\n"))
        cat(paste("Time in BRFA: ", analysis_summary$summary_df$days_tracked_in[i], "\n"))
        cat(paste("Time out of BRFA: ", analysis_summary$summary_df$days_tracked_out[i], "\n"))
        cat(paste("Approximate Distance Tracked (Total): ", analysis_summary$summary_df$distance_tracked[i], "km","\n"))
        cat(paste("Approximate Distance Tracked / Day: ", analysis_summary$summary_df$distance_tracked[i] / analysis_summary$summary_df$days_at_liberty[i],"km", "\n")) 
        cat(paste("Unique Stations_Detected", analysis_summary$summary_df$n_stations[i], "\n"))
        if(analysis_summary$homerange[i,1] != 0){
          cat(paste("Area of detected homerange", round(as.numeric(analysis_summary$homerange[i,1]), digits = 2), 'km^2'))
        }else if(analysis_summary$homerange[i,2] != 0){
          cat(paste("Maximum linear Distance", round(analysis_summary$homerange[i,2], digits = 2), 'km'))
        }
        cat("\n")
        cat("\n")
      }
      sink()
    } # End if(report == TRUE){
    
    ################## Saving Image ##################
    setwd(run.dir)
    analysis_summary$run_directory = run.dir
    analysis_summary$fig.directory = fig.dir
    analysis_summary$report_directory = report.dir
    
    save.image(file = paste("workspace ", run_description, ".RData", sep = ""))
    setwd(project_dir) # resetting to main project directory 
    
    ################## Reporting Success #############
    print('SUCCESS!')
    beep(8) # SUCCESS!
  } else {
    print('No tags with data during this run')
  }
  function_time = proc.time() - function_timer
  send_push(user = 'uGEHvA4hr37tsrCCtpSv4sUUxVuTqN', message = paste("Run", run_description, "Complete! Run Time:", function_time['elapsed']))
  return(analysis_summary)
}

##### Importing Data Files #####
  raw_vue_data = load_vemco_data(filename = file.path(data_dir, 'VUE_Export_22_Jan_2019.csv'))
  receiver_data = load_receiver_data(filename = file.path(data_dir, 'DEPLOYMENT_RECOVERY_LOG.csv'))
  tagging_data = load_tagging_data(filename = file.path(data_dir, 'Bottomfish_Tag_Master.csv'))
  tag_specs = load_tag_specs('/Volumes/GoogleDrive/My Drive/Weng Lab/Data/VEMCO/VEMCO Tag Specifications.csv')
  receiver_event_data = load_receiver_event_data(file.path(data_dir, "Receiver_Event_Export_24_Jan_2019.csv"), receiver_df = receiver_data)
  fdf_report = load_fdf_report(file.path(data_dir, 'FDA_report_Jan_28.csv'), tag_specs = tag_specs)
  
  ##### Cleaning Data Files #####
  #### Cleaning VUE Data File
  ### Removing tags from data set that are not weng lab tags
    vue_data = raw_vue_data[raw_vue_data$full_tag_id %in% tag_specs$vue_tag_id, ]
  ### Reassigning station name and position info in data set using receiver data
    vue_data = clean_vue_data(vue_df = vue_data, receiver_df = receiver_data)
    vue_data = clean_vue_fdf(vue_df = vue_data, fdf_report = fdf_report)
  ### Removing any detections from CPDI testing and random botcam drops
    vue_data = vue_data[!(vue_data$station %in% "Steve and Brendan's Doughnut test"), ]
    vue_data = vue_data[!(vue_data$station %in% c("Doughnut Tank Test 1", "Doughnut Tank Test 2")), ]
    vue_data = vue_data[!(vue_data$station %in% c("With BotCam Crew", 'BotCam 2')), ]
    vue_data = vue_data[!(vue_data$station %in% c("50m Shallow Doughnut Test 260ms blank", "Doughnut Test 220m 260 blanking")), ]
    vue_data = vue_data[!(vue_data$station %in% c("Diamondhead 3m", "Diamondhead 60m")), ]
    vue_data = vue_data[!(vue_data$station %in% c("Diamond Head", "")), ]
    vue_data = vue_data[!(vue_data$station %in% c("Kewalo Range Test 1m receiver assign 3", "Kewalo Range test 15 Nov 2014", "Kewalo Range Test 1m Assign 2", "Kewalo Range Test 1m Assign 1", "Kewalo Range Test 7.5m Assign 5", "Kewalo Range Test 7.5m Assign 4", "Kewalo Range Test 7.5m Assign 6", "Kewalo Range Test 15m Assign 7", "Kewalo Range Test 15m Assign 8", "Kewalo Range Test 15m Assign 9")), ]
    
  ### Removing detections known to be from receivers that broke free, after they broke free
    vue_data = vue_data[vue_data$station != 'Makapuu BRFA 25', ]
  ### Removing any detections not associated with the study or from tags previously deployed in range tests
    vue_data = remove_detections_before_tagging(vue_data, tagging_data)
  ### Removing tags that aren't opakapaka
   # vue_data = vue_data[vue_data$tag_id %in% tagging_data$vem_tag_id[which(tagging_data$species == "Opakapaka" & is.na(tagging_data$vem_tag_id) == FALSE)], ]
  ### Removing tags with issues
    # dead_tags = c(37969, 57459)
    cross_tags = c(57451) # tags are at cross seamount. cannot find tags in hard copy logs. probably monchong tagged with BF tags.
    vue_data = vue_data[!(vue_data$tag_id %in% cross_tags), ]
    vue_data = vue_data[!(vue_data$station %in% c("Cross 1", "Cross 10", "Cross 4", "Cross 11")), ]
   ### Getting time of Day
    vue_data = calculate_time_of_day(vue_data)
   ### Getting station depth
    vue_data = extrapolate_detection_depth_from_receiver_df(vue_df = vue_data, receiver_df = receiver_data)
   ### Removing potentially false detections
    dim(vue_data)[1]
    vue_data = vue_data[vue_data$detection_status == TRUE, ]
    dim(vue_data)[1]
    
  #### Cleaning Receiver Data File
   ### Removing cross seamount from receiver_data
    receiver_data = clean_receiver_stations(receiver_df = receiver_data, region = c('Oahu', 'PB'), remove = FALSE)
    ## Removing SWAC station from receiver data
    receiver_data = receiver_data[-(which(receiver_data$station_name  == 'Oahu - SWAC')), ]
   ### Removing any receivers where we dont have position data
    receiver_data = receiver_data[receiver_data$lat != 0, ]

   ### How many detections have been recorded during this study? 
    n_detections = list()
    n_detections$total = dim(vue_data)[1] # Total number of detections from all tags
  
  #### Cleaning Tagging Data File
   ### Removing any tagging data where fish wasn't tagged with an acoustic tag
    tagging_data = tagging_data[is.na(tagging_data$vem_tag_id) == FALSE, ]  
    
  #### Saving analysis workspace image of cleaned up data ####
    save.image(file.path(project_dir, "workspace_image_updated.RData"))
    send_push(user = 'uGEHvA4hr37tsrCCtpSv4sUUxVuTqN', message = "Data Files Loaded!")

##### Running Analysis ######
  #### Loading in analysis workspace 
    load(file.path(project_dir, "workspace_image_updated"))
    
    all_tag_data = run(run_description = 'All Tag Data',
                       vue_df = vue_data,
                       receiver_df = receiver_data,
                       tagging_df = tagging_data,
                       region = "Oahu and Penguin Banks")
    
    
    
  #### Phase 1 Analysis: 2012 - 2015 
    ### Track Status of Phase 1 tags
    valid_tracks_phase_1  = c(57358, 57457, 57455, 37960, 37940)
    questionable_tracks_phase_1 = c(57464, 37975, 37961)
    
    phase_1_all_tags = run(run_description = "Phase I - All Tags",
                 vue_df = vue_data, 
                 receiver_df = receiver_data, 
                 tagging_df = tagging_data,
                 start_date = as.POSIXct("2012-04-13"), 
                 end_date = as.POSIXct("2014-12-06"),
                 region = "Oahu and Penguin Banks")
    
    phase_1_valid_tags = run(run_description = "Phase I - Good Tags", 
                      vue_df = vue_data, 
                      receiver_df = receiver_data, 
                      tagging_df = tagging_data,
                      tag_ids = valid_tracks_phase_1,
                      start_date = as.POSIXct("2012-04-13"), 
                      end_date = as.POSIXct("2014-12-06"),
                      plot = TRUE,
                      region = "Oahu and Penguin Banks")
     
     phase_1_questable_and_valid_tags = run(run_description = "Phase I - Questionable Tags", 
                                           vue_df = vue_data, 
                                           receiver_df = receiver_data, 
                                           tagging_df = tagging_data,
                                           tag_ids = c(valid_tracks_phase_1, questionable_tracks_phase_1),
                                           start_date = as.POSIXct("2012-04-13"), 
                                           end_date = as.POSIXct("2014-12-06"),
                                           region = "Oahu and Penguin Banks")

  #### Phase 2 Analysis: 2015 - Present 
     ### Track status of Phase II tags
      ## Determined by looking at all records of tag ids individually and making status judgement.
      ## For status justifications see file "/Users/stephenscherrer/Google Drive/Weng Lab/Personal_Folders/Steve/dissertation work/Opakapaka Tagging/Opakapaka Tagging Analysis/Tag Status.csv"
        ## From January 2017
          valid_tracks = c(18249, 18251, 18252, 18253, 18256, 18259, 18260, 916, 905)
          questionable_tracks = c(18250, 18254, 18255, 918, 919, 913, 914, 915, 917, 908, 910, 912, 902, 903, 904, 906, 907, 899, 18262, 18263, 18264, 18265, 18268, 18269, 18270, 18271, 18272, 18273, 18274, 18275, 36799, 36800, 36801)
          dead_tracks = c(18257, 920, 921, 922, 909, 911, 900, 901, 898, 18266, 18267, 36802, 36803, 36804, 36805, 36806, 36807)

        ## From July 2017
          valid_tracks = c(10, 31, 18249, 18251, 18253, 18259, 18275, 51584, 51586, 51588, 916)
          questionable_tracks = c(900, 901, 910, 913, 914, 915, 18250, 18254, 18255, 18257, 902, 918, 919, 912, 18262, 18252, 51582, 18260, 18266, 18267, 18269, 18270, 18274, 36800, 36801, 36802, 36803, 36804, 36806, 36807, 24, 19, 9, 7, 4, 32, 35, 39, 51583, 923, 903, 904)
          dead_tracks = c(905, 906, 907, 908, 909, 911, 917, 920, 921, 922, 18256, 18263, 18264, 18265, 51584, 18271, 18272, 18273, 36799, 36805)


phase_2_all_tags = run(run_description = "Phase 2 all tracks current with fdf_new_func_test", 
                       plot = TRUE, 
                       vue_df = vue_data, 
                       tagging_df = tagging_data, 
                       receiver_df = receiver_data, 
                       start_date = "2015-1-1", 
                       end_date = NULL, 
                       tag_ids = NULL, 
                       region = 'Makapuu', 
                       valid_tracks = valid_tracks, 
                       questionable_tracks = questionable_tracks,
                       expired_tracks = dead_tracks)

phase_2_valid_tags = run(run_description = "Phase 2 valid tracks to current with fdf", 
                          vue_df = vue_data, 
                          tagging_df = tagging_data, 
                          receiver_df = receiver_data, 
                          start_date = "2015-1-1", 
                          end_date = NULL, 
                          tag_ids = valid_tracks, 
                          plot = FALSE,
                          report = TRUE,
                          region = 'Makapuu')

phase_2_questionable_and_valid_tags = run(run_description = "Phase 2 questionable and valid tracks to current with fdf", 
                                          vue_df = vue_data, 
                                          tagging_df = tagging_data, 
                                          receiver_df = receiver_data, 
                                          start_date = "2015-1-1", 
                                          end_date = NULL, 
                                          tag_ids = c(valid_tracks, questionable_tracks), 
                                          plot = FALSE, 
                                          region = 'Makapuu')




#### Rerunning phase 1 analysis without botcam detections
# phase1 = run(vue_data = vue_data, description = "Phase I - For Data", receiver_data = receiver_data, tagging_data = tagging_data,
#              start_date = as.POSIXct("2012-04-13"), 
#              end_date = as.POSIXct("2014-12-06"),
#              region = "Oahu and Penguin Banks")
# 
# #### Rerunning phase 2 analyis without botcam detections
# phase2 = run(vue_data = vue_data, description = "Phase II - For Data",  receiver_data = receiver_data, tagging_data = tagging_data,
#              start_date = as.POSIXct("2014-12-07"),
#              end_date = as.POSIXct("2016-03-10 00:00:00"),
#              region = "Makapuu")
# 
# all_data = run(vue_data = vue_data, description = "All Project Data", receiver_data = receiver_data, tagging_data = tagging_data,
#                start_date = FALSE, 
#                end_date = FALSE,
#                region = "Oahu and Penguin Banks")
# 
# #### Minimum distance across BRFA E - across south border
# brfa_e_min_dim = lldist(point1 = c(-157.6833333, 21.28333333), point2 = c(-157.533333, 21.28333333))
# brfa_e_north_south = lldist(point1 = c(-157.533333, 21.4166666), point2 = c(-157.6833333, 21.28333333))
# #### Minimum distance across BRFA F
# brfa_f_min_dim = lldist(point1 = c(-157.3666666, 20.9166666), point2 = c(-157.3666666, 21.03333333))
# brfa_f_east_west = lldist(point1 = c(-157.566666, 21.03333333), point2 = c(-157.3666666, 21.03333333))
# #### Maximum homerange size
# homerange = round(max_movement(vue_data), digits = 2)
# # View(homeranges)
# 
# 
# 
# 
# 
# ####
# 
# #### Determining mean distance and sd for receivers in phase 2
# receiver_distance_phase_2 = distance_between_receivers(receiver_data, start_date = as.POSIXct("2015-02-28 00:00:00"), 
#                                                        end_date = as.POSIXct("2015-03-01 00:00:00"), include_lost = FALSE)
# # mean = 34.58549, sd = 21.73923, IQR = 15.61947 53.91185
# 
# #### Determining mean distance and sd for receivers in phase 2.5
# receiver_distance_phase_2.5 = distance_between_receivers(receiver_data, start_date = as.POSIXct("2015-12-28 00:00:00"), 
#                                                          end_date = as.POSIXct("2015-12-29 00:00:00"), include_lost = FALSE)
# # mean = 10.41 km sd = 6.85 km, IQR = 4.899075, 15.394144
# 
# receiver_distance_phase_1 = distance_between_receivers(receiver_data, start_date = as.POSIXct("2013-12-06 00:00:00"),
#                                                        end_date = as.POSIXct("2013-12-07 00:00:00"), include_lost = FALSE)
# # mean = 51.88947 km, sd = 27.32509 km, IQR = 32.73332 69.45658
# 
# 
# #### phase 1 dates
# start_date = as.POSIXct('2012-04-13 16:05:30 HST')
# end_date = as.POSIXct("2013-12-07 00:00:00")
# 
# #### Phase 2.1 dates
# start_date = as.POSIXct('2015-03-16 16:05:30 HST')
# end_date = as.POSIXct('2015-05-25 00:00:00')
# 
# #### Phase 2.2 dates
# start_date = as.POSIXct('2015-05-30 00:00:00')
# end_date = as.POSIXct('2015-11-29 00:00:00')
# 
# #### Phase 2.3 dates
# start_date = as.POSIXct('2015-11-29 00:00:00')
# end_date = as.POSIXct("2016-01-08 00:00:00")
# 
# #### Notes on tags 37960 moves between pinicle south and the mound
# 
# #### Phase I 
# p1_tags_good  = c(57358, 57457, 57455, 37960, 37940)
# phase1_good = run(vue_data = vue_data, 
#                   description = "Phase I - Good Tags", 
#                   receiver_data = receiver_data, 
#                   tagging_data = tagging_data,
#                   tag_ids = p1_tags_good,
#                   start_date = as.POSIXct("2012-04-13"), 
#                   end_date = as.POSIXct("2014-12-06"),
#                   region = "Oahu and Penguin Banks")
# 
# p1_tags_quest = c(p1_tags_good, 57464, 37975, 37961)
# phase1_quest = run(vue_data = vue_data, 
#                    description = "Phase I - Questionable Tags", 
#                    receiver_data = receiver_data, 
#                    tagging_data = tagging_data,
#                    tag_ids = p1_tags_quest,
#                    start_date = as.POSIXct("2012-04-13"), 
#                    end_date = as.POSIXct("2014-12-06"),
#                    region = "Oahu and Penguin Banks")
# 
# 
# 
# 
# 
# #### PHASE II 
# phase2_all = run(vue_data = vue_data, 
#                  description = "Phase II - Good Tags", 
#                  receiver_data = receiver_data, 
#                  tagging_data = tagging_data,
#                  start_date = as.POSIXct('2016-01-01'), 
#                  end_date = FALSE,
#                  region = "Makapuu")
# 
# 
# p2_tags_good  = c(18260, 18259, 18256, 18253, 18252, 18251, 18249, 916, 915, 909, 905, 902)
# phase2_good = run(vue_data = vue_data, 
#                   description = "Phase II - Good Tags", 
#                   receiver_data = receiver_data, 
#                   tagging_data = tagging_data,
#                   tag_ids = p2_tags_good,
#                   start_date = FALSE, 
#                   end_date = FALSE,
#                   region = "Makapuu")
# 
# p2_tags_quest = c(p2_tags_good, 18266, 18265, 18263, 18262, 18257, 18255, 18254, 18250, 921, 919, 918, 913, 912, 911, 910, 906, 903, 898)
# phase2_quest = run(vue_data = vue_data, 
#                    description = "Phase II - Questionable Tags", 
#                    receiver_data = receiver_data, 
#                    tagging_data = tagging_data,
#                    tag_ids = p2_tags_quest,
#                    start_date = FALSE, 
#                    end_date = FALSE,
#                    region = "Makapuu")
# #good_tracks_p2 = c(18249, 18251, 18253, 18259, 18260, 916, 915, 905, 909, 902, 18252, 18256)
# #quest_tracks_p2 = c(18250, 18252, 18254, 18255, 18256, 18257, 918, 919, 916, 910)
# 
# phase2$good = list()
# phase2$quest = list()
# phase2$good$data = phase2$data[phase2$data$tag_id %in% good_tracks_p2, ]
# phase2$quest$data = phase2$data[phase2$data$tag_id %in% c(good_tracks_p2, quest_tracks_p2), ]
# 
# phase2$good$homerange = cbind(sort(unique(vue_data$tag_id)), homerange)
# phase2$good$homerange = phase2$good$homerange[phase2$good$homerange [ ,1] %in% good_tracks_p2, ]
# phase2$quest$homerange= cbind(sort(unique(vue_data$tag_id)), homerange)
# phase2$quest$homerange = phase2$quest$homerange[phase2$quest$homerange [ ,1] %in% quest_tracks_p2, ]
# 
# ### Tags with tracks that persist at least a week after tagging
# phase2$good$tag_ids_detected_7days = c()
# for(i in 1:length(unique(phase2$good$data$tag_id))){
#   indv_data = phase2$good$data[phase2$good$data$tag_id == unique(phase2$good$data$tag_id)[i], ]
#   if(abs(difftime(time1 = min(indv_data$datetime), time2 = max(indv_data$datetime), units = "days")) >= 7){
#     phase2$good$tag_ids_detected_7days = c(phase2$good$tag_ids_detected_7days, unique(phase2$good$data$tag_id)[i])
#   }
# }
# phase2$quest$tag_ids_detected_7days = c()
# for(i in 1:length(unique(phase2$quest$data$tag_id))){
#   indv_data = phase2$quest$data[phase2$quest$data$tag_id == unique(phase2$quest$data$tag_id)[i], ]
#   if(abs(difftime(time1 = min(indv_data$datetime), time2 = max(indv_data$datetime), units = "days")) >= 7){
#     phase2$quest$tag_ids_detected_7days = c(phase2$quest$tag_ids_detected_7days, unique(phase2$quest$data$tag_id)[i])
#   }
# }
# 
# ## Tags with more than 7 days of detection data
# phase2$good$tag_ids_detected_7unique = c()
# for(i in 1:length(unique(phase2$good$data$tag_id))){
#   indv_data = phase2$good$data[phase2$good$data$tag_id == unique(phase2$good$data$tag_id)[i], ]
#   days = calculate_days_detected(indv_data[indv_data$station != 'Tagging Location', ]) 
#   if(days >=7){
#     phase2$good$tag_ids_detected_7unique = c(phase2$good$tag_ids_detected_7unique, c(unique(phase2$good$data$tag_id)[i], days[1]))
#   }
# }
# 
# phase2$quest$tag_ids_detected_7unique = c()
# for(i in 1:length(unique(phase2$quest$data$tag_id))){
#   indv_data = phase2$quest$data[phase2$quest$data$tag_id == unique(phase2$quest$data$tag_id)[i], ]
#   days = calculate_days_detected(indv_data[indv_data$station != 'Tagging Location', ]) 
#   if(days >=7){
#     phase2$quest$tag_ids_detected_7unique = c(phase2$quest$tag_ids_detected_7unique, c(unique(phase2$quest$data$tag_id)[i], days[1]))
#   }
# }
# 
# 
# #### track lengths for good and bad tags
# phase2$good$track_distance = list()
# phase2$quest$track_distance = list()
# #phase2$good$track_distance = cbind(phase2$tag_ids, phase2$track_distance)
# phase2$good$track_distance = cbind(good_tracks_p2, phase2$track_distance[phase2$tag_ids %in% good_tracks_p2, ])
# range(phase2$good$track_distance[,2])
# fivenum(phase2$good$track_distance[,2])
# phase2$quest$track_distance = cbind(c(quest_tracks_p2, good_tracks_p2), phase2$track_distance[phase2$tag_ids %in% c(quest_tracks_p2, good_tracks_p2), ])
# 
# phase2$quest$track_distance$quest = cbind(c(good_tracks_p2, quest_tracks_p2), phase2$track_distance[cbind(phase2$tag_ids, phase2$track_distance[ ,1]) %in% c(good_tracks_p2, quest_tracks_p2), 1])
# range(phase2$quest$track_distance$quest[,2])
# fivenum(phase2$quest$track_distance$quest[,2])
# 
# ## How many fish changed locations
# n_stations = cbind(phase2$tag_id, phase2$n_stations_by_tag)
# n_stations.good = n_stations[n_stations[ ,1] %in% good_tracks_p2, ]
# range(n_stations.good[ ,2])
# fivenum(n_stations.good[ ,2])
# n_stations.quest = n_stations[n_stations[ ,1] %in% c(good_tracks_p2, questionable_tracks_p2), ]
# range(n_stations.quest[ ,2])
# fivenum(n_stations.quest[ ,2])
# 

# #### February 26 2016 - Analyizing data from BRFA 38, receiver recovered on this date from a fisherman off kokohead
# b38 = filter(vue_data, station == "Oahu - Makapuu BRFA 38")
# unique(b38$tag_id) # list of tags detected at this receiver
# b38 = generate_study_date(b38)
# stripchart(b38$study_date ~ b38$tag_id)
# ### Appears tag 905 is alive!!!
# t905 = vue_data[vue_data$tag_id == 905, ]
# plot(t905$depth ~ t905$study_date)
# 
# stripchart(all_data$data$study_date~all_data$data$tag_id, las = 2)
# 
# stripchart(all_data$data$study_date[all_data$data$station == "Tagging Location"]~all_data$data$tag_id[all_data$data$station == "Tagging Location"], col = "red", add = TRUE)
# #### Saving Workspace Image ----------------------------------------------------------------
# # save.image(file = "workspace_image.RData")
# beep(2)
# 
# vd = vue_data[vue_data$datetime > as.POSIXct("2014-12-07"), ]
# vd = generate_tagging_detection(tagging_data, vue_data)
# vd = vd[vd$datetime > as.POSIXct("2014-12-07"), ]
# vd = generate_study_date(vd)
# stripchart(vd$study_date~vd$tag_id, las = 2)
# stripchart(vd$study_date[vd$station == "Tagging Location"]~vd$tag_id[vd$station == "Tagging Location"], col = "red", add = TRUE)
# 
# 
# 
# 
# 
# 
# 
# #### Analysis for barotrauma paper. Which fish were heard from >7 days after tagging?
# 
# ### First run up to phase 2 data
# ## Tags presumed dead
# dead_fish_p2 = c(920, 908, 901, 900, 917, 914)
# dead_fish_p1 = c(37969, 57459, 57451)
# p2_mortality = merge(x = phase2$data, y = tagging_data[ ,c("datetime", "vem_tag_id")], by.x = "tag_id", by.y = "vem_tag_id")
# p2_mortality = p2_mortality[!(p2_mortality$tag_id %in% dead_fish_p2), ] # removing dead fish
# tags_with_7plus_days = c()
# for(i in 1:length(unique(p2_mortality$tag_id))){
#   indv_data = p2_mortality[p2_mortality$tag_id == unique(p2_mortality$tag_id)[i], ]
#   a = (difftime(max(indv_data$datetime.x), min(indv_data$datetime.x), units = 'day'))
#   if(a > 7){
#     tags_with_7plus_days = c(tags_with_7plus_days, indv_data$tag_id[1])
#   }
# }
# 
# #### Analysis for CBOGS 2016
# #### Metrics to score:
# # 1. Recovery Rate
# # 2. Time detected after tagging
# # 3. Average number of receivers detected
# # 4. Days detected
# # 5. Pre Analysis of homerange size
# # 6. Movements into/out of BRFA
# setwd(paste(results_dir, '/cbogs2016/', sep = ""))
# dead_fish = c(dead_fish_p1, dead_fish_p2) # Tag IDS for known dead fish go here
# 
# ### CBOGS 2016 # 1. Recovery Rate
# 
# get_recovery_rates = function(vue_data, rm_tags = FALSE, start_date = FALSE, end_date = FALSE, calculate_by = 'pings'){
#   #### calculate_by argument has 2 options
#   #1. 'pings' - calculates end date based on last transmission received
#   #2. 'battery' - calculates end date based on when battery set to expire
#   
#   if(rm_tags[1] != FALSE){
#     vue_data = vue_data[!(vue_data$tag_id %in% rm_tags), ]
#   }
#   if(start_date != FALSE){
#     vue_data = vue_data[vue_data$datetime > start_date, ]
#   }
#   if(end_date != FALSE){
#     vue_data = vue_data[vue_data$datetime < end_date, ]
#   }
#   rec_rates = matrix(nrow = 0, ncol = 4)
#   colnames(rec_rates) = c('tag_id', 'theoretical transmissions', 'observed transmissions', 'recovery rate')
#   for(i in 1:length(sort(unique(vue_data$tag_id)))){
#     indv_data = vue_data[(vue_data$tag_id == sort(unique(vue_data$tag_id))[i]), ]
#     if(sort(unique(vue_data$tag_id))[i] >= 898 && sort(unique(vue_data$tag_id))[i] <= 927){
#       transmission_interval = (30 + 90)/2 # seconds between transmissions
#       battery_life = 257 # days
#     }else if(sort(unique(vue_data$tag_id))[i] >= 18236 && sort(unique(vue_data$tag_id))[i] <= 18275){
#       transmission_interval = (30 + 90) / 2 #seconds between transmissions
#       battery_life = 362 # days
#     }else if(sort(unique(vue_data$tag_id))[i] >= 37935 && sort(unique(vue_data$tag_id))[i] <= 37985){
#       transmission_interval = (110 + 250) / 2 # 180 seconds between transmissions
#       battery_life = 539 # days
#     }else if(sort(unique(vue_data$tag_id))[i] >= 52142 && sort(unique(vue_data$tag_id))[i] <= 52161){
#       transmission_interval = (100 + 200) / 2
#       battery_life = 450 # days
#     }else if(sort(unique(vue_data$tag_id))[i] >= 57371 && sort(unique(vue_data$tag_id))[i] <= 57470){
#       transmission_interval = (200 + 450) / 2 # 325 seconds
#       battery_life = 450 # days
#     }
#     if(calculate_by == 'battery'){
#       ## Date tag should theoretically expire
#       indv_battery_expire = min(indv_data$datetime) + (24 * 60 * 60) * battery_life
#       ## This next bit solves the problem if a tag is still active by only calculating until last download
#       if(end_date[1] != FALSE){
#         # if a tag expires after the end date
#         if(indv_battery_expire >= end_date){
#           # calculate based on the end date
#           theoretical_pings_sent = as.numeric(abs(difftime(time1 = min(indv_data$datetime), time2 = end_date, units = 'secs'))) / transmission_interval
#         }else{
#           indv_data = indv_data[indv_data$datetime <= min(indv_data$datetime) + battery_life*24*60*60, ]
#           theoretical_pings_sent = as.numeric(abs(difftime(time1 = min(indv_data$datetime), time2 = indv_battery_expire, units = 'secs'))) / transmission_interval
#         }
#       }
#     }else if(calculate_by == 'pings'){
#       theoretical_pings_sent = as.numeric(abs(difftime(time1 = min(indv_data$datetime), time2 = max(indv_data$datetime), units = 'secs'))) / transmission_interval
#     }else{
#       print('calculate_by was not supplied with a valid argument. Arguments included: "pings", and "battery"')
#     }
#     observed_pings = length(indv_data$datetime) - 1 ## Acounts for tagging date which is not actually a logged transmission
#     recovery_rate = observed_pings / theoretical_pings_sent
#     rec_rates = rbind(rec_rates, c(sort(unique(vue_data$tag_id))[i], theoretical_pings_sent, observed_pings, recovery_rate))
#   }
#   return(rec_rates)
# }
# 
# ## Running get_recovery_rates function
# p1_rr = get_recovery_rates(vue_data = phase1$data, rm_tags = dead_fish_p1, start_date = FALSE, end_date = FALSE, calculate_by = 'battery')
# fivenum(p1_rr[,'recovery rate'])
# #  3.865182e-06 1.546073e-05 3.865182e-05 7.034632e-04 2.127396e-02
# p2_rr = get_recovery_rates(vue_data = phase2$data, rm_tags = dead_fish_p2, start_date = FALSE, end_date = max(phase2$data$datetime), calculate_by = 'battery')
# fivenum(p2_rr[,'recovery rate'])
# # 1.938275e-05 1.708087e-04 1.099823e-03 1.421844e-02 4.235259e-01
# rr_combined = rbind(cbind(p1_rr, 'phase1'), cbind(p2_rr, 'phase2'))
# colnames(rr_combined) = c(colnames(p1_rr), 'phase')
# boxplot(as.numeric(rr_combined[,'recovery rate'])~rr_combined[,'phase'], ylim = c(0, 0.01))
# wilcox.test(x = as.numeric(p1_rr[ ,'recovery rate']), y = as.numeric(p2_rr[ ,'recovery rate']), paired = FALSE)
# # p-value = 0.004699
# 
# 
# ### CBOGS 2016 # 2. Time detected after tagging
# days_detected = function(vue_data, tagging_data, start_date = FALSE, end_date = FALSE, remove_tags = FALSE){
#   if(start_date != FALSE){
#     vue_data = vue_data[vue_data$datetime >= start_date, ]
#   }
#   if(end_date != FALSE){
#     vue_data = vue_data[vue_data$datetime <= end_date, ]
#   }
#   tagging_data = tagging_data[is.na(tagging_data$vem_tag_id) == FALSE, ]
#   if(remove_tags != FALSE){
#     tagging_data = tagging_data[!(tagging_data$vem_tag_id %in% remove_tags), ]
#   }
#   #print(sort(unique(vue_data$tag_id)))
#   # constructing a matrix to populate for making histograms
#   days_tracked = c()
#   all_days_tracked = c()
#   # Making a list of all tag ids that have been put out before last receiver download
#   tag_data = tagging_data[which(tagging_data$species == "Opakapaka"), ]
#   tag_data = tag_data[which(tag_data$datetime >= min(vue_data$datetime)), ]
#   tag_data = tag_data[which(tag_data$datetime <= max(vue_data$datetime)), ]
#   all_tag_ids = as.numeric(levels(na.exclude(unique(tag_data$vem_tag_id))))
#   # Looping through all tag ids to find the number of days a tag was detected
#   for(i in 1:length(unique(all_tag_ids))){
#     if(all_tag_ids[i] %in% vue_data$tag_id){
#       indv_data = vue_data[vue_data$tag_id == all_tag_ids[i], ]
#       track_length = as.numeric(ceiling(abs(difftime(min(indv_data$datetime), max(indv_data$datetime), units = "days"))))
#       days_tracked = c(days_tracked, track_length)
#       all_days_tracked = c(all_days_tracked, track_length:0)
#     }else{
#       days_tracked = c(days_tracked, 0)
#       all_days_tracked = c(all_days_tracked, 0)
#     }
#     days_tracked_proportion = days_tracked / length(all_tag_ids) # Standardizing this value
#     all_days_tracked_proportion = all_days_tracked / length(all_tag_ids)
#   }
#   track_times = list()
#   track_times$days_tracked = days_tracked
#   track_times$days_tracked_proportion = days_tracked_proportion
#   track_times$all_days_tracked = all_days_tracked
#   track_times$all_days_tracked_proportion = all_days_tracked_proportion
#   return(track_times)
# }
# phase_1_days_detected = days_detected(vue_data = vue_data, tagging_data = tagging_data, start_date = as.POSIXct('2012-04-13 16:05:30 HST'), end_date = as.POSIXct("2014-12-07 00:00:00"), remove_tags = FALSE)
# phase_2_days_detected = days_detected(vue_data = vue_data, tagging_data = tagging_data, start_date = as.POSIXct('2015-03-16 16:05:30 HST'), end_date = FALSE, remove_tags = FALSE)
# 
# 
# hist.scaled = function(histogram_formula){
#   hist_counts = hist(histogram_formula, plot = FALSE)$counts
#   labels = paste(seq(0, 100, 10), '%', sep = "") # x axis labels in groups of 10 from 0 to 100 (%)
#   hist(histogram_formula, ylim = c(0, max(hist_counts)), yaxt = 'n', ylab = '', xlab = 'Days Since Tagging')
#   axis(side = 2, labels = labels, at = seq(0, max(hist_counts), length.out = length(labels)))
# }
# 
# 
# png('days_detected_comparison.png')
# par(mfrow = c(1, 2))
# hist.scaled(phase_1_days_detected$all_days_tracked[phase_1_days_detected$all_days_tracked < 100])
# hist.scaled(phase_2_days_detected$all_days_tracked[phase_2_days_detected$all_days_tracked < 100])
# # hist.scaled(phase_1_days_detected$days_tracked)
# # hist.scaled(phase_2_days_detected$days_tracked)
# dev.off()
# 
# #### CBOGS 2016 #3. Total number of receivers detected
# stations_detected = function(vue_data, tagging_data, start_date = FALSE, end_date = FALSE, remove_tags = FALSE){  
#   if(start_date != FALSE){
#     vue_data = vue_data[vue_data$datetime >= start_date, ]
#   }
#   if(end_date != FALSE){
#     vue_data = vue_data[vue_data$datetime <= end_date]
#   }
#   if(remove_tags[1] != FALSE){
#     vue_data = vue_data[!(vue_data$tag_id %in% remove_tags), ]
#   }
#   stations_detected = c()
#   for(i in 1:length(sort(unique(vue_data$tag_id)))){
#     stations_detected = c(stations_detected, length(unique(vue_data$station[vue_data$tag_id == sort(unique(vue_data$tag_id))[i]])) - 1) #subtract 1 to account for "tagging location"
#   }
#   return(stations_detected)
# }
# 
# ## Running stations detected
# phase_1_stations_detected = stations_detected(vue_data = phase1$data, tagging_data = tagging_data, remove_tags = dead_fish_p1)
# fivenum(phase_1_stations_detected)
# phase_2_stations_detected = stations_detected(vue_data = phase2$data, tagging_data = tagging_data, remove_tags = dead_fish_p2)
# fivenum(phase_2_stations_detected)
# 
# ## Plotting stations detected
# png('p1_p2_stations_detected_hist.png')
# par(mfrow = c(1, 2))  
# hist(phase_1_stations_detected)
# hist(phase_2_stations_detected)
# dev.off()
# 
# png('p1_p2_stations_detected_boxplot.png')
# par(mfrow = c(1, 1))  
# stations_detected_for_boxplot = rbind(cbind(phase_1_stations_detected, 'phase 1'), cbind(phase_2_stations_detected, 'phase 2'))
# boxplot(as.numeric(stations_detected_for_boxplot[,1]) ~ stations_detected_for_boxplot[,2], main = "# Stations Detected")
# dev.off()
# wilcox.test(as.numeric(phase_1_stations_detected),  as.numeric(phase_2_stations_detected))
# 
# 
# #### CBOGS 2016 #4. Days Detected
# days_for_boxplot = rbind(cbind(phase1$unique_days, "Phase I"), cbind(phase2$unique_days, 'Phase II'))
# boxplot(as.numeric(days_for_boxplot[,1]) ~ days_for_boxplot[ ,2], main = 'Unique Days Detected')
# wilcox.test(x = as.numeric(phase1$unique_days[which(!(phase1$tag_ids %in% dead_fish_p1))]), y = as.numeric(phase2$unique_days[which(!(phase2$tag_ids %in% dead_fish_p2))]), paired = FALSE)
# fivenum(phase1$unique_days)
# fivenum(phase2$unique_days)
# hist(phase1$unique_days)
# hist(phase2$unique_days)  
# 
# 
# #### CBOGS 2016 #5. Pre Analysis of homerange size for phase 2 data
# ## Rerunning max_movement for phase 2. Now all tags get a maximum linear distance. with more than 3 locations get max polygon
# p2_homerange_size = max_movement(phase2_quest$data)
# ## Square rooting max polygon to get a linear dimension
# p2_homerange_size[ ,1] = sqrt(p2_homerange_size[ ,1])
# ## Determining if linear or polygon is greater for each tag
# p2_homerange_max = c()
# for(i in 1:dim(p2_homerange_size)[1]){
#   p2_homerange_max = c(p2_homerange_max, max(p2_homerange_size[i, ]))
# }
# ## Removing dead fish from analysis
# p2_homerange_max = p2_homerange_max[!(phase2$tag_ids %in% dead_fish_p2) ]
# ## pasting together tag id and max homerange
# p2_max_homerange = cbind(phase2$tag_ids[!(phase2$tag_ids %in% dead_fish_p2) ],p2_homerange_max)
# fivenum(p2_max_homerange[,2])
# # 0.6056681  2.3533339  3.9627153  5.8499004 14.9273110
# boxplot(p2_max_homerange[,2], main = "Linear Home Range (km)")
# 
# 
# 
# 
# #### CBOGS 2016 #6. Pre Analysis of BRFA Movements for phase 2 data
# 
# 
# #### Making Stripcharts of Detections
# make_detection_plot = function(vue_data){
#   png('detection_stripchart_quest.png')
#   dev.new(width = 863, height = 3000)
#   date_labels = as.character(strptime(seq(min(vue_data$datetime), max(vue_data$datetime), length.out = 10), format = '%Y-%m-%d'))
#   par(mfrow = c(2,1))
#   stripchart(vue_data$study_date ~ as.factor(vue_data$tag_id), xaxt = 'n', las = 2,  xlab = "")
#   axis(side = 1, at = seq(min(vue_data$study_date), max(vue_data$study_date), length.out = 10), labels = date_labels, las = 2, cex = .25)
#   dev.off()
# }
# 
# fivenum(sqrt(brfa_areas))
# 
# linear_homerange_and_brfa = rbind(cbind(p2_max_homerange[,2], ' P. filamentosus'), cbind(sqrt(brfa_areas), 'BRFA'))
# 
# 
# boxplot(as.numeric(linear_homerange_and_brfa[ ,1]) ~ linear_homerange_and_brfa[ ,2], main = "Linear Homerange vs. sqrt(BRFA Areas)", ylab = "km")
# 
# 
# #### CBOGS 2016 #7. Pre Analysis of BRFA Movements in and out for phase 2 data
# boxplot(phase2$brfa_stats[!(phase2$tag_ids %in% dead_fish_p2) ,1] + phase2$brfa_stats[!(phase2$tag_ids %in% dead_fish_p2) ,2], main = "Movements Across BRFA Boundaries By Individual")
# 
# 
# 
# setwd(project_dir)
# 
# 
# #### PreAnalysis for Kevin May 2016
# ### Plotting individual detections by study date
# setwd(analysis_summary$fig.dir)
# dir.create('individual plots by station')
# setwd('individual plots by station')
# for(i in 1:length(unique(analysis_summary$tag_id))){
#   indv_subset = vue_data$tag_id == unique(analysis_summary$tag_id)[i]
#   indv_data = vue_data[indv_subset, ]
#   png(paste(sort(unique(vue_data$tag_id))[i],'detections by station.png', sep = " "))
#   par(mar=c(8,12,1,1))
#   date_labels = as.character(strptime(seq(min(indv_data$datetime), max(indv_data$datetime), length.out = 10), format = '%Y-%m-%d'))
#   stripchart(vue_data$datetime ~ vue_data$station, xlab = '', ylab = '', xaxt = 'n', las = 2, subset = indv_subset, main = paste(sort(unique(vue_data$tag_id))[i],'Detections by Station', sep = " "), cex = .5)      
#   axis(side = 1, at = seq(min(indv_data$datetime), max(indv_data$datetime), length.out = 10), labels = date_labels, las = 2, cex = .5)
#   ## Adding rtation name back to plot if tag only detected in one place
#   if(length(unique(indv_data$station)) == 1){
#     mtext(side=2, text=unique(indv_data$station), las=1, line=2)
#   }
#   dev.off()
# }
# 
# 
# 
# 
# 
# 
# #### Additional summaries for Annual Progress Report August 2016
# ## Total Tagged fish and total detected
# # Phase 1
# p1_tagged = unique(tagging_data$vem_tag_id[which(tagging_data$species == "Opakapaka" & tagging_data$datetime <= "2013-12-07 00:00:00")])
# n_tagged_p1 = length(p1_tagged)
# # 52
# p1_detected = sort(unique(vue_data$tag_id)[vue_data$tag_id %in% p1_tagged])
# n_detected_p1 = length(p1_detected)
# # 16
# n_alive_p1 = 
#   n_quest_p1 = 
#   # Phase 2
#   p2_tagged = unique(tagging_data$vem_tag_id[which(tagging_data$species == "Opakapaka" & tagging_data$datetime > "2013-12-07 00:00:00" & tagging_data$datetime < max(vue_data$datetime))])
# n_tagged_p2 = length(p2_tagged)
# # 59
# p2_detected = sort(unique(vue_data$tag_id)[vue_data$tag_id %in% p2_tagged])
# n_detected_p2 = length(p2_detected)
# # 56
# n_alive_p2 = 
#   n_quest_p2 =
#   # Total
#   n_tagged_total = n_tagged_p1 + n_tagged_p2
# # 111
# n_detected_total = n_detected_p1 + n_detected_p2
# n_alive_total = 
#   n_quest_total =
#   ## Average Size of fish tagged
#   # Phase 1 - All fish
#   fivenum(tagging_data$`fork_length(cm)`[tagging_data$vem_tag_id %in% p1_tagged])
# # 30 40 44 49 65
# # Phase 1 - valid tracks
# # Phase 1 - valid + questionable tracks
# # Phase 2 - All fish
# fivenum(tagging_data$`fork_length(cm)`[tagging_data$vem_tag_id %in% p2_tagged])
# # 30 42 47 60 76
# # Phase 2 - valid tracks
# # Phase 2 - valid + questionable tracks
# 
# ## Tagged since last report
# date_of_last_report = as.POSIXct('2015-09-01')
# n_tagged_since_last_report = length(tagging_data$vem_tag_id[which(tagging_data$species == 'Opakapaka' & tagging_data$datetime >= date_of_last_report)])
# # 47  
# ## Mortality summary
# ## Basis for mortality judgement
# ## Mortality vs. Size
# ## Mortality vs. order tagged
# ## Network Summary
# ## Receivers lost
# ## Analysis
# ## Movements across BRFA boundaries
# # Number of total movements
# # Number of total movements per fish per day
# # Aka Fish crossed every ___ Days
# ## Spatial Structure
# # Zone Cluster by Receiver Analysis
# # Clustering receivers
# phase2_receiver_data = receiver_data[na.omit(receiver_data$deployment_date > as.POSIXct('2015-12-06')), ]
# cluster_fit = cluster_receivers(phase2_receiver_data, lon = FALSE)
# cluster_lat_lon = cluster_fit$centers
# phase2_receiver_data$cluster = cluster_fit$cluster
# plot_clusters(phase2_receiver_data)
# # Depth of receivers where fish were detected
# # Changes with time?
# # Area tracked
# # Linear Distance
# # Convex Polygon
# # Network Cluster Analysis
# ## General Stats
# # Average number of days tracked (Five num)
# # Total distance tracked
# # Average number of receivers detected on
# 
# 
# 
# #### Plotting receiver histories, where they've been, where they're going
# plot_receiver_histories = function(vue_data, receiver_data,  plot_region = 'Makapuu'){
#   ### Plotting network histories
#   ## Three frame graphics
#   # 1. map of network in place when serviced
#   # 2. map of stations recovered
#   # 3. map of network in place after servicing
#   if(exists('bathymetry') == FALSE){
#     if(plot_region == 'Makapuu'){
      # bathymetry = getNOAA.bathy(lon1 = -158,
      #                            lon2 = -157.5,
      #                            lat1 = 21.2,
      #                            lat2 = 21.6,
      #                            resolution = 1)
#     }else if(plot_region == 'Oahu'){
#       bathymetry = getNOAA.bathy(lon1 = -158.5, 
#                                  lon2 = -157.5, 
#                                  lat1 = 21.18, 
#                                  lat2 = 21.82,
#                                  resolution = 1)
#     }else if(plot_region == 'Oahu and Penguin Banks'){
#       bathymetry = getNOAA.bathy(lon1 = -158.5, 
#                                  lon2 = -157, 
#                                  lat1 = 20.75, 
#                                  lat2 = 21.82,
#                                  resolution = 1)
#     }
#   }
#   
#   receiver_dates = (c(unique(receiver_data$deployment_date), unique(receiver_data$recovery_date)))
#   receiver_dates = as.POSIXct(unique(as.Date(receiver_dates[is.na(receiver_dates) == FALSE])))
#   receiver_date_labels = receiver_dates[receiver_dates >= min(vue_data$datetime)] 
#   receiver_dates = receiver_dates[receiver_dates >= min(vue_data$datetime)] + 4 * 60*60*24
#   receiver_data$recovered = as.character(receiver_data$recovered)
#   for(i in 1:length(receiver_dates)){
#     temp_receiver_data = receiver_data[which(receiver_data$deployment_date < receiver_dates[i] &
#                                                (receiver_data$recovery_date >= receiver_dates[i] | is.na(receiver_data$recovery_date))) , ]  # &
#     # receiver_data$recovered == ""), ]
#     
#     
#     png(paste('Receiver Map ', as.character(as.Date(receiver_date_labels[i])),'.png', sep = "" ))
#     par(mfrow = c(1, 1), oma=c(0,0,2,0), pty = 's')
#     
#     ## Making Prior to Service Plot
#     plot.bathy(bathymetry, land = TRUE, image=TRUE, bpal = list(c(-400, -1, 'grey')), deepest.isobath = c(-10000), shallowest.isobath = c(0), step = c(100), drawlabels = TRUE)
#     
#     ## Adding BRFA Boundaries to plot
#     brfa_e = as.data.frame(rbind(c(-157.68333333, 21.28333333), 
#                                  c(-157.53333333, 21.28333333), 
#                                  c(-157.53333333, 21.4166666), 
#                                  c(-157.68333333, 21.4166666)))
#     colnames(brfa_e) = c('lon', 'lat')
#     
#     brfa_f = as.data.frame(rbind(c(-157.5666667, 20.9666667), 
#                                  c(-157.5666667, 21.0333333333),
#                                  c(-157.3666667, 21.0333333333),
#                                  c(-157.3666667, 20.9666667),
#                                  c(-157.5666667, 20.9666667)))
#     colnames(brfa_f) = c('lon', 'lat')
#     lines(lat~lon, data = brfa_e, pch = 19, col = 'purple', lwd = 3, cex = .6)
#     lines(lat~lon, data = brfa_f, pch = 19, col = 'purple', cex = .6)
#     
#     ## Adding receiver locations to plot
#     
#     points(lat~lon, data = temp_receiver_data[temp_receiver_data$recovered == "",], pch = 19, col = 'red',cex = 1)
#     points(lat~lon, data = temp_receiver_data[temp_receiver_data$recovered != "",], pch = 19, col = 'blue',cex = 1)
#     
#     title(paste('Receiver Network ', as.character(as.Date(receiver_date_labels[i])), sep = ""), outer = TRUE)
#     dev.off()
#   }
# }
# 
# download_cruise_dates = c('2015-09-15', '2016-01-08', '2016-03-07', '2016-05-28')
# 
# plot_receiver_histories(vue_data = phase2_good$data, receiver_data = receiver_data, plot_region = 'Makapuu')
# 
# run_time = (proc.time() - fun_timer) # Stop the clock
# print(run_time)
# send_push(user = 'uGEHvA4hr37tsrCCtpSv4sUUxVuTqN', message = paste("Put down that beer, your run is complete! Total Run Time:", run_time))
# 
# 
# #### Analysis for Annual Progress Report 2016 ---------------
# 
# p2_tags_good  = c(18260, 18259, 18256, 18253, 18252, 18251, 18249, 916, 915, 909, 905, 902)
# phase2_good = run(vue_data = vue_data, 
#                   description = "Phase II - Good Tags", 
#                   receiver_data = receiver_data, 
#                   tagging_data = tagging_data,
#                   tag_ids = p2_tags_good,
#                   start_date = FALSE, 
#                   end_date = FALSE,
#                   region = "Makapuu")




##### FUNCTION TO RUN EVERYTHING!!!! ###########


#### Testing Run Function
# data_from_5_29_2016_to_12_6_2016 = run(test = TRUE, plot = TRUE, vue_data = vue_data, tagging_data = tagging_data, receiver_data = receiver_data, start_date = "2016-5-29", end_date = FALSE, tag_ids = FALSE, filter_false_detections = FALSE, run_description = 'TEST: 5-29-2016 to 12-6-2016', region = "Makapuu")
data_from_12_6_2016_to_6_25_2017   = run(test = TRUE, plot = TRUE, vue_data = vue_data, tagging_data = tagging_data, receiver_data = receiver_data, start_date = "2016-12-06", end_date = FALSE, tag_ids = FALSE, filter_false_detections = FALSE, run_description = 'TEST: 12-6-2016 to 6-25-2017', region = "Makapuu")




#######################################################################
############################# PHASE II ################################
#######################################################################

#### Running Phase 2 Analysis over entire data set
data_from_5_29_2016_to_12_6_2016 = run(vue_data = vue_data, tagging_data = tagging_data, receiver_data = receiver_data, start_date = "2016-5-29", end_date = FALSE, tag_ids = FALSE, filter_false_detections = FALSE, run_description = '5-29-2016 to 6-25-2017', region = "Makapuu")
phase_2_current_to_6_25_2017_all_tags = run(plot = FALSE, vue_data = vue_data, tagging_data = tagging_data, receiver_data = receiver_data, start_date = "2015-1-1", end_date = FALSE, tag_ids = FALSE, filter_false_detections = FALSE, run_description = "Phase 2 all tracks current without fdf", region = 'Makapuu')

############ Track Status of Each Tag in Data Set ###########
  #### Last updated 1 Jan 2017
  #### Determined by looking at all records of tag ids individually and making status judgement.
  #### For status justifications see file "/Users/stephenscherrer/Google Drive/Weng Lab/Personal_Folders/Steve/dissertation work/Opakapaka Tagging/Opakapaka Tagging Analysis/Tag Status.csv"
### From January 2017
valid_tracks = c(18249, 18251, 18252, 18253, 18256, 18259, 18260, 916, 905)
questionable_tracks = c(18250, 18254, 18255, 918, 919, 913, 914, 915, 917, 908, 910, 912, 902, 903, 904, 906, 907, 899, 18262, 18263, 18264, 18265, 18268, 18269, 18270, 18271, 18272, 18273, 18274, 18275, 36799, 36800, 36801)
dead_tracks = c(18257, 920, 921, 922, 909, 911, 900, 901, 898, 18266, 18267, 36802, 36803, 36804, 36805, 36806, 36807)



### From July 2017
valid_tracks = c(10, 31, 900, 901, 910, 913, 914, 915, 916, 18249, 18251, 18253, 18259, 18275, 51584, 51586, 51588)
questionable_tracks = c(18250, 18254, 18255, 18257, 902, 918, 919, 912, 18262, 18252, 51582, 18260, 18266, 18267, 18269, 18270, 18274, 36800, 36801, 36802, 36803, 36804, 36806, 36807, 24, 19, 9, 7, 4, 32, 35, 39, 51583, 923, 903, 904)
dead_tracks = c(905, 906, 907, 908, 909, 911, 917, 920, 921, 922, 18256, 18263, 18264, 18265, 51584, 18271, 18272, 18273, 36799, 36805)

###################################################################### 
phase_2_current_to_2017_06_25_all_tags = run(run_description = "Phase 2 all tracks current with fdf", 
                                             plot = TRUE, 
                                             vue_df = vue_data, 
                                             tagging_df = tagging_data, 
                                             receiver_df = receiver_data, 
                                             start_date = "2015-1-1", 
                                             end_date = FALSE, 
                                             tag_ids = FALSE, 
                                             region = 'Makapuu', 
                                             valid_tracks = valid_tracks, 
                                             questionable_tracks = questionable_tracks,
                                             expired_tracks = dead_tracks)

######## Rerunning anlysis with valid and questionable tracks ######
phase_2_current_to_12_6_2016_valid_tracks = run(run_description = "Phase 2 valid tracks to current with fdf", 
                                                vue_data = vue_data, 
                                                tagging_data = tagging_data, 
                                                receiver_data = receiver_data, 
                                                start_date = "2015-1-1", 
                                                end_date = FALSE, 
                                                tag_ids = valid_tracks, 
                                                filter_false_detections = FALSE, 
                                                plot = FALSE,
                                                region = 'Makapuu')

phase_2_current_to_12_6_2016_valid_and_quest_tracks = run(run_description = "Phase 2 questionable and valid tracks to current with fdf", 
                                                          vue_data = vue_data, 
                                                          tagging_data = tagging_data, 
                                                          receiver_data = receiver_data, 
                                                          start_date = "2015-1-1", 
                                                          end_date = FALSE, 
                                                          tag_ids = c(valid_tracks, questionable_tracks), 
                                                          filter_false_detections = FALSE, 
                                                          plot = TRUE, 
                                                          region = 'Makapuu')


#### Running plot path use to generate a larger plot for use at CBOGS 2017

plot_path_use(movement_graph = analysis_summary$movement_graph, vue_df = vue_df, receiver_df = receiver_data, region = 'Makapuu', bathymetry = bathymetry)

#####################################################################
#####################################################################


send_push(user = 'uGEHvA4hr37tsrCCtpSv4sUUxVuTqN', message = "Tag, You're it!")


pdf('Opakapaka Detection Stripchart.pdf', width = 22, height = 17)
start_date = as.POSIXct('2017-03-01')
end_date = as.POSIXct('2017-07-01')
if(is.null(start_date) == FALSE){
vue_df = vue_df[vue_df$datetime >= start_date, ]
}
if(is.null(end_date) == FALSE){
  vue_df = vue_df[vue_df$datetime <= end_date, ]
}

## Converting station class to factor for calling color palette colors
vue_df$station = as.factor(vue_df$station)
color_palette = rainbow(length(levels(vue_df$station)))

vue_df = vue_df[order(vue_df$tag_id, vue_df$datetime), ]
vue_df$tag_id = as.factor(vue_df$tag_id)
par(mfrow = c(1,1), mar=c(3,3,3,3), oma=c(4,2,2,2))
## Creating a sequence of dates (by month) that will be used to produce x axis labels
x_axis_labels = seq.POSIXt(floor_date(min(vue_df$datetime), unit = "month"), ceiling_date(max(vue_df$datetime), unit = "month"), by = 'month')
stripchart(vue_df$datetime ~ vue_df$tag_id, xaxt = "n", xlim = c(start_date, end_date),  xlab = "", las = 1, col = white, pch = 19)
for(i in 1:length(unique(vue_df$tag_id))){
  print(unique(vue_df$tag_id)[i])
  indv_data = vue_df[vue_df$tag_id == unique(vue_df$tag_id)[i], ]
  stripchart(vue_df$datetime[vue_df$tag_id == unique(vue_df$tag_id)[i]] ~ vue_df$tag_id[vue_df$tag_id == unique(vue_df$tag_id)[i]], pch = 15, col = color_palette[vue_df$station[vue_df$tag_id == unique(vue_df$tag_id)[i]]], add = TRUE)
}

axis(side = 1, at = x_axis_labels, labels = as.character(x_axis_labels), las = 2, cex = .25)
  
dev.off()


print_depth_ranges(vue_data){
  for(i in 1:length(sort(unique(vue_data$tag_id)))){
    indv_data = vue_data[vue_data$tag_id == sort(unique(vue_data$tag_id))[i], ]
    indv_data = indv_data[indv_data$station != 'Tagging Location', ]
    if(is.na(range(indv_data$depth)[1]) == FALSE){
      print(paste(indv_data$tag_id[1], ':',min(indv_data$depth), max(indv_data$depth)))
      x_axis_labels = seq.POSIXt(floor_date(min(indv_data$datetime), unit = "day"), ceiling_date(max(indv_data$datetime), unit = "day"), by = 'day')
      plot(indv_data$depth ~ indv_data$datetime, ylim = c(max(indv_data$depth) + 5, min(indv_data$depth) - 4), main = as.character(indv_data$tag_id[1]), type = 'l', xaxt = 'n', xlab = '', xlim = range(x_axis_labels))
      axis(side = 1, at = x_axis_labels, labels = as.character(x_axis_labels), las = 2, cex = .25)
      points(indv_data$depth ~ indv_data$datetime, col = 'black')
      }
  }
}





#### Analysis Criteria

###### Removing tags with less than 10 days of detection records
n_days_tracked = 14
tags_with_n_days = c()
for(i in 1:length(unique(vue_df$tag_id))){
  indv_data = vue_df[vue_df$tag_id == unique(vue_df$tag_id)[i], ]
  print(difftime(time1 = min(indv_data$datetime), time2 = max(indv_data$datetime), units = 'days'))
  if(abs(difftime(time1 = min(indv_data$datetime), time2 = max(indv_data$datetime), units = 'days')) >= n_days_tracked){
    tags_with_n_days = c(tags_with_n_days, unique(vue_df$tag_id)[i])
  }
}

### Data greater than 14 days - otherwise questionable
### Fish doesnt move a crazy amount and then dissapear - otherwise probably dead
### Fish doesnt move a crazy amount and then appear forever at one position - otherwise probably dead
### Appearance and dissapearance not based on receiver deployment/recovery - otherwise questionable or probably dead
### If resident at one receiver for a prolonged time, daily changes in number of detections at that station - otherwise questionable or probably dead
### If depth data exists, not at the same height forever if at the same receiver - otherwise probably dead
### Tags with changes to detection pattern in last 2 weeks probably mortality
 
 
 ### Debugging Run Function Params
 
 start_date = NULL; end_date = NULL; tag_ids = NULL;  run_description = FALSE; region = "Makapuu"; valid_tracks = NULL; questionable_tracks = NULL; expired_tracks = NULL; plot = TRUE; report = TRUE
 run_description = "Phase 2 all tracks current with fdf"; 
 plot = TRUE; 
 vue_df = vue_data; 
 tagging_df = tagging_data; 
 receiver_df = receiver_data; 
 start_date = "2015-1-1"; 
 end_date = NULL; 
 tag_ids = NULL; 
 region = 'Makapuu'; 
 valid_tracks = valid_tracks; 
 questionable_tracks = questionable_tracks;
 expired_tracks = dead_tracks
 
 run_description = "Phase 2 valid tracks to current with fdf"
 vue_df = vue_data
 tagging_df = tagging_data
 receiver_df = receiver_data
 start_date = "2015-1-1"
 end_date = NULL
 tag_ids = tag_status$valid_tracks
 plot = TRUE
 report = TRUE
 region = 'Makapuu'

 
 
 
 #### Random bits of analysis for dissertation
 ### How long was phase 1 in effect?
 
 

 #### MISC.
 

    
    
    vue_df = vue_data[vue_data$datetime >= as.POSIXct('2017-06-28') & vue_data$datetime < as.POSIXct('2018-05-15'), ]
    receiver_df = receiver_data
    tagging_df = tagging_data
    track_status = determine_track_status(vue_df, tagging_df = tagging_data, receiver_df = receiver_data)
    valid_ids = track_status$status_df$tag_id[track_status$status_df$status == 'Valid']
    length(valid_ids)
    
    
    #vue_df = vue_df[vue_df$tag_id != 32, ]
    
    newestdata = run(run_description = "Jan2017-Present - All Track Data",
                           vue_df = vue_data, 
                           receiver_df = receiver_data, 
                           tagging_df = tagging_data,
                           start_date = as.POSIXct('2017-01-01'), 
                           end_date = as.POSIXct('2018-04-15'), 
                           region = "Makapuu",
                           plot = FALSE)
    
    newestdata$track_status$status_df$tag_id[which(newestdata$track_status$status_df$status == 'Alive')][which(newestdata$track_status$status_df$tag_id[which(newestdata$track_status$status_df$status == 'Alive')] %in% vue_data$tag_id[vue_data$datetime >= as.POSIXct('2017-06-28')])]
    
    newestvaliddata = run(run_description = "Spring 2018 - Valid Track Data with Plots",
                     vue_df = vue_data, 
                     receiver_df = receiver_data, 
                     tagging_df = tagging_data,
                     start_date = as.POSIXct('2017-01-01'), 
                     end_date = as.POSIXct('2018-04-15'), 
                     region = "Makapuu",
                     plot = TRUE,
                     tag_ids = newestdata$track_status$valid_tracks
                     )

    ### Modeling the number of detections for a tag as a function of background noise levels      
    ## First select a tag to model
    id_of_tag_to_model = 29
    indv_data = vue_data[vue_data$tag_id == id_of_tag_to_model, ]
    ## Then work out the number of detections for each hour bin
    indv_data$hour_bin = ceiling_date(indv_data$datetime + 0.5 * 60 * 60, unit = 'hour') # if we first step everything up half an hour and then round it down, we get it averaged to the middle of the hour bin it came from
    detections_per_hour = aggregate(indv_data$tag_id, by = list(indv_data$hour_bin), FUN = length)
      colnames(detections_per_hour) = c('hour_bin', 'n_detections')
    ## Combine this with the noise level recorded by receivers
    detections_vs_ave_noise = merge(x = detections_per_hour, y = receiver_event_data$average_noise[ ,c("datetime", "data")], by.x = 'hour_bin', by.y = 'datetime')
    detections_vs_ave_noise$data = as.numeric(detections_vs_ave_noise$data)
    ## Make plot and get summary
    plot(detections_vs_ave_noise$n_detections ~ log(detections_vs_ave_noise$data), pch = 19, cex = .5, ylab = 'n detections', xlab = 'relative noise')    
    summary(lm(detections_vs_ave_noise$n_detections ~ log(detections_vs_ave_noise$data)))
    
  
    

    
    load('/Users/stephenscherrer/Google Drive/Weng Lab/Personal_Folders/Steve/dissertation work/Opakapaka Tagging/Opakapaka Tagging Analysis/results/2018-05-11 20:39:11 - Spring 2018 - ALL Track Data with Plots/workspace Spring 2018 - ALL Track Data with Plots')
    
    
    




#### Testing if the entire population of fish spend equal time in equal places
valid = c(2133, 2136, 28179, 30684, 30705, 30721, 51582, 51586, 51588, 51596) 
questionable = c(2127,2157,2140,30703,30707,28185,30683,28178,28180,30714,30715,30722,30734,30739,30742,30743,30747,30749,30751,36810,51581,51584,51585,51587,51598,30751,30749,28171,28185, 28181)
dead = c(2139,30685,30729,2150,30752,28169,28175,28174,28177,28176,28184,2158,2152,2147,2146,2145,2144,2142,2138,2130,2128,2126,2122,2120,2117,2116,2114,2111,51597,30708,30704,30701,30700,30695,30694,30687,30682,30681,30680,30679,30709)
excluded = c(30717,30753,30750,28166,28170,28173,28180,28183,28182,2159,2155,2154,2149,2141,2137,2135,2132,2131,2129,2125,2121,2119,2118,2115,2112,30706,30702,30697,30689,30690,30686,30746)
sum(length(valid), length(questionable), length(dead), length(excluded))

known_dead


sprint_valid_data = run(run_description = "Spring 2018 - Valid Track Data without Plots",
                      vue_df = vue_data, 
                      receiver_df = receiver_data, 
                      tagging_df = tagging_data,
                      start_date = as.POSIXct('2017-06-26'), 
                      end_date = as.POSIXct('2018-04-15'), 
                      region = "Makapuu",
                      plot = TRUE,
                      tag_ids = valid
)

spring_valid_and_quest_data = run(run_description = "Spring 2018 - Valid and Questionble Track Data without Plots",
                 vue_df = vue_data, 
                 receiver_df = receiver_data, 
                 tagging_df = tagging_data,
                 start_date = as.POSIXct('2017-06-26'), 
                 end_date = as.POSIXct('2018-04-15'), 
                 region = "Makapuu",
                 plot = TRUE,
                 tag_ids = c(valid, questionable)
)

save.image(file.path(bin_dir, 'workspace_for_depth_station_analysis'))

load('/Volumes/GoogleDrive/My Drive/Weng Lab/Personal_Folders/Steve/dissertation work/Opakapaka Tagging/Opakapaka Tagging Analysis/results/2018-11-15 10:45:17 - Spring 2018 - Valid and Questionble Track Data without Plots/workspace Spring 2018 - Valid and Questionble Track Data without Plots')



region = "Makapuu"; valid_tracks = NULL; questionable_tracks = NULL; expired_tracks = NULL; plot = TRUE; report = TRUE
run_description = "2017-2018 any tag present"
vue_df = vue_data
receiver_df = receiver_data
tagging_df = tagging_data
start_date = as.POSIXct('2017-06-26')
end_date = as.POSIXct('2018-04-15')
region = "Makapuu"
plot = FALSE
tag_ids = newestdata$track_status$status_df$tag_id[which(newestdata$track_status$status_df$status == 'Alive')][which(newestdata$track_status$status_df$tag_id[which(newestdata$track_status$status_df$status == 'Alive')] %in% vue_data$tag_id[vue_data$datetime >= as.POSIXct('2017-06-28')])]



try_these_tags = run(run_description = "2017-2018 any tag present",
                         vue_df = vue_data, 
                         receiver_df = receiver_data, 
                         tagging_df = tagging_data,
                         start_date = as.POSIXct('2017-06-26'), 
                         end_date = as.POSIXct('2018-04-15'), 
                         region = "Makapuu",
                         plot = TRUE,
                         tag_ids = track_status$tag_id[track_status$status == 'Alive' & track_status$tag_id %in% unique(vue_data$tag_id[vue_data$datetime >= period_start])]
                         )



