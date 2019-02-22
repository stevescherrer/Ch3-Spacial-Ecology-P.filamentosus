###### Modeling detection probability with distance (DWRT revisited)
#### Written by: Stephen R. Scherrer 
#### 26 January 2017

####### Cleaning workspace and setting directories ----
### Clearing Workspace
# rm(list=ls()) # Clear workspace

### Starting Script Timer
# script_timer <- proc.time()

### Linking to Project Directories
range_test_dir = '/Volumes/GoogleDrive/My Drive/Weng Lab/Manuscripts/Scherrer - CPDI Model Paper/CPDI Submission Repository/'
data_dir = file.path(range_test_dir, 'Data')
results_dir = file.path(range_test_dir, 'Results')
figure_dir = file.path(range_test_dir, 'Figures')
source_dir = file.path(range_test_dir, 'Code')

### Setting Project Directory 
# setwd(range_test_dir)

### Establishing History 
# savehistory(file= file.path(results_dir, "Rhistory"))

####### Importing principle dependencies ----
# install.packages('geosphere')
library('geosphere') # distGeo() Note: wrapped in old lldist function
# install.packages('reshape')
library('reshape') # melt()
# install.packages('MuMIn')
library('MuMIn') # AICc()
# install.packages('dplyr')
library('dplyr') # filter()
#install.packages('doParallel')
library('doParallel') # do_parallel()
#install.package('lubridate')
library('lubridate') # floor_date()
## install.packages('beepr')
library('beepr') # beep()
## install.packages('notifyR')
library('notifyR') # send_push()
## install.packages('ggplot2')
library('ggplot2') # geom_bar()
## install.packages('data.table') 
library('data.table') # uniqueN()
## install.packages('marmap')
library('marmap') # readBathy(), subsetBathy(), getNOAAbathy(), plot.bathy(), scaleBathy()
## install.packages('data.table')
library('data.table') # uniqueN()  
# install.packages('mgcv')
library('mgcv') # gam()

### Sourcing Mechanistic CPDI Model
source(file.path(source_dir, 'Mechanistic Model Implemented - R/Mechanistic CPDI Model.R')) # model_receiver_interferenece()

####### Setting Up Parallel Environment ----
### Setting up parallel processing for later analysis using pdredge() and foreach()
## Setting number of cores
n_cores = detectCores() - 1 # Default to the number of cores available on the computer
## Creating a cluster
clust = makeCluster(n_cores)
clusterEvalQ(clust, library('MuMIn')) # loading MuMIn package to clusters for using pdredge function
clusterEvalQ(clust, library('mgcv')) # loading mgcv package to clusters for using GAM function


####### General Parameters ----
expected_detections = data.frame(
  ## From: https://vemco.com/collision-calculator/
  'n_tags' = c(1:18),
  'hourly_detections_total' = c(57, 102, 138, 165, 186, 200, 210, 216, 218, 218, 215, 211, 205, 198, 191, 183, 175, 166),
  stringsAsFactors = FALSE
)
expected_detections$hourly_detections_per_tag = expected_detections$hourly_detections_total / expected_detections$n_tags

####### Experiment 1: Deep Water Range Test ----

#### Loading In and Cleaning Data Files ----
setwd(data_dir)

### Loading Receiver Data
load_receiver = function(filename, filepath = FALSE){
  ## Function to load in receiver data .csv file
  proj_dir = getwd()
  if (filepath != FALSE){
    setwd(filepath)
  }
  receiver_dates = receiver_col_names(read.csv(filename))
  receiver_dates$deployment_date = strptime(receiver_dates$deployment_date, format = '%m/%d/%y %H:%M', tz = 'HST', origin="1970-01-01")
  receiver_dates$recovery_date = strptime(receiver_dates$recovery_date, format = '%m/%d/%y %H:%M', tz = 'HST', origin="1970-01-01")
  receiver_dates$lat = convert_lat_lon(receiver_dates$lat_deg, receiver_dates$lat_min)
  receiver_dates$lon = convert_lat_lon(receiver_dates$lon_deg, receiver_dates$lon_min)
  setwd(proj_dir)
  return (receiver_dates)
}
receiver_data = load_receiver_data(filename = 'DEPLOYMENT_RECOVERY_LOG.csv')
# dim(receiver_data)
# 217  28

### Loading VUE data 
range_data = load_vemco(filename = 'Range_Test_June_2014_All_Receivers.csv', format = '%m/%d/%y %H:%M')
  dim(range_data)
# 130284     10

### Importing Windspeed Data from from NOAA Honolulu station Station ID 1612340
# http://www.ndbc.noaa.gov/view_text_file.php?filename=oouh1h2014.txt.gz&dir=data/historical/stdmet/
# parent: http://www.ndbc.noaa.gov/station_history.php?station=oouh1
# Data dates range between dates 2014-01-01 00:00:00 and 2014-12-31 23:54:00
# with the following format: 
# YY  MM DD hh mm WDIR WSPD GST  WVHT   DPD   APD MWD   PRES  ATMP  WTMP  DEWP  VIS  TIDE
# with the following units:   
# yr  mo dy hr mn degT m/s  m/s     m   sec   sec degT   hPa  degC  degC  degC   mi    ft
wind_data = as.data.frame(read.table("oouh1h2014.txt"), header = TRUE, sep = " ")
# dim(wind_data)
# 85864    18
colnames(wind_data) = c("YY",  "MM", "DD", "hh", "mm", "WDIR", "WSPD", "GST",  "WVHT",   "DPD",   "APD", "MWD",   "PRES",  "ATMP",  "WTMP",  "DEWP",  "VIS",  "TIDE")
### Convert dates and times to POSIXct format. (Station data time is in local (UTC) time by default)
wind_data$datetime = as.POSIXct(paste(wind_data$YY, "-", wind_data$MM, "-", wind_data$DD, " ", wind_data$hh, ":", wind_data$mm, sep = ""), format = "%Y-%m-%d %H:%M", tz = "GMT")
### Convert datetime to HST
wind_data$datetime = strftime(wind_data$datetime, format = "%Y-%m-%d %H:%M")
wind_data$hourly = strftime(wind_data$datetime, format = "%Y-%m-%d %H")

### Grouping windspeed data into hourly means (mean_wspd)
wind_data_grouped = group_by(wind_data, hourly)
wspd_by_hour = summarize(wind_data_grouped, mean_wspd = mean(WSPD))
wspd_by_hour$hourly = as.POSIXct(wspd_by_hour$hourly, format = "%Y-%m-%d %H")

### Grouping windgust data into hourly means
gst_by_hour = summarize(wind_data_grouped, mean_gst = mean(GST))
gst_by_hour$hourly = as.POSIXct(gst_by_hour$hourly, format = "%Y-%m-%d %H")

### Importing tide data from: NOAA Honolulu station, accessed 28 OCT 2015
# http://tidesandcurrents.noaa.gov/waterlevels.html?id=1612340&units=standard&bdate=20110930&edate=20100930&timezone=LST&datum=MLLW&interval=h&action=## Meta data available here: 
# Data accessed for dates ranging between 2013-09-30 00:00:00 HST and 2014-09-30 13:00:00 HST
# Data is already in HST
tide_data = read.csv('noaa tide data 2013-2014.csv')
# dim(tide_data)
# 8774    5
colnames(tide_data) = c("hour_bin", "water_level", 'sigma', 'I', "L")
tide_data$hour_bin = as.POSIXct(tide_data$hour_bin, format = "%Y-%m-%d %H:%M")
### Determining if tide was going in or out.
## First assume all data is headed in
tide_data$direction = "In"
## Then loop through data. If current water level is less than previous water level, tide is going out.
for(i in 2:length(tide_data$direction)){
  if(tide_data$water_level[i-1] > tide_data$water_level[i]){
    tide_data$direction[i] = "Out"
  }
}
tide_data$direction = as.factor(tide_data$direction)

### Importing Tag Deployment Meta Data File
tag_meta_data = read.csv('Range Test June 2014 - Tag Meta Data Locations and Depth.csv')
 dim(tag_meta_data)
# 12  9
### Converting tag_meta_data Lat Lons from degree minutes to decimal degrees
tag_meta_data$lat = convert_lat_lon(tag_meta_data$lat_deg, tag_meta_data$lat_min)
tag_meta_data$lon = convert_lat_lon(tag_meta_data$lon_deg, tag_meta_data$lon_min)

### Determining distance of tag from receiver string using lon and lat waypoints from gps
for (i in 1:length(tag_meta_data$distance)){
  tag_meta_data$distance[i] = round(1000*(lldist(point1 = c(tag_meta_data$lon[i], 
                                                            tag_meta_data$lat[i]), 
                                                 point2 = c(receiver_data$lon[receiver_data$station_name == 'Range Test - June 2014 - Diamond Head 1m'][1], 
                                                            receiver_data$lat[receiver_data$station_name == 'Range Test - June 2014 - Diamond Head 1m'][1]))))
}
unique(tag_meta_data$distance)
# 0 199 399 578 766 959

### Removing any other receivers in the database
receiver_1m = 123736 # serial number of receiver in 1 m depth
receiver_30m = 123732 # serial number of receiver in 30 m depth
range_data = range_data[which(range_data$receiver %in% c(receiver_1m, receiver_30m)), ]

### Removing detections prior to experiment (occurred durring experiment set up on boat) and determining an end to the experiment.
start_date = as.POSIXct('2014-06-07 8:00:00', tz = "HST")
# "2014-06-07 08:00:00 HST"
end_date = as.POSIXct("2014-06-26 05:00:00", format = "%Y-%m-%d %H:%M:%S")
# "2014-06-26 05:00:00 HST"
range_data = range_data[which(range_data$datetime >= start_date & range_data$datetime < end_date), ]

### Rounding off of final partial hour to account for transmissions received while gear was recovered
range_data = range_data[which(range_data$datetime >= ceiling_date(min(range_data$datetime), unit = 'hour') & range_data$datetime < floor_date(max(range_data$datetime), unit = 'hour')), ]
range(range_data$datetime)
# "2014-06-07 09:00:00 HST" "2014-06-26 03:59:00 HST"

## How many total detections occurred durring the course of the experiment? (includes tags not part of range test)
dim(range_data)[1]
# 129622

### Removing tags not in experiment. This could be a tagged fish that swam by or a false detection
tags_0m    = c(18236, 18237)
tags_200m  = c(18238, 18239)
tags_400m  = c(18240, 18241)
tags_600m  = c(18242, 18243)
tags_800m  = c(18244, 18245)
tags_1000m = c(18246, 18247)
tag_ids = c(tags_0m, tags_200m, tags_400m, tags_600m, tags_800m, tags_1000m) 
range_data = range_data[range_data$tag_id %in% tag_ids, ] 

## How many detections were from tags that were part of the experiment?
dim(range_data)[1]
# 128178
dim(range_data)[1] / detections_all * 100
# 98.88599 %

## How many detections were from tags that were not part of the experiment?
detections_all - dim(range_data)[1]
# 1444
(detections_all - dim(range_data)[1]) / detections_all * 100
# 1.114008

### Assigning Lon Lat positions to range_data
range_data = clean_vue_data(vue_df = range_data, 
                             receiver_df = receiver_data)
### Getting estimated distances between receivers and tags based on deployment coordinates
for (i in 1:length(tag_meta_data$lon)){
  tag_meta_data$distance[i] = lldist(point1 = c(unique(range_data$lon)[1], 
                                                unique(range_data$lat)[1]),
                                     point2 = c((tag_meta_data$lon)[i],
                                                (tag_meta_data$lat)[i])) *
    1000 # m/km
}
round(unique(tag_meta_data$distance))
# 0 199 399 578 766 959

#### Hourly Detection Data Analysis ----
### Separating data by tag, receiver, and hour bin
## First assign each datum to an date and hour bin
range_data$hour_bin = floor_date(range_data$datetime, unit = "hour")
## Then loop through combinations of date/hour, tag id, and receiver to get the number of detections from a tag at a receiver each hour
detection_df_1 = data.frame() 
registerDoParallel(cores = n_cores)
detection_df_1 = foreach(b = c(1:length(unique(range_data$hour_bin))), .combine = rbind) %:% 
  foreach(i = c(1:length(unique(range_data$tag_id))), .combine = rbind) %:% 
  foreach(r = c(1:length(unique(range_data$receiver))), .combine = rbind) %dopar%{
    filtered = filter(range_data, hour_bin == unique(range_data$hour_bin)[b], 
                      tag_id == unique(range_data$tag_id)[i],
                      receiver == unique(range_data$receiver)[r])
    write_line = cbind(as.character(unique(range_data$hour_bin))[b],
                       as.character(unique(range_data$tag_id))[i], 
                       as.character(unique(range_data$receiver))[r], 
                       dim(filtered)[1])
    return(write_line)
  }
## Cleaning detection_df_1
detection_df_1 = as.data.frame(detection_df_1)
colnames(detection_df_1) = c('hour_bin', 'tag_id', 'receiver', 'detections')
detection_df_1$detections = as.numeric(as.character(detection_df_1$detections))
 dim(detection_df_1)
# 10824     4
detections_per_hour = aggregate(detection_df_1$tag_id[detection_df_1$detections != 0], by = list(detection_df_1$hour_bin[detection_df_1$detections != 0]), FUN = uniqueN)

### Combining detection dataframe with distance, bottom depth, height condition, lat, and lon tag_meta_data
detection_df_1 = merge(detection_df_1, tag_meta_data, 
                       by.x = colnames(detection_df_1) == 'tag_id', 
                       by.y = colnames(tag_meta_data) == 'tag_id')  
detection_df_1$distance = detection_df_1$distance
detection_df_1$height_off_bottom = as.factor(detection_df_1$height_off_bottom)
detection_df_1$recovery_rate = detection_df_1$detections / 60
detection_df_1$day = as.factor(strftime(detection_df_1$hour_bin, format = "%Y-%m-%d"))
detection_df_1$time = as.factor(strftime(detection_df_1$hour_bin, format = "%H:%M:%S"))
colnames(detection_df_1)[colnames(detection_df_1) == 'height_off_bottom'] = "height_of_tag_off_bottom"
detection_df_1$hour_bin = as.POSIXct(detection_df_1$hour_bin, format = "%Y-%m-%d %H:%M:%S")

## Calculating the number of unique tags detected during each hour bin
tags_per_hour = aggregate(detection_df_1$tag_id[detection_df_1$detections != 0], by = list(detection_df_1$hour_bin[detection_df_1$detections != 0]), FUN = uniqueN)
colnames(tags_per_hour) = c("hour_bin", "tags_per_hour")
detection_df_1 = merge(detection_df_1, tags_per_hour, by.x = colnames(detection_df_1) == "hour_bin", by.y = colnames(tags_per_hour) == 'hour_bin')

### Combining detection data with wind speed, and wind gust data
detection_df_1 = merge(detection_df_1, wspd_by_hour,
                       by.x = colnames(detection_df_1) == 'hour_bin',
                       by.y = colnames(wspd_by_hour) == 'hourly')
detection_df_1 = merge(detection_df_1, gst_by_hour,
                       by.x = colnames(detection_df_1) == 'hour_bin',
                       by.y = colnames(gst_by_hour) == 'hourly')
detection_df_1 = merge(detection_df_1, tide_data,
                       by.x = colnames(detection_df_1) == 'hour_bin',
                       by.y = colnames(tide_data) == 'hour_bin')

detection_df_1$hour_bin = as.factor(detection_df_1$hour_bin)

### Assigning diurnal period to detections based around 12 hour time bins. Before 6am or after 6pm considered night, between 6am and 6pm considered day
detection_df_1$day_night = 'Night'
for(i in 1:length(detection_df_1$hour_bin)){
  if(hour(detection_df_1$hour_bin[i]) < 18 & hour(detection_df_1$hour_bin[i]) > 6){
    detection_df_1$day_night[i] = 'Day'
  }
}
detection_df_1$day_night = as.factor(detection_df_1$day_night)

### Assigning receiver height
detection_df_1$height_of_receiver_off_bottom = NULL
detection_df_1$height_of_receiver_off_bottom[detection_df_1$receiver == receiver_1m] = '1'
detection_df_1$height_of_receiver_off_bottom[detection_df_1$receiver == receiver_30m] = '30'
detection_df_1$height_of_receiver_off_bottom = as.factor(detection_df_1$height_of_receiver_off_bottom)

### Determining recovery rate based on the number of detections logged and an average of 60 transmissions per tag per hour
detection_df_1$recovery_rate = NA
for(i in 1:length(detection_df_1$tags_per_hour)){
  detection_df_1$recovery_rate[i] = detection_df_1$detections[i] / expected_detections$hourly_detections_per_tag[which(expected_detections$n_tags %in% detection_df_1$tags_per_hour[i])]
}

 dim(detection_df_1)
# 10824    27

#### Statistical Analysis - Modeling Detection Probability ----

### Fitting a GAM model 
global.gam_1 = gam( formula = detections ~ s(distance, k = 6) + s(day_night, bs = 're') + height_of_tag_off_bottom + height_of_receiver_off_bottom + s(direction, bs = "re") + s(mean_wspd, bs = "re") + s(mean_gst, bs = "re") + s(water_level, bs = "re"),
                    data = detection_df_1, keepData = TRUE, family = poisson(link = 'log'),  na.action = "na.fail")
global_gam.summary_1 = summary(global.gam_1)

## R sqr value from GAM
global_gam.summary_1$r.sq
# r.sq = 0.6465411

## Checking GAM smoother terms are appropriate
gam.check(global.gam_1)

### Using Dredge function to compare this GAM to all possible combinations of variables
## exporting mod.gam and data to cluster for using pdredge function
clusterExport(clust, c('global.gam_1', 'detection_df_1'))
mod.dredged_1 = pdredge(global.gam_1, cluster = clust)

### Comparing predictions for various canadate models from dredge that are within delta 2 of the lowest AICc
best_model = get.models(mod.dredged_1, subset = delta <= 2)[[1]]

### Setting Model Parameters
alpha = 0.05
transmissions_per_hour = 60
detection_threshold = (alpha * transmissions_per_hour)

## Looping through different combinations of predictor variables
pred_out = list()
for(time_of_day in unique(detection_df_1$day_night)){
  for(receiver_height in as.numeric(levels(unique(detection_df_1$height_of_receiver_off_bottom))[unique(detection_df_1$height_of_receiver_off_bottom)])){
    for(tag_height in as.numeric(levels(unique(detection_df_1$height_of_tag_off_bottom))[unique(detection_df_1$height_of_tag_off_bottom)])){
      for(tide_direction in unique(detection_df_1$direction)){
        
        
        var_predictors = list(
          day_night = time_of_day,
          height_of_tag_off_bottom = tag_height,
          height_of_receiver_off_bottom = receiver_height,
          mean_wspd = median(detection_df_1$mean_wspd),
          mean_gst = median(detection_df_1$mean_gst),
          direction = tide_direction,
          water_level = median(detection_df_1$water_level)
        )
        
  
        ## Looping through best models
          candidate_model_id = paste('best model')
          candidate_model = best_model
          
          ## Subsetting terms from master list of predictors from global model if they appear as model terms in the candidate model 
          candidate_model_terms = attr(candidate_model$terms, which = 'term.labels')
          new_mod_data = var_predictors[names(var_predictors) %in% candidate_model_terms]
          
          ### Predicting detection rates using the candidate model under evaluation
          predicted_rates = c()
          predicted_error_fits.fit = c()
          predicted_error_fits.se = c()
          for(dist in 0:1000){
            new_mod_data$distance = dist
            predicted_rates = c(predicted_rates, predict(candidate_model, newdata = new_mod_data, type = "response")[[1]])
            predicted_error_fits.fit = c(predicted_error_fits.fit, predict(candidate_model, newdata = new_mod_data, type = "response", se.fit = TRUE)$fit[[1]])
            predicted_error_fits.se = c(predicted_error_fits.se, predict(candidate_model, newdata = new_mod_data, type = "response", se.fit = TRUE)$se.fit[[1]])
          }
          
          ## Saving model predictions
          model_predictions = list()
          model_predictions$predicted_rates = predicted_rates
          model_predictions$predicted_error_fits.fit = predicted_error_fits.fit
          model_predictions$predicted_error_fits.se = predicted_error_fits.se

          predicted_lower_ci = predicted_error_fits.fit - predicted_error_fits.se
          predicted_upper_ci = predicted_error_fits.fit + predicted_error_fits.se
          
          ### Summary Stats
          ## Determining the average maximum detection radius
          ave_max_distance = 500+(which(abs(predicted_rates[500:1000] - detection_threshold) == min(abs(predicted_rates[500:1000] - detection_threshold))))-1 #subtract 1 because predictions start at 0 but indexing starts at 1 1
          upper_ci_max_distance = 500+(which(abs(predicted_upper_ci[500:1000] - detection_threshold) == min(abs(predicted_upper_ci[500:1000] - detection_threshold))))-1 #subtract 1 because predictions start at 0 but indexing starts at 1 1
          lower_ci_max_distance = 500+(which(abs(predicted_lower_ci[500:1000] - detection_threshold) == min(abs(predicted_lower_ci[500:1000] - detection_threshold))))-1 #subtract 1 because predictions start at 0 but indexing starts at 1 1
          
          min_median_minus_se = 10000 # Something absurdly high
          max_median_plus_se = 0 # Something absurdly low
          
          if(upper_ci_max_distance > max_median_plus_se){
            max_median_plus_se = upper_ci_max_distance
          }
          if(lower_ci_max_distance < min_median_minus_se){
            min_median_minus_se = lower_ci_max_distance
          }
          predicting_conditions = paste('Time of Day: ', time_of_day, ', Receiver Height: ', receiver_height, ', Tag Height: ', tag_height, ', Tidal Direction: ', tide_direction, sep = "")
          pred_out[[length(pred_out) + 1]] = model_predictions$predicted_rates
          names(pred_out)[[length(pred_out)]] = predicting_conditions
        }
      }
  }
}

preds = c()
for(i in 1:length(pred_out)){
  preds = c(preds, pred_out[[i]][1001])
}

worst_case_conditions = pred_out[[which.min(preds)]]
names(pred_out)[which.min(preds)]

## Convert predicted detections into a predicted % of transmissions sent
pred_recovery_rate = (worst_case_conditions/transmissions_per_hour) * 100


plot(pred_recovery_rate ~ c(1:1001), ylab = '% recovery rate', xlab = 'Distance (m)')

#### If you know distance and want to get recovery rate
predict_rr_at_distance = 827 # m
pred_recovery_rate[predict_rr_at_distance - 1 ]

#### If you know recovery rate and want to get corrosponding distance
  predict_distance_at_rr = 25  # %
  which.min(abs(pred_recovery_rate[which.max(pred_recovery_rate):1000] - predict_distance_at_rr)) + (which.max(pred_recovery_rate)-1) # We have to do this little adustment bit with which.max so we can avoid picking detection rates in the region affected by CPDI.
  