#### For Committee Meeting Jan 2017 and for NMFS Talk

#### NOTE: Most of this analysis has been incorperated into the general run functions of the script "Paka Tagging Analysis July 2017.R"
    ## Original functions are preserved here but since this script calls the run function from that file, 

#### SHIT THAT NEEDS FIXING - station named BRFA E 15 rather than Oahu - Makapuu BRFA 15 (Makapuu In BRFA)

start = as.POSIXct('2015-01-01')
end = as.POSIXct('2017-06-26')
results_dir = '/Users/stephenscherrer/Google Drive/Weng Lab/Personal_Folders/Steve/dissertation work/Opakapaka Tagging/Opakapaka Tagging Analysis/results/NMFS Talk:Committee Meeting Jan 2018'

#### Making a 3d Pie Chart showing Deep 7 species breakdowns
library(plotrix) 
library(wesanderson) # wes_palette
library(data.table) # aggregate

### LBS by species (FY 2016-2017)
slices <- c(134805, 45756, 23814, 9020, 7790, 10741, 2098) 
# lbls <- c("Opakapaka", "Onaga", "Ehu", "Lehi", "Hapuupuu", 'Kalekale', "Gindai")
png(file.path(results_dir, 'fishery_lbs_pie.png'))
  pie3D(slices,explode=0.2,
        main="Lbs of Deep 7 Landed",
        col = wes_palette(name = 'Zissou',n = 7, type = "continuous"))
### Note: Add and format labels for species in manually 
dev.off()

### $ by species (FY 2017 - 2018)
slices <- c(934359, 406207, 144626, 41844, 42927, 38639, 7737) 
png(file.path(results_dir, 'fishery_dollars_pie.png'))
# lbls <- c("Opakapaka", "Onaga", "Ehu", "Lehi", "Hapuupuu", 'Kalekale', "Gindai")
  pie3D(slices,explode=0.2,
        main="Value of Deep 7 Catch",
        col = wes_palette(name = 'Zissou',n = 7, type = "continuous"))
  dev.off()

#### First load analysis workspace from Tagging Analysis July 2017
  # load("/Users/stephenscherrer/Google Drive/Weng Lab/Personal_Folders/Steve/dissertation work/Opakapaka Tagging/Opakapaka Tagging Analysis/workspace_image_updated")

#### Run all data for analysis period - To get Mortality Tree
  tag_status = determine_track_status(vue_df = vue_data[vue_data$datetime >= start & vue_data$datetime <= end, ], tagging_df = tagging_data, receiver_df = receiver_data)
    vue_no_depth = vue_data[vue_data$datetime >= start & vue_data$datetime <= end, ]
    vue_no_depth$depth = NA
  tag_status_no_depth = determine_track_status(vue_df = vue_no_depth[vue_no_depth$datetime >= as.POSIXct('2015-01-01') & vue_no_depth$datetime <= as.POSIXct('2017-06-24'), ], tagging_df = tagging_data, receiver_df = receiver_data)
    ### YES removal of depth sensor doesn't change outcomes

  ### Getting gif of clearly dead fish
  generate_gif_images(vue_df = raw_vue_data[raw_vue_data$tag_id == 28, ], receiver_df = receiver_data, region = 'Makapuu')
  
  ### Adjusting bad receiver data
  receiver_data$recovery_date[which(receiver_data$recovery_date == as.POSIXct("2017-06-11 10:04:00 HST"))] = as.POSIXct("2017-06-11 22:04:00 HST")
  receiver_data$recovery_date[which(receiver_data$recovery_date == as.POSIXct("2017-06-11 10:30:00 HST"))] = as.POSIXct("2017-06-11 22:04:00 HST")
  vue_data = clean_vue_data(vue_df = vue_data, receiver_df = receiver_data)
  
  ## Scenario 0 - All Tags
  start_date = NULL; end_date = NULL; tag_ids = NULL;  run_description = FALSE; region = "Makapuu"; valid_tracks = NULL; questionable_tracks = NULL; expired_tracks = NULL; plot = TRUE; report = TRUE
  run_description = "Phase 2 Scenario 0 - All tracks through 24 June 2017"; 
  plot = FALSE; 
  vue_df = vue_data; 
  tagging_df = tagging_data; 
  receiver_df = receiver_data; 
  start_date = start;
  end_date = end;
  tag_ids = tagging_data$vem_tag_id[tagging_data$datetime >= as.POSIXct("2015-01-01") & tagging_data$datetime < as.POSIXct("2017-06-10") & tagging_data$species == "Opakapaka"];
  region = 'Makapuu'; 
  valid_tracks = NULL; 
  questionable_tracks = NULL;
  expired_tracks = NULL
  
  
  
  p2s0  = run(run_description = "Phase 2 Scenario 0 - All tracks through 24 June 2017", 
              plot = TRUE, 
              vue_df = vue_data, 
              tagging_df = tagging_data, 
              receiver_df = receiver_data, 
              start_date = start,
              end_date = end,
              tag_ids = tagging_data$vem_tag_id[tagging_data$datetime >= as.POSIXct("2015-01-01") & tagging_data$datetime < as.POSIXct("2017-06-10") & tagging_data$species == "Opakapaka"],
              region = 'Makapuu', 
              valid_tracks = NULL, 
              questionable_tracks = NULL,
              expired_tracks = NULL)
  
  ## Conservative scenario - Only valid tracks
  p2s1  = run(run_description = "Phase 2 Scenario 1 - valid tracks through 24 June 2017", 
                                   plot = FALSE, 
                                   vue_df = vue_data, 
                                   tagging_df = tagging_data, 
                                   receiver_df = receiver_data, 
                                   start_date = start, 
                                   end_date = end, 
                                   tag_ids = p2s0$track_status$valid_tracks,
                                   region = 'Makapuu', 
                                   valid_tracks = NULL, 
                                   questionable_tracks = NULL,
                                   expired_tracks = NULL)
  
  ## Less conservative scenario - Includes tracks of undetermined status
  p2s2 =   phase_2_all_tags = run(run_description = "Phase 2 Scenario 2 - valid and undetermined tracks through 24 June 2017", 
                                   plot = TRUE, 
                                   vue_df = vue_data, 
                                   tagging_df = tagging_data, 
                                   receiver_df = receiver_data, 
                                   start_date = start,
                                   end_date = end,
                                   tag_ids = c(p2s0$track_status$valid_tracks, p2s0$track_status$uncertain), 
                                   region = 'Makapuu', 
                                   valid_tracks = NULL, 
                                   questionable_tracks = NULL,
                                   expired_tracks = NULL)
  
#### Mortality Decision Tree: Compare flow chart results to mannual selection ####
  ### Manually identified tracks
    ## Phase 1
    valid_tracks_phase_1  = c(57358, 57457, 57455, 37960, 37940)
    questionable_tracks_phase_1 = c(57464, 37975, 37961)
    
    ## Phase 2
    valid_tracks = c(10, 31, 18249, 18251, 18253, 18259, 18275, 51584, 51586, 51588, 916)
    questionable_tracks = c(900, 901, 910, 913, 914, 915, 18250, 18254, 18255, 18257, 902, 918, 919, 912, 18262, 18252, 51582, 18260, 18266, 18267, 18269, 18270, 18274, 36800, 36801, 36802, 36803, 36804, 36806, 36807, 24, 19, 9, 7, 4, 32, 35, 39, 51583, 923, 903, 904)
    dead_tracks = c(905, 906, 907, 908, 909, 911, 917, 920, 921, 922, 18256, 18263, 18264, 18265, 51584, 18271, 18272, 18273, 36799, 36805)

#### Distribution of movement scales: Comparing our results to O'malley (2015) and Okamoto data ####
  # Barchart where fish are binned by how far they've moved
  barchart_of_movement_scales = function(description, receiver_df){
    analysis_summary = receiver_df
    analysis_summary$homerange$tag_id = analysis_summary$tag_ids
    analysis_summary$homerange$distance_bin = NA
    analysis_summary$homerange$distance_bin[analysis_summary$homerange$`max linear area` < 1] = '0-1 km'
    analysis_summary$homerange$distance_bin[analysis_summary$homerange$`max linear area` >= 1 & analysis_summary$homerange$`max linear area` < 5] = '1-5 km'
    analysis_summary$homerange$distance_bin[analysis_summary$homerange$`max linear area` >= 5 & analysis_summary$homerange$`max linear area` < 10] = '5-10 km'
    analysis_summary$homerange$distance_bin[analysis_summary$homerange$`max linear area` >= 10 & analysis_summary$homerange$`max linear area` < 20] = '10-20 km'
    analysis_summary$homerange$distance_bin[analysis_summary$homerange$`max linear area` > 15] = '>20 km'
    homeranges_for_barchart = aggregate(analysis_summary$homerange$tag_id, by = list(analysis_summary$homerange$distance_bin), FUN = length)
      colnames(homeranges_for_barchart) = c('distance', 'n')
    ## Converting counts to percentages
      homeranges_for_barchart$percent = homeranges_for_barchart$n / sum(homeranges_for_barchart$n)
    ## Reordering for barcharting
    homeranges_for_barchart = homeranges_for_barchart[c(2, 3, 5, 4, 1), ]
  
    png(file.path(results_dir, paste('movement_bar_Scherrer', description, '2017.png')),  width = 826, height = 216)
    barplot(height = homeranges_for_barchart$percent * 100, names.arg = homeranges_for_barchart$distance,
            main = 'Acoustic Tagging Data',
            ylab = '% of Fish',
            xlab = 'Linear Home Range Area')
    dev.off()
  }
    barchart_of_movement_scales(description = 'valid_tags', receiver_df = p2s1)
    barchart_of_movement_scales(description = 'valid_and_questionable_tags', receiver_df = p2s2)
    
      
    # Fake a barchart for O'malley Data
    png(file.path(results_dir, 'movement_bar_Omalley2015.png'), width = 826, height = 216)
        pifg = list()
        pifg$distance = c('<1 km', '1-5 km', '5-10 km', '10-20 km', '>20 km')
        pifg$percent = c(53, 33, 5, 4, 6)
        barplot(height = pifg$percent, names.arg = pifg$distance,
                main = 'PIFG Mark Recapture Data',
                ylab = '% of Recaptures',
                xlab = 'Distance from Tagging Location (km)')
    dev.off()

#### Barchart showing mortality of our fish
    plot_mortality_barchart = function(receiver_df){
      tag_status$status_df = tag_status$status_df[tag_status$status_df$tag_id %in% unique(tagging_data$vem_tag_id[tagging_data$datetime >= start & tagging_data$datetime <= end & tagging_data$species == "Opakapaka"]), ]
      status_counts = aggregate(tag_status$status_df$tag_id, by = list(tag_status$status_df$status), FUN = uniqueN)
      colnames(status_counts) = c('classification', 'n')
      
    
    png(file.path(results_dir, 'Mortality Status of Tags.png'), width = 826, height = 437)
      status_counts = status_counts[c(which(status_counts$classification == 'Valid'), which(status_counts$classification == 'Uncertain'), which(status_counts$classification == 'Expired')), ]
      barplot(height = status_counts$n, names.arg = status_counts$classification, col = c('Green', 'Yellow', 'Red'), ylim = c(0, max(status_counts$n) * 1.1), main = 'Tag Status')
      text(x = c(.75,1.9,3.1), y = c(status_counts$n + max(status_counts$n) * 0.05), labels = status_counts$n)
    dev.off()
    }
    
    plot_mortality_barchart(p2s1)
    # plot_mortality_barchart(p2s2)
    
 #### Validating mortality assumptions: For under 2 week fish, does it look like thye're fleeing the area or not? ####
 ### If a fish is tagged for less than 2 weeks, is it last seen outside?

#### Validating mortality assumptions: Variability in a Reference Tag analysis: Variability in detection rates of known reference tag(s) ####
 ### Tag 57420 was deployed on a block 400 m from BRFA 15 Station on 5/29/2016. Station 15 was moved on 12/05/2016
  ref_tag = raw_vue_data[raw_vue_data$tag_id == 57420 & raw_vue_data$station == "BRFA E 15", ]
  ref_tag$date = as.Date(ref_tag$datetime)
  ref_tag = ref_tag[ref_tag$date > as.Date('2016-05-29') & ref_tag$date < as.Date('2016-12-05'), ]
  dim(ref_tag)
  ref_tag_variability = aggregate(ref_tag$datetime, by = list(ref_tag$date), FUN = uniqueN)
  colnames(ref_tag_variability) = c('date', 'n_detections')
  ref_tag_variability$recovery_rate = ref_tag_variability$n_detections / (60 * 24)
  mean(ref_tag_variability$n_detections)
  sd(ref_tag_variability$n_detections)
  mean(ref_tag_variability$recovery_rate)
  sd(ref_tag_variability$recovery_rate)
  hist(as.Date(ref_tag$datetime), breaks = length(unique(as.Date(ref_tag$datetime))) -1)
  dev.off()
  
#### For fish that cross borders, do they come back?
    ## Group fish into three catagories, 'Resident', 'Commuter', 'Tourist'
  brfa_crossing_stats_hist = function(description, receiver_df){
    vue_data = receiver_df$data
    brfa_crossing_stats = brfa_movements(vue_data)
    movement_catagory = data.frame('catagory' = c('locals', 'commuters', 'tourists', 'transplants', 'outsiders'), 'n' = NA)
    brfa_crossing_stats$catagory = NA
    ## Locals never leave
    brfa_crossing_stats$catagory[which(brfa_crossing_stats$in_to_out == 0 & brfa_crossing_stats$out_to_in == 0 & brfa_crossing_stats$time_tracked_in > 0)] = 'local'
    ## Commuters move across boundaries 
    brfa_crossing_stats$catagory[which(brfa_crossing_stats$in_to_out > 0 & brfa_crossing_stats$out_to_in > 0)] = 'commuter'
    ## Tourists were tagged in, and then left
    brfa_crossing_stats$catagory[which(brfa_crossing_stats$in_to_out > 0 & brfa_crossing_stats$out_to_in == 0)] = 'tourist'
    ## Transplants are tagged outside and then move in, never leave
    brfa_crossing_stats$catagory[which(brfa_crossing_stats$in_to_out == 0 & brfa_crossing_stats$out_to_in > 0)] = 'transplant'
    ## Outsiders are never detected in the BRFA
    brfa_crossing_stats$catagory[which(brfa_crossing_stats$in_to_out == 0 & brfa_crossing_stats$out_to_in == 0 & brfa_crossing_stats$time_tracked_out > 0)] = 'outsider'

    brfa_catagory = aggregate(brfa_crossing_stats$tag_id, by = list(brfa_crossing_stats$catagory), FUN = uniqueN)
      colnames(brfa_catagory) = c('classification', 'n')

    png(file.path(results_dir, paste('Residency Status of Tags', description, '.png')), width = 826, height = 437)
      brfa_catagory = brfa_catagory[c(which(brfa_catagory$classification == 'local'), which(brfa_catagory$classification == 'transplant'), which(brfa_catagory$classification == 'commuter'), which(brfa_catagory$classification == 'tourist'), which(brfa_catagory$classification == 'outsider')), ]
      barplot(height = brfa_catagory$n, names.arg = brfa_catagory$classification, ylim = c(0, max(brfa_catagory$n) * 1.1), main = 'Residency Status')
      text(x = c(.75, 1.9, 3.1, 4.3, 5.45), y = c(brfa_catagory$n + max(brfa_catagory$n) * 0.05), labels = brfa_catagory$n)
    dev.off()
  }
  
  brfa_crossing_stats_hist(description = 'valid_tags', receiver_df = p2s1)
  brfa_crossing_stats_hist(description = 'valid_and_questionable_tags', receiver_df = p2s2)
  
  
#### Is movement scale larger than the BRFA?

#### How many border crossings have we detected? By how many fish, How many per fish?

#### How do fish navigate the network?

#### How do we know a fish is dead vs. highly resident
  ### Compare reference tag data to actual fish


###### Network Design - Current Acoustic Web App #####
  # 
  # ## Running two instances of the acoustic web app.
  #   ## Instance 1. Network Spring 2017 (6 Dec 2016 - 23 June 2017)
  #   ## Instance 2. Network Fall 2017 (1 July 2017 - Present)
  # 
  # ### ii. Installing principle dependencies ---
  # 
  # ## Source Acoustic Web App via wrapper Function
  # source('/Users/stephenscherrer/Google Drive/Weng Lab/Personal_Folders/Steve/dissertation work/Acoustic Network Design/Acoustic Web App/wrapper_function.R')
  # 
  # ## Source SensorFenceR program
  # source('/Users/stephenscherrer/Google Drive/Weng Lab/Personal_Folders/Steve/dissertation work/Acoustic Network Design/Acoustic Fences/src/SensorFenceR.R')
  # 
  # ## Source VemcoUtilityFunctions
  # source('/Users/stephenscherrer/Google Drive/Weng Lab/Code_Library/R_code/vemcoUtilityFunctions.R')
  # 
  # ## Importing current receiver position data ---
  # receiver_data = load_receiver_data('/Users/stephenscherrer/Google Drive/Weng Lab/Data/Bottomfish/Oahu Receiver Data Files/DEPLOYMENT_RECOVERY_LOG.csv')
  # 
  # #### iii. User Selected Parameters ---
  # ### General Parameters
  # sensor_elevation = 6.1 # in meters from the benthos
  # 
  # ### Acoustic Web App Parameters
  # bathymetry_map = "MHI_50m" # Bathymetry file/resolution
  # ## Sensor Detection Range
  # sensor_detection_range = 843 # 847 m From range testing results for receiver at 3 m. See R script Range_test_june_july2014Analysis_2.R
  # 
  # ## User Specified sensors positions to include (all currently placed sensors)
  # # Extracting just the sensors that are in the water following the last deployment (2016-12-05)
  # current_sensors = receiver_data[which(receiver_data$deployment_date >= as.POSIXct('2016-12-01') & receiver_data$deployment_date < as.POSIXct('2017-6-30') & !is.na(receiver_data$recovery_date)), ]
  # user_sensors = c()
  # for(i in 1:dim(current_sensors)[1]){
  #   user_sensors = c(user_sensors, c(as.character(current_sensors[i, c("lon", "lat")])))
  # }
  # 
  # ### Run an Instance of the Acoustic Web App with the following Paramaters:
  # 
  # spring_2017_network = run_web_app(bathymetry_dataset = bathymetry_map, # options are 'MHI_1km', 'MHI_50m', and 'Palmyra'
  #                                   ### Bound by the boundaries of of BRFA E to east and west and the output of SensorFenceR to the north and south
  #                                   northern_grid_boundary = 21.44640 + .01, # as decimal degrees 
  #                                   southern_grid_boundary = 21.27367 - .01, # as decimal degrees
  #                                   western_grid_boundary = -157.72, # as decimal degrees
  #                                   eastern_grid_boundary = -157.50, # as decimal degrees
  #                                   ### Fish Movement Model
  #                                   fish_model_algorithm = 'rw', # options are rw = random walk or ou = Ornstein-Uhlenbeck
  #                                   home_range_center_x = FALSE, # as decimal degrees. Required for OU fish model. 
  #                                   home_range_center_y = FALSE, # as decimal degrees. Required for OU fish model
  #                                   home_range_extent_x = FALSE, # in meters. Required for OU fish model. Standard deviation in meters of home range extent in the x direction. Controls home range width. A rule of thumb says that for each direction in isolation approximately 95% of the time is spent within plus minus two standard deviations. A non-negative real number.
  #                                   home_range_extent_y = FALSE, # in meters. Required for OU fish model. Standard deviation in meters of home range extent in the y direction. Controls home range height. A rule of thumb says that for each direction in isolation approximately 95% of the time is spent within plus minus two standard deviations. A non-negative real number.
  #                                   directional_correlation_of_home_range = FALSE, # Required for OU fish model. Correlation of spatial distribution between directions. Controls the tilt of the home range. This is useful if the home range shape is angled relative to the x,y coordinate system. The correlation must be between -1 and 1.
  #                                   ### Fish Distribution
  #                                   restrict_vertical_habitat_range = TRUE, # Options are TRUE or FALSE#### Habitat depths
  #                                   ## Selected after consulting bottomfish distributions presented on pages 19 and 89 of Appendix 3. of 
  #                                   ## Kelley, C.D., Moriwake, V.N., 2012. Appendix 3: essential fish habitat descriptions, Part 1: bottomfish. In: WPRFMC (ed) Final fishery management plan for coral reef ecosystems of the western Pacific region, volume III, Essential Fish Habitat for Management Unit Species, p 597.
  #                                   ### These depths represent 90% the vertical distribution for all bottomfish encounters
  #                                   min_depth = -100, # units are in negative meters. Required if restrict_vertical_habitat_range == TRUE. [Optional] Specifies the shallowest depth in meters a fish will visit. Valid values are non-positive real numbers. Specifying a value for mindepth requires specifying a value for maxdepth. 
  #                                   max_depth = -400, # units are in negative meters. [Optional] Specifies the deepest depth in meters a fish will visit. Valid values are non-positive real numbers. Specifying a value for maxdepth requires specifying a value for mindepth.
  #                                   ### Depth preference also based on Kelley & Moriwake 2012 which says 2-10 m. Pg 88 based on Haight 1989
  #                                   use_depth_preference = TRUE, # Options are TRUE or FALSE
  #                                   prefered_depth = 6, # [Optional] Depth preference of fish relative to bottom (in meters off the bottom). Valid values are non-negative real numbers. Specifying a value for depth_off_bottom requires specifying a value for depth_off_bottom_sd. 
  #                                   sd_of_preferred_depth = 2, # Set to 2 by default# [Optional] Standard deviation of preferred depth (in meters). Animals spend 95% of their time within plus/minus 2 SD of their preferred depth.
  #                                   ### List of already existing sensors, two that we dont want to move as tehy have been part of the study for some time and are in areas of critical habitat. Also results from sensorFenceR
  #                                   user_specified_sensors = user_sensors, #  These sensors are placed and down-weighed before running the program. Useful for including already existing sensors, or forcing the placement of a few sensors. Do not include these sensors in the sensor count below. Provide sensor locations as a string in the format: <sensor1_Long>, <sensor1_Lat>, <sensor2_Long>, <sensor2_Lat>..." # If blank, leave as NULL
  #                                   ### The total number of available receivers minus those used in the fences
  #                                   number_of_sensors_to_use =  0, # Specifies how many sensors the program should place. Valid values are non-negative integers. 
  #                                   number_of_sensors_to_project = 0, # Specifies how many sensors the program should provide projections for. Valid values are non-negative integers. 
  #                                   goodness_algorthm = 3, # options are 1, 2, 3. Specifies how you want the system to determine the "goodness" of a cell. Valid values are 1, 2, or 3.
  #                                   # A value of '1' indicates that a "good" cell has a high number of animals within detection range (ignoring line of sight). This is useful for sensors not restricted to line-of-sight detection.
  #                                   # A value of '2' indicates that a "good" cell has the best visibility (taking into account bathymetry and shadowing, but completely ignoring fish density). This is useful for networks restricted to line-of-sight detection and having no prior knowledge of animal habitat.
  #                                   # A value of '3' indicates that a "good" cell has a high number of visible fish (incorporating both bathymetry and animal density). This is useful for networks restricted to line-of-sight detection, and having some idea of animal habitat.
  #                                   sensor_elevation = sensor_elevation, # Specifies how far off the bottom a sensor should be placed in meters. Valid values are non-negative real numbers. 
  #                                   sensor_shape_function = 'shape.gauss', # Determines which functional shape to represent horizontal acoustic attenuation in the water. The detection function specifies how the chance of signal detection declines as a function of distance to sensor. Ranging experiments should preferably be carried out locally at the study site to approximate this function. Currently, the only valid value is "shape.gauss". 
  #                                   max_detection_value = .98, # The probability of detecting a fish located right next to the sensor. Specifies a maximum value for the shape function. Valid values should be a decimal in the range (0.05,1]. 
  #                                   # From range testing results for receiver at 3 m. See R script Range_test_june_july2014Analysis_2.R
  #                                   detection_range = sensor_detection_range, # in meters. The distance in meters from the sensor where the chance of detecting a signal is 0.05. Valid values are non-negative real numbers. 
  #                                   supression_factor = 2, # Specifies the range of suppression in multiples of the detection range. For example, a suppression factor of 2 enforces a suppression range of two times the detection range. Cells within the suppression range of a sensor will be subject to the specified suppression function. To deter sensor overlap, a good value to use is twice the sensor detectionRange. Valid values are non-negative real numbers. 
  #                                   supression_function = 'supression.scale', # Specifies which suppression function to use. Options are suppression.static, suppression.scale, detection.function, detection.function.shadow, or detection.function.exact. 
  #                                   # suppression.static: Replaces all cells within range of a sensor with the value specified in minsuppressionValue.
  #                                   # suppression.scale: Multiplies the values of cells within range of a sensor by a scaling factor according to the cell's distance from the sensor. Nearby cells receive a higher scaling factor, and lower cells receive a lower scaling factor. The scaling factor is linearly related to the distance between the sensor and cell. Selecting this option requires specifyign values for minsuppressionValue and maxsuppressionValue
  #                                   # detection.function: Uses the inverse of the detection function to down scale goodness of grid cells near the sensor, but does not take objects that block signal into account. Grid cells' goodness will increase as a function of distance to the sensor.
  #                                   # detection.function.shadow: Uses the inverse of the shapeFcn to down scale goodness of grid cells near the sensor, and does take objects that block signal into account. This means that sensors on opposite sides of a blocking wall will not affect each other's goodness. Unblocked grid cells' goodness will increase as a function of distance to the sensor.
  #                                   # detection.function.exact: 
  #                                   # The above suppression functions do not recalculate the goodness grid after placing a new sensor and therefore only provide an approximately optimal solution. This suppression function does recalculate the goodness iteratively, and is therefore slower by a factor equal to the number of sensors.
  #                                   max_supression_value = 1, # Specifies the minimum scaling factor to apply. Cells at the edge of the suppression Range will receive this as a scaling factor. Valid values are non negative real numbers in the range [0,1] 
  #                                   min_supression_value = .5, # Specifies the maximum scaling factor to apply. Cells directly adjacent to a sensor will receive this as a scaling factor. Valid values are non negative real numbers in the range [0,1] 
  #                                   input_file = FALSE,  # Currently false because file is selected using bathymetry_dataset argument. # Specifies the file location of the Bathy File to use. For arcGIS filetypes, this is a path to the folder containing the data files. For netCDF filetypes, this is a path to the ncdf file itself. )
  #                                   input_file_type = FALSE)
  # 
  # 
  # ### 2. Fall 2017 Network
  # ## User Specified sensors positions to include (all currently placed sensors)
  # # Extracting just the sensors that are in the water following the last deployment (2016-12-05)
  # current_sensors = receiver_data[which(receiver_data$deployment_date >= as.POSIXct('2016-06-01') & is.na(receiver_data$recovery_date)), ]
  # user_sensors = c()
  # for(i in 1:dim(current_sensors)[1]){
  #   user_sensors = c(user_sensors, c(as.character(current_sensors[i, c("lon", "lat")])))
  # }
  # dim(current_sensors)
  # 
  # ## Run an Instance of the Acoustic Web App with the following Paramaters:
  # Fall_2017_network = run_web_app(bathymetry_dataset = bathymetry_map, # options are 'MHI_1km', 'MHI_50m', and 'Palmyra'
  #                                 ### Bound by the boundaries of of BRFA E to east and west and the output of SensorFenceR to the north and south
  #                                 northern_grid_boundary = 21.44640 + .01, # as decimal degrees 
  #                                 southern_grid_boundary = 21.27367 - .01, # as decimal degrees
  #                                 western_grid_boundary = -157.72, # as decimal degrees
  #                                 eastern_grid_boundary = -157.50, # as decimal degrees
  #                                 ### Fish Movement Model
  #                                 fish_model_algorithm = 'rw', # options are rw = random walk or ou = Ornstein-Uhlenbeck
  #                                 home_range_center_x = FALSE, # as decimal degrees. Required for OU fish model. 
  #                                 home_range_center_y = FALSE, # as decimal degrees. Required for OU fish model
  #                                 home_range_extent_x = FALSE, # in meters. Required for OU fish model. Standard deviation in meters of home range extent in the x direction. Controls home range width. A rule of thumb says that for each direction in isolation approximately 95% of the time is spent within plus minus two standard deviations. A non-negative real number.
  #                                 home_range_extent_y = FALSE, # in meters. Required for OU fish model. Standard deviation in meters of home range extent in the y direction. Controls home range height. A rule of thumb says that for each direction in isolation approximately 95% of the time is spent within plus minus two standard deviations. A non-negative real number.
  #                                 directional_correlation_of_home_range = FALSE, # Required for OU fish model. Correlation of spatial distribution between directions. Controls the tilt of the home range. This is useful if the home range shape is angled relative to the x,y coordinate system. The correlation must be between -1 and 1.
  #                                 ### Fish Distribution
  #                                 restrict_vertical_habitat_range = TRUE, # Options are TRUE or FALSE#### Habitat depths
  #                                 ## Selected after consulting bottomfish distributions presented on pages 19 and 89 of Appendix 3. of 
  #                                 ## Kelley, C.D., Moriwake, V.N., 2012. Appendix 3: essential fish habitat descriptions, Part 1: bottomfish. In: WPRFMC (ed) Final fishery management plan for coral reef ecosystems of the western Pacific region, volume III, Essential Fish Habitat for Management Unit Species, p 597.
  #                                 ### These depths represent 90% the vertical distribution for all bottomfish encounters
  #                                 min_depth = -100, # units are in negative meters. Required if restrict_vertical_habitat_range == TRUE. [Optional] Specifies the shallowest depth in meters a fish will visit. Valid values are non-positive real numbers. Specifying a value for mindepth requires specifying a value for maxdepth. 
  #                                 max_depth = -400, # units are in negative meters. [Optional] Specifies the deepest depth in meters a fish will visit. Valid values are non-positive real numbers. Specifying a value for maxdepth requires specifying a value for mindepth.
  #                                 ### Depth preference also based on Kelley & Moriwake 2012 which says 2-10 m. Pg 88 based on Haight 1989
  #                                 use_depth_preference = TRUE, # Options are TRUE or FALSE
  #                                 prefered_depth = 6, # [Optional] Depth preference of fish relative to bottom (in meters off the bottom). Valid values are non-negative real numbers. Specifying a value for depth_off_bottom requires specifying a value for depth_off_bottom_sd. 
  #                                 sd_of_preferred_depth = 2, # Set to 2 by default# [Optional] Standard deviation of preferred depth (in meters). Animals spend 95% of their time within plus/minus 2 SD of their preferred depth.
  #                                 ### List of already existing sensors, two that we dont want to move as tehy have been part of the study for some time and are in areas of critical habitat. Also results from sensorFenceR
  #                                 user_specified_sensors = user_sensors, #  These sensors are placed and down-weighed before running the program. Useful for including already existing sensors, or forcing the placement of a few sensors. Do not include these sensors in the sensor count below. Provide sensor locations as a string in the format: <sensor1_Long>, <sensor1_Lat>, <sensor2_Long>, <sensor2_Lat>..." # If blank, leave as NULL
  #                                 ### The total number of available receivers minus those used in the fences
  #                                 number_of_sensors_to_use =  0, # Specifies how many sensors the program should place. Valid values are non-negative integers. 
  #                                 number_of_sensors_to_project = 0, # Specifies how many sensors the program should provide projections for. Valid values are non-negative integers. 
  #                                 goodness_algorthm = 3, # options are 1, 2, 3. Specifies how you want the system to determine the "goodness" of a cell. Valid values are 1, 2, or 3.
  #                                 # A value of '1' indicates that a "good" cell has a high number of animals within detection range (ignoring line of sight). This is useful for sensors not restricted to line-of-sight detection.
  #                                 # A value of '2' indicates that a "good" cell has the best visibility (taking into account bathymetry and shadowing, but completely ignoring fish density). This is useful for networks restricted to line-of-sight detection and having no prior knowledge of animal habitat.
  #                                 # A value of '3' indicates that a "good" cell has a high number of visible fish (incorporating both bathymetry and animal density). This is useful for networks restricted to line-of-sight detection, and having some idea of animal habitat.
  #                                 sensor_elevation = sensor_elevation, # Specifies how far off the bottom a sensor should be placed in meters. Valid values are non-negative real numbers. 
  #                                 sensor_shape_function = 'shape.gauss', # Determines which functional shape to represent horizontal acoustic attenuation in the water. The detection function specifies how the chance of signal detection declines as a function of distance to sensor. Ranging experiments should preferably be carried out locally at the study site to approximate this function. Currently, the only valid value is "shape.gauss". 
  #                                 max_detection_value = .98, # The probability of detecting a fish located right next to the sensor. Specifies a maximum value for the shape function. Valid values should be a decimal in the range (0.05,1]. 
  #                                 # From range testing results for receiver at 3 m. See R script Range_test_june_july2014Analysis_2.R
  #                                 detection_range = sensor_detection_range, # in meters. The distance in meters from the sensor where the chance of detecting a signal is 0.05. Valid values are non-negative real numbers. 
  #                                 supression_factor = 2, # Specifies the range of suppression in multiples of the detection range. For example, a suppression factor of 2 enforces a suppression range of two times the detection range. Cells within the suppression range of a sensor will be subject to the specified suppression function. To deter sensor overlap, a good value to use is twice the sensor detectionRange. Valid values are non-negative real numbers. 
  #                                 supression_function = 'supression.scale', # Specifies which suppression function to use. Options are suppression.static, suppression.scale, detection.function, detection.function.shadow, or detection.function.exact. 
  #                                 # suppression.static: Replaces all cells within range of a sensor with the value specified in minsuppressionValue.
  #                                 # suppression.scale: Multiplies the values of cells within range of a sensor by a scaling factor according to the cell's distance from the sensor. Nearby cells receive a higher scaling factor, and lower cells receive a lower scaling factor. The scaling factor is linearly related to the distance between the sensor and cell. Selecting this option requires specifyign values for minsuppressionValue and maxsuppressionValue
  #                                 # detection.function: Uses the inverse of the detection function to down scale goodness of grid cells near the sensor, but does not take objects that block signal into account. Grid cells' goodness will increase as a function of distance to the sensor.
  #                                 # detection.function.shadow: Uses the inverse of the shapeFcn to down scale goodness of grid cells near the sensor, and does take objects that block signal into account. This means that sensors on opposite sides of a blocking wall will not affect each other's goodness. Unblocked grid cells' goodness will increase as a function of distance to the sensor.
  #                                 # detection.function.exact: 
  #                                 # The above suppression functions do not recalculate the goodness grid after placing a new sensor and therefore only provide an approximately optimal solution. This suppression function does recalculate the goodness iteratively, and is therefore slower by a factor equal to the number of sensors.
  #                                 max_supression_value = 1, # Specifies the minimum scaling factor to apply. Cells at the edge of the suppression Range will receive this as a scaling factor. Valid values are non negative real numbers in the range [0,1] 
  #                                 min_supression_value = .5, # Specifies the maximum scaling factor to apply. Cells directly adjacent to a sensor will receive this as a scaling factor. Valid values are non negative real numbers in the range [0,1] 
  #                                 input_file = FALSE,  # Currently false because file is selected using bathymetry_dataset argument. # Specifies the file location of the Bathy File to use. For arcGIS filetypes, this is a path to the folder containing the data files. For netCDF filetypes, this is a path to the ncdf file itself. )
  #                                 input_file_type = FALSE)
  # 
  # #### Old Penguin Banks/Oahu Array
  # 
  # pb_oahu_array_locations = load_receiver_data('/Users/stephenscherrer/Google Drive/Weng Lab/Personal_Folders/Steve/dissertation work/Opakapaka Tagging/AWA Runs For Makapuu Network/Post Mortum on Oahu-PB Array/receiver_locations.csv')
  # dim(pb_oahu_array_locations) 
  # # 17 25
  # 
  # user_sensors = NULL
  # for(i in 1:dim(pb_oahu_array_locations)[1]){
  #   user_sensors = c(user_sensors, c(as.character(pb_oahu_array_locations[i, c("lon", "lat")])))
  # }
  # 
  # bathymetry_map = 'MHI_50m'
  # 
  # ## Run an Instance of the Acoustic Web App with the following Paramaters:
  # Fall_2017_network = run_web_app(bathymetry_dataset = bathymetry_map, # options are 'MHI_1km', 'MHI_50m', and 'Palmyra'
  #                                 ### Bound by the boundaries of of BRFA E to east and west and the output of SensorFenceR to the north and south
  #                                 northern_grid_boundary = 21.8, # as decimal degrees 
  #                                 southern_grid_boundary = 20.83, # as decimal degrees
  #                                 western_grid_boundary = -158.29, # as decimal degrees
  #                                 eastern_grid_boundary = -157.3, # as decimal degrees
  #                                 ### Fish Movement Model
  #                                 fish_model_algorithm = 'rw', # options are rw = random walk or ou = Ornstein-Uhlenbeck
  #                                 home_range_center_x = FALSE, # as decimal degrees. Required for OU fish model. 
  #                                 home_range_center_y = FALSE, # as decimal degrees. Required for OU fish model
  #                                 home_range_extent_x = FALSE, # in meters. Required for OU fish model. Standard deviation in meters of home range extent in the x direction. Controls home range width. A rule of thumb says that for each direction in isolation approximately 95% of the time is spent within plus minus two standard deviations. A non-negative real number.
  #                                 home_range_extent_y = FALSE, # in meters. Required for OU fish model. Standard deviation in meters of home range extent in the y direction. Controls home range height. A rule of thumb says that for each direction in isolation approximately 95% of the time is spent within plus minus two standard deviations. A non-negative real number.
  #                                 directional_correlation_of_home_range = FALSE, # Required for OU fish model. Correlation of spatial distribution between directions. Controls the tilt of the home range. This is useful if the home range shape is angled relative to the x,y coordinate system. The correlation must be between -1 and 1.
  #                                 ### Fish Distribution
  #                                 restrict_vertical_habitat_range = TRUE, # Options are TRUE or FALSE#### Habitat depths
  #                                 ## Selected after consulting bottomfish distributions presented on pages 19 and 89 of Appendix 3. of 
  #                                 ## Kelley, C.D., Moriwake, V.N., 2012. Appendix 3: essential fish habitat descriptions, Part 1: bottomfish. In: WPRFMC (ed) Final fishery management plan for coral reef ecosystems of the western Pacific region, volume III, Essential Fish Habitat for Management Unit Species, p 597.
  #                                 ### These depths represent 90% the vertical distribution for all bottomfish encounters
  #                                 min_depth = -100, # units are in negative meters. Required if restrict_vertical_habitat_range == TRUE. [Optional] Specifies the shallowest depth in meters a fish will visit. Valid values are non-positive real numbers. Specifying a value for mindepth requires specifying a value for maxdepth. 
  #                                 max_depth = -400, # units are in negative meters. [Optional] Specifies the deepest depth in meters a fish will visit. Valid values are non-positive real numbers. Specifying a value for maxdepth requires specifying a value for mindepth.
  #                                 ### Depth preference also based on Kelley & Moriwake 2012 which says 2-10 m. Pg 88 based on Haight 1989
  #                                 use_depth_preference = TRUE, # Options are TRUE or FALSE
  #                                 prefered_depth = 6, # [Optional] Depth preference of fish relative to bottom (in meters off the bottom). Valid values are non-negative real numbers. Specifying a value for depth_off_bottom requires specifying a value for depth_off_bottom_sd. 
  #                                 sd_of_preferred_depth = 2, # Set to 2 by default# [Optional] Standard deviation of preferred depth (in meters). Animals spend 95% of their time within plus/minus 2 SD of their preferred depth.
  #                                 ### List of already existing sensors, two that we dont want to move as tehy have been part of the study for some time and are in areas of critical habitat. Also results from sensorFenceR
  #                                 user_specified_sensors = user_sensors, #  These sensors are placed and down-weighed before running the program. Useful for including already existing sensors, or forcing the placement of a few sensors. Do not include these sensors in the sensor count below. Provide sensor locations as a string in the format: <sensor1_Long>, <sensor1_Lat>, <sensor2_Long>, <sensor2_Lat>..." # If blank, leave as NULL
  #                                 ### The total number of available receivers minus those used in the fences
  #                                 number_of_sensors_to_use =  0, # Specifies how many sensors the program should place. Valid values are non-negative integers. 
  #                                 number_of_sensors_to_project = 0, # Specifies how many sensors the program should provide projections for. Valid values are non-negative integers. 
  #                                 goodness_algorthm = 3, # options are 1, 2, 3. Specifies how you want the system to determine the "goodness" of a cell. Valid values are 1, 2, or 3.
  #                                 # A value of '1' indicates that a "good" cell has a high number of animals within detection range (ignoring line of sight). This is useful for sensors not restricted to line-of-sight detection.
  #                                 # A value of '2' indicates that a "good" cell has the best visibility (taking into account bathymetry and shadowing, but completely ignoring fish density). This is useful for networks restricted to line-of-sight detection and having no prior knowledge of animal habitat.
  #                                 # A value of '3' indicates that a "good" cell has a high number of visible fish (incorporating both bathymetry and animal density). This is useful for networks restricted to line-of-sight detection, and having some idea of animal habitat.
  #                                 sensor_elevation = sensor_elevation, # Specifies how far off the bottom a sensor should be placed in meters. Valid values are non-negative real numbers. 
  #                                 sensor_shape_function = 'shape.gauss', # Determines which functional shape to represent horizontal acoustic attenuation in the water. The detection function specifies how the chance of signal detection declines as a function of distance to sensor. Ranging experiments should preferably be carried out locally at the study site to approximate this function. Currently, the only valid value is "shape.gauss". 
  #                                 max_detection_value = .98, # The probability of detecting a fish located right next to the sensor. Specifies a maximum value for the shape function. Valid values should be a decimal in the range (0.05,1]. 
  #                                 # From range testing results for receiver at 3 m. See R script Range_test_june_july2014Analysis_2.R
  #                                 detection_range = 843, # in meters. The distance in meters from the sensor where the chance of detecting a signal is 0.05. Valid values are non-negative real numbers. 
  #                                 supression_factor = 2, # Specifies the range of suppression in multiples of the detection range. For example, a suppression factor of 2 enforces a suppression range of two times the detection range. Cells within the suppression range of a sensor will be subject to the specified suppression function. To deter sensor overlap, a good value to use is twice the sensor detectionRange. Valid values are non-negative real numbers. 
  #                                 supression_function = 'supression.scale', # Specifies which suppression function to use. Options are suppression.static, suppression.scale, detection.function, detection.function.shadow, or detection.function.exact. 
  #                                 # suppression.static: Replaces all cells within range of a sensor with the value specified in minsuppressionValue.
  #                                 # suppression.scale: Multiplies the values of cells within range of a sensor by a scaling factor according to the cell's distance from the sensor. Nearby cells receive a higher scaling factor, and lower cells receive a lower scaling factor. The scaling factor is linearly related to the distance between the sensor and cell. Selecting this option requires specifyign values for minsuppressionValue and maxsuppressionValue
  #                                 # detection.function: Uses the inverse of the detection function to down scale goodness of grid cells near the sensor, but does not take objects that block signal into account. Grid cells' goodness will increase as a function of distance to the sensor.
  #                                 # detection.function.shadow: Uses the inverse of the shapeFcn to down scale goodness of grid cells near the sensor, and does take objects that block signal into account. This means that sensors on opposite sides of a blocking wall will not affect each other's goodness. Unblocked grid cells' goodness will increase as a function of distance to the sensor.
  #                                 # detection.function.exact: 
  #                                 # The above suppression functions do not recalculate the goodness grid after placing a new sensor and therefore only provide an approximately optimal solution. This suppression function does recalculate the goodness iteratively, and is therefore slower by a factor equal to the number of sensors.
  #                                 max_supression_value = 1, # Specifies the minimum scaling factor to apply. Cells at the edge of the suppression Range will receive this as a scaling factor. Valid values are non negative real numbers in the range [0,1] 
  #                                 min_supression_value = .5, # Specifies the maximum scaling factor to apply. Cells directly adjacent to a sensor will receive this as a scaling factor. Valid values are non negative real numbers in the range [0,1] 
  #                                 input_file = FALSE,  # Currently false because file is selected using bathymetry_dataset argument. # Specifies the file location of the Bathy File to use. For arcGIS filetypes, this is a path to the folder containing the data files. For netCDF filetypes, this is a path to the ncdf file itself. )
  #                                 input_file_type = FALSE)
  # 
  # 
  # 
  # 
  
  ##### Fork Length Histogram
  hist_fl = function(description = 'all_tags', receiver_df){
  data = as.numeric(receiver_df$summary_df$fork_length)
  hist_breaks = seq(min(data)-5, max(data)+5, by = 5)
  pre_hist = hist(data, breaks = hist_breaks, plot = FALSE)
  png(file.path(results_dir, paste('fl hist', description, '.png', sep = "")), width = 826, height = 437)
    hist(data, xlab = 'Fork Length (cm)', main = NULL, breaks = hist_breaks, xlim = c(min(hist_breaks), max(hist_breaks)), ylim = c(0, max(pre_hist$counts)*1.2))
    text(x = hist_breaks + 2.5, y = pre_hist$counts + max(pre_hist$count) / 10 , labels = pre_hist$counts)
    legend('topright', legend = c(paste('Median =',  fivenum(data, na.rm = TRUE)[3], '\n IQR =', fivenum(data, na.rm = TRUE)[2], '-', fivenum(data, na.rm = TRUE)[4])))
  dev.off()
  }
  
  hist_fl(description = 'all tags', p2s0)
  hist_fl(description = 'valid tags', p2s1)
  hist_fl(description = 'valid and questionable tags', p2s2)
  
  #### Time at Liberty Histogram
  hist_tal = function(description = 'all_tags', receiver_df){
    data = as.numeric(receiver_df$summary_df$time_at_liberty)
    hist_breaks = seq(min(data)-5, max(data)+5, by = 5)
    pre_hist = hist(data, breaks = hist_breaks, plot = FALSE)
    png(file.path(results_dir, paste('tal hist', description, '.png', sep = "")), width = 826, height = 437)
    hist(data, xlab = 'Time at Liberty (Days)', main = NULL, breaks = hist_breaks, xlim = c(min(hist_breaks), max(hist_breaks)), ylim = c(0, max(pre_hist$counts)*1.2))
    # text(x = hist_breaks + 2.5, y = pre_hist$counts + max(pre_hist$count) / 10 , labels = pre_hist$counts)
    legend('topright', legend = c(paste('Median =',  fivenum(data, na.rm = TRUE)[3], '\n IQR =', fivenum(data, na.rm = TRUE)[2], '-', fivenum(data, na.rm = TRUE)[4])))
    dev.off()
  }
  
  # hist_tal(description = 'all tags', p2s0)
  hist_tal(description = 'valid tags', p2s1)
  hist_tal(description = 'valid and questionable tags', p2s2)
  
  #### Number of Detections Histogram
  hist_n_detections = function(description = 'all_tags', receiver_df){
    data = as.numeric(receiver_df$summary_df$n_detections)
    hist_breaks = seq(0, max(data)+500, by = 500)
    pre_hist = hist(data, breaks = hist_breaks, plot = FALSE)
    png(file.path(results_dir, paste('n detections', description, '.png', sep = "")), width = 826, height = 437)
    hist(data, xlab = '# of detections', main = NULL, breaks = hist_breaks, xlim = c(min(hist_breaks), max(hist_breaks)), ylim = c(0, max(pre_hist$counts)*1.2))
    # text(x = hist_breaks + 2.5, y = pre_hist$counts + max(pre_hist$count) / 10 , labels = pre_hist$counts)
    legend('topright', legend = c(paste('Median =',  fivenum(data, na.rm = TRUE)[3], '\n IQR =', fivenum(data, na.rm = TRUE)[2], '-', fivenum(data, na.rm = TRUE)[4])))
    dev.off()
  }
  
  hist_n_detections(description = 'all tags', p2s0)
  hist_n_detections(description = 'valid tags', p2s1)
  hist_n_detections(description = 'valid and questionable tags', p2s2)
  
  
  #### Percent of transmissions detected (recovery rate)
  hist_recovery_rate = function(description = 'all_tags', receiver_df){
    data = as.numeric(receiver_df$summary_df$percent_of_detections)
    hist_breaks = seq(0, max(data)+.005, by = .005)
    pre_hist = hist(data, breaks = hist_breaks, plot = FALSE)
    png(file.path(results_dir, paste('recovery rate hist', description, '.png', sep = "")), width = 826, height = 437)
    hist(data, xlab = 'Transmission Recovery Rate', main = NULL, breaks = hist_breaks, xlim = c(min(hist_breaks), max(hist_breaks)), ylim = c(0, max(pre_hist$counts)*1.2))
    text(x = hist_breaks + 2.5, y = pre_hist$counts + max(pre_hist$count) / 10 , labels = pre_hist$counts)
    legend('topright', legend = c(paste('Median =',  round(fivenum(data, na.rm = TRUE)[3], digits = 2), '\n IQR =', round(fivenum(data, na.rm = TRUE)[2], digits = 2), '-', round(fivenum(data, na.rm = TRUE)[4], digits = 2))))
    dev.off()
  }
  
  hist_recovery_rate(description = 'all tags', p2s0)
  hist_recovery_rate(description = 'valid tags', p2s1)
  hist_recovery_rate(description = 'valid and questionable tags', p2s2)
  
  
  #### Detections Per Day
  hist_detections_per_day = function(description = 'all_tags', receiver_df){
    data = as.numeric(receiver_df$summary_df$detections_per_day)
    hist_breaks = seq(min(data)-5, max(data)+5, by = 5)
    pre_hist = hist(data, breaks = hist_breaks, plot = FALSE)
    png(file.path(results_dir, paste('detections per day hist', description, '.png', sep = "")), width = 826, height = 437)
    hist(data, xlab = 'Mean Detections Per Day', main = NULL, breaks = hist_breaks, xlim = c(min(hist_breaks), max(hist_breaks)), ylim = c(0, max(pre_hist$counts)*1.2))
    # text(x = hist_breaks + 2.5, y = pre_hist$counts + max(pre_hist$count) / 10 , labels = pre_hist$counts)
    legend('topright', legend = c(paste('Median =',  round(fivenum(data, na.rm = TRUE)[3], digits = 2), '\n IQR =', round(fivenum(data, na.rm = TRUE)[2], digits = 2), '-', round(fivenum(data, na.rm = TRUE)[4], digits = 2))))
    dev.off()
  }
  
  hist_detections_per_day(description = 'all tags', p2s0)
  hist_detections_per_day(description = 'valid tags', p2s1)
  hist_detections_per_day(description = 'valid and questionable tags', p2s2)
  
  
  ### Unique Days Detected
  hist_unique_days = function(description = 'all_tags', receiver_df){
    data = as.numeric(receiver_df$summary_df$unique_days_detected)
    hist_breaks = seq(0, max(data)+5, by = 5)
    pre_hist = hist(data, breaks = hist_breaks, plot = FALSE)
    png(file.path(results_dir, paste('unique days hist', description, '.png', sep = "")), width = 826, height = 437)
    hist(data, xlab = 'Unique Days Detected', main = NULL, breaks = hist_breaks, xlim = c(min(hist_breaks), max(hist_breaks)), ylim = c(0, max(pre_hist$counts)*1.2))
    # text(x = hist_breaks + 2.5, y = pre_hist$counts + max(pre_hist$count) / 10 , labels = pre_hist$counts)
    legend('topright', legend = c(paste('Median =',  round(fivenum(data, na.rm = TRUE)[3], digits = 2), '\n IQR =', round(fivenum(data, na.rm = TRUE)[2], digits = 2), '-', round(fivenum(data, na.rm = TRUE)[4], digits = 2))))
    dev.off()
  }
  
  #hist_unique_days(description = 'all tags', p2s0)
  hist_unique_days(description = 'valid tags', p2s1)
  hist_unique_days(description = 'valid and questionable tags', p2s2)
  
  
  
  #### Distance Tracked
  hist_distance_tracked = function(description = 'all_tags', receiver_df){
    data = as.numeric(receiver_df$summary_df$distance_tracked)
    hist_breaks = seq(0, max(data)+5, by = 5)
    pre_hist = hist(data, breaks = hist_breaks, plot = FALSE)
    png(file.path(results_dir, paste('distance tracked hist', description, '.png', sep = "")), width = 826, height = 437)
    hist(data, xlab = 'Distance Tracked (km)', main = NULL, breaks = hist_breaks, xlim = c(min(hist_breaks), max(hist_breaks)), ylim = c(0, max(pre_hist$counts)*1.2))
    # text(x = hist_breaks + 2.5, y = pre_hist$counts + max(pre_hist$count) / 10 , labels = pre_hist$counts)
    legend('topright', legend = c(paste('Median =',  round(fivenum(data, na.rm = TRUE)[3], digits = 2), '\n IQR =', round(fivenum(data, na.rm = TRUE)[2], digits = 2), '-', round(fivenum(data, na.rm = TRUE)[4], digits = 2))))
    dev.off()
  }
  
  #hist_distance_tracked(description = 'all tags', p2s0)
  hist_distance_tracked(description = 'valid tags', p2s1)
  hist_distance_tracked(description = 'valid and questionable tags', p2s2)
  
  
  #### Distance Per Day
  hist_distance_per_day = function(description = 'all_tags', receiver_df){
    data = as.numeric(receiver_df$summary_df$distance_per_day)
    hist_breaks = seq(0, max(data)+.05, by = .05)
    pre_hist = hist(data, breaks = hist_breaks, plot = FALSE)
    png(file.path(results_dir, paste('distance per day hist', description, '.png', sep = "")), width = 826, height = 437)
    hist(data, xlab = 'Mean Distance Tracked Per Day (km)', main = NULL, breaks = hist_breaks, xlim = c(min(hist_breaks), max(hist_breaks)), ylim = c(0, max(pre_hist$counts)*1.2))
    # text(x = hist_breaks + 2.5, y = pre_hist$counts + max(pre_hist$count) / 10 , labels = pre_hist$counts)
    legend('topright', legend = c(paste('Median =',  round(fivenum(data, na.rm = TRUE)[3], digits = 2), '\n IQR =', round(fivenum(data, na.rm = TRUE)[2], digits = 2), '-', round(fivenum(data, na.rm = TRUE)[4], digits = 2))))
    dev.off()
  }
  
  #hist_distance_per_day(description = 'all tags', p2s0)
  hist_distance_per_day(description = 'valid tags', p2s1)
  hist_distance_per_day(description = 'valid and questionable tags', p2s2)
  
  
  #### Receivers Detected
  hist_receivers_detected = function(description = 'all_tags', receiver_df){
    data = as.numeric(receiver_df$summary_df$receivers_detected)
    hist_breaks = seq(0, max(data)+5, by = 1)
    pre_hist = hist(data, breaks = hist_breaks, plot = FALSE)
    png(file.path(results_dir, paste('n receivers detected', description, '.png', sep = "")), width = 826, height = 437)
    hist(data, xlab = 'n Receivers Detected', main = NULL, breaks = hist_breaks, xlim = c(min(hist_breaks), max(hist_breaks)), ylim = c(0, max(pre_hist$counts)*1.2))
    # text(x = hist_breaks + 2.5, y = pre_hist$counts + max(pre_hist$count) / 10 , labels = pre_hist$counts)
    legend('topright', legend = c(paste('Median =',  round(fivenum(data, na.rm = TRUE)[3], digits = 2), '\n IQR =', round(fivenum(data, na.rm = TRUE)[2], digits = 2), '-', round(fivenum(data, na.rm = TRUE)[4], digits = 2))))
    dev.off()
  }
  
  #hist_receivers_detected(description = 'all tags', p2s0)
  hist_receivers_detected(description = 'valid tags', p2s1)
  hist_receivers_detected(description = 'valid and questionable tags', p2s2)
  
  
  #### Movements Detected
  hist_movements_detected = function(description = 'all_tags', receiver_df){
    data = as.numeric(receiver_df$summary_df$movements)
    hist_breaks = seq(0, max(data)+1, by = 1)
    pre_hist = hist(data, breaks = hist_breaks, plot = FALSE)
    png(file.path(results_dir, paste('n movements detected', description, '.png', sep = "")), width = 826, height = 437)
    hist(data, xlab = 'n Movements Detected', main = NULL, breaks = hist_breaks, xlim = c(min(hist_breaks), max(hist_breaks)), ylim = c(0, max(pre_hist$counts)*1.2))
    # text(x = hist_breaks + 2.5, y = pre_hist$counts + max(pre_hist$count) / 10 , labels = pre_hist$counts)
    legend('topright', legend = c(paste('Median =',  round(fivenum(data, na.rm = TRUE)[3], digits = 2), '\n IQR =', round(fivenum(data, na.rm = TRUE)[2], digits = 2), '-', round(fivenum(data, na.rm = TRUE)[4], digits = 2))))
    dev.off()
  }
  
  #hist_movements_detected(description = 'all tags', p2s0)
  hist_movements_detected(description = 'valid tags', p2s1)
  hist_movements_detected(description = 'valid and questionable tags', p2s2)
  
  
  #### Movements Per Day
  hist_movements_per_day = function(description = 'all_tags', receiver_df){
    data = as.numeric(receiver_df$summary_df$movements_by_day)
    hist_breaks = seq(0, max(data)+1, by = 1)
    pre_hist = hist(data, breaks = hist_breaks, plot = FALSE)
    png(file.path(results_dir, paste('mean movements by tal', description, '.png', sep = "")), width = 826, height = 437)
    hist(data, xlab = 'Mean Movements Per Day at Liberty', main = NULL, breaks = hist_breaks, xlim = c(min(hist_breaks), max(hist_breaks)), ylim = c(0, max(pre_hist$counts)*1.2))
    # text(x = hist_breaks + 2.5, y = pre_hist$counts + max(pre_hist$count) / 10 , labels = pre_hist$counts)
    legend('topright', legend = c(paste('Median =',  round(fivenum(data, na.rm = TRUE)[3], digits = 2), '\n IQR =', round(fivenum(data, na.rm = TRUE)[2], digits = 2), '-', round(fivenum(data, na.rm = TRUE)[4], digits = 2))))
    dev.off()
  }
  
  #hist_movements_per_day(description = 'all tags', p2s0)
  hist_movements_per_day(description = 'valid tags', p2s1)
  hist_movements_per_day(description = 'valid and questionable tags', p2s2)
  
  #### BRFA Crossings Detected
  hist_brfa_crossings = function(description = 'all_tags', receiver_df){
    data = as.numeric(receiver_df$summary_df$brfa_crossings)
    hist_breaks = seq(0, max(data)+1, by = 1)
    pre_hist = hist(data, breaks = hist_breaks, plot = FALSE)
    png(file.path(results_dir, paste('BRFA Crossings Detected - ', description, '.png', sep = "")), width = 826, height = 437)
    hist(data, xlab = 'Movements Across BRFA', main = NULL, breaks = hist_breaks, xlim = c(min(hist_breaks), max(hist_breaks)), ylim = c(0, max(pre_hist$counts)*1.2))
    # text(x = hist_breaks + 2.5, y = pre_hist$counts + max(pre_hist$count) / 10 , labels = pre_hist$counts)
    legend('topright', legend = c(paste('Median =',  round(fivenum(data, na.rm = TRUE)[3], digits = 2), '\n IQR =', round(fivenum(data, na.rm = TRUE)[2], digits = 2), '-', round(fivenum(data, na.rm = TRUE)[4], digits = 2))))
    dev.off()
  }
  
  hist_brfa_crossings(description = 'valid tags', p2s1)
  hist_brfa_crossings(description = 'valid and questionable tags', p2s2)
  
  
  #### Mean BRFA Crossings per day at liberty
  hist_brfa_crossings_per_day = function(description = 'all_tags', receiver_df){
    data = as.numeric(receiver_df$summary_df$brfa_crossings_per_day)
    hist_breaks = seq(0, max(data)+.1, by = .01)
    pre_hist = hist(data, breaks = hist_breaks, plot = FALSE)
    png(file.path(results_dir, paste('Mean BRFA Crossings Per Day ', description, '.png', sep = "")), width = 826, height = 437)
    hist(data, xlab = 'Movements Across BRFA / Time At Liberty', main = NULL, breaks = hist_breaks, xlim = c(min(hist_breaks), max(hist_breaks)), ylim = c(0, max(pre_hist$counts)*1.2))
    # text(x = hist_breaks + 2.5, y = pre_hist$counts + max(pre_hist$count) / 10 , labels = pre_hist$counts)
    legend('topright', legend = c(paste('Median =',  round(fivenum(data, na.rm = TRUE)[3], digits = 2), '\n IQR =', round(fivenum(data, na.rm = TRUE)[2], digits = 2), '-', round(fivenum(data, na.rm = TRUE)[4], digits = 2))))
    dev.off()
  }
  
  #hist_brfa_crossings_per_day(description = 'all tags', p2s0)
  hist_brfa_crossings_per_day(description = 'valid tags', p2s1)
  hist_brfa_crossings_per_day(description = 'valid and questionable tags', p2s2)
  
  #### Time Tracked in BRFA
  hist_time_tracked_in = function(description = 'all_tags', receiver_df){
    data = as.numeric(receiver_df$summary_df$time_tracked_in)
    hist_breaks = seq(0, max(data)+5, by = 5)
    pre_hist = hist(data, breaks = hist_breaks, plot = FALSE)
    png(file.path(results_dir, paste('time tracked in BRFA', description, '.png', sep = "")), width = 826, height = 437)
    hist(data, xlab = 'Days In BRFA', main = NULL, breaks = hist_breaks, xlim = c(min(hist_breaks), max(hist_breaks)), ylim = c(0, max(pre_hist$counts)*1.2))
    # text(x = hist_breaks + 2.5, y = pre_hist$counts + max(pre_hist$count) / 10 , labels = pre_hist$counts)
    legend('topright', legend = c(paste('Median =',  round(fivenum(data, na.rm = TRUE)[3], digits = 2), '\n IQR =', round(fivenum(data, na.rm = TRUE)[2], digits = 2), '-', round(fivenum(data, na.rm = TRUE)[4], digits = 2))))
    dev.off()
  }
  
  hist_time_tracked_in(description = 'all tags', p2s0)
  hist_time_tracked_in(description = 'valid tags', p2s1)
  hist_time_tracked_in(description = 'valid and questionable tags', p2s2)
  
  #### Time tracked out of BRFA
  hist_time_tracked_out = function(description = 'all_tags', receiver_df){
    data = as.numeric(receiver_df$summary_df$time_tracked_out)
    hist_breaks = seq(0, max(data)+5, by = 5)
    pre_hist = hist(data, breaks = hist_breaks, plot = FALSE)
    png(file.path(results_dir, paste('time tracked outside BRFA', description, '.png', sep = "")), width = 826, height = 437)
    hist(data, xlab = 'Days Outside BRFA', main = NULL, breaks = hist_breaks, xlim = c(min(hist_breaks), max(hist_breaks)), ylim = c(0, max(pre_hist$counts)*1.2))
    # text(x = hist_breaks + 2.5, y = pre_hist$counts + max(pre_hist$count) / 10 , labels = pre_hist$counts)
    legend('topright', legend = c(paste('Median =',  round(fivenum(data, na.rm = TRUE)[3], digits = 2), '\n IQR =', round(fivenum(data, na.rm = TRUE)[2], digits = 2), '-', round(fivenum(data, na.rm = TRUE)[4], digits = 2))))
    dev.off()
  }
  
  hist_time_tracked_out(description = 'all tags', p2s0)
  hist_time_tracked_out(description = 'valid tags', p2s1)
  hist_time_tracked_out(description = 'valid and questionable tags', p2s2)

  
  #### Homerange Polygon
  hist_homerange_polgyon = function(description = 'all_tags', receiver_df){
    data = as.numeric(receiver_df$summary_df$homerange_polygon)
    hist_breaks = seq(0, max(data)+1, by = 1)
    pre_hist = hist(data, breaks = hist_breaks, plot = FALSE)
    png(file.path(results_dir, paste('Polygonal Homerange', description, '.png', sep = "")), width = 826, height = 437)
    hist(data, xlab = 'Polygonal Homerange (km)', main = NULL, breaks = hist_breaks, xlim = c(min(hist_breaks), max(hist_breaks)), ylim = c(0, max(pre_hist$counts)*1.2))
    # text(x = hist_breaks + 2.5, y = pre_hist$counts + max(pre_hist$count) / 10 , labels = pre_hist$counts)
    legend('topright', legend = c(paste('Median =',  round(fivenum(data, na.rm = TRUE)[3], digits = 2), '\n IQR =', round(fivenum(data, na.rm = TRUE)[2], digits = 2), '-', round(fivenum(data, na.rm = TRUE)[4], digits = 2))))
    dev.off()
  }
  
  hist_homerange_polgyon(description = 'all tags', p2s0)
  hist_homerange_polgyon(description = 'valid tags', p2s1)
  hist_homerange_polgyon(description = 'valid and questionable tags', p2s2)
  
  #### Homerange Linear
  hist_homerange_linear = function(description = 'all_tags', receiver_df){
    data = as.numeric(receiver_df$summary_df$homerange_linear)
    hist_breaks = seq(0, max(data)+1, by = 1)
    pre_hist = hist(data, breaks = hist_breaks, plot = FALSE)
    png(file.path(results_dir, paste('Linear Homerange', description, '.png', sep = "")), width = 826, height = 437)
    hist(data, xlab = 'Linear Homerange (km)', main = NULL, breaks = hist_breaks, xlim = c(min(hist_breaks), max(hist_breaks)), ylim = c(0, max(pre_hist$counts)*1.2))
    # text(x = hist_breaks + 2.5, y = pre_hist$counts + max(pre_hist$count) / 10 , labels = pre_hist$counts)
    legend('topright', legend = c(paste('Median =',  round(fivenum(data, na.rm = TRUE)[3], digits = 2), '\n IQR =', round(fivenum(data, na.rm = TRUE)[2], digits = 2), '-', round(fivenum(data, na.rm = TRUE)[4], digits = 2))))
    dev.off()
  }
  
  hist_homerange_linear(description = 'all tags', p2s0)
  hist_homerange_linear(description = 'valid tags', p2s1)
  hist_homerange_linear(description = 'valid and questionable tags', p2s2)
  
  #### Unique Days/Time At Liberty
  unique_days_by_tal = function(description = 'all_tags', receiver_df){
    data = as.numeric(receiver_df$unique_days_detected) / ceiling(as.numeric(receiver_df$time_at_liberty))
    hist_breaks = seq(0, max(data)+1, by = .1)
    pre_hist = hist(data, breaks = hist_breaks, plot = FALSE)
    png(file.path(results_dir, paste('days_detected per day at liberty - ', description, '.png', sep = "")), width = 826, height = 437)
    hist(data, xlab = 'Unique Days / Time at Liberty', main = NULL, breaks = hist_breaks, xlim = c(min(hist_breaks), max(hist_breaks)), ylim = c(0, max(pre_hist$counts)*1.2))
    # text(x = hist_breaks + 2.5, y = pre_hist$counts + max(pre_hist$count) / 10 , labels = pre_hist$counts)
    legend('topright', legend = c(paste('Median =',  round(fivenum(data, na.rm = TRUE)[3], digits = 2), '\n IQR =', round(fivenum(data, na.rm = TRUE)[2], digits = 2), '-', round(fivenum(data, na.rm = TRUE)[4], digits = 2))))
    dev.off()
  }
  
  unique_days_by_tal(description = 'valid tags', p2s1)
  unique_days_by_tal(description = 'valid and questionable tags', p2s2)
  
  
#### Map of tagging locations
  plot_tagging_locations = function(description = 'all tags', tag_ids){
    tagging_locations = c()
  tagging_locations = as.data.frame(cbind(tagging_data$vem_tag_id, tagging_data$lat, tagging_data$lon, paste(tagging_data$lat, tagging_data$lon), tagging_data$species), stringsAsFactors = FALSE)
  colnames(tagging_locations) = c('tag_id', 'lat', 'lon', 'location', 'species')
  tagging_locations = tagging_locations[tagging_locations$tag_id %in% tag_ids, ]
  tagging_locations = tagging_locations[tagging_locations$species == "Opakapaka" | tagging_locations$species == "Opakapaka (Deceased)", ]
  tagging_locations = aggregate(tagging_locations$tag_id, by = list(tagging_locations$lat, tagging_locations$lon), FUN = uniqueN)
    colnames(tagging_locations) = c('lat', 'lon', 'n')
  
    plot_col_pal = rainbow(max(tagging_locations$n), start = 4/6, end = 0)
  

  bathymetry = getNOAA.bathy(lon1 = -157.8, 
                             lon2 = -157.5, 
                             lat1 = 21.2, 
                             lat2 = 21.5,
                             resolution = .75)
  png(file.path(results_dir, paste('tagging locations - ', description, '.png', sep = "")), width = 1000, height = 860)
  ## Plotting basemap
  plot.bathy(bathymetry, land = TRUE, image=TRUE, bpal = 'white', deepest.isobath = c(-10000), shallowest.isobath = c(0), step = c(100), drawlabels = TRUE, main = 'Tagging Locations')
  ## Adding scale legend
  scaleBathy(bathymetry, deg = .1, cex = .5)
  
  ## Adding tagging locations
  for(i in min(tagging_locations$n):max(tagging_locations$n)){
    points(as.numeric(tagging_locations$lat[tagging_locations$n == i]) ~ as.numeric(tagging_locations$lon[tagging_locations$n  == i]), pch = 19, col = plot_col_pal[i], cex = 2)
    points(as.numeric(tagging_locations$lat[tagging_locations$n == i]) ~ as.numeric(tagging_locations$lon[tagging_locations$n  == i]), pch = 19, col = plot_col_pal[i], cex = 1)
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
  png(file.path(results_dir, paste('tagging locations colorbar scale - ', description, '.png', sep = "")), width = 430, height = 213)
    plot(rep(1, max(tagging_locations$n)) ~ c(1:max(tagging_locations$n)), col = plot_col_pal[1:max(tagging_locations$n)], pch = 15, cex = 5)
  dev.off()
  }
  
  plot_tagging_locations(description = 'all tags', tag_ids = tagging_data$vem_tag_id[tagging_data$datetime >= start & tagging_data$datetime < end ])
  plot_tagging_locations(description = 'valid tags', tag_ids = p2s1$tag_ids)
  plot_tagging_locations(description = 'valid and questionable tags', tag_ids = p2s2$tag_ids[p2s2$tag_ids %in% tagging_data$vem_tag_id[tagging_data$datetime >= start & tagging_data$datetime < as.POSIXct("2017-6-11")]])

####  Map of receivers colored by number of fish that hit on each
  receiver_use_individual_level_map = function(description, receiver_df){
    receiver_use_by_individuals = aggregate(receiver_df$data$tag_id, by = list(receiver_df$data$station), FUN = uniqueN)
    colnames(receiver_use_by_individuals) = c('station', 'n_fish_detected')
    receiver_use_by_individuals = receiver_use_by_individuals[receiver_use_by_individuals$station != 'Tagging Location', ]
    
    plot_col_pal = rainbow(max(tagging_locations$n), start = 4/6, end = 0)
    
    bathymetry = getNOAA.bathy(lon1 = -157.8, 
                               lon2 = -157.5, 
                               lat1 = 21.2, 
                               lat2 = 21.5,
                               resolution = .75)
    png(file.path(results_dir, paste('Receiver Use By Individuals - ', description, '.png', sep = "")), width = 1000, height = 860)
    ## Plotting basemap
    plot.bathy(bathymetry, land = TRUE, image=TRUE, bpal = 'white', deepest.isobath = c(-10000), shallowest.isobath = c(0), step = c(100), drawlabels = TRUE, main = 'Tagging Locations')
    ## Adding scale legend
    scaleBathy(bathymetry, deg = .1, cex = .5)
    
    unique_receiver_list = data.frame(stringsAsFactors = FALSE)
    receiver_df$receiver_data = receiver_df$receiver_data[which(receiver_df$receiver_data$deployment_date >= min(receiver_df$data$datetime) & receiver_df$receiver_data$deployment_date < max(receiver_df$data$datetime) & (receiver_df$receiver_data$recovered == "" | is.na(receiver_df$receiver_data$recovered))), ]
      for(i in 1:length(unique(receiver_df$receiver_data$station_name))){
        unique_receiver_list = rbind(unique_receiver_list, receiver_df$receiver_data[which(receiver_df$receiver_data$station_name == unique(receiver_df$receiver_data$station_name)[i])[1], ])
      }
    
    points(unique_receiver_list$lat~ unique_receiver_list$lon)
    
    receiver_use_by_individuals = merge(x = receiver_use_by_individuals, y = unique_receiver_list[ ,c('station_name', 'lon', 'lat')], by.x = 'station', by.y = 'station_name')
    
    
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
    png(file.path(results_dir, paste('Receiver Use By Individuals colorbar scale - ', description, '.png', sep = "")), width = 430, height = 213)
      plot(rep(1, max(receiver_use_by_individuals$n_fish_detected)) ~ c(1:max(receiver_use_by_individuals$n_fish_detected)), col = plot_col_pal[1:max(receiver_use_by_individuals$n_fish_detected)], pch = 15, cex = 5)
    dev.off()
  }
  receiver_use_individual_level_map(description = 'all tags', receiver_df = p2s0)
  receiver_use_individual_level_map(description = 'valid tags', receiver_df = p2s1)
  receiver_use_individual_level_map(description = 'valid and questionable tags', receiver_df = p2s2)
  
####  Map of receivers colored by detections on each
  receiver_use_detection_level_map = function(description, receiver_df){
    receiver_use_by_detections = aggregate(receiver_df$data$datetime, by = list(receiver_df$data$station), FUN = uniqueN)
    colnames(receiver_use_by_detections) = c('station', 'n_detections')
    receiver_use_by_detections = receiver_use_by_detections[receiver_use_by_detections$station != 'Tagging Location', ]
    
    plot_col_pal = rainbow(max(receiver_use_by_detections$n), start = 4/6, end = 0)
    
    bathymetry = getNOAA.bathy(lon1 = -157.8, 
                               lon2 = -157.5, 
                               lat1 = 21.2, 
                               lat2 = 21.5,
                               resolution = .75)
    png(file.path(results_dir, paste('Receiver Use By Detections - ', description, '.png', sep = "")), width = 1000, height = 860)
    ## Plotting basemap
    plot.bathy(bathymetry, land = TRUE, image=TRUE, bpal = 'white', deepest.isobath = c(-10000), shallowest.isobath = c(0), step = c(100), drawlabels = TRUE, main = 'Tagging Locations')
    ## Adding scale legend
    scaleBathy(bathymetry, deg = .1, cex = .5)
    
    unique_receiver_list = data.frame(stringsAsFactors = FALSE)
    receiver_df$receiver_data = receiver_df$receiver_data[which(receiver_df$receiver_data$deployment_date >= min(receiver_df$data$datetime) & receiver_df$receiver_data$deployment_date < max(receiver_df$data$datetime) & (receiver_df$receiver_data$recovered == "" | is.na(receiver_df$receiver_data$recovered))), ]
    for(i in 1:length(unique(receiver_df$receiver_data$station_name))){
      unique_receiver_list = rbind(unique_receiver_list, receiver_df$receiver_data[which(receiver_df$receiver_data$station_name == unique(receiver_df$receiver_data$station_name)[i])[1], ])
    }
    
    points(unique_receiver_list$lat~ unique_receiver_list$lon)
    
    receiver_use_by_detections = merge(x = receiver_use_by_detections, y = unique_receiver_list[ ,c('station_name', 'lon', 'lat')], by.x = 'station', by.y = 'station_name')
    
    
    for(i in receiver_use_by_detections$n){
      points(as.numeric(receiver_use_by_detections$lat[receiver_use_by_detections$n_detections == i]) ~ as.numeric(receiver_use_by_detections$lon[receiver_use_by_detections$n_detections  == i]), pch = 19, col = plot_col_pal[i], cex = 2)
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
    png(file.path(results_dir, paste('Receiver Use By Detections colorbar scale - ', description, '.png', sep = "")), width = 430, height = 213)
      plot(rep(1, max(receiver_use_by_detections$n_detections)) ~ c(1:max(receiver_use_by_detections$n_detections)), col = plot_col_pal[1:max(receiver_use_by_detections$n_detections)], pch = 15, cex = 5)
    dev.off()
  }
  receiver_use_detection_level_map(description = 'all tags', receiver_df = p2s0)
  receiver_use_detection_level_map(description = 'valid tags', receiver_df = p2s1)
  receiver_use_detection_level_map(description = 'valid and questionable tags', receiver_df = p2s2)
  
  
  #### Lunar phase vs. detections
  # Are there some locations more popular at full moon?  New moon?  Visualize using map of receiver network. 
  library(lunar)
  
  individuals_by_lunar_phase = function(description, receiver_df){
    # Maps of stations where each station is colored by number of fish detected during that quarter of moon phase.  For entire dataset
    ### A fish detected at that receiver, during that week (moon quadrant) gets a 1. 
    
    ## Constructing a data frame of lunar phases for each day during the period of interest
    lunar_weeks = data.frame('day' = as.Date(seq(start, end, by = 'day')), 'week' = NA, stringsAsFactors = FALSE)
    lunar_weeks$phase = lunar.phase(x = lunar_weeks$day, shit = -10, name = 4)
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
    receiver_df$data$day = as.Date(receiver_df$data$datetime)
    receiver_df$data = merge(x = receiver_df$data, y = lunar_weeks, by.x = 'day', by.y = 'day')
    
    ## Aggregating data by lunar phase. How many unique individuals detected at a station during each lunar phase?
      regolith = aggregate(receiver_df$data$tag_id, by = list(receiver_df$data$phase, receiver_df$data$week, receiver_df$data$station), FUN = uniqueN) ## Lunar aggregate
        colnames(regolith) = c('phase', 'week', 'station', 'n_tags')
        regolith = regolith[regolith$station != 'Tagging Location', ]
    ## Aggregating this data down again to sum across all weeks/same lunar phases
      compounded_regolith = aggregate(regolith$n_tags, by = list(regolith$phase, regolith$station), FUN = sum)
        colnames(compounded_regolith) = c('phase', 'station', 'n_sum_tags')
  
      
    ### Plotting this data
    png(file.path(results_dir, paste('n_fish_by_lunar_phase - ', description, '.png', sep = "")), width = 826, height = 437)
    par(mfrow = c(2,2))
    
    ## Creating a color palette
    plot_col_pal = rainbow(max(compounded_regolith$n_sum_tags), start = 4/6, end = 0)
    
    ## Sourcing bathy data
    bathymetry = getNOAA.bathy(lon1 = -157.8, 
                               lon2 = -157.5, 
                               lat1 = 21.2, 
                               lat2 = 21.5,
                               resolution = .75)
    
    ## Getting a list of coordinate positions for receivers - One location per station (ignores variability in deployment over time)
    unique_receiver_list = data.frame(stringsAsFactors = FALSE)
    receiver_df$receiver_data = receiver_df$receiver_data[which(receiver_df$receiver_data$deployment_date >= min(receiver_df$data$datetime) & receiver_df$receiver_data$deployment_date < max(receiver_df$data$datetime) & (receiver_df$receiver_data$recovered == "" | is.na(receiver_df$receiver_data$recovered))), ]
  
    for(i in 1:length(unique(receiver_df$receiver_data$station_name))){
      unique_receiver_list = rbind(unique_receiver_list, receiver_df$receiver_data[which(receiver_df$receiver_data$station_name == unique(receiver_df$receiver_data$station_name)[i])[1], ])
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
    png(file.path(results_dir, paste('n_fish_by_lunar_phase colorbar scale - ', description, '.png', sep = "")), width = 430, height = 213)
      par(mfrow = c(1, 1))
      plot(rep(1, max(receiver_use_by_lunar_phase$n_sum_tags)) ~ c(1:max(receiver_use_by_lunar_phase$n_sum_tags)), col = plot_col_pal[1:max(receiver_use_by_lunar_phase$n_sum_tags)], pch = 15, cex = 5)
    dev.off()
  }
  
  individuals_by_lunar_phase(description = 'all tags', receiver_df = p2s0)
  individuals_by_lunar_phase(description = 'valid tags', receiver_df = p2s1)
  individuals_by_lunar_phase(description = 'valid and questionable tags', receiver_df = p2s2)
  
  
  # If not enough data, then batch receivers into zones
 
  
  # Is the depth of the fish changing during the moon phase.  Same 4 moon quadrants.  What was the depth of the receiver?  Problematic, because receivers on a steep slope are listening over a great depth range .... 
  
  
  
  #### Proportion of fish detected - Day Night Dusk Dawn
  
  
  ### How many fish tagged in FY 17, in 18 so far
  n_tagged_fy17 = length(unique(tagging_data$vem_tag_id[tagging_data$datetime >= as.POSIXct('2016-07-01') & tagging_data$datetime < as.POSIXct('2017-07-01') & tagging_data$species == "Opakapaka" & !is.na(tagging_data$vem_tag_id)]))
  n_tagged_fy17_in_data = length(unique(tagging_data$vem_tag_id[tagging_data$datetime >= as.POSIXct('2016-07-01') & tagging_data$datetime < as.POSIXct('2017-06-12') & tagging_data$species == "Opakapaka" & !is.na(tagging_data$vem_tag_id)]))
  
  n_tagged_fy18 = length(tagging_data$vem_tag_id[tagging_data$datetime >= as.POSIXct('2017-07-01') & tagging_data$datetime < as.POSIXct('2018-07-01') & tagging_data$species == "Opakapaka" & !is.na(tagging_data$vem_tag_id)])
  
  ### Have we gotten better at keeping fish alive?
  merge(x = p2s0$summary_df, y = p2s0$track_status$status_df, by.x = p2s0$summary_df$tag_id , by.y = p2s0$track_status$status_df$tag_id)
  percent_alive_fy17 = length(which(p2s0$summary_df$status == 'Valid' & p2s0$summary_df$tagging_date > start & p2s0$summary_df$tagging_date < as.POSIXct("2016-07-01"))) / length(which(p2s0$summary_df$tagging_date > start & p2s0$summary_df$tagging_date < as.POSIXct("2016-07-01")))
  percent_alive_fy16 = length(which(p2s0$summary_df$status == 'Valid' & p2s0$summary_df$tagging_date >= as.POSIXct("2016-07-01") & p2s0$summary_df$tagging_date < end)) / length(which(p2s0$summary_df$tagging_date >= as.POSIXct("2016-07-01") & p2s0$summary_df$tagging_date < end))

