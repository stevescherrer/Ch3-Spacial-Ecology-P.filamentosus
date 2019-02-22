#### Designing Acoustic Network for Inside BRFA E 

#### Written by: Stephen Scherrer
#### Written on: 14 October 2016

#### A Script to automate the placement of a network of Static Sensors (Vemco VR2-W Acoustic Receivers) off Makapuu, Oahu. Program Runs as follows

## i.   Set up the project workspace
## ii.  Installing Principle dependencies - SensorFenceR, and Acoustic Web App (Via wrapper_function.R)
## iii. User Selected Parameters
## iv.  Run instance of Sensor FenceR
## v.   Update user_sensors parameter for Acoustic Web App with results from SensorFenceR runs
## vi   Run Instance of Acoustic Web App 

##############################################################################################################

#### i. Setting up the project workspace -----
## Clearing workspace
rm(list = ls()) 
## Creating output file
project_dir = "/Users/stephenscherrer/Google Drive/Weng Lab/Personal_Folders/Steve/dissertation work/Acoustic Network Design/Web app for in BRFA E receivers/BRFA E Receivers/Spring 2017"
src_dir = paste(project_dir, '/src/', sep = "")
output_dir ="/Users/stephenscherrer/Desktop/AWA Runs For Makapuu Network/Spring 2017/results/PDF Rerun"
data_dir = paste(project_dir, '/data/', sep = "")

setwd(output_dir)

#### ii. Installing principle dependencies ----
# install.packages('knitr')
# library('knitr') # stitch()

## Source Acoustic Web App via wrapper Function
source('/Volumes/GoogleDrive/My Drive/Weng Lab/Personal_Folders/Steve/dissertation work/Acoustic Network Design/Acoustic Web App/wrapper_function.R')

## Source SensorFenceR program
source('/Volumes/GoogleDrive/My Drive/Weng Lab/Personal_Folders/Steve/dissertation work/Acoustic Network Design/Acoustic Fences/src/SensorFenceR.R')

## Source VemcoUtilityFunctions
source('/Volumes/GoogleDrive/My Drive/Weng Lab/Code_Library/R_code/vemcoUtilityFunctions.R')

## Importing current receiver position data ----
receiver_data = load_receiver_data('/Volumes/GoogleDrive/My Drive/Weng Lab/Data/Bottomfish/Oahu Receiver Data Files/DEPLOYMENT_RECOVERY_LOG.csv')

#### iii. User Selected Parameters ------
### General Parameters
n_receivers = 41  # Number of total receivers to place (As of 31 May 2017, there are 24 receivers currently deployed with 20 more on order and 1 set in lab. Will keep one as backup)
sensor_elevation = 6.1 # in meters from the benthos

n_place_round_1 = 6

### Acoustic Web App Parameters
bathymetry_map = "MHI_50m" # Bathymetry file/resolution
## Sensor Detection Range
sensor_detection_range = 847 # 847 m From range testing results for receiver at 3 m. See R script Range_test_june_july2014Analysis_2.R

## User Specified sensors positions to include (all currently placed sensors)
# Extracting just the sensors that are in the water following the last deployment (2016-12-05)
current_sensors = receiver_data[which(receiver_data$deployment_date >= as.POSIXct('2016-12-01') & receiver_data$recovery_date < as.POSIXct('2017-6-24') & !is.na(receiver_data$recovery_date)), ]
user_sensors = c()
for(i in 1:dim(current_sensors)[1]){
  user_sensors = c(user_sensors, c(as.character(current_sensors[i, c("lon", "lat")])))
}

### SensorFenceR Parameters
# Pointing program at bathy transects to place fences on
south_fence_outside_transect_data = "/Volumes/GoogleDrive/My Drive/Weng Lab/Personal_Folders/Steve/dissertation work/Acoustic Network Design/Web app for in BRFA E receivers/BRFA E Receivers/Spring 2017/Data/Outside South Fence Transect - 1000 m from BRFA E.txt"
north_fence_outside_transect_data = "/Volumes/GoogleDrive/My Drive/Weng Lab/Personal_Folders/Steve/dissertation work/Acoustic Network Design/Web app for in BRFA E receivers/BRFA E Receivers/Spring 2017/Data/Outside North Fence Transect - 1000 m from BRFA E.txt"
south_fence_inside_transect_data = "/Volumes/GoogleDrive/My Drive/Weng Lab/Personal_Folders/Steve/dissertation work/Acoustic Network Design/Web app for in BRFA E receivers/BRFA E Receivers/Spring 2017/Data/Inside South Fence Transect - 1000 m from BRFA E.txt"
north_fence_inside_transect_data = "/Volumes/GoogleDrive/My Drive/Weng Lab/Personal_Folders/Steve/dissertation work/Acoustic Network Design/Web app for in BRFA E receivers/BRFA E Receivers/Spring 2017/Data/Inside North Fence Transect - 1000 m from BRFA E.txt"

#### iv. Running Instance of SensorFenceR ------
south_fence_outside_results = run_sensorFenceR(TransectFileName = south_fence_outside_transect_data, 
                                               RunDescription = 'BRFA E South Fence',
                                               UpperDepthLimit = -75,  # in meters
                                               LowerDepthLimit = -450,  # in meters
                                               HeightOfReceiverOffBottom = sensor_elevation,  # in meters
                                               Receiver12Percent = 600,  # Radius of receiver detection sphere where 12.5 percent of pings are picked up from range testing data
                                               Receiver25Percent = 500,  # Radius of receiver detection sphere where 25 percent of pings are picked up from range test data
                                               NumberOfReceiversToPlace = NULL,
                                               HeightOfFenceFromBottom = 150,  # Distance (m) from the benthos that a detection sphere must encompass. Leave NULL if not specified. 
                                               SwimSpeedOfStudySpecies = .5,  # In meters / second)
                                               plot_results = FALSE
)   

north_fence_outside_results = run_sensorFenceR(TransectFileName = north_fence_outside_transect_data, 
                                               RunDescription = 'BRFA E South Fence',
                                               UpperDepthLimit = -75,  # in meters
                                               LowerDepthLimit = -450,  # in meters
                                               HeightOfReceiverOffBottom = sensor_elevation,  # in meters
                                               Receiver12Percent = 600,  # Radius of receiver detection sphere where 12.5 percent of pings are picked up from range testing data
                                               Receiver25Percent = 500,  # Radius of receiver detection sphere where 25 percent of pings are picked up from range test data
                                               NumberOfReceiversToPlace = NULL,
                                               HeightOfFenceFromBottom = 150,  # Distance (m) from the benthos that a detection sphere must encompass. Leave NULL if not specified. 
                                               SwimSpeedOfStudySpecies = .5,
                                               plot_results = FALSE  # In meters / second)
)

south_fence_inside_results = run_sensorFenceR(TransectFileName = south_fence_inside_transect_data, 
                                              RunDescription = 'BRFA E South Fence',
                                              UpperDepthLimit = -75,  # in meters
                                              LowerDepthLimit = -450,  # in meters
                                              HeightOfReceiverOffBottom = sensor_elevation,  # in meters
                                              Receiver12Percent = 600,  # Radius of receiver detection sphere where 12.5 percent of pings are picked up from range testing data
                                              Receiver25Percent = 500,  # Radius of receiver detection sphere where 25 percent of pings are picked up from range test data
                                              NumberOfReceiversToPlace = NULL,
                                              HeightOfFenceFromBottom = 150,  # Distance (m) from the benthos that a detection sphere must encompass. Leave NULL if not specified. 
                                              SwimSpeedOfStudySpecies = .5, # In meters / second)
                                              plot_results = FALSE 
)

north_fence_inside_results = run_sensorFenceR(TransectFileName = north_fence_inside_transect_data, 
                                              RunDescription = 'BRFA E South Fence',
                                              UpperDepthLimit = -75,  # in meters
                                              LowerDepthLimit = -450,  # in meters
                                              HeightOfReceiverOffBottom = sensor_elevation,  # in meters
                                              Receiver12Percent = 600,  # Radius of receiver detection sphere where 12.5 percent of pings are picked up from range testing data
                                              Receiver25Percent = 500,  # Radius of receiver detection sphere where 25 percent of pings are picked up from range test data
                                              NumberOfReceiversToPlace = NULL,
                                              HeightOfFenceFromBottom = 150,  # Distance (m) from the benthos that a detection sphere must encompass. Leave NULL if not specified. 
                                              SwimSpeedOfStudySpecies = .5,  # In meters / second)
                                              plot_results = FALSE
)
#### Combining North and South fence results together
## We are not moving North and South outside fences so I haven't included them here.
fence_results = rbind(north_fence_inside_results, south_fence_inside_results) #, north_fence_outside_results, south_fence_outside_results)

#### v. Update user_sensors parameter for Acoustic Web App with results from SensorFenceR runs ----
for(i in 1:length(fence_results$receiverLon)){
  user_sensors = c(user_sensors, fence_results$receiverLon[i], fence_results$receiverLat[i])
}

#### vi. Running an instance of the Acoustic Web App ------
## Run an Instance of the Acoustic Web App with the following Paramaters:

my_run = run_web_app(bathymetry_dataset = bathymetry_map, # options are 'MHI_1km', 'MHI_50m', and 'Palmyra'
                     ### Bound by the boundaries of of BRFA E to east and west and the output of SensorFenceR to the north and south
                     northern_grid_boundary = 21.44640 + .001, # as decimal degrees 
                     southern_grid_boundary = 21.27367 - .001, # as decimal degrees
                     western_grid_boundary = -157.72, # as decimal degrees
                     eastern_grid_boundary = -157.50, # as decimal degrees
                     ### Fish Movement Model
                     fish_model_algorithm = 'rw', # options are rw = random walk or ou = Ornstein-Uhlenbeck
                     home_range_center_x = FALSE, # as decimal degrees. Required for OU fish model. 
                     home_range_center_y = FALSE, # as decimal degrees. Required for OU fish model
                     home_range_extent_x = FALSE, # in meters. Required for OU fish model. Standard deviation in meters of home range extent in the x direction. Controls home range width. A rule of thumb says that for each direction in isolation approximately 95% of the time is spent within plus minus two standard deviations. A non-negative real number.
                     home_range_extent_y = FALSE, # in meters. Required for OU fish model. Standard deviation in meters of home range extent in the y direction. Controls home range height. A rule of thumb says that for each direction in isolation approximately 95% of the time is spent within plus minus two standard deviations. A non-negative real number.
                     directional_correlation_of_home_range = FALSE, # Required for OU fish model. Correlation of spatial distribution between directions. Controls the tilt of the home range. This is useful if the home range shape is angled relative to the x,y coordinate system. The correlation must be between -1 and 1.
                     ### Fish Distribution
                     restrict_vertical_habitat_range = TRUE, # Options are TRUE or FALSE#### Habitat depths
                     ## Selected after consulting bottomfish distributions presented on pages 19 and 89 of Appendix 3. of 
                     ## Kelley, C.D., Moriwake, V.N., 2012. Appendix 3: essential fish habitat descriptions, Part 1: bottomfish. In: WPRFMC (ed) Final fishery management plan for coral reef ecosystems of the western Pacific region, volume III, Essential Fish Habitat for Management Unit Species, p 597.
                     ### These depths represent 90% the vertical distribution for all bottomfish encounters
                     min_depth = -100, # units are in negative meters. Required if restrict_vertical_habitat_range == TRUE. [Optional] Specifies the shallowest depth in meters a fish will visit. Valid values are non-positive real numbers. Specifying a value for mindepth requires specifying a value for maxdepth. 
                     max_depth = -150, # units are in negative meters. [Optional] Specifies the deepest depth in meters a fish will visit. Valid values are non-positive real numbers. Specifying a value for maxdepth requires specifying a value for mindepth.
                     ### Depth preference also based on Kelley & Moriwake 2012 which says 2-10 m. Pg 88 based on Haight 1989
                     use_depth_preference = TRUE, # Options are TRUE or FALSE
                     prefered_depth = 6, # [Optional] Depth preference of fish relative to bottom (in meters off the bottom). Valid values are non-negative real numbers. Specifying a value for depth_off_bottom requires specifying a value for depth_off_bottom_sd. 
                     sd_of_preferred_depth = 2, # Set to 2 by default# [Optional] Standard deviation of preferred depth (in meters). Animals spend 95% of their time within plus/minus 2 SD of their preferred depth.
                     ### List of already existing sensors, two that we dont want to move as tehy have been part of the study for some time and are in areas of critical habitat. Also results from sensorFenceR
                     user_specified_sensors = user_sensors, #  These sensors are placed and down-weighed before running the program. Useful for including already existing sensors, or forcing the placement of a few sensors. Do not include these sensors in the sensor count below. Provide sensor locations as a string in the format: <sensor1_Long>, <sensor1_Lat>, <sensor2_Long>, <sensor2_Lat>..." # If blank, leave as NULL
                     ### The total number of available receivers minus those used in the fences
                     number_of_sensors_to_use =  n_receivers - length(user_sensors)/2, # Specifies how many sensors the program should place. Valid values are non-negative integers. 
                     number_of_sensors_to_project = 0, # Specifies how many sensors the program should provide projections for. Valid values are non-negative integers. 
                     goodness_algorthm = 3, # options are 1, 2, 3. Specifies how you want the system to determine the "goodness" of a cell. Valid values are 1, 2, or 3.
                     # A value of '1' indicates that a "good" cell has a high number of animals within detection range (ignoring line of sight). This is useful for sensors not restricted to line-of-sight detection.
                     # A value of '2' indicates that a "good" cell has the best visibility (taking into account bathymetry and shadowing, but completely ignoring fish density). This is useful for networks restricted to line-of-sight detection and having no prior knowledge of animal habitat.
                     # A value of '3' indicates that a "good" cell has a high number of visible fish (incorporating both bathymetry and animal density). This is useful for networks restricted to line-of-sight detection, and having some idea of animal habitat.
                     sensor_elevation = sensor_elevation, # Specifies how far off the bottom a sensor should be placed in meters. Valid values are non-negative real numbers. 
                     sensor_shape_function = 'shape.gauss', # Determines which functional shape to represent horizontal acoustic attenuation in the water. The detection function specifies how the chance of signal detection declines as a function of distance to sensor. Ranging experiments should preferably be carried out locally at the study site to approximate this function. Currently, the only valid value is "shape.gauss". 
                     max_detection_value = .98, # The probability of detecting a fish located right next to the sensor. Specifies a maximum value for the shape function. Valid values should be a decimal in the range (0.05,1]. 
                     # From range testing results for receiver at 3 m. See R script Range_test_june_july2014Analysis_2.R
                     detection_range = sensor_detection_range, # in meters. The distance in meters from the sensor where the chance of detecting a signal is 0.05. Valid values are non-negative real numbers. 
                     supression_factor = 2, # Specifies the range of suppression in multiples of the detection range. For example, a suppression factor of 2 enforces a suppression range of two times the detection range. Cells within the suppression range of a sensor will be subject to the specified suppression function. To deter sensor overlap, a good value to use is twice the sensor detectionRange. Valid values are non-negative real numbers. 
                     supression_function = 'supression.scale', # Specifies which suppression function to use. Options are suppression.static, suppression.scale, detection.function, detection.function.shadow, or detection.function.exact. 
                     # suppression.static: Replaces all cells within range of a sensor with the value specified in minsuppressionValue.
                     # suppression.scale: Multiplies the values of cells within range of a sensor by a scaling factor according to the cell's distance from the sensor. Nearby cells receive a higher scaling factor, and lower cells receive a lower scaling factor. The scaling factor is linearly related to the distance between the sensor and cell. Selecting this option requires specifyign values for minsuppressionValue and maxsuppressionValue
                     # detection.function: Uses the inverse of the detection function to down scale goodness of grid cells near the sensor, but does not take objects that block signal into account. Grid cells' goodness will increase as a function of distance to the sensor.
                     # detection.function.shadow: Uses the inverse of the shapeFcn to down scale goodness of grid cells near the sensor, and does take objects that block signal into account. This means that sensors on opposite sides of a blocking wall will not affect each other's goodness. Unblocked grid cells' goodness will increase as a function of distance to the sensor.
                     # detection.function.exact: 
                     # The above suppression functions do not recalculate the goodness grid after placing a new sensor and therefore only provide an approximately optimal solution. This suppression function does recalculate the goodness iteratively, and is therefore slower by a factor equal to the number of sensors.
                     max_supression_value = 1, # Specifies the minimum scaling factor to apply. Cells at the edge of the suppression Range will receive this as a scaling factor. Valid values are non negative real numbers in the range [0,1] 
                     min_supression_value = .5, # Specifies the maximum scaling factor to apply. Cells directly adjacent to a sensor will receive this as a scaling factor. Valid values are non negative real numbers in the range [0,1] 
                     input_file = FALSE,  # Currently false because file is selected using bathymetry_dataset argument. # Specifies the file location of the Bathy File to use. For arcGIS filetypes, this is a path to the folder containing the data files. For netCDF filetypes, this is a path to the ncdf file itself. )
                     input_file_type = FALSE)

second_run_user_sensors = c()
for(i in 1:length(my_run$params$sensorLatLon$lat)){
  second_run_user_sensors = c(second_run_user_sensors, c(my_run$params$sensorLatLon$lon[i], my_run$params$sensorLatLon$lat[i]))
}


my_run = run_web_app(bathymetry_dataset = bathymetry_map, # options are 'MHI_1km', 'MHI_50m', and 'Palmyra'
                     ### Bound by the boundaries of of BRFA E to east and west and the output of SensorFenceR to the north and south
                     northern_grid_boundary = 21.44640 + .001, # as decimal degrees 
                     southern_grid_boundary = 21.27367 - .001, # as decimal degrees
                     western_grid_boundary = -157.72, # as decimal degrees
                     eastern_grid_boundary = -157.50, # as decimal degrees
                     ### Fish Movement Model
                     fish_model_algorithm = 'rw', # options are rw = random walk or ou = Ornstein-Uhlenbeck
                     home_range_center_x = FALSE, # as decimal degrees. Required for OU fish model. 
                     home_range_center_y = FALSE, # as decimal degrees. Required for OU fish model
                     home_range_extent_x = FALSE, # in meters. Required for OU fish model. Standard deviation in meters of home range extent in the x direction. Controls home range width. A rule of thumb says that for each direction in isolation approximately 95% of the time is spent within plus minus two standard deviations. A non-negative real number.
                     home_range_extent_y = FALSE, # in meters. Required for OU fish model. Standard deviation in meters of home range extent in the y direction. Controls home range height. A rule of thumb says that for each direction in isolation approximately 95% of the time is spent within plus minus two standard deviations. A non-negative real number.
                     directional_correlation_of_home_range = FALSE, # Required for OU fish model. Correlation of spatial distribution between directions. Controls the tilt of the home range. This is useful if the home range shape is angled relative to the x,y coordinate system. The correlation must be between -1 and 1.
                     ### Fish Distribution
                     restrict_vertical_habitat_range = TRUE, # Options are TRUE or FALSE#### Habitat depths
                     ## Selected after consulting bottomfish distributions presented on pages 19 and 89 of Appendix 3. of 
                     ## Kelley, C.D., Moriwake, V.N., 2012. Appendix 3: essential fish habitat descriptions, Part 1: bottomfish. In: WPRFMC (ed) Final fishery management plan for coral reef ecosystems of the western Pacific region, volume III, Essential Fish Habitat for Management Unit Species, p 597.
                     ### These depths represent 90% the vertical distribution for all bottomfish encounters
                     min_depth = -75, # units are in negative meters. Required if restrict_vertical_habitat_range == TRUE. [Optional] Specifies the shallowest depth in meters a fish will visit. Valid values are non-positive real numbers. Specifying a value for mindepth requires specifying a value for maxdepth. 
                     max_depth = -450, # units are in negative meters. [Optional] Specifies the deepest depth in meters a fish will visit. Valid values are non-positive real numbers. Specifying a value for maxdepth requires specifying a value for mindepth.
                     ### Depth preference also based on Kelley & Moriwake 2012 which says 2-10 m. Pg 88 based on Haight 1989
                     use_depth_preference = TRUE, # Options are TRUE or FALSE
                     prefered_depth = 6, # [Optional] Depth preference of fish relative to bottom (in meters off the bottom). Valid values are non-negative real numbers. Specifying a value for depth_off_bottom requires specifying a value for depth_off_bottom_sd. 
                     sd_of_preferred_depth = 2, # Set to 2 by default# [Optional] Standard deviation of preferred depth (in meters). Animals spend 95% of their time within plus/minus 2 SD of their preferred depth.
                     ### List of already existing sensors, two that we dont want to move as tehy have been part of the study for some time and are in areas of critical habitat. Also results from sensorFenceR
                     user_specified_sensors = second_run_user_sensors, #  These sensors are placed and down-weighed before running the program. Useful for including already existing sensors, or forcing the placement of a few sensors. Do not include these sensors in the sensor count below. Provide sensor locations as a string in the format: <sensor1_Long>, <sensor1_Lat>, <sensor2_Long>, <sensor2_Lat>..." # If blank, leave as NULL
                     ### The total number of available receivers minus those used in the fences
                     number_of_sensors_to_use = n_receivers - (length(user_sensors)/2 + n_place_round_1), # Specifies how many sensors the program should place. Valid values are non-negative integers. 
                     number_of_sensors_to_project = 0, # Specifies how many sensors the program should provide projections for. Valid values are non-negative integers. 
                     goodness_algorthm = 3, # options are 1, 2, 3. Specifies how you want the system to determine the "goodness" of a cell. Valid values are 1, 2, or 3.
                     # A value of '1' indicates that a "good" cell has a high number of animals within detection range (ignoring line of sight). This is useful for sensors not restricted to line-of-sight detection.
                     # A value of '2' indicates that a "good" cell has the best visibility (taking into account bathymetry and shadowing, but completely ignoring fish density). This is useful for networks restricted to line-of-sight detection and having no prior knowledge of animal habitat.
                     # A value of '3' indicates that a "good" cell has a high number of visible fish (incorporating both bathymetry and animal density). This is useful for networks restricted to line-of-sight detection, and having some idea of animal habitat.
                     sensor_elevation = sensor_elevation, # Specifies how far off the bottom a sensor should be placed in meters. Valid values are non-negative real numbers. 
                     sensor_shape_function = 'shape.gauss', # Determines which functional shape to represent horizontal acoustic attenuation in the water. The detection function specifies how the chance of signal detection declines as a function of distance to sensor. Ranging experiments should preferably be carried out locally at the study site to approximate this function. Currently, the only valid value is "shape.gauss". 
                     max_detection_value = .98, # The probability of detecting a fish located right next to the sensor. Specifies a maximum value for the shape function. Valid values should be a decimal in the range (0.05,1]. 
                     # From range testing results for receiver at 3 m. See R script Range_test_june_july2014Analysis_2.R
                     detection_range = sensor_detection_range, # in meters. The distance in meters from the sensor where the chance of detecting a signal is 0.05. Valid values are non-negative real numbers. 
                     supression_factor = 2, # Specifies the range of suppression in multiples of the detection range. For example, a suppression factor of 2 enforces a suppression range of two times the detection range. Cells within the suppression range of a sensor will be subject to the specified suppression function. To deter sensor overlap, a good value to use is twice the sensor detectionRange. Valid values are non-negative real numbers. 
                     supression_function = 'supression.scale', # Specifies which suppression function to use. Options are suppression.static, suppression.scale, detection.function, detection.function.shadow, or detection.function.exact. 
                     # suppression.static: Replaces all cells within range of a sensor with the value specified in minsuppressionValue.
                     # suppression.scale: Multiplies the values of cells within range of a sensor by a scaling factor according to the cell's distance from the sensor. Nearby cells receive a higher scaling factor, and lower cells receive a lower scaling factor. The scaling factor is linearly related to the distance between the sensor and cell. Selecting this option requires specifyign values for minsuppressionValue and maxsuppressionValue
                     # detection.function: Uses the inverse of the detection function to down scale goodness of grid cells near the sensor, but does not take objects that block signal into account. Grid cells' goodness will increase as a function of distance to the sensor.
                     # detection.function.shadow: Uses the inverse of the shapeFcn to down scale goodness of grid cells near the sensor, and does take objects that block signal into account. This means that sensors on opposite sides of a blocking wall will not affect each other's goodness. Unblocked grid cells' goodness will increase as a function of distance to the sensor.
                     # detection.function.exact: 
                     # The above suppression functions do not recalculate the goodness grid after placing a new sensor and therefore only provide an approximately optimal solution. This suppression function does recalculate the goodness iteratively, and is therefore slower by a factor equal to the number of sensors.
                     max_supression_value = 1, # Specifies the minimum scaling factor to apply. Cells at the edge of the suppression Range will receive this as a scaling factor. Valid values are non negative real numbers in the range [0,1] 
                     min_supression_value = .5, # Specifies the maximum scaling factor to apply. Cells directly adjacent to a sensor will receive this as a scaling factor. Valid values are non negative real numbers in the range [0,1] 
                     input_file = FALSE,  # Currently false because file is selected using bathymetry_dataset argument. # Specifies the file location of the Bathy File to use. For arcGIS filetypes, this is a path to the folder containing the data files. For netCDF filetypes, this is a path to the ncdf file itself. )
                     input_file_type = FALSE)




#### Writing out locations to deploy receivers
setwd(output_dir)
write.csv(my_run$params$sensorLatLon, 'receiver_lat_lons.csv')

#### Creating a matching plot for where receivers are placed
bathymetry = getNOAA.bathy(lon1 = -157.8, 
                           lon2 = -157.5, 
                           lat1 = 21.2, 
                           lat2 = 21.5,
                           resolution = .01)
png('Deployment Map.png')
plot.bathy(bathymetry, bpal = FALSE, land = TRUE, image=TRUE, deepest.isobath = c(-10000), shallowest.isobath = c(0), step = c(100), drawlabels = TRUE)
text(lat~lon, labels = my_run$params$sensorLatLon$sensor_number,data = my_run$param$sensorLatLon, pch = 19, col = 'black', cex = .5)
dev.off()

save.image('receiver array workspace.R')
# stitch(paste(src_dir, "running_web_app_brfa_e_network.R", sep = ""))


#### Printing out a pdf of the basemap in white for use in black presentations
pdf('greyscale_basemap.pdf', height = 6, width = 9)
  contour(my_run$topographyGrid$x, my_run$topographyGrid$y, my_run$topographyGrid$topographyGrid, add = FALSE, col = 'grey')
dev.off()
