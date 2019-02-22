
# Written 14 January 2015 by Stephen Scherrer

#### Clearning Workspace and setting directory ---------
rm(list=ls()) # Clear workspace
setwd('/Users/stephenscherrer/Documents/Work/UH/Projects/dissertation work/Spacial Ecology/Bottomfish Analysis Feb 2015/Output Files')

# Import any principle dependencies----------
# install.packages('wesanderson') # color palett for plotting
# install.packages('matlab')
# install.packages('maps')
# install.packages('mapdata')
# install.packages('maptools')
# install.packages('scales')
# install.packages('ggmap')
library("matlab", lib.loc)
library('maps')
library('mapdata')
library('maptools')  # for shapefiles
library('scales')  # for transparency
library('ggmap')
source('/Users/stephenscherrer/Documents/Programming/R/utility_functions.R')
library('reshape') # merge_all
library('marmap') # 
source('/Users/stephenscherrer/Documents/Work/UH/Projects/dissertation work/Spacial Ecology/Bottomfish Analysis Feb 2015/R Code/bf_analysis_functions.R')
library('plotly')
library('doParallel')

#### USAGE -------------------------------------


### Importing Data -----------------------------

## Importing receiver data

receiver_data = load_receiver(filename = 'DEPLOYMENT_RECOVERY_LOG.csv', filepath = '/Users/stephenscherrer/Dropbox/Lab Folder/Oahu Receiver Data Files/')

## Import tagging log
tagging_data = load_tagging_data('/Users/stephenscherrer/Dropbox/Lab Folder/Oahu Receiver Data Files/Bottomfish_Tag_Master.csv')

## Importing VUE Data
vue_data = load_vemco('/Users/stephenscherrer/Dropbox/Lab Folder/Oahu Receiver Data Files/VUE_Export_2015-June-3.csv')

### Cleaning data


## Fixing missing Lat and Lons

time_parallel = proc.time() # Timing process for shits and gigles
parallel_vue_data = data.frame() # Repository for Vue Data
registerDoParallel(cores = 8) # Sets up number of cores
parallel_vue_data = foreach(i = c(1:length(vue_data$datetime)),.combine = rbind) %dopar% {
  cleaned_latlon = clean_vue_lat_lon(vue_data[i, ], receiver_data)
  return(cleaned_latlon)
}
proc.time() - time_parallel # Stops timer

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
  opakapaka_tag_ids = na.exclude(tagging_data$vem_tag_id[tagging_data$species == 'Opakapaka']) # All Tag IDs associated with opakapaka
  opakapaka_tag_ids = unique(as.numeric(levels(opakapaka_tag_ids))[opakapaka_tag_ids])
  opakapaka_tag_ids = clean_tags(opakapaka_tag_ids, vue_data[vue_data$station != 'Tagging Location', ])

  ehu_tag_ids = na.exclude(tagging_data$vem_tag_id[tagging_data$species == 'Ehu'])
  ehu_tag_ids = unique(as.numeric(levels(ehu_tag_ids))[ehu_tag_ids])
  ehu_tag_ids = clean_tags(ehu_tag_ids, vue_data[vue_data$station != 'Tagging Location', ])

  gindai_tag_ids = na.exclude(tagging_data$vem_tag_id[tagging_data$species == 'Gindai'])
  gindai_tag_ids = unique(as.numeric(levels(gindai_tag_ids))[gindai_tag_ids])
  gindai_tag_ids = clean_tags(gindai_tag_ids, vue_data[vue_data$station != 'Tagging Location', ])

  kalekale_tag_ids = na.exclude(tagging_data$vem_tag_id[tagging_data$species == 'Kalekale'])
  kalekale_tag_ids = unique(as.numeric(levels(kalekale_tag_ids))[kalekale_tag_ids])
  kalekale_tag_ids = clean_tags(kalekale_tag_ids, vue_data[vue_data$station != 'Tagging Location', ])

  all_bottomfish_tags = c(opakapaka_tag_ids, ehu_tag_ids, gindai_tag_ids, kalekale_tag_ids)
# Removing data from unwanted tags
  vue_data = clean_vue(vue_data, opakapaka_tag_ids)
  dead_fish = clean_vue(vue_data, c(37969, 57459))
  vue_data = clean_vue(vue_data, c(57451, 37954, 37969, 57459), exclude = TRUE) # first 2 tags are at cross seamount. cannot find tags in hard copy logs. probably monchong tagged with BF tags. Third and fourth tags are very dead fish right next to a recevier
  opakapaka_tag_ids = opakapaka_tag_ids[!(opakapaka_tag_ids %in% c(57451, 37954, 37969, 57459))]

# Removing botcam detections for which I don't have time to look up lon/lats
  vue_data = remove_location(vue_data, 'With BotCam Crew')

# Add column to vue_data data frame for what day of study a detection occurred
  vue_data = adjust_vue_study_dates(vue_data, tagging_data, opakapaka_tag_ids)
  dead_fish = adjust_vue_study_dates(dead_fish, tagging_data, opakapaka_tag_ids)

# Add column to tagging_data frame for what day of study tagging occurred
  tagging_data = adjust_tagging_study_dates(vue_data, tagging_data, opakapaka_tag_ids)

# add columns to receiver_data frame for what day of study deployments and recoveries occured
  receiver_data = adjust_receiver_study_dates(receiver_data, tagging_data)
  write.csv(receiver_data, file = "receiver_data_with_study_dates.csv")
# Determining first, last and elapsed number of days in study
  exp_dates = experiment_dates(vue_data)
  print(exp_dates)

# pulling first detection, last detection, and time at liberty for each tag
  all_transmission_stats = transmission_stats(vue_data, tagging_data)
  print(all_transmission_stats)

# pulling out tags with > 7 days detection history
  tags_of_interest = all_transmission_stats$tag_id[all_transmission_stats$time_at_liberty>7]
  ## Subsets the following tags: 37975 57455 57457 57458 57464 37980 37960 37961 37940 37950 37952 37958
  toi_transmission_stats = transmission_stats(vue_data, tagging_data, tags_of_interest)
  print(toi_transmission_stats)
  toi_unique_days = unique_days_detected(vue_data, tags_of_interest)

# paring that downt ot tags with >= 10 days unique data response
  toi_unique_days = unique_days_detected(vue_data, tags_of_interest)
  tags_of_interest = toi_unique_days$tag_id[toi_unique_days$unique_days_detected >= 10]
  ## Subsets the following tags: 57455 57457 57458 37960 37940
  toi_transmission_stats = transmission_stats(vue_data, tagging_data, tags_of_interest)
  print(toi_transmission_stats)

# getting meta data associated with transmitters
  all_meta_data = get_tagging_metadata(tagging_data)
  toi_meta_data = get_tagging_metadata(tagging_data, tags_of_interest)

# getting tagging dates
  all_tagging_date = tagging_date(tagging_data, unique(as.character(vue_data$tag_id)))
  toi_tagging_date = tagging_date(tagging_data, tags_of_interest)

# number of detections and percentage of total detections for each individual
  all_number_of_detections = number_of_detections(vue_data)
  print(all_number_of_detections)

  toi_number_of_detections = number_of_detections(vue_data, tags_of_interest)
  print(toi_number_of_detections)

# number of detections/day for each individual
  #### IS THIS ALL DAYS OF STUDY OR ALL DAYS AT LIBERTY?
  all_detections_per_day = detections_per_day(vue_data, tagging_data)
  print(all_detections_per_day)

  toi_detections_per_day = detections_per_day(vue_data, tagging_data, tags_of_interest)
  print(toi_detections_per_day)

# distance traveled by individual
  all_distance_traveled = distance_traveled(vue_data)
  print(all_distance_traveled)

  toi_distance_traveled = distance_traveled(vue_data, tags_of_interest)
  print(toi_distance_traveled)

# distance/day at liberty
  all_distance_per_day = distance_per_day(vue_data, tagging_data)
  print(all_distance_per_day)

  toi_distance_per_day = distance_per_day(vue_data, tagging_data, tags_of_interest)
  print(toi_distance_per_day)

# All/Unique stations detected per individual
  all_stations_detected(vue_data)
  unique_stations_detected(vue_data, opakapaka_tag_ids)

  all_stations_detected(vue_data, tags_of_interest)
  unique_stations_detected(vue_data, tags_of_interest)

# number of movements
  all_movements = number_of_movements(vue_data)
  print(all_movements)

  toi_movements = number_of_movements(vue_data, tags_of_interest)
  print(toi_movements)

#number of movements / day at liberty
  all_movements_per_day = number_of_movements_per_day(vue_data, tagging_data)
  print(all_movements_per_day)

  toi_movements_per_day = number_of_movements_per_day(vue_data, tagging_data, tags_of_interest)
  print(toi_movements_per_day)

# number of BRFA crossings and crossings/day
all_brfa_crossings = brfa_crossings(vue_data, tagging_data)
print (all_brfa_crossings)

toi_brfa_crossings = brfa_crossings(vue_data, tagging_data, tags_of_interest)
print(toi_brfa_crossings)


# number of receivers visited
all_receivers_detected = unique_receivers(vue_data, tagging_data)
print(all_receivers_detected)

toi_receivers_detected = unique_receivers(vue_data, tagging_data, tags_of_interest)
print(toi_receivers_detected)


# Spatial Evenness
toi_spatial_evenness = spatial_evenness(vue_data = vue_data, 
                                        reciever_data = plot_receivers_phase_1, 
                                        bottomfish_tag_ids = tags_of_interest)
spatial_evenness_for_plot = toi_spatial_evenness$spatial_evenness_metric
names(spatial_evenness_for_plot) = as.character(toi_spatial_evenness$tag_id)
## Plotting spatial evenness
barplot(toi_spatial_evenness$spatial_evenness_metric, 
        xlab = 'Tagged Fish',
        ylab = 'Spatial Evenness (0-1)',
        ylim = c(0,1),
        axisnames = TRUE,
        main = 'Spatial Evenness',
        names.arg = names(spatial_evenness_for_plot))
text(.58, .1, as.character(round(spatial_evenness_for_plot[1], (5)))
text(1.9, .1, round(spatial_evenness_for_plot[2], 5))
text(3.1, .1, round(spatial_evenness_for_plot[3], 5))
text(4.3, .1, round(spatial_evenness_for_plot[4], 5))
text(5.5, .1, round(spatial_evenness_for_plot[5], 5))

### Combining all data:
all_data_out = cbind(all_meta_data,
                     all_transmission_stats, 
                     all_tagging_date[ ,-1],
                     all_number_of_detections[ ,-1],
                     all_detections_per_day[ ,-1],
                     all_unique_days[ ,-1],
                     all_movements[ ,-1],
                     all_movements_per_day[ ,-1],
                     all_brfa_crossings[ ,-1],
                     all_distance_traveled[ ,-1],
                     all_distance_per_day[ ,-1],
                     all_receivers_detected[ ,-1]
                     )

toi_data_output = cbind(toi_meta_data,
                        toi_transmission_stats, 
                        toi_tagging_date[ ,-1],
                        toi_number_of_detections[ ,-1],
                        toi_detections_per_day[ ,-1],
                        toi_unique_days[ ,-1],
                        toi_movements[ ,-1],
                        toi_movements_per_day[ ,-1],
                        toi_brfa_crossings[ ,-1],
                        toi_distance_traveled[ ,-1],
                        toi_distance_per_day[ ,-1],
                        toi_receivers_detected[ ,-1]
)

                 
sink("bottomfish_analysis_output.txt")
for(i in 1:length(all_data_out$tag_id)){
  cat(paste("Transmitter ID: ", all_data_out$tag_id[i], "\n"))
  cat(paste("Species: ", all_data_out$species[i], "\n"))
  cat(paste("Fork Length: ", all_data_out$fork_length[i], " cm", "\n"))
  cat(paste("Tagging Date: ", all_data_out$tagging_date[i], "\n"))
  cat(paste("Tagging Location: ", all_data_out$tagging_location[i], "\n"))  
  cat(paste("Day of First Detected: ", all_data_out$first_transmission[i], "\n"))
  cat(paste("Day of Last Detection: ", all_data_out$last_transmission[i], "\n"))
  cat(paste("Days at Liberty: ", all_data_out$time_at_liberty[i], "\n"))
  cat(paste("Unique Days Detected: ", all_data_out$"all_unique_days[, -1]"[i], "\n"))
  cat(paste("Number of Detections (Total): ", all_data_out$"#_of_detections"[i], "\n"))
  cat(paste("Number of Detections / Day: ", all_data_out$"all_detections_per_day[, -1]"[i], "\n"))
  cat(paste("Number of Receivers Detected (Total): ", all_data_out$"receivers_detected"[i], "\n"))
  cat(paste("Number of receivers Detected / Day: ", all_data_out$receivers_detected_per_day[i], "\n"))
  cat(paste("Movements Into/Out of BRFA (Total): ", all_data_out$BRFA_crossings[i], "\n"))
  cat(paste("Movements Into/Out of BRFA / Day: ", all_data_out$BRFA_crossings_per_day[i], "\n"))
  cat(paste("Approximate Distance Tracked (Total): ", all_data_out$all_distance_traveled[i], "km","\n"))
  cat(paste("Approximate Distance Tracked / Day: ", all_data_out$all_distance_per_day[i],"km", "\n")) 
  cat(paste(all_stations_detected(vue_data, all_data_out$tag_id[i]), "\n"))
  cat("\n")
}
sink()

write.csv(all_data_out, file = 'bottomfish_summary.csv')

plot.new()
pdf('Number of Detections by Tag Boxplot.pdf')
par(mfrow = c(1,1), oma=c(0,0,2,0))  
median = signif(fivenum(toi_data_output$"#_of_detections")[3], 3)
first_q = signif(fivenum(toi_data_output$"#_of_detections")[2], 3)
third_q = signif(fivenum(toi_data_output$"#_of_detections")[4], 3)
boxplot(toi_data_output$"#_of_detections", main = 'Number of Detections by Tag', ylab = 'Detections')
legend('topright', cex = .75, legend = c(sprintf('1st Quartile: %s', first_q), sprintf('Median: %s', median), sprintf('3rd Quartile: %s', third_q)),
        col = 'black')
dev.off()

plot.new()
pdf('Percentage of All Detections By Tag Boxplot.pdf')
median = signif(fivenum(toi_data_output$"%_of_all_detections" )[3], 3)
first_q = signif(fivenum(toi_data_output$"%_of_all_detections" )[2], 3)
third_q = signif(fivenum(toi_data_output$"%_of_all_detections" )[4], 3)
boxplot(toi_data_output$"%_of_all_detections" , main = 'Percentage of All Detections by Tag', ylab = '%')
legend('topright', cex = .75, legend = c(sprintf('1st Quartile: %s', first_q), sprintf('Median: %s', median), sprintf('3rd Quartile: %s', third_q)),
       col = 'black')
dev.off()

plot.new()
pdf('Mean Detections Per Day Boxplot.pdf')
median = signif(fivenum(toi_data_output$"toi_detections_per_day[, -1]")[3], 3)
first_q = signif(fivenum(toi_data_output$"toi_detections_per_day[, -1]")[2], 3)
third_q = signif(fivenum(toi_data_output$"toi_detections_per_day[, -1]")[4], 3)
boxplot(toi_data_output$"toi_detections_per_day[, -1]", main = 'Mean Detections By Day', ylab = '# Detections/Day')
legend('topright', cex = .75, legend = c(sprintf('1st Quartile: %s', first_q), sprintf('Median: %s', median), sprintf('3rd Quartile: %s', third_q)),
       col = 'black')
dev.off()

plot.new()
pdf('Unique Days Detected Boxplot.pdf')
median = signif(fivenum(toi_data_output$"toi_unique_days[, -1]")[3], 3)
first_q = signif(fivenum(toi_data_output$"toi_unique_days[, -1]")[2], 3)
third_q = signif(fivenum(toi_data_output$"toi_unique_days[, -1]")[4], 3)
boxplot(toi_data_output$"toi_unique_days[, -1]" , main = 'Unique Days Detected', ylab = 'Days')
legend('topright', cex = .75, legend = c(sprintf('1st Quartile: %s', first_q), sprintf('Median: %s', median), sprintf('3rd Quartile: %s', third_q)),
       col = 'black')
dev.off()

plot.new()
pdf('Total Distance Tracked Boxplot.pdf')
median = signif(fivenum(toi_data_output$"toi_distance_traveled[, -1]")[3], 3)
first_q = signif(fivenum(toi_data_output$"toi_distance_traveled[, -1]")[2], 3)
third_q = signif(fivenum(toi_data_output$"toi_distance_traveled[, -1]")[4], 3)
boxplot(toi_data_output$"toi_distance_traveled[, -1]", main = 'Total Distance Tracked', ylab = 'km')
legend('topright', cex = .75, legend = c(sprintf('1st Quartile: %s', first_q), sprintf('Median: %s', median), sprintf('3rd Quartile: %s', third_q)),
       col = 'black')
dev.off()

plot.new()
pdf('Distance Tracked Per Day Boxplot.pdf')
median = signif(fivenum(toi_data_output$"toi_distance_per_day[, -1]")[3], 3)
first_q = signif(fivenum(toi_data_output$"toi_distance_per_day[, -1]")[2], 3)
third_q = signif(fivenum(toi_data_output$"toi_distance_per_day[, -1]")[4], 3)
boxplot(toi_data_output$"toi_distance_per_day[, -1]", main = 'Distance Tracked/Day', ylab = 'km/Day')
legend('topright', cex = .75, legend = c(sprintf('1st Quartile: %s', first_q), sprintf('Median: %s', median), sprintf('3rd Quartile: %s', third_q)),
       col = 'black')
dev.off()

plot.new()
pdf('Number of Unique Receivers Visited Boxplot.pdf')
median = signif(fivenum(toi_data_output$receivers_detected)[3], 3)
first_q = signif(fivenum(toi_data_output$receivers_detected)[2], 3)
third_q = signif(fivenum(toi_data_output$receivers_detected)[4], 3)
boxplot(toi_data_output$receivers_detected, main = 'Receivers Visited', ylab = '# Receivers')
legend('topright', cex = .75, legend = c(sprintf('1st Quartile: %s', first_q), sprintf('Median: %s', median), sprintf('3rd Quartile: %s', third_q)),
       col = 'black')
dev.off()

plot.new()
pdf('Movements Between Receivers By Day Boxplot.pdf')
median = signif(fivenum(toi_data_output$"toi_movements_per_day[, -1]")[3], 3)
first_q = signif(fivenum(toi_data_output$"toi_movements_per_day[, -1]")[2], 3)
third_q = signif(fivenum(toi_data_output$"toi_movements_per_day[, -1]")[4], 3)
boxplot(toi_data_output$"toi_movements_per_day[, -1]", main = 'Movements Between Receivers/Day', ylab = 'Movements/Day')
legend('topright', cex = .75, legend = c(sprintf('1st Quartile: %s', first_q), sprintf('Median: %s', median), sprintf('3rd Quartile: %s', third_q)),
       col = 'black')
dev.off()

plot.new()
pdf('Total BRFA Crossings Boxplot.pdf')
median = signif(fivenum(toi_data_output$BRFA_crossings)[3], 3)
first_q = signif(fivenum(toi_data_output$BRFA_crossings)[2], 3)
third_q = signif(fivenum(toi_data_output$BRFA_crossings)[4], 3)
boxplot(toi_data_output$BRFA_crossings, main = 'Total BRFA Crossings', ylab = 'BRFA Crossings')
legend('topright', cex = .75, legend = c(sprintf('1st Quartile: %s', first_q), sprintf('Median: %s', median), sprintf('3rd Quartile: %s', third_q)),
       col = 'black')
dev.off()

plot.new()
pdf('BRFA Crossings Per Day Boxplot.pdf')
median = signif(fivenum(toi_data_output$BRFA_crossings_per_day)[3], 3)
first_q = signif(fivenum(toi_data_output$BRFA_crossings_per_day)[2], 3)
third_q = signif(fivenum(toi_data_output$BRFA_crossings_per_day)[4], 3)
boxplot(toi_data_output$BRFA_crossings_per_day, main = 'BRFA Crossings/Day', ylab = 'BRFA Crossings/Day')
legend('topright', cex = .75, legend = c(sprintf('1st Quartile: %s', first_q), sprintf('Median: %s', median), sprintf('3rd Quartile: %s', third_q)),
       col = 'black')
dev.off()


## Plotting out detections by study date

for (i in 1:length(tags_of_interest)){
  tag_id = tags_of_interest[i]
  title = sprintf('%s Study Date of Detections.pdf', tag_id)
  pdf(title)
  par(mfrow = c(1,1))
plot_title = sprintf('Detections of Tag %s', tag_id)
indv_data = vue_data[which(vue_data$tag_id == tag_id), ]
hist(indv_data$study_date,  
     breaks = ceiling(max(vue_data$study_date)), 
     main = plot_title, xlab = 'Study Days', 
     ylab = 'Transmissions Detected', ylim = c(0,30), 
     xlim = c(0, max(vue_data$study_date))+5)
abline(v = tagging_data$study_date[which(tagging_data$vem_tag_id == tag_id)], col = 'red')
dates = dates_of_location_switching(vue_data, tag_id)
abline(v = dates[dates[,5] == 1, 4], col = 258)
dev.off()
}


#### For Dead Tags
for (i in 1:length(unique(dead_fish$tag_id))){
  tag_id = dead_fish$tag_id[i]
  title = sprintf('%s Study Date of Detections.pdf', tag_id)
  pdf(title)
  par(mfrow = c(1,1))
  plot_title = sprintf('Detections of Tag %s', tag_id)
  indv_data = dead_fish[which(dead_fish$tag_id == tag_id), ]
  hist(indv_data$study_date,  
       breaks = ceiling(max(vue_data$study_date)), 
       main = plot_title, xlab = 'Days at Liberty (Binned Daily)', 
       ylab = 'Number of Transmissions', ylim = c(0,30), 
       xlim = c(0, max(vue_data$study_date))+5)
  abline(v = tagging_data$study_date[which(as.character(tagging_data$vem_tag_id) == as.character(tag_id))], col = 'red')
  dates = dates_of_location_switching(dead_fish, tag_id)
  abline(v = dates[dates[,5] == 1, 4], col = 258)
  dev.off()
}


## All Detections
pdf('All Detections During Study.pdf')
par(mfrow = c(1,1))
hist(vue_data$study_date, breaks = ceiling(max(vue_data$study_date)/30), 
     main = 'Total Transmissions Detected During Study', 
     xlab = 'Days (Binned Every 30 Days)', xlim = c(0, max(vue_data$study_date)+5),
     ylab = 'Transmissions Detected', ylim = c(0,7000))
dev.off()

## Detection Stripchart
vemco_stripchart(vue_data)
vemco_stripchart(dead_fish)

## Making Maps
## loading in receivers to plot
plot_receivers = load_receiver("/Users/stephenscherrer/Documents/Work/UH/Projects/dissertation work/Spacial Ecology/Bottomfish Analysis Feb 2015/plotting_files/phase_1_deployment_recovery_log.csv")
plotting_movements(vue_data, plot_receivers)




### Plotting Receiver Positions

## Phase 1 acoustic array
plot_receivers_phase_1 = load_receiver("/Users/stephenscherrer/Documents/Work/UH/Projects/dissertation work/Spacial Ecology/Bottomfish Analysis Feb 2015/plotting_files/phase_1_deployment_recovery_log.csv")
## Defining map shapefile
pngMAP_df<- get_map(location = c(lon = -157.75, lat = 21.251), 
                    source = "google", zoom = 9,color='color')
  
  plot.new()
  
  png("Locations of Phase 1 Receivers.png")
  print(ggmap(pngMAP_df) + 
          geom_point(color = zissou.red, size = 2, data = plot_receivers_phase_1,
                     aes(x = lon, y = lat)) +
          geom_path(color = zissou.gold, mapping = aes(x = c(-157.566, -157.566), y = c(21.0333, 20.9166))) +
          geom_path(color = zissou.gold, mapping = aes(x = c(-157.566, -157.366), y = c(20.9166, 20.9166))) +
          geom_path(color = zissou.gold, mapping = aes(x = c(-157.366, -157.366), y = c(20.9166, 21.0333))) +
          geom_path(color = zissou.gold, mapping = aes(x = c(-157.366, -157.566), y = c(21.0333, 21.0333))) +
          geom_path(color = zissou.gold, mapping = aes(x = c(-157.683, -157.533), y = c(21.4166, 21.4166))) +
          geom_path(color = zissou.gold, mapping = aes(x = c(-157.533, -157.533), y = c(21.4166, 21.2833))) +
          geom_path(color = zissou.gold, mapping = aes(x = c(-157.683, -157.533), y = c(21.2833, 21.2833))) +
          labs(title = "Locations of Phase 1 Receivers"))
  dev.off()


## Phase 2 acoustic array
plot_receivers_phase_2 = load_receiver("/Users/stephenscherrer/Documents/Work/UH/Projects/dissertation work/Spacial Ecology/Bottomfish Analysis Feb 2015/plotting_files/phase_2_deployment_recovery_log.csv")
## Defining map shapefile
pngMAP_df<- get_map(location = c(lon = -157.75, lat = 21.251), 
                    source = "google", zoom = 9,color='color')

plot.new()

png("Locations of Phase 2 Receivers.png")
print(ggmap(pngMAP_df) + 
        geom_point(color = zissou.red, size = 2, data = plot_receivers_phase_2,
                   aes(x = lon, y = lat)) +
        geom_path(color = zissou.gold, mapping = aes(x = c(-157.566, -157.566), y = c(21.0333, 20.9166))) +
        geom_path(color = zissou.gold, mapping = aes(x = c(-157.566, -157.366), y = c(20.9166, 20.9166))) +
        geom_path(color = zissou.gold, mapping = aes(x = c(-157.366, -157.366), y = c(20.9166, 21.0333))) +
        geom_path(color = zissou.gold, mapping = aes(x = c(-157.366, -157.566), y = c(21.0333, 21.0333))) +
        geom_path(color = zissou.gold, mapping = aes(x = c(-157.683, -157.533), y = c(21.4166, 21.4166))) +
        geom_path(color = zissou.gold, mapping = aes(x = c(-157.533, -157.533), y = c(21.4166, 21.2833))) +
        geom_path(color = zissou.gold, mapping = aes(x = c(-157.683, -157.533), y = c(21.2833, 21.2833))) +
        labs(title = "Locations of Phase 2 Receivers"))
dev.off()


paka_tagging_data = tagging_data[as.character(tagging_data$species) == "Opakapaka", ]
fivenum(as.numeric(paka_tagging_data$"fork_length(cm)"))

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

distances_between_receivers1 = matrix(0, length(plot_receivers_phase_1$lat), length(plot_receivers_phase_1$lat))
for(i in 1:length(plot_receivers_phase_1$lat)){
  for (a in 1:length(plot_receivers_phase_1$lat)){
    distances_between_receivers1[i, a] = lldist(point1 = c(plot_receivers_phase_1$lon[i], plot_receivers_phase_1$lat[i]), point2 = c(plot_receivers_phase_1$lon[a], plot_receivers_phase_1$lat[a]))
  }
}

minimum_distance_between_receivers_phase_1 = min(distances_between_receivers1[distances_between_receivers1 != 0])
maximum_distance_between_receivers_phase_1 = max(distances_between_receivers1)
mean_distance_between_receivers_phase_1 = mean(distances_between_receivers1)
distance_between_receivers_five_num_phase_1 = fivenum(distances_between_receivers1)



distances_between_receivers2 = matrix(0, length(plot_receivers_phase_2$lat), length(plot_receivers_phase_2$lat))
for(i in 1:length(plot_receivers_phase_2$lat)){
  for (a in 1:length(plot_receivers_phase_2$lat)){
  distances_between_receivers2[i, a] = lldist(point1 = c(plot_receivers_phase_2$lon[i], plot_receivers_phase_2$lat[i]), point2 = c(plot_receivers_phase_2$lon[a], plot_receivers_phase_2$lat[a]))
  }
}

minimum_distance_between_receivers_phase_2 = min(distances_between_receivers2[distances_between_receivers2 != 0])
maximum_distance_between_receivers_phase_2 = max(distances_between_receivers2)
mean_distance_between_receivers_phase_2 = mean(distances_between_receivers2)
distance_between_receivers_five_num_phase_2 = fivenum(distances_between_receivers2)

#### Plotting fish movements ------

## Resettingplot window
par(mfrow = c(1,1))
## Loading bathymetry

#### Bathymetry for full array around Oahu and PB
# bathymetry = getNOAA.bathy(lon1 = min(receiver_data$lon[receiver_data$lon != 0]) - 0.2, 
#                            lon2 = max(receiver_data$lon[receiver_data$lon != 0]) + 0.2, 
#                            lat1=20.83, 
#                            lat2=max(receiver_data$lat[receiver_data$lat != 0]) + 0.2,
#                            resolution = 1)

plot_movements = function(vue_data, receiver_data, bottomfish_tag_ids = FALSE){
  if (bottomfish_tag_ids[1] == FALSE){
    bottomfish_tag_ids = unique(as.numeric(levels(vue_data$tag_id))[vue_data$tag_id])}
  for (id in bottomfish_tag_ids){
    indv_data = vue_data[vue_data$tag_id == id, ]
    png(paste(id, 'Movement Map.png'))
    if(exists('bathymetry') == FALSE){
      bathymetry = getNOAA.bathy(lon1 = -158, 
                                 lon2 = -157.1, 
                                 lat1 = 20.8, 
                                 lat2 = 21.65,
                                 resolution = 1)
    }
    
  ## Plotting basemap
  plot.bathy(bathymetry, land = TRUE, image=TRUE, bpal = gray.colors(10), deepest.isobath = c(-500), shallowest.isobath = c(0), step = c(100), drawlabels = TRUE)
  ## Adding scale legend
  scaleBathy(bathymetry, deg = .1, cex = .5)
  #scaleBathy(bathymetry, deg = .48, cex = .5)
  ## Adding receiver locations
  points(lat~lon, data = receiver_data, pch = 19, col = 'red', cex = .4)
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
  
  ## Plotting fish movements
  lines(lat~lon, data = indv_data, col = 'blue', lty = 1, lwd = 5)
  points(lat~lon, data = indv_data, col = 'blue',cex = 0.6, pch = 19)
  dev.off()
  }
  return(bathymetry)
}



