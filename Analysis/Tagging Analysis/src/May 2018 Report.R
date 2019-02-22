####
load("/Volumes/GoogleDrive/My Drive/Weng Lab/Personal_Folders/Steve/dissertation work/Opakapaka Tagging/Opakapaka Tagging Analysis/results/report/report workspace")
report_dir = "/Volumes/GoogleDrive/My Drive/Weng Lab/Personal_Folders/Steve/dissertation work/Opakapaka Tagging/Opakapaka Tagging Analysis/results/report"
setwd(report_dir)

# bathymetry = get_bathymetry('MHI', 'medium')
vue_data = vue_data[vue_data$tag_serial %in% tag_specs$tag_serial, ]
tagging_data = tagging_data[tagging_data$species == "Opakapaka" & !is.na(tagging_data$vem_tag_id), ]
receiver_data = receiver_data[receiver_data$recovery_date >= min(tagging_data$datetime),]

## Bathymetry
bathymetry = get_bathymetry(region = 'makapuu', resolution = 'medium')

### How many station deployments were there
length(receiver_data$station_name)
# 268
## How many were lost
length(which(receiver_data$recovered != ""))
# 22

## When were fish tagged
range(unique(tagging_data$datetime[tagging_data$species == "Opakapaka" & !is.na(tagging_data$vem_tag_id)]))
# "2012-04-13 14:09:00 HST" "2018-01-11 18:36:00 HST"

## How many opaka tagged total
length(unique(tagging_data$vem_tag_id[tagging_data$species == "Opakapaka"]))
## 292

plot_tagging_locations(tagging_data, tag_ids = NULL, region = "Oahu and Pengin Banks", bathymetry = get_bathymetry(region = 'Makapuu', resolution = 'Medium'))

## Size range
range(tagging_data$`fork_length(cm)`)
png('fork_length_hist.png')
hist(tagging_data$`fork_length(cm)`, xlab = 'Fork Length (cm)', col = 'lightblue')
dev.off()
fivenum(tagging_data$`fork_length(cm)`)
# [1] 30.0 41.0 45.5 53.0 76.0

## How many tagged fish were detected on array
length(which(tagging_data$vem_tag_id %in% vue_data$tag_id))
# 240

## How many tagged fish were detected on array in last two periods?
length(which(tagging_data$vem_tag_id %in% vue_data$tag_id[vue_data$datetime >= as.POSIXct('2016-12-07')]))

## What were our specific analysis periods?
p1 = c(as.POSIXct("2012-09-01") , as.POSIXct('2014-12-06'))
p2 = c(as.POSIXct("2015-01-20") , as.POSIXct('2015-05-23'))
p3 = c(as.POSIXct('2015-05-30') , as.POSIXct('2015-09-17'))
p4 = c(as.POSIXct('2015-09-18') , as.POSIXct('2016-01-10'))
p5 = c(as.POSIXct('2016-01-10') , as.POSIXct('2016-03-07'))
p6 = c(as.POSIXct('2016-03-08') , as.POSIXct('2016-05-28'))
p7 = c(as.POSIXct('2016-05-28') , as.POSIXct('2016-12-05'))
p8 = c(as.POSIXct('2016-12-07') , as.POSIXct('2017-06-10'))
p9 = c(as.POSIXct('2017-06-10') , as.POSIXct('2017-06-24'))
p10 = c(as.POSIXct('2017-06-26') , as.POSIXct('2018-04-25'))
phases = list(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10)

for(i in 1:length(phases)){
  print(i)
  snapshot_date = (min(phases[[i]]) + (4 * 24*60*60))
  if(i == 1){
    plot_receiver_map(receiver_df = receiver_data, snapshot_date = (min(phases[[i]]) + (4 * 24*60*60)), region = 'Oahu and Penguin Banks', resolution = 'medium')
  } else if(i == 2){
    plot_receiver_map(receiver_df = receiver_data, snapshot_date = min(phases[[i]])+ (4 * 24*60*60), region = 'Oahu', resolution = 'medium')
  } else { 
    plot_receiver_map(receiver_df = receiver_data, snapshot_date = min(phases[[i]]) + (4 * 24*60*60), region = 'Makapuu', resolution = 'medium')
  }
}

plot_receiver_map = function(receiver_df = receiver_data, snapshot_date = NULL, bathymetry = NULL, region = 'Makapuu', resolution = 'Medium', save_plot = TRUE, receiver_plot_colors = 'red'){
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



### How many fish made it?
track_status = determine_track_status(vue_data, bathymetry = get_bathymetry(region = 'mhi', resolution = 'low'))
valid_tags = track_status$status_df$tag_id[track_status$status_df$status == 'Alive']
length(valid_tags)

valid_tags_in_p10 = valid_tags[valid_tags %in% vue_data$tag_id[vue_data$datetime > p9[2]]]

write.csv(track_status$status_df, 'track status.csv')


results_by_phase = list()
results_by_phase$phase_1 = run(run_description = "Phase 1 - Valid Tags",
                               vue_df = vue_data, 
                               receiver_df = receiver_data, 
                               tagging_df = tagging_data,
                               start_date = p1[1], 
                               end_date = p1[2],
                               region = "Oahu and Penguin Banks",
                               tag_ids = valid_tags)

results_by_phase$phase_2 = run(run_description = "Phase 2 - Valid Tags",
                               vue_df = vue_data, 
                               receiver_df = receiver_data, 
                               tagging_df = tagging_data,
                               start_date = p2[1], 
                               end_date = p2[2],
                               region = "Oahu",
                               tag_ids = valid_tags)

for(i in 3:10){
  results_by_phase[[i]] = run(run_description = paste("Phase", i, "- Valid Tags"),
                              vue_df = vue_data, 
                              receiver_df = receiver_data, 
                              tagging_df = tagging_data,
                              start_date = phases[[i]][1], 
                              end_date = phases[[i]][2],
                              region = "Makapuu",
                              tag_ids = valid_tags,
                              plot = FALSE)
  print(paste('Individuals Analyzed in Phase ', i,': ', length(results_by_phase[[i]]$tag_ids), sep = ""))
  names(results_by_phase)[i] = paste('phase', i)
}


valid_tags = c(2127, 2133, 2136, 2157, 28179, 30684, 30685, 30695, 30705, 30721, 30729, 51582, 51586, 51588, 51596)
questionable_tags = c(2139, # Possible movement but depth record doesnt change any
                      track_status$status_df$tag_id[track_status$status_df$status == 'Unknown'])
dead_tags = c(6, # Algorthm thrown off by single deep ping, otherwise definitely dead
              2140) # Algorthm thrown off by apparent movement, but depth record indicates fish is dead

track_status$status_df$status[track_status$status_df$tag_id %in% dead_tags] = 'Dead'
track_status$status_df$status[track_status$status_df$tag_id %in% questionable_tags] = 'Unknown'


phase_10_valid = run(run_description = paste("Phase 10 Valid Tags - QC"),
                     vue_df = vue_data, 
                     receiver_df = receiver_data, 
                     tagging_df = tagging_data,
                     start_date = phases[[10]][1], 
                     end_date = phases[[10]][2],
                     region = "Makapuu",
                     tag_ids = valid_tags,
                     plot = T)

unique(track_status$status_df$rationalle[track_status$status_df$tag_id %in% phase_10_valid$tag_ids])

phase_10_valid_and_questionable = run(run_description = paste("Phase 10 Valid and Questionable tags"),
                                      vue_df = vue_data, 
                                      receiver_df = receiver_data, 
                                      tagging_df = tagging_data,
                                      start_date = phases[[10]][1], 
                                      end_date = phases[[10]][2],
                                      region = "Makapuu",
                                      tag_ids = c(valid_tags, questionable_tags),
                                      plot = T)
send_push(user = 'uGEHvA4hr37tsrCCtpSv4sUUxVuTqN', message = 'if you get this before run complete, something went wrong...')

### Plotting fork lengths
hist_breaks = seq(min(as.numeric(tagging_data$`fork_length(cm)`), na.rm = T)-5, max(as.numeric(tagging_data$`fork_length(cm)`), na.rm = T)+5, by = 5)
pre_hist = hist(as.numeric(tagging_data$`fork_length(cm)`), breaks = hist_breaks, plot = FALSE)
png('fl hist.png', width = 826, height = 437)
hist(as.numeric(tagging_data$`fork_length(cm)`), xlab = 'Fork Length (cm)', main = NULL, breaks = hist_breaks, xlim = c(min(hist_breaks), max(hist_breaks)), ylim = c(0, max(pre_hist$counts)*1.2))
#text(x = hist_breaks + 2.5, y = pre_hist$counts + max(pre_hist$count) / 10 , labels = pre_hist$counts)
legend('topright', legend = c(paste('Median =',  fivenum(as.numeric(tagging_data$`fork_length(cm)`), na.rm = TRUE)[3], '\n IQR =', fivenum(as.numeric(tagging_data$`fork_length(cm)`), na.rm = TRUE)[2], '-', fivenum(as.numeric(tagging_data$`fork_length(cm)`), na.rm = TRUE)[4])))
dev.off()

## Movement patterns and residency classification
analysis_summary = phase_10_valid
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

p10_tagging = tagging_data[tagging_data$datetime >= p9[2], ]
length(p10_tagging$unique_id)
# 137
length(which(p10_tagging$vem_tag_id %in% unique(vue_data$tag_id[vue_data$datetime >= p9[2]])))


indv_data = vue_data[vue_data$tag_id == 2139, ]
plot(indv_data$depth ~ indv_data$datetime, type = 'l') 










p10_statuses = track_status$status_df[track_status$status_df$tag_id %in% tagging_data$vem_tag_id[tagging_data$datetime >= p10[1]], ]
p10_valid_statuses = track_status$status_df[track_status$status_df$tag_id %in% vue_data$tag_id[vue_data$datetime >= p10[1]], ]

for(i in 1:length(unique(p10_statuses$rationalle))){
  print(paste(length(which(p10_statuses$rationalle == unique(p10_statuses$rationalle)[i])), unique(p10_statuses$rationalle)[i]))
}


status_df = track_status$status_df
status_counts = aggregate(status_df$tag_id, by = list(status_df$status), FUN = uniqueN)
colnames(status_counts) = c('classification', 'n')
png('Mortality Status of Tags.png', width = 826, height = 437)
status_counts = status_counts[c(which(status_counts$classification == 'Alive'), which(status_counts$classification == 'Unknown'), which(status_counts$classification == 'Dead'), which(status_counts$classification == 'Excluded From Analysis')), ]
barplot(height = status_counts$n, names.arg = status_counts$classification, col = c('Green', 'Yellow', 'Red', 'lightblue'), ylim = c(0, max(status_counts$n) * 1.2), main = 'Tag Status')
text(x = c(.75,1.9,3.1, 4.3), y = c(status_counts$n + max(status_counts$n) * 0.1), labels = status_counts$n)
dev.off()


status_df = track_status$status_df[track_status$status_df$tag_id %in% vue_data$tag_id[vue_data$datetime > p9[2]], ]
status_counts = aggregate(status_df$tag_id, by = list(status_df$status), FUN = uniqueN)
colnames(status_counts) = c('classification', 'n')
png('Mortality Status of Tags FY 2018.png', width = 826, height = 437)
status_counts = status_counts[c(which(status_counts$classification == 'Alive'), which(status_counts$classification == 'Unknown'), which(status_counts$classification == 'Dead'), which(status_counts$classification == 'Excluded From Analysis')), ]
barplot(height = status_counts$n, names.arg = status_counts$classification, col = c('Green', 'Yellow', 'Red', 'lightblue'), ylim = c(0, max(status_counts$n) * 1.2), main = 'Tag Status')
text(x = c(.75,1.9,3.1, 4.3), y = c(status_counts$n + max(status_counts$n) * 0.1), labels = status_counts$n)
dev.off()


valid_tags = status_df$tag_id[status_df$status == 'Alive']

setwd(run.dir)




track_status$status_df$tag_id[track_status$status_df$status == 'Unknown' & track_status$status_df$tag_id %in% vue_data$tag_id[vue_data$datetime >= p9[2]]]


#### Were forklengths of fish that lived different than those that didnt?
dead_or_not = data.frame('tag_id' = tagging_data$vem_tag_id, 'fl' = tagging_data$`fork_length(cm)`, 'status' = 'Dead', stringsAsFactors = FALSE)
dead_or_not$status[dead_or_not$tag_id %in%  valid_tags] = 'Alive'

wilcox.test(dead_or_not$fl ~ dead_or_not$status)
fivenum(dead_or_not$fl[dead_or_not$status == 'Dead'])
fivenum(dead_or_not$fl[dead_or_not$status == 'Alive'])

survivorship_before_fy2018 = track_status$status_df[track_status$status_df$tag_id %in% tagging_data$vem_tag_id[tagging_data$datetime <= p9[2]],]
length(which(survivorship_before_fy2018$status == 'Alive')) / length(survivorship_before_fy2018$status)

survivorship_after_fy2018 = track_status$status_df[track_status$status_df$tag_id %in% tagging_data$vem_tag_id[tagging_data$datetime > p9[2]],]
length(which(survivorship_after_fy2018$status == 'Alive')) / length(survivorship_after_fy2018$status)

plot_receiver_map = function(receiver_df, region = 'Oahu and Penguin Banks', resolution = 'low', start_date = NULL, bathymetry = NULL, save_plot = TRUE){

  ## Getting bathymetry basemap
  if(is.null(bathymetry)){
    bathymetry = get_bathymetry(region, resolution)
  }
  
  ## Subsetting receiver data
  if(!is.null(start_date)){
    receiver_df = receiver_df[receiver_df$recovery_date >= start_date & receiver_df$deployment_date <= start_date, ]
  }
  
  ### Plotting receiver map
  if(save_plot == TRUE){
    png(paste('Receiver Configuration ', start_date, '.png', sep = ""))
  }
  
  ## Plotting basemap
  plot.bathy(bathymetry, land = TRUE, image=TRUE, bpal = list(c(-400, -100, "lightskyblue")), deepest.isobath = c(-10000), shallowest.isobath = c(0), step = c(100), drawlabels = TRUE)
  scaleBathy(bathymetry, deg = .05, cex = .5)
  ## Plotting receivers
  points(lat~lon, data = receiver_df[receiver_df$recovered == "", ], pch = 19, col = 'red',  cex = 2)
  points(lat~lon, data = receiver_df[which(receiver_df$recovered != ""), ], pch = 1, col = 'red', cex = 2)
  ## Adding BRFA Boundaries
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
  if(save_plot == TRUE){
    dev.off()
  }
}

plot_receiver_map(receiver_df = receiver_data, start_date = min(phases[[1]]), save = FALSE)
plot_receiver_map(receiver_df = receiver_data, start_date = min(phases[[i]]), end_date = max(phases[[i]]), region = 'Oahu')

r10 = receiver_data[receiver_data$deployment_date <= min(p10) & receiver_data$recovery_date >= max(p10), ]
dist_mat = matrix(data = NA, nrow = dim(r10)[1], ncol = dim(r10)[1])
for(i in 1:dim(r10)[1]){
  for(j in 1:dim(r10)[1]){
    dist_mat[i,j] = lldist(c(r10$lon[i], r10$lat[i]), c(r10$lon[j], r10$lat[j]))  
  }
}
colnames(dist_mat) = r10$station_name
rownames(dist_mat) = r10$station_name
dist_mat = round(dist_mat, digits = 3)

#### Making a table of fish tagged by fiscal year
tagging_data$fy = year(tagging_data$datetime)
tagging_data$fy[which(month(tagging_data$datetime) >= 7)] = tagging_data$fy[which(month(tagging_data$datetime) >= 7)] + 1
tagging_table = aggregate(tagging_data$unique_id, by = list(tagging_data$fy, tagging_data$species), FUN = uniqueN)
colnames(tagging_table) = c('Fiscal Year', 'Species', 'n')
tagging_table = tagging_table[order(tagging_table$`Fiscal Year`), ]
write.csv(tagging_table, 'bottomfish tagged by fiscal year.csv', row.names = F)
paka_tagging_table = tagging_table[tagging_table$Species == "Opakapaka", ]
write.csv(paka_tagging_table, 'paka_tagged_by_fy.csv', row.names = F)

#### How did the loss of station 340 affect the integrety of the southern fence?
tag_340 = vue_data[vue_data$tag_id == 61390, ]
r341 = 546912
r339 = 546910

## After 8/29, all tags set to transmit once ever 10 minutes
tag_340 = tag_340[tag_340$datetime > as.POSIXct('2017-08-30'), ]

## What are daily recovery rates of lost station (340) at station 341?
r341 = tag_340[tag_340$receiver == 546912, ]
elapsed_time = as.numeric(abs(difftime(range(r341$datetime)[1], range(r341$datetime)[2]))) # Days
theoretical_transmissions = (elapsed_time * 24 * 6) # 24 hours in a day, 6 transmissions per hour at a transmission rate of once every 10 minutes
recovery_rate_341 = dim(r341)[1] / theoretical_transmissions

daily_recovery_rates = c()
daily_transmissions = 24 * 6
for(i in unique(tag_340$date)){
  daily_recovery_rates = c(daily_recovery_rates, length(which(tag_340$date == i)) / daily_transmissions)
  }

## What are daily recovery rates of lost station (340) at station 339
r339 = tag_340[tag_340$receiver == 546910, ]

elapsed_time = as.numeric(abs(difftime(range(r339$datetime)[1], range(r339$datetime)[2]))) # Days
theoretical_transmissions = (elapsed_time * 24 * 6) # 24 hours in a day, 6 transmissions per hour at a transmission rate of once every 10 minutes
recovery_rate_339 = dim(r339)[1] / theoretical_transmissions

daily_transmissions = 24 * 6
for(i in unique(tag_340$date)){
  daily_recovery_rates = c(daily_recovery_rates, length(which(tag_340$date == i)) / daily_transmissions)
}

fivenum(daily_recovery_rates)

## What is total daily recovery rate at either station?
## First we have to remove same detection at two stations. Since internal tag fires once every 10 minutes, we'll say any detections within 30 seconds are the same transmission
rm_index = c()
for(i in 1:(length(tag_340$datetime)-1)){
  if(any(tag_340$datetime[i+1] - tag_340$datetime[1:i] < 30)){
    rm_index = c(rm_index, i)
  }
}
tag_340 = tag_340[-rm_index, ]

daily_transmissions = 24 * 6
for(i in unique(tag_340$date)){
  daily_recovery_rates = c(daily_recovery_rates, length(which(tag_340$date == i)) / daily_transmissions)
}

fivenum(daily_recovery_rates)



depth_tags = unique(vue_data$tag_id[!is.na(vue_data$depth)])
valid_depth_tags = depth_tags[depth_tags %in% phase_10_valid$tag_ids]

for (i in 1:length(valid_depth_tags)){
  indv_data = vue_data[vue_data$tag_id == valid_depth_tags[i], ]
  depth_by_tag_and_diurnal = aggregate(indv_data$depth, by = list(indv_data$time_of_day), FUN = mean) 
    colnames(depth_by_tag_and_diurnal) = c('time_of_day', 'mean')
  depth_by_tag_and_diurnal$se = aggregate(indv_data$depth, by = list(indv_data$time_of_day), FUN = std_error)$'x'
  depth_by_tag_and_diurnal$n_obs = aggregate(indv_data$depth, by = list(indv_data$time_of_day), FUN = length)$'x'
  
  print(valid_depth_tags[i])
  print(depth_by_tag_and_diurnal)
}
depth_by_tag_and_diurnal = aggregate(vue_df$depth[!is.na(vue_df$depth)], by = list(vue_df$tag_id[!is.na(vue_df$depth)], vue_df$time_of_day[!is.na(vue_df$depth)]), FUN = mean) 


all_project_all_tags = run(run_description = paste("All Tags"),
                        vue_df = vue_data, 
                        receiver_df = receiver_data, 
                        tagging_df = tagging_data,
                        region = "Oahu and Penguin Banks",
                        plot = F)

six_pannel_plot = function(analysis_summary, vue_df = NULL, start_date = p10[1], end_date = p10[2]){
  
  if(is.null(vue_df)){
    vue_df = analysis_summary$data
  }
  makapuu_bathy = get_bathymetry(region = "Makapuu", resolution = 'Medium')
  oahu_bathy = get_bathymetry(region = "Oahu", resolution = 'Medium')
  pb_bathy = get_bathymetry(region = "Oahu and Penguin Banks", resolution = 'Medium')
  

  trans_makapuu = trans.mat(makapuu_bathy, min.depth = -100 , max.depth = -400)
  trans_oahu = trans.mat(oahu_bathy, min.depth = -100 , max.depth = -400)
  trans_pb = trans.mat(analysis_summary$bathymetry, min.depth = -100 , max.depth = -400)
  
  receiver_data = analysis_summary$receiver_data
  
  receiver_data$station_number = NA
  for(j in 1:length(receiver_data$station_name)){
    receiver_data$station_number[j] = strsplit(receiver_data$station_name, split = ' ')[[j]][5]
  }
  
  pdf('individual plot reports.pdf', height = 11, width = 8.5)
  par(mfrow = c(3, 2))
  for(i in 1:length(unique(vue_df$tag_id))){
      
    print(unique(vue_df$tag_id)[i])
    indv_data = vue_df[vue_df$tag_id == unique(vue_df$tag_id)[i], ]
    
    if(analysis_summary$summary_df$tagging_date[analysis_summary$summary_df$tag_id == unique(vue_df$tag_id)[i]] <= as.POSIXct('2015-03-17 0:00:00 HST')){
      trans1 = trans_pb
    } else if (analysis_summary$summary_df$tagging_date[analysis_summary$summary_df$tag_id == unique(vue_df$tag_id)[i]] < ('2015-05-30')) {
      trans1 = trans_oahu
    } else {
      trans1 = trans_makapuu
    }
     
       # Assigning station palette on an individual level so it doesnt make 42 colors that are super close together
    station_palette = assign_color_palette(indv_data) # Assigning mapping colors
      
    ### 1. Print Vital Stats
    par(las = 1, mar=c(0,0,0,0))
    plot.new()
    # Tag ID
    text(x = 0, y = 1, paste('Tag ID:', unique(indv_data$tag_id)), pos = 4)
    
    # Track Status
    text(x = 0, y = .9, paste('Track Status:', analysis_summary$track_status$status_df$status[analysis_summary$track_status$status_df$tag_id == unique(indv_data$tag_id)]), pos = 4)
    
    # Tagging Date
    text(x = 0, y = .8, paste('Tagging Date:', analysis_summary$summary_df$tagging_date[analysis_summary$summary_df$tag_id == unique(indv_data$tag_id)]), pos = 4)
    
    # Data Density
    text(x = 0, y = .7, paste('Data Density:', round(as.numeric(analysis_summary$summary_df$data_density[analysis_summary$summary_df$tag_id == unique(indv_data$tag_id)]), digits = 2)), pos = 4)
    
    # Total detections
    text(x = 0, y = 0.6, paste('Tag Detections:', analysis_summary$summary_df$n_detections[analysis_summary$summary_df$tag_id == unique(indv_data$tag_id)]), pos = 4)
    
    # Time at Liberty
    text(x = 0, y = 0.5, paste('Track Length:', analysis_summary$summary_df$days_at_liberty[analysis_summary$summary_df$tag_id == unique(indv_data$tag_id)], 'Days at Liberty'), pos = 4)
    
    # Unique Stations Detected
    text(x = 0, y = 0.4, paste('Unique Locations Detected:', analysis_summary$summary_df$n_stations[analysis_summary$summary_df$tag_id == unique(indv_data$tag_id)]), pos = 4)
    
    # Movements Detected
    text(x = 0, y = 0.3, paste('Movements Observed:', analysis_summary$summary_df$movements_detected[analysis_summary$summary_df$tag_id == unique(indv_data$tag_id)]), pos = 4)
    
    # BRFA Crossings
    text(x = 0, y = 0.2, paste('BRFA Crossings Observed:', analysis_summary$summary_df$total_brfa_crossings[analysis_summary$summary_df$tag_id == unique(indv_data$tag_id)]), pos = 4)
    
    # maximum observed movement
    text(x = 0, y = 0.1, paste('Maximum Linear Movement:', round(as.numeric(analysis_summary$summary_df$z_constrained_path_distance[analysis_summary$summary_df$tag_id == unique(indv_data$tag_id)], 'km'), digits = 2)), pos = 4)
    
    ### 2. Constrained movement path
    ## Plotting basemap with brfa boundary
    plot(analysis_summary$bathymetry, main = indv_data$tag_id[1], land = TRUE, image=TRUE, bpal = list(c(-100, -400, "lightblue")), deepest.isobath = c(-10000), shallowest.isobath = c(-1), step = c(100), drawlabels = TRUE)
    
    brfa_e = as.data.frame(rbind(c(-157.68333333, 21.28333333), 
                                 c(-157.53333333, 21.28333333), 
                                 c(-157.53333333, 21.4166666), 
                                 c(-157.68333333, 21.4166666)))
    colnames(brfa_e) = c('lon', 'lat')
    lines(lat~lon, data = brfa_e, pch = 19, col = 'purple', lwd = 3, cex = .6)
    
    ## Pairing plotting colors with receivers
      plot_colors = as.data.frame(cbind(station_palette$station, station_palette$colors))
        colnames(plot_colors) = c('station_name', 'plot_color')
    
    ## Add points representing receivers
      receivers_to_plot = receiver_data[receiver_data$recovery_date >= end_date & receiver_data$deployment_date <= start_date & !is.na(receiver_data$station_name), ]
        receivers_to_plot = merge(receivers_to_plot,plot_colors, all.x = TRUE)
      
      ## Filled points for stations where individuals were detected
      points(lat~lon, data = receivers_to_plot[which(receivers_to_plot$station_name %in% indv_data$station), ], col = plot_color, pch = 19, cex = 2.5)
      # Hollow points for stations without detection
      points(lat~lon, data = receivers_to_plot[-which(receivers_to_plot$station_name %in% indv_data$station), ], col = 'red', pch = 1, cex = 2.5)
      
      ### Adding movement paths to map
      points(lat ~ lon, data = tagging_data[tagging_data$vem_tag_id == unique(indv_data$tag_id), ], col = 'yellow', pch = 17, cex = 2.5)
      indv_locations = as.data.frame(rbind(cbind(tagging_data$lon[tagging_data$vem_tag_id == unique(indv_data$tag_id)], tagging_data$lat[tagging_data$vem_tag_id == unique(indv_data$tag_id)]), cbind(indv_data$lon, indv_data$lat)))
      
      ## Removing multiple successive detections at same station
      rm_index = c()
      for(j in 2:dim(indv_locations)[1]){
        if(all(indv_locations[j, ] == indv_locations[j-1, ])){
          rm_index = c(rm_index, j)
        }
      }
      indv_locations = indv_locations[-rm_index, ]
      
      path_to_plot = lc.dist(trans1, indv_locations, res = c("path"))
      # all_constrained_paths = lc.dist(trans1, indv_locations, res = "dist")
      for(j in 1:length(path_to_plot)){
        lines(path_to_plot[[j]], col = 'yellow', lwd = 2)
      }
      
      ## Add station numbers
      receivers_to_plot$station_number = NA
      for(j in 1:length(receivers_to_plot$station_name)){
        receivers_to_plot$station_number[j] = strsplit(receivers_to_plot$station_name, split = ' ')[[j]][5]
      }
      receivers_to_plot$plot_color = as.character(receivers_to_plot$plot_color)
      receivers_to_plot$plot_color[is.na(receivers_to_plot$plot_color)] = 'red'
      text(lat ~ lon, labels = station_number, data = receivers_to_plot, pos = 1, cex = .5, col = plot_color)
    
    ### 3. Detection Stripchart
      vemco_stripplot(vue_df = indv_data, save_plot = F)
      
    ### 4. Day Night Plot
      par(las = 1, mar=c(5.5,4,1,1))
      create_day_night_plot(vue_df = indv_data, save_plot = F, color_palette = station_palette)
      
    ### 5. Daily Detection Plot
      tag_detection_histogram(indv_data, analysis_summary$receiver_data, save_plot = F)
    
    ### 6. Depth History
      if(!is.na(indv_data$depth[2])){
      par(las = 1, mar=c(5.5,4,1,1))
        plot_depths(vue_df = indv_data, color_palette = station_palette, save_plot = F)
      } else {
        plot.new()
      }
    }
  dev.off()
}
 

six_pannel_plot(analysis_summary = all_project_all_tags, vue_df = vue_data)

save.image('/Volumes/GoogleDrive/My Drive/Weng Lab/Personal_Folders/Steve/dissertation work/Opakapaka Tagging/Opakapaka Tagging Analysis/results/report/report workspace')
  save.image(file.path(getwd(), 'results/report/', "report workspace"))
  
  
  
### Things to do:
  ## Make receiver plot maps for each fiscal year and each download cruise.
  ## 
  
  ### Make FY tables showing number of fish tagged
  fiscal_years = list()
  fiscal_years$fy2013 = c(as.POSIXct('2012-07-01'), as.POSIXct('2013-06-30'))
  fiscal_years$fy2014 = c(as.POSIXct('2013-07-01'), as.POSIXct('2014-06-30'))
  fiscal_years$fy2015 = c(as.POSIXct('2014-07-01'), as.POSIXct('2015-06-30'))
  fiscal_years$fy2016 = c(as.POSIXct('2015-07-01'), as.POSIXct('2016-06-30'))
  fiscal_years$fy2017 = c(as.POSIXct('2016-07-01'), as.POSIXct('2017-06-30'))
  fiscal_years$fy2018 = c(as.POSIXct('2017-07-01'), as.POSIXct('2018-06-30'))
  
  
  ### Making a list of the species and numbers tagged each fiscal year
  for(i in 1:length(fiscal_years)){
      fy_tagging = tagging_data[tagging_data$datetime >= fiscal_years[[i]][1] & tagging_data$datetime <= fiscal_years[[i]][2], ]
      if(dim(fy_tagging)[1] > 0){
      fy_tagging = fy_tagging[!is.na(fy_tagging$vem_tag_id), ]
      print(names(fiscal_years)[i])
      print(aggregate(fy_tagging$vem_tag_id, by = list(fy_tagging$species), FUN = length))
    }
  }
  

  
  #### CURRENT DATA??
  get_current_direction = function(receiver_event_data, degree_calibration, vue_df){
    ## note: Degree calibration is the offset between the receiver's 0° and the floats
    ## For receiver ID 546924 deployed Summer 2017 - Spring 2018
      ## Light to floats = 34° # Floats are 34° anticlockwise from Red Light. if floats are at \ angle, light is |. In photo, relative position is floats at 120°, VR2AR light at 86°
      ## Light to internal 0° offset = Red light is 225° Clockwise from Internal zero
      ## This makes floats are 259° relative to internal or 101° anti-clockwise
    receiver_event_data$rotation_angle$calibrated_angle = NA
    receiver_event_data$rotation_angle$calibrated_angle[receiver_event_data$rotation_angle$receiver == 546924] = receiver_event_data$rotation_angle$data[receiver_event_data$rotation_angle$receiver == 546924] + degree_calibration
    receiver_event_data$rotation_angle$cartesian = NA
    receiver_event_data$rotation_angle$cartesian[receiver_event_data$rotation_angle$calibrated_angle < 11.25] = 'N'
    receiver_event_data$rotation_angle$cartesian[receiver_event_data$rotation_angle$calibrated_angle >= 11.25 & receiver_event_data$rotation_angle$calibrated_angle < 33.75] = 'NNE'
    receiver_event_data$rotation_angle$cartesian[receiver_event_data$rotation_angle$calibrated_angle >= 33.75 & receiver_event_data$rotation_angle$calibrated_angle < 56.25] = 'NE'
    receiver_event_data$rotation_angle$cartesian[receiver_event_data$rotation_angle$calibrated_angle >= 56.25 & receiver_event_data$rotation_angle$calibrated_angle < 78.75] = 'ENE'
    receiver_event_data$rotation_angle$cartesian[receiver_event_data$rotation_angle$calibrated_angle >= 78.75 & receiver_event_data$rotation_angle$calibrated_angle < 101.25] = 'E'
    receiver_event_data$rotation_angle$cartesian[receiver_event_data$rotation_angle$calibrated_angle >= 101.25 & receiver_event_data$rotation_angle$calibrated_angle < 123.75] = 'ESE'
    receiver_event_data$rotation_angle$cartesian[receiver_event_data$rotation_angle$calibrated_angle >= 123.75 & receiver_event_data$rotation_angle$calibrated_angle < 146.25] = 'SE'
    receiver_event_data$rotation_angle$cartesian[receiver_event_data$rotation_angle$calibrated_angle >= 146.25 & receiver_event_data$rotation_angle$calibrated_angle < 168.75] = 'SSE'
    receiver_event_data$rotation_angle$cartesian[receiver_event_data$rotation_angle$calibrated_angle >= 168.75 & receiver_event_data$rotation_angle$calibrated_angle < 191.25] = 'S'
    receiver_event_data$rotation_angle$cartesian[receiver_event_data$rotation_angle$calibrated_angle >= 191.25 & receiver_event_data$rotation_angle$calibrated_angle < 213.75] = 'SSW'
    receiver_event_data$rotation_angle$cartesian[receiver_event_data$rotation_angle$calibrated_angle >= 213.75 & receiver_event_data$rotation_angle$calibrated_angle < 236.25] = 'SW'
    receiver_event_data$rotation_angle$cartesian[receiver_event_data$rotation_angle$calibrated_angle >= 236.25 & receiver_event_data$rotation_angle$calibrated_angle < 258.75] = 'WSW'
    receiver_event_data$rotation_angle$cartesian[receiver_event_data$rotation_angle$calibrated_angle >= 258.75 & receiver_event_data$rotation_angle$calibrated_angle < 281.25] = 'W'
    receiver_event_data$rotation_angle$cartesian[receiver_event_data$rotation_angle$calibrated_angle >= 281.25 & receiver_event_data$rotation_angle$calibrated_angle < 303.75] = 'WNW'
    receiver_event_data$rotation_angle$cartesian[receiver_event_data$rotation_angle$calibrated_angle >= 303.75 & receiver_event_data$rotation_angle$calibrated_angle < 326.25] = 'NW'
    receiver_event_data$rotation_angle$cartesian[receiver_event_data$rotation_angle$calibrated_angle >= 326.25 & receiver_event_data$rotation_angle$calibrated_angle < 348.75] = 'NNW'
    receiver_event_data$rotation_angle$cartesian[receiver_event_data$rotation_angle$calibrated_angle >= 348.75] = 'N'
  if (is.null(vue_df)) {
    return(receiver_event_data)
  } else {
    vue_df$current = NA
    ## Remove anything that isn't calibrated
    receiver_event_data$rotation_angle = receiver_event_data$rotation_angle[which(!is.na(receiver_event_data$rotation_angle$cartesian)), ]
    for(i in 1:length(vue_df$datetime)){
      if(vue_df$datetime >= min(receiver_event_data$rotation_angle$datetime)){
        vue_df$current[i] = receiver_event_data$rotation_angle$cartesian[which.min(abs(vue_df$datetime[i] - receiver_event_data$rotation_angle$datetime))]
      }
    }
      return(vue_df)
  }
  }
  
  

  ##### Receiver history
  receiver_df = list()
  for(i in 1:length(fiscal_years)){
    receiver_df[[i]] = receiver_data[which((receiver_data$deployment_date >= fiscal_years[[i]][1] & receiver_data$deployment_date < fiscal_years[[i]][2]) |  (receiver_data$recovery_date >= fiscal_years[[i]][1] & receiver_data$recovery_date < fiscal_years[[i]][2])), ]
    receiver_df[[i]] = receiver_df[[i]][order(as.Date(receiver_df[[i]][ ,'recovery_date']), as.Date(receiver_df[[i]][ ,'deployment_date'])), ]
    names(receiver_df)[[i]] = names(fiscal_years)[i]
  }

  
  ##### Annual objectives #####
  #### Objective 1: Objective A:  Maintain a network of acoustic receiver fences 
    ## Find sensorFenceR output for final iteration of fences?
    ## Which receivers were parts of fence?
    ## How far apart were receivers spaced?
    ## Which fence nodes were lost? When?
    ## What does this mean for detection?
    
    
  #### Objective 2: Tag bottomfish with acoustic transmitters. ####
  obj3_dir = file.path(report_dir, 'Objective 2')
  if(!file.exists(obj2_dir)){
    dir.create(obj2_dir)
  }
  setwd(obj2_dir)
  
  # bottomfish_spp = c('Ehu', 'Gindai', 'Kalekale', 'Opakapaka', 'Hapuupuu')
  # tagging_data = tagging_data[tagging_data$species %in% bottomfish_spp, ]
  # ## Making a table of fish tagged by fiscal year
  # tagging_df = tagging_data[!is.na(tagging_data$vem_tag_id), ]
  # tagging_df$fy = year(tagging_df$datetime)
  # tagging_df$fy[which(month(tagging_df$datetime) >= 7)] = tagging_df$fy[which(month(tagging_df$datetime) >= 7)] + 1
  # tagging_table = aggregate(tagging_df$unique_id, by = list(tagging_df$fy, tagging_df$species, tagging_df$vem_tag_type), FUN = uniqueN)
  #   colnames(tagging_table) = c('Fiscal Year', 'Species', 'vem_tag_type', 'n')
  # tagging_table = tagging_table[order(tagging_table$`Fiscal Year`, tagging_table$Species, tagging_table$vem_tag_type), ]
  # 
  # bf_tagging_table = tagging_table[which(tagging_table$Species %in% c('Ehu', 'Gindai', 'Kalekale', 'Opakapaka', 'Hapuupuu')), ]
  #   sum(bf_tagging_table$n)
  # 
  # write.csv(bf_tagging_table, 'Bottomfish Tagging Table by Fiscal Year.csv', row.names = FALSE)
    
  ## How many fish were tagged in FY2018
  ## Number of tagging trips per year and number of fish tagged
    unique(as.Date(tagging_df$datetime))
    
  ## How many fish tagged since last download
  ## How many fish total appeared on array in 2018
  ## How many fish alive from FY2018?
  ## Discuss changes in release methods
  
  ## Location of tagging
  plot_tagging_locations_and_receivers = function(tagging_df, receiver_df, region = 'Oahu and Penguin Banks', bathymetry = NULL, description = NULL, save_plot = TRUE){
    
    ### Getting bathymetry
    if(is.null(bathymetry)){
      bathymetry = get_bathymetry(region = region, resolution = "medium")
    }
    
    ## Plotting basemap
    if(save_plot){
      png(paste('tagging locations', description, '.png', sep = ''), width = 1000, height = 860)
    }
    
    plot.bathy(bathymetry, land = TRUE, image=TRUE, bpal = list(c(-400, -100, "lightskyblue")), deepest.isobath = c(-10000), shallowest.isobath = c(0), step = c(100), drawlabels = TRUE)
    ## Adding scale legend
    scaleBathy(bathymetry, deg = .1, cex = .5)
    
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
    
    ### Plotting receiver locations
    # If receiver was recovered, it gets full dot
    points(lat~lon, data = receiver_df[receiver_df$recovered == "", ], pch = 19, col = 'red', cex = 2)
    # If receiver was lost, it gets empty dot
    points(lat~lon, data = receiver_df[which(receiver_df$recovered != ""), ], pch = 1, col = 'red', cex = 2)
    
    ### Plotting tagging locations
    points(jitter(lat) ~ jitter(lon), data = tagging_df, cex = 1.5, pch = 19, col = 'lightgreen')
    
    if(save_plot){
      dev.off()
    }
    
  }
  
  ### Make maps of tagging locations for each fiscal year
  for(i in 1:length(fiscal_years)){
    tagging_df = tagging_data[tagging_data$datetime >= fiscal_years[[i]][1] & tagging_data$datetime < fiscal_years[[i]][2] & !is.na(tagging_data$vem_tag_id) , ]
    receiver_df = receiver_data[which((receiver_data$deployment_date <= fiscal_years[[i]][1] & receiver_data$recovery_date > fiscal_years[[i]][1]) | (receiver_data$deployment_date < fiscal_years[[i]][2] & receiver_data$recovery_date > fiscal_years[[i]][2]) | (receiver_data$deployment_date > fiscal_years[[i]][1] & receiver_data$recovery_date < fiscal_years[[i]][2])), ]
    if(i %in% 1:2){
      plot_tagging_locations_and_receivers(tagging_df, receiver_df, region = 'Oahu and Penguin Banks', description = names(fiscal_years)[i], save_plot = TRUE)
    } else {
      plot_tagging_locations_and_receivers(tagging_df, receiver_df, region = 'Makapuu', description = names(fiscal_years)[i], save_plot = TRUE)
    }
  }
  
  ### Counting release methods
  unique(tagging_data$release_method)
  aggregate(tagging_data$vem_tag_id, by = list(tagging_data$release_method), FUN = length)
  
  #### Objective 3: Download and servicing of receiver array. ####
  obj3_dir = file.path(report_dir, 'Objective 3')
  if(!file.exists(obj3_dir)){
    dir.create(obj3_dir)
  }
  setwd(obj3_dir)
  
  fy_2016_dates = c('2015-07-01', '2015-11-18', '2016-01-09', '2016-03-6', '2016-05-28', '2016-06-20', '2016-06-30')
  fy_2017_dates = c('2016-07-01', '2016-12-06', '2017-03-17', '2017-06-11', '2017-06-25', '2017-06-30')
  fy_2018_dates = c('2017-07-01', '2018-04-29', '2018-05-05')
  
  for(service_date in as.POSIXct(fy_2018_dates)){
    plot_receiver_map(receiver_df = receiver_df$fy2018, snapshot_date = service_date, bathymetry = oahu_pb_bathy, save_plot = TRUE, receiver_plot_colors = 'red')
  }
  
  #### Objective 4: Analyze data to test the following hypotheses. This objective will be measured by the success in testing the hypotheses, which is a factor both of the adequacy of the data and the nature of the analysis. The analytical component of the project will commence after the first download (mid-way through study period) and be completed after the second download (late in study period). ####
  ## 1. Bottomfish routinely move across the borders of existing BRFAs. 
  ## 2. Bottomfish movements exceed the scale of individual fishery closed areas (BRFAs). 
  ## 3. Bottomfish travel between islands, crossing major channels with depths exceeding our present definition of bottomfish habitat (400 m). 
  ## 4. Bottomfish do not utilize habitat uniformly. 
  ## 5. Bottomfish mortality following capture and tagging is nonzero and varies depending on handling and tagging protocols. 
  


  
  
