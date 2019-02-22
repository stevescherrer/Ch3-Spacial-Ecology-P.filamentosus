load(file.path(project_dir, "workspace_image_updated"))
report_dir = "/Volumes/GoogleDrive/My Drive/Weng Lab/Personal_Folders/Steve/dissertation work/Opakapaka Tagging/Opakapaka Tagging Analysis/results/report"
setwd(report_dir)

##### Annual objectives #####
  fy2018 = as.POSIXct('2017-07-01')


#### Removing false detections
dim(vue_data)
vue_data = vue_data[vue_data$detection_status == TRUE, ]
dim(vue_data)
#### Objective 1: Objective A:  Maintain a network of acoustic receiver fences ####
  ## Find sensorFenceR output for final iteration of fences?
 
  ## Which receivers were parts of fence?
  outside_north = paste('Oahu - Makapuu BRFA', c(5, 4, 3, 2))
  outside_south = paste('Oahu - Makapuu BRFA', c(311, 312, 313, 314, 315, 316, 317, 318, 319, 320))
  inside_north = paste('Oahu - Makapuu BRFA', c(331, 332, 333))
  inside_south = paste('Oahu - Makapuu BRFA', c(334, 335, 336, 337, 338, 339, 340, 341))
  
  ## How far apart were receivers spaced?
  outside_north_receiver_dist_mat = distance_between_receivers(receiver_data[receiver_data$station_name %in% outside_north, ], start_date = fy2018, end_date = fy2018 + (24*60*60), include_lost = TRUE)
    range(apply(outside_north_receiver_dist_mat$matrix, 1, min, na.rm = TRUE))

  outside_south_receiver_dist_mat = distance_between_receivers(receiver_data[receiver_data$station_name %in% outside_south, ], start_date = fy2018, end_date = fy2018 + (24*60*60), include_lost = TRUE)
    range(apply(outside_south_receiver_dist_mat$matrix, 1, min, na.rm = TRUE))
  
  inside_north_receiver_dist_mat = distance_between_receivers(receiver_data[receiver_data$station_name %in% inside_north, ], start_date = fy2018, end_date = fy2018 + (24*60*60), include_lost = TRUE)
    range(apply(inside_north_receiver_dist_mat$matrix, 1, min, na.rm = TRUE))
  
  inside_south_receiver_dist_mat = distance_between_receivers(receiver_data[receiver_data$station_name %in% inside_south, ], start_date = fy2018, end_date = fy2018 + (24*60*60), include_lost = TRUE)
    range(apply(inside_south_receiver_dist_mat$matrix, 1, min, na.rm = TRUE))
  
  ## Which fence nodes were lost? When?
  View(receiver_data[receiver_data$recovered != "" & receiver_data$recovery_date >= fy2018 & receiver_data$deployment_date <= fy2018, ])
  View(receiver_data[receiver_data$station_name == 'Oahu - Makapuu BRFA 338', ])
  
  ## What does this mean for detection?
  inside_south_recovered_receiver_dist_mat = distance_between_receivers(receiver_data[receiver_data$station_name %in% inside_south & receiver_data$recovered == "", ], start_date = fy2018, end_date = fy2018 + (24*60*60), include_lost = TRUE)
  max_dist_between_is_fence_receivers = max(apply(inside_south_recovered_receiver_dist_mat$matrix, 1, min, na.rm = TRUE))
    max_dist_between_is_fence_receivers / 2 
    # Now the greatest distance is 1.857 between receivers, or 0.929 m. This corrosponds to a detection rate of 2.1176826 or ~ 4%
  
  ## Plotting out a receiver map
    source(file.path(src_dir, 'print_receiver_recovery_map.R'))
    print_receiver_map(receiver_data = receiver_data, snapshot_date = as.POSIXct("2017-07-01 00:00:00"), save_plot = TRUE)
    
    
#### Objective 2: Tag bottomfish with acoustic transmitters. ####
  obj2_dir = file.path(report_dir, 'Objective 2')
  if(!file.exists(obj2_dir)){
    dir.create(obj2_dir)
  }
  setwd(obj2_dir)
  
  ## How many fish tagged since last download/report
    # All things tagged in this project
      dim(tagging_data)[1]
      # Broken out by species and tag type
      tagging_table_all = aggregate(tagging_data$unique_id, by = list(tagging_data$species, tagging_data$vem_tag_type), FUN = length)
      sum(tagging_table_all$x[tagging_table_all$Group.1 %in% c('Opakapaka', 'Kalekale', 'Hapuupuu')])
        # 310
      # How many had pressure tags?
      sum(tagging_table_all$x[tagging_table_all$Group.2 == 'V13P'])
      
    tagging_df = tagging_data[tagging_data$datetime >= as.POSIXct('2017-06-01'), ] # Note: This is all tagged fish from June 2017 onward
      dim(tagging_df)[1]
        # 165
    
    # Broken out by species and tag type  
    tagging_table = aggregate(tagging_df$unique_id, by = list(tagging_df$species, tagging_df$vem_tag_type), FUN = length)
    
    # How many are bottomfish?
    sum(tagging_table$x[tagging_table$Group.1 %in% c('Opakapaka', 'Kalekale', 'Hapuupuu')])
      #156
    
    # How many had V13P tags?
    aggregate(tagging_table$x, by = list(tagging_table$Group.2), FUN = sum)
    
    # How many are paka?
    sum(tagging_table$x[tagging_table$Group.1 == 'Opakapaka'])
      # 110
    
    dim(tagging_data[tagging_data$datetime >= as.POSIXct('2017-06-01') & tagging_data$datetime < fy2018 & tagging_data$species %in% c('Opakapaka', 'Kalekale', 'Hapuupuu'), ])
    
  ## How many fish tagged in FY2018
    fy_2018_tagging_df = tagging_df[which(tagging_df$datetime >= fy2018), ]
    fy_2018_tagging_df = fy_2018_tagging_df[fy_2018_tagging_df$species %in% c("Opakapaka", "Hapuupuu", "Kalekale") & !is.na(fy_2018_tagging_df$vem_tag_id), ]
      dim(fy_2018_tagging_df)[1]
        # 120
      
    tagging_table_2018 = aggregate(fy_2018_tagging_df$unique_id, by = list(fy_2018_tagging_df$species, fy_2018_tagging_df$vem_tag_type), FUN = length)
    
    # How many are Paka?
    sum(tagging_table_2018$x[tagging_table_2018$Group.1 == 'Opakapaka'])
        # 104
    
    # How were 2018 fish released?
    aggregate(fy_2018_tagging_df$vem_tag_id, by = list(fy_2018_tagging_df$release_method), FUN = length)
    
  ## Number of tagging trips per year 
    unique(as.Date(fy_2018_tagging_df$datetime[fy_2018_tagging_df$datetime >= fy2018]))

  ## How many fish total appeared on array in 2018
    length(unique(vue_data$tag_id[vue_data$datetime >= fy2018]))
      # 154
  
  ## How many fish tagged in FY2018 showed up on the array?
    
    
  ### Determining track status  
    track_status_all = determine_track_status(vue_df = vue_data, bathymetry = get_bathymetry('mhi', 'low'))
    
  ## How many fish alive from FY2018?
    # Opakapaka tagged in FY2018
    track_status_paka_tagged_2018 = determine_track_status(vue_data[vue_data$tag_id %in% tagging_data$vem_tag_id[tagging_data$species == "Opakapaka" & tagging_data$datetime >= fy2018], ], bathymetry = get_bathymetry(region = 'mhi', resolution = 'low'))
    
    # All paka detected on array in FY 2018
    track_status = track_status_all
      track_status$status_df = track_status$status_df[which(track_status$status_df$tag_id %in% vue_data$tag_id[vue_data$datetime >= fy2018] & track_status$status_df$tag_id %in% tagging_data$vem_tag_id[tagging_data$species == 'Opakapaka']), ]
      track_status$valid_tracks = track_status$status_df$tag_id[track_status$status_df$status == 'Alive']
      track_status$unknown_tracks = track_status$status_df$tag_id[track_status$status_df$status == 'Unknown']
      track_status$dead_tracks = track_status$status_df$tag_id[track_status$status_df$status == 'Dead']
      track_status$excluded_tracks = track_status$status_df$tag_id[track_status$status_df$status == 'Excluded From Analysis']
      
    # Kalekale
    kalekale_fy2018 = raw_vue_data[raw_vue_data$tag_id %in% tagging_data$vem_tag_id[tagging_data$species %in% c("Kalekale")] & raw_vue_data$datetime >= fy2018, ]
    ### Removing tags from data set that are not weng lab tags
    kalekale_fy2018 = kalekale_fy2018[kalekale_fy2018$full_tag_id %in% tag_specs$vue_tag_id, ]
    ### Reassigning station name and position info in data set using receiver data
    kalekale_fy2018 = clean_vue_data(vue_df = kalekale_fy2018, receiver_df = receiver_data)
    track_status_kale = determine_track_status(kalekale_fy2018, bathymetry = get_bathymetry(region = 'mhi', resolution = 'low'))
      View(track_status_kale$status_df)
      
    # Hapuupuu
    hapuupuu_fy2018 = raw_vue_data[raw_vue_data$tag_id %in% tagging_data$vem_tag_id[tagging_data$species %in% c("Hapuupuu")] & raw_vue_data$datetime >= fy2018, ]
    ### Removing tags from data set that are not weng lab tags
    hapuupuu_fy2018 = hapuupuu_fy2018[hapuupuu_fy2018$full_tag_id %in% tag_specs$vue_tag_id, ]
    ### Reassigning station name and position info in data set using receiver data
    hapuupuu_fy2018 = clean_vue_data(vue_df = hapuupuu_fy2018, receiver_df = receiver_data)
    track_status_hapuupuu = determine_track_status(hapuupuu_fy2018, bathymetry = get_bathymetry(region = 'mhi', resolution = 'low'))
      View(track_status_hapuupuu$status_df)
    
  ## Need to make some adjustments based on qc of data
    View(track_status$status_df[track_status$status_df$status == 'Alive', ])
    ## These are tags that were determined to be alive after visually inspecting all tag data from plots
      not_valid_tags = c(6, 12, 927, 2122, 2139, 2140) 
        ## Notes on tags: 
          # Tag 6 have depth records showing that they're probably dead. One point is off making it look alive and is likely a false detection.
          # Tag 12 have depth records showing that they're probably dead (though has a weird step function in depth record).
          # Tag 927 depth record hardly changes and though some movement is detected, it is rare and likely either a false detection or a particularly good day for detecting things
          # Tag 2122 has a depth record that looks awfully sharked. Also the few detections that occur later are not really enough to improve data
          # Tag 2139 classification likely false detection. 
          # Tag 2140 classification likely false detection.
      
          # Tag 30695 does move a little bit, but probably false detection. 
      valid_tags = sort(c(track_status$status_df$tag_id[track_status$status_df$status == 'Alive' & !track_status$status_df$tag_id %in% not_valid_tags])) ## Note: 

    ## These are tags that are unknown status
      uncertain_tags = c(track_status$status_df$tag_id[track_status$status_df$status == 'Unknown'], 2139, 2140) # Note: 2139 and 2140 are tags that show movement but only detected once at each station. Could be false detection but the depths match those from similar detections so...?
        length(uncertain_tags)
          # 26
        
    ## These are tags that were removed due to insufficient data
      removed_tags = c(track_status$status_df$tag_id[track_status$status_df$status == 'Excluded From Analysis'])
        length(removed_tags)
          # 38
        
    ## These are the dead tags
      dead_tags = c(track_status$status_df$tag_id[track_status$status_df$status == 'Dead'], not_valid_tags[-which(not_valid_tags %in% uncertain_tags)])
        length(dead_tags)
          # 6
    
    ## Conservative Scenario: How many fish alive on array in FY2018?  
      length(which(valid_tags %in% vue_data$tag_id[vue_data$datetime >= fy2018]))
        # 15  
      
    ## Less Conservative Scenario: How many fish alive + uncertain on array in FY2018?  
      length(which(c(valid_tags, uncertain_tags) %in% vue_data$tag_id[vue_data$datetime >= fy2018]))
      # 
      

        # How many fish alive total?
        all_track_status = determine_track_status(vue_data[vue_data$tag_id %in% tagging_data$vem_tag_id[tagging_data$species %in% c("Opakapaka", "Kalekale", "Hapuupuu")], ], bathymetry = get_bathymetry(region = 'mhi', resolution = 'low'))
        
        all_valid_tags = sort(c(all_track_status$status_df$tag_id[all_track_status$status_df$status == 'Alive' & !all_track_status$status_df$tag_id %in% not_valid_tags])) ## Note: 
        all_uncertain_tags = c(all_track_status$status_df$tag_id[all_track_status$status_df$status == 'Unknown'], 2139, 2140) # Note: 2139 and 2140 are tags that show movement but only detected once at each station. Could be false detection but the depths match those from similar detections so...?
        all_dead_tags = c(all_track_status$status_df$tag_id[all_track_status$status_df$status == 'Dead'], not_valid_tags[-which(not_valid_tags %in% all_uncertain_tags)])
        all_insufficient_tags = all_track_status$status_df$tag_id[all_track_status$status_df$status == 'Excluded From Analysis']
        
        sum(length(all_valid_tags), length(all_uncertain_tags), length(all_dead_tags), length(all_insufficient_tags))
        dim(all_track_status$status_df)
        
        # How many paka alive total?
        length(which(all_valid_tags %in% tagging_data$vem_tag_id[tagging_data$species == "Opakapaka"]))
        
        # How many paka alive + unknown
        length(which(c(all_valid_tags, all_uncertain_tags) %in% tagging_data$vem_tag_id[tagging_data$species == "Opakapaka"]))
        
        
        # How many paka questionable total?
        length(which(all_track_status$status_df$tag_id[all_track_status$status_df$status %in% c("Alive", "Unknown")] %in% tagging_data$vem_tag_id[tagging_data$species == "Opakapaka"]))
        
        ## Was there an observable size selection bias in survivorship? Were forklengths of fish that lived different than those that didnt?
          # Previously both a wilcox and t-test were used but this violates assumptions of independence. Kyle suggested using simulation instaed
        
        # Sampling tagging data 10000 times without replacement
        resample_n_times = 10000
        resample_mat = matrix(data = NA, nrow = 0, ncol = 5)
        while(dim(resample_mat)[1] < resample_n_times){
          sim_sample = sample(tagging_data$`fork_length(cm)`[!is.na(tagging_data$vem_tag_id) & tagging_data$species == 'Opakapaka' & !is.na(tagging_data$`fork_length(cm)`)], size = length(all_valid_tags))
          resample_mat = rbind(resample_mat, cbind(mean(sim_sample), sd(sim_sample), fivenum(sim_sample)[2], fivenum(sim_sample)[3], fivenum(sim_sample)[4]))
        }
        resample_df = as.data.frame(resample_mat)
        colnames(resample_df) = c('mean', 'sd', 'iqr1', 'median', 'iqr3')
        
        ## Plotting simulation distributions 
        hist(resample_df$mean)
        abline(v = mean(tagging_data$`fork_length(cm)`[tagging_data$vem_tag_id %in% all_valid_tags & tagging_data$species == 'Opakapaka']), col = 'blue')
        
        hist(resample_df$sd)
        abline(v = sd(tagging_data$`fork_length(cm)`[tagging_data$vem_tag_id %in% all_valid_tags & tagging_data$species == 'Opakapaka']), col = 'blue')
        
        ## What are cut-offs for CIs?
        ## For Means
        conf_level = .95
        min_cutoff = sort(resample_df$mean)[resample_n_times * ((1-conf_level)/2)]
        max_cutoff = sort(resample_df$mean)[resample_n_times * ((1-conf_level)/2 + conf_level)]
        
        did_or_did_not_there_was_no_try = 'did not fall'
        if(mean(tagging_data$`fork_length(cm)`[tagging_data$vem_tag_id %in% all_valid_tags & tagging_data$species == 'Opakapaka']) > min_cutoff & mean(tagging_data$`fork_length(cm)`[tagging_data$vem_tag_id %in% all_valid_tags & tagging_data$species == 'Opakapaka']) < max_cutoff){
          did_or_did_not_there_was_no_try = 'fell'
        }
        
        print(paste('The mean fork length of P. filamentosus classified alive (', round(mean(tagging_data$`fork_length(cm)`[tagging_data$vem_tag_id %in% all_valid_tags & tagging_data$species == 'Opakapaka']), digits = 3),' cm) ', did_or_did_not_there_was_no_try, ' within the ', conf_level*100, '% confidence interval from simulation data sampled without replacement (', round(min_cutoff, digits = 3), ' - ', round(max_cutoff, digits = 3), ').', sep = ""))
        
        ## For standard deviations
        min_cutoff = sort(resample_df$sd)[resample_n_times * ((1-conf_level)/2)]
        max_cutoff = sort(resample_df$sd)[resample_n_times * ((1-conf_level)/2 + conf_level)]
        
        did_or_did_not_there_was_no_try = 'did not fall'
        if(sd(tagging_data$`fork_length(cm)`[tagging_data$vem_tag_id %in% all_valid_tags & tagging_data$species == 'Opakapaka']) > min_cutoff & sd(tagging_data$`fork_length(cm)`[tagging_data$vem_tag_id %in% all_valid_tags & tagging_data$species == 'Opakapaka']) < max_cutoff){
          did_or_did_not_there_was_no_try = 'fell'
        }
        
        print(paste('The standard deviation of fork lengths of P. filamentosus classified alive (', round(sd(tagging_data$`fork_length(cm)`[tagging_data$vem_tag_id %in% all_valid_tags & tagging_data$species == 'Opakapaka']), digits = 3),' cm) ', did_or_did_not_there_was_no_try, ' within the ', conf_level*100, '% confidence interval from simulation data sampled without replacement (', round(min_cutoff, digits = 3), ' - ', round(max_cutoff, digits = 3), ').', sep = ""))
        
        
        # Making a figure for this
        pdf('Size Selection Bias.pdf')
        par(mfrow = c(2, 2))
          ## Survivor Fork Lengths
          hist(tagging_data$`fork_length(cm)`[tagging_data$vem_tag_id %in% all_valid_tags & tagging_data$species == 'Opakapaka'], xlim = c(0, 80), main = 'Surviving P. filamentosus', breaks = seq(0,80,5), xlab = 'Fork Length (cm)')
          ## Simulated Means
          hist(resample_df$mean, main = 'Simulated Means', xlab = expression(mu))
          abline(v = mean(tagging_data$`fork_length(cm)`[tagging_data$vem_tag_id %in% all_valid_tags & tagging_data$species == 'Opakapaka']), col = 'blue')
          ## Population Fork Lengths
          hist(tagging_data$`fork_length(cm)`[!is.na(tagging_data$vem_tag_id) & tagging_data$species == 'Opakapaka'], xlim = c(0, 80), main = 'All P. filamentosus Tagged', breaks = seq(0,80,5), xlab = 'Fork Length (cm)')
          ## Simulated Standard Deviations
          hist(resample_df$sd, main =  'Simulated Std. Dev.', xlab = expression(sigma))
          abline(v = sd(tagging_data$`fork_length(cm)`[tagging_data$vem_tag_id %in% all_valid_tags & tagging_data$species == 'Opakapaka']), col = 'blue')
        dev.off()
        
        
        
  ## Plotting Location of tagging
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
    
    pdf('Tagging Locations FY 2018.pdf')
      plot_tagging_locations_and_receivers(tagging_df, receiver_df, region = 'Makapuu', description = names(fiscal_years)[i], save_plot = FALSE)
      plot_tagging_locations_and_receivers(tagging_df[tagging_df$vem_tag_id %in% valid_tags], receiver_df, region = 'Makapuu', description = names(fiscal_years)[i], save_plot = FALSE)
      plot_tagging_locations_and_receivers(tagging_df[tagging_df$vem_tag_id %in% uncertain_tags], receiver_df, region = 'Makapuu', description = names(fiscal_years)[i], save_plot = FALSE)
    dev.off()
    
  ## Plotting Six pannel plots  
    six_pannel_plot = function(analysis_summary, vue_df = NULL, start_date = NULL, end_date = NULL){
      
      if(is.null(vue_df)){
        vue_df = analysis_summary$data
      }
      
      if(is.null(start_date)){
        start_date = min(vue_df$datetime)
      }
      
      if(is.null(end_date)){
        end_date = max(vue_df$datetime)
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
      
      for(i in 1:length(unique(vue_df$tag_id))){
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
        
        # Assigning station palette on an individual level so it doesnt make 42 colors that are super close together
        station_palette = assign_color_palette(indv_data) # Assigning mapping colors
        
        ### 1. Print Vital Stats
        print('1. Vital Stats')
        par(las = 1, mar=c(0,0,0,0))
        plot.new()
        # Tag ID
        text(x = 0, y = .95, paste('Tag ID:', unique(indv_data$tag_id)), pos = 4, cex = 1.5)
        
        # Track Status
        text(x = 0, y = .85, paste('Track Status:', analysis_summary$track_status$status_df$status[analysis_summary$track_status$status_df$tag_id == unique(indv_data$tag_id)]), pos = 4, cex = 1.5)
        
        # Tagging Date
        text(x = 0, y = .75, paste('Tagging Date:', analysis_summary$summary_df$tagging_date[analysis_summary$summary_df$tag_id == unique(indv_data$tag_id)]), pos = 4, cex = 1.5)
        
        # Data Density
        text(x = 0, y = .65, paste('Data Density:', round(as.numeric(analysis_summary$summary_df$data_density[analysis_summary$summary_df$tag_id == unique(indv_data$tag_id)]), digits = 2)), pos = 4, cex = 1.5)
        
        # Total detections
        text(x = 0, y = 0.55, paste('Tag Detections:', analysis_summary$summary_df$n_detections[analysis_summary$summary_df$tag_id == unique(indv_data$tag_id)]), pos = 4, cex = 1.5)
        
        # Time at Liberty
        text(x = 0, y = 0.45, paste('Track Length:', analysis_summary$summary_df$days_at_liberty[analysis_summary$summary_df$tag_id == unique(indv_data$tag_id)], 'Days at Liberty'), pos = 4, cex = 1.5)
        
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
        
        vemco_stripplot(vue_df = indv_data, save_plot = F)
        
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
        plot_colors = as.data.frame(cbind(station_palette$station, station_palette$colors))
        colnames(plot_colors) = c('station_name', 'plot_color')
        
        ## Add points representing receivers
        receivers_to_plot = receiver_data[receiver_data$recovery_date >= end_date & receiver_data$deployment_date <= start_date & !is.na(receiver_data$station_name), ]
        receivers_to_plot = merge(receivers_to_plot, plot_colors, all.x = TRUE)
        
        ## Filled points for stations where individuals were detected
        points(lat~lon, data = receivers_to_plot[receivers_to_plot$recovered == "", ], col = 'red', pch = 19, cex = 1.5)
        points(lat~lon, data = receivers_to_plot[receivers_to_plot$recovered == "", ], col = receivers_to_plot$plot_color, pch = 19, cex = 1.5)
        # Hollow points for stations without detection
        points(lat~lon, data = receivers_to_plot[receivers_to_plot$recovered != "", ], col = 'red', pch = 1, cex = 1.5)
        
        ### Adding movement paths to map
        points(lat ~ lon, data = tagging_data[tagging_data$vem_tag_id == unique(indv_data$tag_id), ], col = 'yellow', pch = 17, cex = 1.5)
        indv_locations = as.data.frame(rbind(cbind(tagging_data$lon[tagging_data$vem_tag_id == unique(indv_data$tag_id)], tagging_data$lat[tagging_data$vem_tag_id == unique(indv_data$tag_id)]), cbind(indv_data$lon, indv_data$lat)))
        
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
        
        ## Removing successive detection pairs
        rm_index = c()
        if(dim(indv_locations)[1] >= 4){
        for(j in seq(3, dim(indv_locations)[1], 2)){
          if(j+1 <= dim(indv_locations)[1]){
              if(all(indv_locations[c(j, j+1), ] == indv_locations[c(j-2, j-1), ])){
              rm_index = c(rm_index, c(j, j+1))
            }
          }
        }
        }
        if(length(rm_index) > 0){
          indv_locations = indv_locations[-rm_index, ]
        }
        
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
        
        ### 4. Day Night Plot
        print('4. Day Night Plot')
        
        par(las = 1, mar=c(5.5,4,1,1))
        create_day_night_plot(vue_df = indv_data, save_plot = F, color_palette = station_palette, plot_title = FALSE)
        
        ### 5. Daily Detection Plot
        print('5. Daily Detection Histogram')
        tag_detection_histogram(indv_data, analysis_summary$receiver_data, save_plot = F, plot_title = FALSE)
        
        ### 6. Depth History
        print('6. Depth History')
        
        if(!is.na(indv_data$depth[2])){
          par(las = 1, mar=c(5.5,4,1,1))
          plot_depths(vue_df = indv_data, color_palette = station_palette, save_plot = F, plot_title = FALSE)
        } else {
          plot.new()
        }
        dev.off()
      }
    }
    
    
    plot_detection_records(vue_df = fy2018_valid_and_uncertain$data, receiver_df = fy2018_valid_and_uncertain$receiver_data)
    
    ## Valid and uncertain tags
    six_pannel_plot(analysis_summary = fy2018_valid_and_uncertain, vue_df = vue_data[vue_data$tag_id %in% c(track_status$valid_tracks, track_status$uncertain_tracks), ])
    ## Dead tags
    # fy2018_dead_tags = run(run_description = paste("FY2018 Dead Tags"),
    #                        vue_df = vue_data[-which(vue_data$tag_id %in% all_valid_tags[which(all_valid_tags %in% vue_data$tag_id[vue_data$datetime >= fy2018])]), ], 
    #                        receiver_df = receiver_data, 
    #                        tagging_df = tagging_data,
    #                        start_date = fy2018,
    #                        end_date = as.POSIXct('2018-04-15'),
    #                        region = "Makapuu",
    #                        tag_ids = tagging_data$vem_tag_id[tagging_data$datetime >= fy2018],
    #                        plot = F)
                           
    six_pannel_plot(analysis_summary = fy2018_dead_tags, vue_df = fy2018_dead_tags$data)
    
    six_pannel_plot(analysis_summary = fy2018_valid, vue_df = vue_data[vue_data$tag_id == 51586, ])
    
  ## Discuss changes in release methods

  # Counting release methods
    unique(tagging_data$release_method)
    aggregate(tagging_data$vem_tag_id[!is.na(tagging_data$vem_tag_id) & tagging_data$datetime >= fy2018], by = list(tagging_data$release_method[!is.na(tagging_data$vem_tag_id) & tagging_data$datetime >= fy2018]), FUN = length)

#### Objective 3: Download and servicing of receiver array. ####
obj3_dir = file.path(report_dir, 'Objective 3')
if(!file.exists(obj3_dir)){
  dir.create(obj3_dir)
}
setwd(obj3_dir)

## How many stations were deployed?
  length(which(receiver_data$recovery_date >= fy2018))
## How many stations were recovered
  length(which(receiver_data$recovery_date[receiver_data$recovered == ""] >= fy2018))
  

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

pdf('Receiver Deployment Map 2018.pdf')
  plot_receiver_map(receiver_data, snapshot_date = fy2018, save_plot = FALSE)
dev.off()

#### Objective 4: Analyze data to test the following hypotheses. This objective will be measured by the success in testing the hypotheses, which is a factor both of the adequacy of the data and the nature of the analysis. The analytical component of the project will commence after the first download (mid-way through study period) and be completed after the second download (late in study period). ####
obj4_dir = file.path(report_dir, 'Objective 4')
if(!file.exists(obj4_dir)){
  dir.create(obj4_dir)
}
setwd(obj4_dir)

fy2018_valid = run(run_description = paste("FY2018 Valid Tags"),
                     vue_df = vue_data[vue_data$tag_id %in% all_valid_tags[which(all_valid_tags %in% vue_data$tag_id[vue_data$datetime >= fy2018])], ], 
                     receiver_df = receiver_data, 
                     tagging_df = tagging_data,
                     start_date = fy2018,
                     end_date = as.POSIXct('2018-04-15'),
                     region = "Makapuu",
                     tag_ids = valid_tags,
                     plot = T)

fy2018_valid_and_uncertain = run(run_description = paste("FY2018 Valid and Uncertain Tags"),
                                 vue_df = vue_data[vue_data$tag_id %in% c(all_valid_tags, all_uncertain_tags)[which(c(all_valid_tags, all_uncertain_tags) %in% vue_data$tag_id[vue_data$datetime >= fy2018])], ], 
                                 receiver_df = receiver_data, 
                                 tagging_df = tagging_data,
                                 start_date = fy2018,
                                 end_date = as.POSIXct('2018-04-15'),
                                 region = "Makapuu",
                                 tag_ids = c(valid_tags, uncertain_tags),
                                 plot = T)

## 1. Bottomfish routinely move across the borders of existing BRFAs. 
dissertation_H1(fy2018_valid)


analysis_summary = fy2018_valid
hist_breaks = seq(0, max(1/as.numeric(analysis_summary$summary_df$mean_brfa_movements_per_day[analysis_summary$summary_df$mean_brfa_movements_per_day != 0]))+5, by = 5)
pre_hist = hist((1/as.numeric(analysis_summary$summary_df$mean_brfa_movements_per_day[analysis_summary$summary_df$mean_brfa_movements_per_day != 0])), breaks = hist_breaks, plot = FALSE)
png('H1 - Valid Mean BRFA Days Between Crossings.png', width = 826, height = 437)
hist(1/as.numeric(analysis_summary$summary_df$mean_brfa_movements_per_day), xlab = 'Average Time Between BRFA Crossing (days)', main = NULL, breaks = hist_breaks, xlim = c(min(hist_breaks), max(hist_breaks)), ylim = c(0, max(pre_hist$counts)*1.2))
# text(x = hist_breaks + 2.5, y = pre_hist$counts + max(pre_hist$count) / 10 , labels = pre_hist$counts)
#legend('topright', legend = c(paste('Median =',  round(1/fivenum(as.numeric(analysis_summary$summary_df$mean_brfa_movements_per_day), na.rm = TRUE)[3], digits = 2), '\n IQR =', round(1/fivenum(as.numeric(analysis_summary$summary_df$mean_brfa_movements_per_day), na.rm = TRUE)[2], digits = 2), '-', round(1/fivenum(as.numeric(analysis_summary$summary_df$mean_brfa_movements_per_day), na.rm = TRUE)[4], digits = 2))))
dev.off()

dissertation_H1(fy2018_valid_and_uncertain)

analysis_summary = fy2018_valid_and_uncertain
hist_breaks = seq(0, max(1/as.numeric(analysis_summary$summary_df$mean_brfa_movements_per_day[analysis_summary$summary_df$mean_brfa_movements_per_day != 0]))+5, by = 5)
pre_hist = hist((1/as.numeric(analysis_summary$summary_df$mean_brfa_movements_per_day[analysis_summary$summary_df$mean_brfa_movements_per_day != 0])), breaks = hist_breaks, plot = FALSE)
png('H1 - Valid and Uncertain Mean BRFA Days Between Crossings.png', width = 826, height = 437)
hist(1/as.numeric(analysis_summary$summary_df$mean_brfa_movements_per_day), xlab = 'Average Time Between BRFA Crossing (days)', main = NULL, breaks = hist_breaks, xlim = c(min(hist_breaks), max(hist_breaks)), ylim = c(0, max(pre_hist$counts)*1.2))
# text(x = hist_breaks + 2.5, y = pre_hist$counts + max(pre_hist$count) / 10 , labels = pre_hist$counts)
legend('topright', legend = c(paste('Median =',  round(1/fivenum(as.numeric(analysis_summary$summary_df$mean_brfa_movements_per_day), na.rm = TRUE)[3], digits = 2), '\n IQR =', round(1/fivenum(as.numeric(analysis_summary$summary_df$mean_brfa_movements_per_day), na.rm = TRUE)[2], digits = 2), '-', round(1/fivenum(as.numeric(analysis_summary$summary_df$mean_brfa_movements_per_day), na.rm = TRUE)[4], digits = 2))))
dev.off()


## 2. Bottomfish movements exceed the scale of individual fishery closed areas (BRFAs). 
dissertation_H2(fy2018_valid)
dissertation_H2(fy2018_valid_and_uncertain)

## 3. Bottomfish do not use habitat uniformily
# How north is the most northern receiver station a tag was detected and which receiver was it?
northmost_index = which.max(receiver_data$lat[receiver_data$station %in% unique(fy2018_valid$data$station)])
max(receiver_data$lat[receiver_data$station %in% unique(fy2018_valid$data$station)])
receiver_data$station[receiver_data$station %in% unique(fy2018_valid$data$station)][northmost_index]

# What is the most north a fish was tagged?
max(tagging_data$lat[tagging_data$species == 'Opakapaka'])

#### Script cleanup
save.image(file.path(report_dir, 'Fall 2018 Report Analysis Workspace.Rdata'))
load(file.path(report_dir, 'Fall 2018 Report Analysis Workspace.Rdata'))







########## Do we see changes in behavior?

## Movement defined as leaving a station or arriving at a station

##### Distribution of time spent at stations vs. 


######## FFT Analysis #######
#make fft data
fft_df = vue_df

#round down to the hour
fft_df$datetime<-trunc(fft_df$datetime,"hours")

#change dt to character
#fft_df$datetime<-as.character(fft_df$datetime)

#make table with dt and transmitter name  
fft_hour_data<-table(as.character(fft_df$datetime),fft_df$tag_id)

##generate hour table

#create a study hours data frame
study_hours<-as.character(seq(trunc(min(fft_df$datetime),'hour'),max(fft_df$datetime),by = "hour"))

fft_hour_data<-as.data.frame.matrix(fft_hour_data)

#Find the study hours not in data (when no fish)
study_hours<-study_hours[!study_hours %in% fft_df$datetime]

#combine the two tables
null_hour_data <- as.data.frame(matrix(data=0,nrow=length(study_hours),ncol=length(unique(fft_df$tag_id))))
row.names(null_hour_data)<-study_hours
names(null_hour_data)<-sort(unique(fft_df$tag_id))
fft_hour_data<-rbind(fft_hour_data,null_hour_data)
fft_hour_data<-fft_hour_data[order(rownames(fft_hour_data)),]

# export.CSV
write.csv(fft_hour_data,"fft.csv")


####FFT data prep############# 0 replaced with NA (#1 remove dates prior to tagging each fish)--------------------------------
#make fft data
fft_df_NA = vue_df

#round down to the hour
fft_df_NA$datetime<-trunc(fft_df_NA$datetime,"hours")

#change dt to character
#fft_df$datetime<-as.character(fft_df$datetime)

#make table with dt and transmitter name  
fft_hour_data_NA<-table(as.character(fft_df_NA$datetime),fft_df_NA$tag_id)

##generate hour table

#create a study hours data frame
study_hours_NA<-as.character(seq(trunc(min(fft_df_NA$datetime),'hour'),max(fft_df_NA$datetime),by = "hour"))

fft_hour_data_NA<-as.data.frame.matrix(fft_hour_data_NA)

#Find the study hours not in data (when no fish)
study_hours_NA<-study_hours_NA[!study_hours_NA %in% fft_df_NA$datetime]

#combine the two tables
null_hour_data_NA <- as.data.frame(matrix(data=0,nrow=length(study_hours_NA),ncol=length(unique(fft_df_NA$tag_id))))
row.names(null_hour_data_NA)<-study_hours_NA
names(null_hour_data_NA)<-sort(unique(fft_df_NA$tag_id))
fft_hour_data_NA<-rbind(fft_hour_data_NA,null_hour_data_NA)
fft_hour_data_NA<-fft_hour_data_NA[order(rownames(fft_hour_data_NA)),]

for (i in 1:ncol(fft_hour_data_NA)){
  temp <- fft_hour_data_NA[,i]>0
  if(fft_hour_data_NA[1,i]>0){} else  fft_hour_data_NA[1:(grep(TRUE,temp)[1]-1),i]<-NA
}

# export.CSV
write.csv(fft_hour_data_NA,"fft.csv")

###loop for seprate csv

fishlist<-unique(molokini_data$tag_id)
numfish<-length(fishlist)
fishlist<-sort(fishlist)

for (i in 1:ncol(fft_hour_data_NA)) {
  temp<-fft_hour_data_NA[,i]
  temp<-na.omit(temp)
  write.csv(temp,paste('fft_NA',colnames(fft_hour_data_NA)[i],"export.csv",sep="_"))
}


##Paolo's fft

library('TSA')

x<-fft_hour_data_NA

#openpdf

pdf("paka_FFT.pdf", width=8.5,height=11)
par(mfcol=c(3,2))

for ( i in 1:ncol(x)){
  temp_fish_1<-na.omit(x[,i])   
  #detrend time series
  d<-diff(temp_fish_1)
  #normalize to 12 hours so daily trends can be seen
  e<-d[-(1:(length(d) %% 12))]
  #Use function periodigram from TSA package to plot periodogram
  temp<-periodogram(e,ylab='Periodogram',plot=FALSE)
  #extract results from temp and plot spectrum versus inverse
  #of frequency
  plot((1/temp$freq),temp$spec,type="l", main= colnames(x)[i], xlim = c(0,72))
  }

dev.off()

temp1 = spec.fft(y = e, center = FALSE)
temp1$A == temp$spec
temp$freq
temp1$fx == temp$freq


plot((1/temp1$fx), temp1$A, type = 'l', main = colnames(x)[i], xlim = c(0,24))

plot((1/temp$spec))





#### How many fish appear sharked? - 55
not_valid_tags = track_status_paka_tagged_2018$status_df$tag_id[-which(track_status_paka_tagged_2018$status_df$tag_id %in% valid_tags)]

predated_tags = c()
for(i in 1:length(not_valid_tags)){
  indv_data = vue_data[vue_data$tag_id == not_valid_tags[i], ]
  indv_data = indv_data[indv_data$datetime <= (tagging_data$datetime[tagging_data$vem_tag_id == not_valid_tags[i]] + 14*24*60*60), ]
  if(length(unique(indv_data$station)) > 3 + ("Tagging Location" %in% indv_data$station)){
    predated_tags = c(predated_tags, not_valid_tags[i])
  }
}

length(predated_tags)

###########
## What was $ live in each year of project? Did we get better at it?

track_status_df = track_status_all$status_df
track_status_df = merge(x = track_status_df, y = tagging_data[ ,c('vem_tag_id', 'datetime')], by.x = 'tag_id', by.y = 'vem_tag_id', all.x = TRUE, all.y = TRUE)
track_status_df$year = year(track_status_df$datetime) + (month(track_status_df$datetime) >= 7)
track_status_df = track_status_df[track_status_df$tag_id %in% tagging_data$vem_tag_id[tagging_data$species == 'Opakapaka'], ]

annual_mortality_rate = data.frame('year' = sort(unique(track_status_df$year)), 'n' = NA, '% Valid' = NA, '% Uncertain' = NA, '% Dead' = NA, '% Insufficient Data' = NA, '% Not Detected', stringsAsFactors = F)
for(i in 1:length(sort(unique(track_status_df$year)))){
  annual_ts = track_status_df[track_status_df$year == sort(unique(track_status_df$year))[i], ]
  annual_mortality_rate$n[i] = length(annual_ts$status)
  annual_mortality_rate$X..Valid[i] = length(which(annual_ts$status == 'Alive')) / length(annual_ts$status)
  annual_mortality_rate$X..Uncertain[i] = length(which(annual_ts$status == 'Unknown')) / length(annual_ts$status)
  annual_mortality_rate$X..Dead[i] = length(which(annual_ts$status == 'Dead')) / length(annual_ts$status)
  annual_mortality_rate$X..Insufficient.Data[i] = length(which(annual_ts$status == 'Excluded From Analysis')) / length(annual_ts$status)
  annual_mortality_rate$X...Not.Detected.[i] = length(which(is.na(annual_ts$status))) / length(annual_ts$status)
  ## Hard coding some things
  if(sort(unique(track_status_df$year))[i] == 2018){
    annual_mortality_rate$X..Valid[i] = 10 / length(annual_ts$status)
    annual_mortality_rate$X..Uncertain[i] = 26 / length(annual_ts$status)
  }
}

write.csv(annual_mortality_rate, file = 'mortality_rates_by_year.csv', row.names = FALSE)

track_status_df$tag_id