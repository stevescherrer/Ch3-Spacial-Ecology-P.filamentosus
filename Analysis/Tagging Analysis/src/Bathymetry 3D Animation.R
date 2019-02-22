##### Making sick ass 3d gif of Makapuu - Using 50 m bathymetry files #####
 #### Written by: Stephen Scherrer
 #### Written on: 25-27 January 2018
 #### All rights preserved, all wrongs traversed

##### Description #####
 #### Script to create a GIF animation showcasing bathymetry off Makapuu
 #### As written, camera pans down to look north and then starts scanning to look south before returning to look north
 #### Bathymetry begins full and then slowly cuts out until only predefined depth strip remains. Full bathymetry then expands back out

  ### Old Perspective notes (Do I still need this?): z = -64, y = -16, x = -69  

##### Workspace and Data Setup #####
  #### Installing Principle Dependencies
  library('ncdf4') # nc_open()
  library('marmap') # subsetBathy()
  library('lattice') # wireframe()
  library('notifyR') # send_push()
  
  #### Setting Up Directories
  results_dir = '/Volumes/GoogleDrive/My Drive/Weng Lab/Personal_Folders/Steve/dissertation work/Opakapaka Tagging/Opakapaka Tagging Analysis/results'
  setwd(results_dir)
  gif.dir = create_save_directory('Makapuu Rotating Wireframe Gif')
  setwd(gif.dir)
  
  #### Importing Data
  nc_bathy_dat <- nc_open('/Volumes/GoogleDrive/My Drive/Weng Lab/Personal_Folders/Steve/dissertation work/Acoustic Network Design/Acoustic Web App/src/himbsyn.bathy.v19.grd/himbsyn.bathy.v19.grd')
  
  #### Cleaning up data
  ## Reclassing ncdf into class bathy
  lat <-  ncvar_get(nc_bathy_dat,"lat")
  lon <-  ncvar_get(nc_bathy_dat,"lon")
  z   <- ncvar_get(nc_bathy_dat, "z")
  
  bathy_mat = matrix(z, byrow = FALSE, nrow = length(lon), dimnames = list(lon, lat))
  bathy_mat[is.nan(bathy_mat)] = NA
  class(bathy_mat) <- "bathy"
  
  ## Subsetting full bathymetry
  master_bath = subsetBathy(bathy_mat, x = c(-157.8,-157.51), y = c( 21.2, 21.5), locator = FALSE)
  bathymetry = master_bath

##### Animation #####
  #### Animation parameters
  deepest_depth = min(bathymetry, na.rm = TRUE) # Deepest Depth in bathy file
  shallowest_depth = max(bathymetry, na.rm = TRUE) # Shallowest Depth in bathy file
  upper_depth_cutoff = -100 # The upper limit of the bathymetry following removal
  lower_depth_cutoff = -400 # The lower limit of the bathymetry following removal
  end_pan_down = 70
  end_side_pan = 120
  start_frame_cut = 35 # The frame number to begin removing bathymetry
  end_frame_cut = 75 # The frame number to stop removing bathymetry
  start_frame_add = 150 # The frame number to begin readding bathymetry
  end_frame_add = 190 # The frame number to stop readding bathymetry
  upper_cuts_per_frame = ceiling(abs(max(bathymetry, na.rm = TRUE) - upper_depth_cutoff) / abs(start_frame_cut - end_frame_cut)) # The depth (in m), to remove during each removal frame
  lower_cuts_per_frame = ceiling(abs(min(bathymetry, na.rm = TRUE) - lower_depth_cutoff) / abs(start_frame_cut - end_frame_cut)) # The depth (in m), to remove during each removal frame
  upper_adds_per_frame = ceiling(abs(max(bathymetry, na.rm = TRUE) - upper_depth_cutoff) / abs(start_frame_add - end_frame_add)) # The depth (in m), to add during each adding frame
  lower_adds_per_frame = ceiling(abs(min(bathymetry, na.rm = TRUE) - lower_depth_cutoff) / abs(start_frame_add - end_frame_add)) # The depth (in m), to add during each adding frame
  background_color = 'black'
  
  #### Animating GIF
  ## (Re)setting Loop Counters
  cut_bathy_ceiling = max(master_bath, na.rm = TRUE)
  cut_bathy_floor = min(master_bath, na.rm = TRUE)
  ## Producing individual GIF images
  for(i in 0:240){
    iter_num = NULL
    if(i < 10){
      iter_num = paste('00', i, sep = "")
    }else if(i < 100){
      iter_num = paste('0', i, sep = "")
    }else{
      iter_num = i
    }
    
    ## Removing a single max and min value (in corners) to keep color scale uniform
    bathymetry[1,1] = NA
    bathymetry[1,ncol(bathymetry)] = NA
    
    ## Removal of bathymetry outside of specified depth range
    if(i >= start_frame_cut & i < end_frame_cut & max(bathymetry, na.rm = TRUE) > upper_depth_cutoff){
      cut_bathy_ceiling = cut_bathy_ceiling - upper_cuts_per_frame
      bathymetry[which(bathymetry > cut_bathy_ceiling)] = NA
    }
    if(i >= start_frame_cut & i < end_frame_cut & min(bathymetry, na.rm = TRUE) < lower_depth_cutoff){
      cut_bathy_floor = cut_bathy_floor + lower_cuts_per_frame
      bathymetry[which(bathymetry < cut_bathy_floor)] = NA
    }
    
    ## Adding back in bathymetry outside of specified depth range
    if(i >= start_frame_add & i < end_frame_add & min(bathymetry, na.rm = TRUE) <= upper_depth_cutoff){
      cut_bathy_floor = cut_bathy_floor - lower_adds_per_frame
      cut_bathy_ceiling = cut_bathy_ceiling + upper_adds_per_frame
      bathymetry[which(master_bath >= cut_bathy_floor & master_bath <= cut_bathy_ceiling)] = master_bath[which(master_bath >= cut_bathy_floor & master_bath <= cut_bathy_ceiling)] 
    }
    
    ## Adding a single max and min value (in corners) to keep color scale (and boudning box) consistant across entire animation
    bathymetry[1,1] = shallowest_depth
    bathymetry[1,ncol(bathymetry)] = deepest_depth
    
    ## Animating pan down
    if (i <= end_pan_down){
      png(paste('makapuu_wireframe_gif_tiltpan_seq_', iter_num, '.png', sep = ""), width = 800, height = 800)
      par(bg = background_color)
      print(wireframe(unclass(bathymetry), shade = TRUE, aspect = c(1/2, 0.1), xlab = "", ylab = "", zlab = "", distance = 0, screen = list(z = -i, y = 0, x = -i), par.settings = list(axis.line = list(col = "transparent"))))
      dev.off()
      
    ## Animating side pan
    }else if (i > end_pan_down & i <= end_side_pan){
      png(paste('makapuu_wireframe_gif_tiltpan_seq_', iter_num, '.png', sep = ""), width = 800, height = 800)
      par(bg = background_color)
      print(wireframe(unclass(bathymetry), shade = TRUE, aspect = c(1/2, 0.1), xlab = "", ylab = "", zlab = "", distance = 0, screen = list(z = -i, y = 0, x = -70), par.settings = list(axis.line = list(col = "transparent"))))
      dev.off()
      
    ## Animating side pan return
    }else if (i > end_side_pan){
      png(paste('makapuu_wireframe_gif_tiltpan_seq_', iter_num, '.png', sep = ""), width = 800, height = 800)
      par(bg = background_color)
      print(wireframe(unclass(bathymetry), shade = TRUE, aspect = c(1/2, 0.1), xlab = "", ylab = "", zlab = "", distance = 0, screen = list(z = (-240 + i), y = 0, x = -70), par.settings = list(axis.line = list(col = "transparent"))))
      dev.off()
    }
  }
  
  #### Stitching images into GIF
  command = paste("convert -delay .1 -loop 1 *.png ", 'makapuu_bathy', ".gif", sep = "")
  system(command)
  
  #### Removing Image Files from folder
  file.remove(list.files(pattern=".png"))
  
  #### Notifying User
  send_push(user = 'uGEHvA4hr37tsrCCtpSv4sUUxVuTqN', message = "Step The Fuck Aside Pixar, Rendering Complete!")
