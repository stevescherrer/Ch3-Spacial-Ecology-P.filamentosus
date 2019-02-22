#### Getting tag IDs for One Ocean Diving


#### Importing vue
load_vemco = function(filename, format = '%Y-%m-%d %H:%M:%S', tz = 'HST'){
  #### Loads in a csv datafile exported from VUE and cleans up file for further analysis
  ### Loading in data file
  vue_data = read.csv(filename, stringsAsFactors = FALSE)
  ### Adjusting Column names
  colnames(vue_data)[1]  <- 'datetime'
  colnames(vue_data)[2]  <- 'receiver'
  colnames(vue_data)[3]  <- 'tag_id'
  colnames(vue_data)[4]  <- 'name'
  colnames(vue_data)[5]  <- 'tag_serial'
  colnames(vue_data)[6]  <- 'depth'
  colnames(vue_data)[7]  <- 'sensor.unit'
  colnames(vue_data)[8]  <- 'station'
  colnames(vue_data)[9]  <- 'lat'
  colnames(vue_data)[10] <- 'lon'
  ### Converting datetime to POSIXct Object
  vue_data$datetime = as.POSIXct(vue_data$datetime, format = format)
  attr(vue_data$datetime, "tzone") = tz
  ### Cleaning up tag ids - removing the 'A69-####-' prefix
  vue_data$full_tag_id = vue_data$tag_id
  vue_data$tag_id = substring(vue_data$tag_id, 10)
  ### Cleaning up receiver - remvoing the 'VR2W-' Prefix
  if (substr(vue_data$receiver[1], 1, 5) == 'VR2W-'){
    vue_data$receiver = substring(vue_data$receiver, 6)
  }
  return (vue_data)
}


all_vemco_data = load_vemco("/Users/stephenscherrer/Google Drive/Weng Lab/Data/Bottomfish/Oahu Receiver Data Files/VUE_Export_2017_June_25.csv" )

##### Subsetting tags for Ocean
one_ocean_tag_ids = c(20996:20999, 24641, 24644, 24646, 24652:24656, 28326, 28328:28331, 28333:28336, 28343, 47128, 53205:53213, 53898:53917, 62813:62816, 6180, 6181, 6184, 6185, 8018, 8019)
ocean_data = all_vemco_data[all_vemco_data$tag_id %in% one_ocean_tag_ids, ]
write.csv(ocean_data, 'vue_data_for_ocean.csv', row.names = FALSE)


####