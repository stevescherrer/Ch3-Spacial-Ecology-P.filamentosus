##### Getting the distance between deployed fence nodes from deployment positions
#### Written by Stephen Scherrer, July 5 2017

#### Setup workspace
### Clearning all workspace data
rm(list=ls()) # Clear workspace
script_timer = proc.time()

### Sourcing packages and R function scripts
## Sourcing range testing results to use predicted rates from deep water range testing
load('/Users/stephenscherrer/Google Drive/Weng Lab/Manuscripts/Scherrer - CPDI Model Paper/CPDI Submission Repository/Results/completed_script.R')
## Sourcing general R functions
source('/Users/stephenscherrer/Google Drive/Weng Lab/Code_Library/R_code/vemcoUtilityFunctions.R')


#### Load and clean data files
### Loading in receiver data
receiver_data = load_receiver_data('/Users/stephenscherrer/Google Drive/Weng Lab/Data/Bottomfish/Oahu Receiver Data Files/DEPLOYMENT_RECOVERY_LOG.csv')

### Date to perform analysis over
snapshot_date = as.POSIXct('2017-07-01')
receiver_data = receiver_data[which(receiver_data$deployment_date <= snapshot_date & (receiver_data$recovery_date >= snapshot_date | is.na(receiver_data$recovery_date))), ] # Removing receivers not in the water at the snapshot date
receiver_data = receiver_data[which(is.na(receiver_data$recovered)), ] # Removing any receivers that were lost

### Creating a function to get the distance (in KM) between subsequent receiver stations.
calculate_sequential_receiver_distances = function(station_names, receiver_data = receiver_data){
  # Inputs: 
    # station_names: A vector of station names corrosponding to the receivers in the fence (to be used for subsetting)
    # receiver_data: Receiver data frame
  # Outputs:
    # Prints - mean, std deviation, and 5 number summary
    # returns - a vector of distances (in km) between subsequent stations (ordered by longitude)
receiver_subset = receiver_data[which(receiver_data$station_name %in% station_names), ] # Subestting receivers
receiver_subset = receiver_subset[order(receiver_subset$lon), ]
distance_between_receivers = c()
for(i in 2:length(receiver_subset$lon)){
  distance_between_receivers = c(distance_between_receivers, lldist(p1 = c(receiver_subset$lon[i], receiver_subset$lat[i]), p2 = c(receiver_subset$lon[i-1], receiver_subset$lat[i-1])))
}
report_stats(distance_between_receivers)
return(distance_between_receivers)
}

### Fence 1: Outside South
station_names_outside_south = paste('Oahu - Makapuu BRFA', 311:320)
outside_south_distances = calculate_sequential_receiver_distances(station_names = station_names_outside_south, receiver_data = receiver_data)

### Fence 2: Inside South
station_names_inside_south = paste('Oahu - Makapuu BRFA', 334:341)
inside_south_distances = calculate_sequential_receiver_distances(station_names_inside_south, receiver_data = receiver_data)

### Fence 3: Inside North
station_names_inside_north = paste('Oahu - Makapuu BRFA', 331:333)
inside_north_distances = calculate_sequential_receiver_distances(station_names_inside_north, receiver_data = receiver_data)
  
### Fence 4: Outside North
station_names_outside_north = paste('Oahu - Makapuu BRFA', 2:5)
outside_north_distances = calculate_sequential_receiver_distances(station_names_outside_north, receiver_data = receiver_data)

### All fences combined
all_fence_distances = c(outside_south_distances, inside_south_distances, inside_north_distances, outside_north_distances)
report_stats(all_fence_distances)

#### Check distances against range test data/fence parameters
## Converting distances to m
all_fence_distances_m = all_fence_distances * 1000
## Getting the radius of overlap between a receiver and the next receiver to determine distance between them
## Defining minimum detection rate as the intersection between two receivers at a 45% angle
receiver_adjacent = ceiling(all_fence_distances_m / 2)
max(receiver_adjacent) # The maximum distance between any two receivers in a fence is 524 m

## A matrix where the rows are model predictions from each candidate model and the columns are the distances between receivers
model_prediction_matrix = matrix(ncol = length(receiver_adjacent), nrow = length(candidate_model_predictions_1))
for(i in 1:length(candidate_model_predictions_1)){
  model_prediction_matrix[i, ] = candidate_model_predictions_1[[i]]$predicted_rates[receiver_adjacent]
}

## Minimum detection % between receivers
minimum_predicted_rates = apply(model_prediction_matrix, 2, min)
min(minimum_predicted_rates)

cat('The mimimum predicted detection efficiency for adjacent fence receivers is', round(min(minimum_predicted_rates), digits = 2),  '%. \n \nCompare to our cutoff threshold of 12.5% used to construct a fence with a 25% chance of detection.')

cat(paste('Infact, the lowest chance of detection is', round(min(minimum_predicted_rates), digits = 2) * 2, '%'))

cat('There are', length(which(isTRUE(min(minimum_predicted_rates) < .125))), 'gaps in the fences')
### The minimum predicted rate between any two receiver nodes was 15.855.
### This is better than our cuttoff threshold of 12.5% to construct at 25% receiver fence


