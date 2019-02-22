#### Code for determining the minimum FL for tagging paka using V13 and V13P tags.
#### Written By: Steve Scherrer 
#### Written On: 6 November 2017
#### All Rights Preserved

#### Function uses Uchiyama and Kazama's parameter estimates for Kleiber’s allometric growth function
#### To determine the minimum size of fish that can be tagged given a maximum percentage of the 
#### fish's body weight a tag can equal. 

### Percentage of body weight that a tag must be less than
percent_threshold = 2 # 2%

### Scaling parameters from Uchyama and Kazama 2003
a = 38.7175
b = 0.34481

### Weight of each tag
V13_weight  = 10.2024 # g
V13P_weight = 12.7698 # g

### Calculating minimum fish weight
min_weight_v13  = 10.2024 / (percent_threshold/100) / 1000
min_weight_v13p = 12.7698 / (percent_threshold/100) / 1000

### Converting weight to FL
weight_to_fl = function(X, a, b){
  return(a * X ^ b)
}

min_paka_fl_for_v13  = weight_to_fl(X = min_weight_v13, a, b)
min_paka_fl_for_v13p = weight_to_fl(X = min_weight_v13p, a, b)

print(paste('The smallest fish that can be tagged with a V13 tag is', floor(min_paka_fl_for_v13), 'cm'))
  # "The smallest fish that can be tagged with a V13 tag is 30 cm"

print(paste('The smallest fish that can be tagged with a V13P tag is', floor(min_paka_fl_for_v13p), 'cm'))
  # "The smallest fish that can be tagged with a V13P tag is 33 cm"