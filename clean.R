library(tidyverse)
library(caret)
library(lubridate)

setwd('~/Dropbox/CurrentClasses/PublicSpaceResearchTutorial/Analysis/HalloweenFarmersMarket')

data <- read_csv('data/bicyclist.csv') %>%
  rename_all(funs(make.names(.))) %>%
  select(-X19) # legacy

View(data)

get_lat <- function(lat, location) {
  num_obs <- length(lat)
  parsed_lats <- vector(length=num_obs)
  for (i in 1:num_obs) {
    parsed_lat <- parse_lat(lat[i])
    if (is.na(parsed_lat)) {
      parsed_lat <- parse_lat(location[i])
    }
    parsed_lats[i] <- parsed_lat
  }
  parsed_lats
}

get_long <- function(long, location, misentered_location) {
  num_obs <- length(long)
  parsed_longs <- vector(length=num_obs)
  for (i in 1:num_obs) {
    parsed_long <- parse_long(long[i])
    if (is.na(parsed_long)) {
      parsed_long <- parse_long(location[i])
    }
    # Location data was entered in wrong field
    if (is.na(parsed_long)) {
      parsed_long <- parse_long(misentered_location[i])
    }
    # Long was entered incorrectly
    tol <- 1e-5
    incorrect_long <- 82.54179
    is_incorrect_long <- abs(parsed_long - incorrect_long) <= tol
    if (is_incorrect_long) {
      parsed_long = -parsed_long
    }
    parsed_longs[i] <- parsed_long
  }
  parsed_longs
}

is_lat_number <- function(guess) {
  is_number <- !is.na(suppressWarnings(as.numeric(guess)))
  if (is_number) {
    as.numeric(guess)
  }
  else {
    NA
  }
}

parse_lat <- function(guess) {
  lat <- is_lat_number(guess)
  if (!is.na(lat)) {
    return(lat)
  }
  
  parsed_lat <- gsub('\\(|\\)|,.*', '', guess)
  as.numeric(parsed_lat)
}

is_long_number <- function(guess) {
  is_lat_number(guess)
}

parse_long <- function(guess) {
  long <- is_long_number(guess)
  if (!is.na(long)) {
    return(long)
  }
  
  parsed_long <- gsub('\\(|\\)|.*,\\s', '', guess)
  as.numeric(parsed_long)
}

get_timestamp <- function(timestamps) {
  lapply(timestamps, function(timestamp) {
    format(ymd_hms(timestamp), '%H:%M:%S')
  }) %>% unlist()
}

cleaned_data <- data %>%
 # select(-Timestamp) %>%
  mutate(Timestamp = get_timestamp(Timestamp)) %>%
  mutate(Location..long. = ifelse(!is.na(X18), X18, Location..long.)) %>% # legacy
  select(-X18) %>%
  mutate(Latitude = get_lat(Location..lat., Location..lat..long.)) %>%
  mutate(Longitude = get_long(Location..long., Location..lat..long., Location..lat.)) %>%
  select(-c(Location..lat., Location..long., Location..lat..long.)) %>%
  mutate(Helmet = ifelse(Wearing.helmet. == 'Yes', 1, 0)) %>%
  select(-Wearing.helmet.) %>%
  mutate(Walking.or.riding = ifelse(is.na(Walking.or.riding), 'Riding', Walking.or.riding)) %>%
  select(c(Timestamp,
           Time.of.observation,
           Longitude,
           Latitude,
           Helmet,
           Walking.or.riding,
           Personal.or.tourist.bike.,
           Outfit.,
           Age.,
           In.a.group.,
           Crossing.road.,
           Going.with.or.against.traffic.,
           In.road.,
           In.bike.lane.,
           On.sidewalk.))

View(cleaned_data)

observation_times <- cleaned_data %>%
  select(Timestamp, Time.of.observation) %>%
  rename(TimeOfObservation = Time.of.observation,
         TimeOfRecording = Timestamp) %>%
  drop_na()

View(observation_times)
write_csv(observation_times, path = 'data/times.csv')

cleaned_data <- cleaned_data %>%
  select(-c(Timestamp, Time.of.observation))

row_to_remove <- cleaned_data %>%
  filter(Age. == '0-12;31-45')
# Seperate family into individual obs
family_rows <- row_to_remove[rep(1, 3), 1:dim(row_to_remove)[2]] %>%
  mutate(Age. = c('0-12', '31-45', '31-45'))

cleaned_data.with_family <- cleaned_data %>%
  anti_join(row_to_remove) %>%
  bind_rows(family_rows) %>%
  mutate(Age. = str_extract(Age., '[:digit:][:digit:]?[-+]?[:digit:]?[:digit:]?'))
  
View(cleaned_data.with_family)

hot_encoder <- dummyVars(~ Walking.or.riding + Personal.or.tourist.bike. + Outfit. + Age. + In.a.group. + Crossing.road. + Going.with.or.against.traffic. + In.road. + In.bike.lane. + On.sidewalk., data = cleaned_data.with_family)
hot_encoded <- as.tibble(predict(hot_encoder, cleaned_data.with_family))

View(hot_encoded)

# Behavior
# Bike type
# Rider type
# Location
cleaned_hot_encoded <- hot_encoded %>%
  rename_all(funs(make.names(.))) %>%
  bind_cols(cleaned_data.with_family) %>%
  rename(BehaviorRiding      = Walking.or.ridingRiding,
         BehaviorWalking     = Walking.or.ridingWalking,
         BehaviorGroupSingle = In.a.group.No,
         BehaviorGroupSmall  = In.a.group.Small.group..2.3.,
         BehaviorCrossing    = Crossing.road.Yes,
         BehaviorWithTraffic = Going.with.or.against.traffic.With,
         BikePersonal        = Personal.or.tourist.bike.Personal,
         BikeTourist         = Personal.or.tourist.bike.Tourist,
         RiderHelmet         = Helmet,
         RiderOutfitBusiness = Outfit.Business,
         RiderOutfitCasual   = Outfit.Casual,
         RiderOutfitCycling  = Outfit.Cycling.Apparel,
         RiderOutfitOther    = Outfit.wearing.neon.vest,
         RiderAge0to12       = Age.0.12,
         RiderAge18to30      = Age.18.30,
         RiderAge31to45      = Age.31.45,
         RiderAge55          = Age.55.,
         LocationRoad        = In.road.Yes,
         LocationBikeLane    = In.bike.lane.Yes,
         LocationNoBikeLane  = In.bike.lane.No.bike.lane,
         LocationSidewalk    = On.sidewalk.Yes,
         LocationNoSidewalk  = On.sidewalk.No.sidewalk,
         LocationLatitude    = Latitude,
         LocationLongitude   = Longitude) %>%
  select(c(BehaviorRiding,      
           BehaviorWalking,    
           BehaviorGroupSingle, 
           BehaviorGroupSmall,  
           BehaviorCrossing,   
           BehaviorWithTraffic, 
           BikePersonal,      
           BikeTourist,
           RiderHelmet,
           RiderOutfitBusiness,
           RiderOutfitCasual,   
           RiderOutfitCycling,  
           RiderOutfitOther,   
           RiderAge0to12,      
           RiderAge18to30,     
           RiderAge31to45,     
           RiderAge55,        
           LocationRoad,      
           LocationBikeLane,   
           LocationNoBikeLane, 
           LocationSidewalk,   
           LocationNoSidewalk,
           LocationLatitude,
           LocationLongitude)) %>%
  drop_na()

View(cleaned_hot_encoded)

write_csv(cleaned_hot_encoded, path = 'data/bicyclist_cleaned.csv')
