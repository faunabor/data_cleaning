# Main

setwd("~/GitHub/data_cleaning")
source("functions.R")

library(tidyverse)

# Read in the data
boo <- read_in_and_parse_dates(in_data = "~/Google Drive/sensitive_datasets/data_cleaning/All Collar Data.csv",
               raw = T)

# Reduce the dataset to only the animals of interest
boo <- filter_dataset_to_id_naming_pattern(collar_data = boo, Animal_Id_pattern = "BWCA-WL-17")

# Remove bad location data from collar data from the API input
boo <- remove_bad_location_class(collar_data = boo)

# Remove both full row duplicates and duplicates of c(id, datetime, lat, long).
boo <- remove_duplicate_rows(collar_data = boo)

# Remove any locations that are greater than the offset time (in seconds) from 
# an expected return time.
## This function has a bunch of assumption, will break with animals that have:
## 1) multiple return intervals over their collared lifespan
## 2) a return interval that cycles over a daily period and starts at any time
##    other than at 0:00.
bbak <- boo
boo <- bbak
boo <- remove_off_time_locations(collar_data = boo, offset_value = 99999, keep_extra_fields = T)

# Remove any duplicate locations (select the most recent over older ones)
## show distribution of offset_values
library(ggplot2)
ggplot(filter(boo, Animal.Id == "BWCA-WL-17-01"), aes(x = Date, y = offset)) +
  geom_point()
ggplot(filter(boo, Animal.Id == "BWCA-WL-17-01"), aes(x = Longitude, y = Latitude)) +
  geom_point(aes(size = offset > 500, colour = offset > 500))

## are there any duplicate locations?
dup_locs <- boo %>%
  group_by(Animal.Id, Latitude, Longitude) %>%
  summarize(number = n()) %>%
  filter(number > 1)
length(unique(dup_locs$Animal.Id))

## Yes.

## Investiage an example or two
bbak <- boo
boo <- droplevels(filter(boo, Animal.Id %in% dup_locs$Animal.Id))
boo <- droplevels(filter(boo, Animal.Id == "BWCA-WL-17-02"))





# Remove any duplicate time, different locations

# Add coordinates in canada albers equal area conic projection (default, can
# be changed).
boo <- add_projected_coordinates(collar_data = boo)

# Compute the statistics associated with the animal trajectories (distance
# moved, time between points, turning angles, etc).
booLT <- create_trajectory(collar_data = boo, output = "dataframe")

# Remove any large movements or spikes from the collar data (WIP, needs to be
# better parameterized).
booLT <- remove_large_movements_and_spikes(collar_data = booLT)


