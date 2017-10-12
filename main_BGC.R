# main BGC

setwd("~/GitHub/data_cleaning")
source("functions.R")

library(tidyverse)

# Read in the data
boo <- read_in_and_parse_dates(in_data = "~/Google Drive/sensitive_datasets/data_cleaning/All Collar Data_BGC.csv",
                               raw = T)

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

# explore the offtime locations in BGC
## Explore the distribution of offset values
ggplot(boo, aes(x = Date, y = offset)) +
  geom_point(aes(colour = as.factor(mode)))