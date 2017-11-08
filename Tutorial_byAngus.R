# Data Cleaning - B. Fournier 2017
library(tidyverse)
# Read in the data
BGC <- read_in_and_parse_dates("H:\\BONNIE\\DataCleaning\\Data\\All Collar Data.csv")

# Read in the tpf data
tpf <- read.csv("H:\\BONNIE\\DataCleaning\\Data\\tpf_schedules_BGCA.csv")

# Pull out the first unique animal ID from the tpf dataset, and make it a character.
First_an <- as.character(unique(tpf$AnimalId)[1])

# Subset the BGC dataset to only "First_an", and remove unused factor levels.
BGC_1 <- droplevels(BGC[BGC$Animal.Id == First_an,])

# Plot out the x y data to get a quick look at it (does it make sense?).
plot(BGC_1$Longitude, BGC_1$Latitude)

# Remove bad location class data from the subset
BGC_2 <- remove_bad_location_class(BGC_1)

# plot BGC_1 and BGC_2 to show the change in the data
plot(BGC_1$Longitude, BGC_1$Latitude, col = "red")
points(BGC_2$Longitude, BGC_2$Latitude)

# subset tpf to the animal of interest (First_an).
tpf_1 <- tpf[tpf$AnimalId == First_an,]
tpf_1_p1 <- tpf_1[2,]

#subset BGC_2 to only period 1 (tpf_1_p1)
start_date <- as.character(tpf_1_p1$StartDate)
end_date <- as.character(tpf_1_p1$EndDate)
library(lubridate)
start_date <- mdy(start_date)
end_date <- mdy(end_date)

boo <- BGC_2[BGC_2$Date >= start_date & BGC_2$Date <= end_date,]

boo %>%
  group_by(date = date(Date)) %>%
  summarise(num = n()) %>%
  filter(num > 3)
