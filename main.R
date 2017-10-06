# Main

setwd("~/GitHub/data_cleaning")
source("functions.R")

library(tidyverse)

boo <- read_in(in_data = "~/Google Drive/sensitive_datasets/data_cleaning/All Collar Data.csv",
               raw = T)

boo <- droplevels(boo[!(boo$Project == "Alberta - Boreal Woodland Caribou GPS and VHF Collar Locations 2005 - present"),])

boo2 <- boo %>%
  group_by(Animal.Id, Date) %>%
  filter(n() > 1)

nrow(boo2)
summary(boo2)
