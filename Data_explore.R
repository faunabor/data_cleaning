# Data Cleaning - B. Fournier 2017

# Read in the data
BGC <- read_in_and_parse_dates("H:\\BONNIE\\DataCleaning\\Data\\All Collar Data.csv")
class(BGC)
names(BGC)
summary(BGC)

tpf <- read.csv("H:\\BONNIE\\DataCleaning\\Data\\tpf_schedules_BGCA.csv")
class(tpf)
names(tpf)
summary(tpf)
View(tpf)
tpf$AnimalId
unique(tpf$AnimalId)
First_an <- unique(tpf$AnimalId)[1]
class(First_an)
as.character(First_an)
First_an <- as.character(First_an)


BGC[BGC$Animal.Id == First_an,]
BGC_1 <- BGC[BGC$Animal.Id == First_an,]
summary(BGC_1)
droplevels(BGC_1)
summary(droplevels(BGC_1))

plot(BGC_1$Longitude, BGC_1$Latitude)
