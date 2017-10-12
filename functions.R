# Functions

read_in_and_parse_dates <- function(in_data, raw = T){
  # FCT import and clean All_Collared_Animal_Locations
  # This function reads in collar data drawn from the BI launch pad report
  # "All_Collared_Animal_Locations", based on the default selection of checks
  # in the report.
  library(tidyverse)
  library(lubridate)
  library(stringr)
  library(sp)
  

  
  if (raw){
    # Read in data (assumes hasn't been opened in excel, and the top three empty rows
    # are still present).
    headers = read.csv(in_data, skip = 3, header = F, nrows = 1, as.is = T)
    headers <- gsub(" ", ".", headers)
    temp = read.csv(in_data, skip = 4, header = F)
    colnames(temp)= headers
    
    
    # Parse the dates
    temp$Date <- ymd_hms(temp$TXT.Date)
    
    # Remove blank rows (not sure why they're introduced).
    temp <- droplevels(temp[!is.na(temp$Date),])
  } else{
    # Read in data (assumes that the data has been opened, top three rows were
    # removed and th)
    temp <- read.csv(in_data, header = T, sep = ",")
    
    # Parse the dates
    temp$Date <- mdy_hms(temp$TXT.Date) # MAY BE INCORRECT GOING FROM MEMORY HERE.
  }
  
  return(temp)
}

filter_dataset_to_id_naming_pattern <- function(collar_data, Animal_Id_pattern = NULL){
  temp <- collar_data
  # Filter to the specified naming pattern if provided.
  if (!is.null(Animal_Id_pattern)){
    temp <- temp[str_detect(string = temp$Animal.Id, pattern = Animal_Id_pattern),]
    temp <- droplevels(temp)
  }
  return(temp)
}

remove_bad_location_class <- function(collar_data){
  temp <- collar_data
  # Parse out the bad Location.Class from API data
  temp1 <- temp[temp$Dataset == "Api Data" & temp$Location.Class == "G",]
  temp <- temp[!(temp$Dataset == "Api Data"),]
  temp <- bind_rows(temp, temp1)
  temp <- droplevels(temp)
  return(temp)
}

remove_duplicate_rows <- function(collar_data){
  temp <- collar_data
  # remove duplicates
  ## remove full row duplicates
  temp <- temp[!duplicated(temp),]
  ## remove duplicates of subset of fields (Animal.Id, Latitude, Longitude, TXT.Date)
  temp <- temp[!duplicated(temp[,c("Animal.Id", "Latitude", "Longitude", "Date")]),]
  return(temp)
}

remove_off_time_locations <- function(collar_data, offset_value, keep_extra_fields = F){
  # Determine the on time locations (Won't capture points that return at the
  # mid-point of the day). Needs some more work.
  # Currently assumes any point schedule starts at 0:00. Not necessarily true.
  get_return_interval <- function(collar_data){
    library(tidyverse)
    library(lubridate)
    temp <- collar_data
    getMode <- function(x, method = "one", na.rm = FALSE) {
      # modified from https://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode/38097776#38097776
      x <- unlist(x)
      if (na.rm) {
        x <- x[!is.na(x)]
      }
      
      # Get unique values
      ux <- unique(x)
      n <- length(ux)
      
      # Get frequencies of all unique values
      frequencies <- tabulate(match(x, ux))
      modes <- frequencies == max(frequencies)
      
      # Determine number of modes
      nmodes <- sum(modes)
      # nmodes <- ifelse(nmodes==n, 0L, nmodes) # removed to allow for a "mode" from a single value vector. May cause weirdness.
      
      if (method %in% c("one", "mode", "") | is.na(method)) {
        # Return NA if not exactly one mode, else return the mode
        if (nmodes != 1) {
          return(NA)
        } else {
          return(ux[which(modes)])
        }
      } else if (method %in% c("n", "nmodes")) {
        # Return the number of modes
        return(nmodes)
      } else if (method %in% c("all", "modes")) {
        # Return NA if no modes exist, else return all modes
        if (nmodes > 0) {
          return(ux[which(modes)])
        } else {
          return(NA)
        }
      }
      warning("Warning: method not recognised.  Valid methods are 'one'/'mode' [default], 'n'/'nmodes' and 'all'/'modes'")
    }
    temp <- temp %>%
      group_by(Animal.Id, date = date(Date)) %>%
      summarise(nday = n()) %>%
      summarise(mode = getMode(nday)) %>%
      mutate(mode = 86400 / mode) %>%
      left_join(collar_data, ., by = "Animal.Id")
    temp2 <- temp %>%
      group_by(Animal.Id, date = date(Date)) %>%
      summarize()
    return(temp)
  }
  temp <- get_return_interval(collar_data = collar_data)
  
  # Remove locations that are more than 60 seconds from an expected time.
  temp <- temp %>%
    mutate(sec_from_midnight = as.numeric(difftime(temp$Date, as_datetime(date(temp$Date)), units = "secs")))
  ## Form of next bit is:
  ## min(seconds_from_midnight, min(seconds_from_midnight %% mode, abs((seconds_from_midnight %% mode) - seconds_from_midnight)))
  temp$offset <- apply(temp[,c("sec_from_midnight", "mode")], 1,
                       function(x) min(x[1], min(x[1] %% x[2], abs((x[1] %% x[2]) - x[2]))))
  # If the offset is greater than "offset_value" then remove that location.
  temp <- temp[temp$offset < offset_value,]
  temp <- droplevels(temp)
  
  if (!keep_extra_fields){
    temp <- select(temp, -c(mode, sec_from_midnight, offset))
    return(temp)
  }
  if (keep_extra_fields){
    return(temp)
  }
}

add_projected_coordinates <- function(collar_data, coordinate_reference = "+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"){
  # Defaults to Canada Albers Equal Area Conic
  library(tidyverse)
  library(sp)
  
  temp <- collar_data
  
  # Add fields for the x and y coordinates that are in the projected coordinate system provided.
  tempSP <- SpatialPoints(coords = temp[,c("Longitude", "Latitude")],
                          proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
  tempSP <- spTransform(x = tempSP, 
                        CRSobj = CRS(coordinate_reference))
  temp$LongitudeProj <- tempSP@coords[,"Longitude"]
  temp$LatitudeProj <- tempSP@coords[,"Latitude"]
  
  return(temp)
}

create_trajectory <- function(collar_data, output = "dataframe"){
  # Get basic movement stats for the dataset
  # Assumes coordinates coming in from CAEAC.
  library(tidyverse)
  library(adehabitatLT)
  temp <- collar_data
  temp <- temp %>%
    group_by(Animal.Id) %>%
    arrange(Date, .by_group = T)
  temp <- SpatialPointsDataFrame(coords = temp[,c("LongitudeProj", "LatitudeProj")],
                                 proj4string = CRS("+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"),
                                 data = temp)
  temp_LT <- as.ltraj(xy = coordinates(temp[,c("LongitudeCAEAC", "LatitudeCAEAC")]), date = temp$Date, id = temp$Animal.Id, burst = temp$Animal.Id)
  
  # Return either an ltraj object or the dataframe version of the output.
  if (output == "ltraj"){
    return(temp_LT)
  }
  if (output == "dataframe"){
    return(ld(temp_LT))
  }
}

remove_large_movements_and_spikes <- function(collar_data, medcrit=100000, meancrit=10000, spikesp=1500, spikecos=(-0.97)){
  # Screen out large movments and spikes (WIP)
  ## From: Bjørneraas, K., Van Moorter, B., Rolandsen, C. M. & Herfindal, I.
  ## Screening Global Positioning System Location Data for Errors Using Animal
  ## Movement Characteristics. J. Wildl. Manage. 74, 1361–1366 (2010).
  ## Modified to update to adehabitatLT (was for adehabitat)
  ## I can't get the function to work for more than 1 id at a time. Looping it by ID for now.
  # https://stackoverflow.com/questions/29402528/append-data-frames-together-in-a-for-loop ## This is the for loop method
  # https://stackoverflow.com/questions/12193779/how-to-write-trycatch-in-r ## This is how to handle errors in the loop.
  GPS.screening <- function(x, medcrit, meancrit, spikesp, spikecos){
    ###round one:
    ###remove points far off:
    RoundOne <- function(x, df, win, MM){
      if (x<=win){
        dfT <- df[c(1:2*win),]
      }
      if (x>(nrow(df)-win)){
        dfT <- df[c((nrow(df)-2*win):(nrow(df))),]
      }
      if (x>win & x<=(nrow(df)-win)){
        dfT <- df[c((x-win):(x+win)),]
      }
      if (MM=="mean"){
        temp <- sqrt((df$x[x]-mean(dfT$x, na.rm=T))^2+(df$y[x]-mean(dfT$y, na.rm=T))^2)
        #calculate the distance between the mean and the focal point
      }
      if (MM=="median"){
        temp <- sqrt((df$x[x]-median(dfT$x, na.rm=T))^2+(df$y[x]-median(dfT$y, na.rm=T))^2)
        #calculate the distance between the median and the focal point
      }
      return(temp)
    }
    
    ##get the points that are really far off, by using the median
    x$R1dmed <- unlist(lapply(c(1:nrow(x)), RoundOne, df=x, win=10, MM="median"))
    
    ##get the points that are rather far off, by using the mean
    x$R1dmean <- meancrit+1
    x$R1dmean[x$R1dmed<medcrit] <- unlist(lapply(c(1:nrow(x[x$R1dmed<medcrit,])),  RoundOne, df=x[x$R1dmed<medcrit,], win=10, MM="mean"))
    x$R1error <- ifelse(x$R1dmean>meancrit, TRUE, FALSE)
    
    ###round two:
    ##find spikes
    xT <- x[x$R1error==FALSE,]
    library(adehabitatLT)
    xT <- as.ltraj(xT[,c("x","y")], date=xT$date, id=xT$id)[[1]]
    
    x$R2error <- NA
    x$R2error[x$R1error==FALSE] <- ((xT$dist/xT$dt*3600)>spikesp & c(NA, (xT$dist/xT$dt*3600)[-nrow(xT)])>spikesp & cos(xT$rel.angle)<(spikecos))
    return(x)
  }
  GPS.screening.wrp <- function(id, x, y, da, medcrit, meancrit, spikesp, spikecos){
    library(adehabitatLT)
    out <- tryCatch(
      {
        mydata <- ld(as.ltraj(id = id, xy = data.frame(x=x,y=y), date = da))
        mydata <- split(mydata, mydata$id)
        mydata <- lapply(mydata, GPS.screening, medcrit=medcrit, meancrit=meancrit, spikesp=spikesp, spikecos=spikecos)
        mydata <- do.call("rbind", mydata)
        data.frame(id=mydata$id, x=mydata$x, y=mydata$y, date=mydata$date, R1dmed=mydata$R1dmed, R1dmean=mydata$R1dmean, R1error=mydata$R1error, R2error=mydata$R2error)
      },
      error=function(cond) {
        message("ERROR: ID didn't clean properly.")
        message(paste("Here's the original error message for", id, ":"))
        message(cond)
        # Choose a return value in case of error
        return(NA)
      },
      warning=function(cond) {
        message(paste("ID caused a warning:", id))
        message("Here's the original warning message:")
        message(cond)
        # Choose a return value in case of warning
        return(NULL)
      }
    )    
    return(out)
  }
  
  datalist = list()
  for (i in 1:length(unique(boo$Animal.Id))){
    tempboo <- boo[boo$Animal.Id == unique(boo$Animal.Id)[i],]
    tempboo <- droplevels(tempboo)
    tempboo <- GPS.screening.wrp(id = tempboo$Animal.Id, x = tempboo$LongitudeCAEAC, y = tempboo$LatitudeCAEAC,
                                 da = tempboo$Date, medcrit=medcrit, meancrit=meancrit, spikesp=spikesp, spikecos=spikecos)
    datalist[[i]] <- tempboo
  }
  # NO ERROR HANDLING AT THIS STEP - if the datalist contains any NAs (in the form of 
  # the original data for some reason) then it will fail.
  big_data <- do.call(rbind, datalist)
  
  # Remove the flagged data
  # REMOVE THIS FOR TROUBLESHOOTING
  big_data <- big_data[big_data$R1error == F & big_data$R2error == F,]
  big_data <- droplevels(big_data)
  big_data <- big_data[,1:4]
  names(big_data) <- c("Animal.Id", "LongitudeCAEAC", "LatitudeCAEAC", "Date")
  big_data <- big_data[!(is.na(big_data$LongitudeCAEAC) | is.na(big_data$LatitudeCAEAC)),]
  big_data <- droplevels(big_data)
  return(big_data)
}