#!/usr/bin/env Rscript

# 3. Air and water temperature retrieval, conversion, and bias examination.R
#    Licence: GNU GPLv3 - see LICENSE.txt for more details

#    Various functions to perform the air to water temperature conversion
#    necessary to turn GCM projected air temperatures into something that 
#    can be used with the young-of-year life-history model & simulations

#    Also assessing if there is a potential bias in the GCM projections
#    for years 2015-2025 (extent of historical GCM projections)


# INFORMATION
#﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋

# website for USGS gaging data: 
# https://waterdata.usgs.gov/monitoring-location/USGS-<STATION ID>

# website for R package to retrieve this data:
# https://doi-usgs.github.io/dataRetrieval/articles/tutorial.html

# 0. initalize the various functions used in subsequent analyses 

# 1. get air & water temperature data for each tributary 
 
# 2. check: compare air temperatures from weather stations VS GCM projections

# 3. check: compare water temperatures from gages VS converted GCM projections


# ---- STEP ZERO ---------------------------------------------------------------

# load libraries
library(magrittr)      # for tidyverse-style pipes
library(reshape2)      # for melting/casting data

library(canadaHCDx)    # canada weather station data
library(rnoaa)         # USA weather station data
library(dataRetrieval) # USA water gage data


# Initialize functions
#﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋

# function to convert julian (ordinal) date to gregorian for interpretation
juliGreg <- function(jday, year=71){

    # a fix for years 2000-2009
    # because you can't store "07" as a value, it just becomes "7", for instance
    if(year<10){
        year <- paste0("0",as.character(year))
    }

    # get the string representing the date, then return gregorian
    fullDate <- as.character(paste0(1,year,jday))
    return(as.Date(fullDate, "1%y%j"))
}


# function to interpolate temperature for Feb 29th if needed/missing
# (I think the new date fill method makes this obselete)
addLeap <- function(temp.vector){

  # first off, make a working copy
  series <- temp.vector

  # check if we're missing a day 
  if(length(series) == 365) {

    # average the temperatures on Feb 28th and Mar 1st
    # (ordinal dates 59 and 60)
    leapDay <- (series[59] + series[60])/2

    # and insert that day into the series 
    series <- c(series[1:59], leapDay, series[-(1:59)]) 

  } else {

    # abort and return error message
    stop("Temperature series is not 365 days long")
  }#/ifelse

  # return the augmented temperature series 
  return(series)
}#/function


# function to fill missing days from imputing values from the same day in other years
impute <- function(dataset, visualCheck = 0){

  # get all row positions where there is an NA
  indexNA <- which(dataset$temp %>% is.na)

  # set up a column that marks if the temperature needs to be imputed 
  dataset <- cbind(dataset, impute = 0)

  # loop over each row with an NA and fill in the missing temperature data
  for (index in indexNA) {

    # get the ordinal day of that NA
    day <- dataset[index, "j.date"]

    # double check if there's actually an NA that should be overwritten 
    if (dataset[index,"temp"] %>% is.na) {

      # impute the average temperature from that date on other non-NA years 
      dataset[index,"temp"] <- subset(dataset, j.date == day)$temp %>% mean(na.rm = T)

      # mark that it's been edited via imputation
      dataset[index,"impute"] <- 1

    } #\if statement
  } #\loop

  if (visualCheck == 1) {

    # loop to plot each year's temperatures for visual inspection of imputed values 
    for (annum in dataset$year |> unique()) {

      # plot the temperature series over the year
      plot(temp~j.date, subset(dataset, year == annum), main = annum, pch = 20, ylim = c(0,30))

      # highlight the imputted values so you can see if they look reasonable 
      points(temp~j.date, subset(dataset, year == annum & impute == 1), pch = 19, col = "green")
    }#\loop

  }#\if

  # having passed inspection, we can trim off the impute column 
  dataset <- dataset[,-4]

  # return the modified dataset
  return(dataset)

}#\function


#      Temperature time series data cleaning functions (date formatting)
#﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋

# for time series with missing dates, this gives you NAs for those days 
dateFill <- function(dataset, startDate, endDate){

  # make an empty vector of the full range of dates desired 
  template <- data.frame(date = seq(as.Date(startDate),
                                    as.Date(endDate),
                                    by="days"))

  # outer join fills in corresponding data (requires "date" column)
  output <- merge(template, dataset, by = "date", all = T)

  # return the merged data
  return(output)

}#\function


# turns data frames' datestrings into year & ordinal date columns
add.jdate <- function(input){

  # sandwich the inputted dataset between new j.date and year columns
  dataset <- cbind(j.date = 0, 
                   input, 
                   year = input$date %>% format("%Y") %>% as.numeric)

  # loop over each year, add the right number of j-days 
  for (year in unique(dataset$year)) {

    # get all rows for a given year
    index <- dataset$year == year 

    # assign ordinal dates spanning the length of year 
    dataset$j.date[index]  <- (1:sum(index))

  }#\loop

  # remove the old date column 
  dataset <- dataset[,-2]

  # output the amended dataset
  return(dataset)

}#\function


# NOAA weather station air temperature data processing functions
#﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋

# takes 1 month-row from NOAA data and extracts only the daily temperatures 
rowGrab <- function(dataset.row){

  # the NOAA data has temperature values for day 1 of the month as "VALUE1"
  daily.temps <- dataset.row[ c("VALUE1", "VALUE2", "VALUE3", "VALUE4", 
                                "VALUE5", "VALUE6", "VALUE7", "VALUE8", 
                                "VALUE9", "VALUE10", "VALUE11", "VALUE12", 
                                "VALUE13", "VALUE14", "VALUE15", "VALUE16",
                                "VALUE17", "VALUE18", "VALUE19", "VALUE20",
                                "VALUE21", "VALUE22", "VALUE23", "VALUE24",
                                "VALUE25", "VALUE26", "VALUE27", "VALUE28",
                                "VALUE29", "VALUE30", "VALUE31") ] 

  return(daily.temps)
}


# processes multiple month-rows from NOAA dataset into rows of daily mean air temperature 
airGrab <- function(dataset, type = "avg"){

  # set up a container for the processed data
  output <- data.frame()

  # check if we want TAVG or TMIN & TMAX, set up appropriate loop index 
  if (type == "avg") {

    # for TAVG, index every row
    index <- 1:nrow(dataset)

    for (eachrow in index) {

      # extract date information
      year <- dataset[eachrow,][c("year")] %>% unlist %>% as.vector
      month <- dataset[eachrow,][c("month")] %>% unlist %>% as.vector

      # make a vector of dates
      dates <- paste(year, month, 1:31, sep = "-") %>% as.Date

      # extract daily temperatures and perform unit correction
      airTemps <- (dataset[eachrow,] %>% rowGrab)/10

      # put all the information together in a better format (and fix air temps)
      newRows <- airTemps %>% unlist %>% as.vector %>% data.frame(date = dates, temp = .)

      # attach each month's data to the variable to be outputted
      output <- rbind(output, newRows) 

      # remove nonsensical dates (NA values)
      output <- na.omit(output)

    }#\end loop

  } else if (type == "minmax") {

    # for TMIN/TMAX, index every other row
    index <- seq(1, nrow(dataset), 2)

    for (eachrow in index) {

      # extract date information
      year <- dataset[eachrow,][c("year")] %>% unlist %>% as.vector
      month <- dataset[eachrow,][c("month")] %>% unlist %>% as.vector
      
      # make a vector of dates
      dates <- paste(year, month, 1:31, sep = "-") %>% as.Date

      # extract minimum and maximum daily temperatures
      minTemps <- dataset[eachrow,] %>% rowGrab
      maxTemps <- dataset[eachrow+1,] %>% rowGrab

      # average them, then perform unit correction
      airTemps <- ((minTemps + maxTemps)/2)/10

      # put all the information together in a better format (and fix air temps)
      newRows <- airTemps %>% unlist %>% as.vector %>% data.frame(date = dates, temp = .)

      # attach each month's data to the variable to be outputted
      output <- rbind(output, newRows) 
      
      # remove nonsensical dates (NA values)
      output <- na.omit(output)

    }#\end loop

  } else {
    # for anything else give an error message
    cat("Options: \"avg\"    — for variable TAVG
         \"minmax\" — for variables TMIN and TMAX")
  }

  # return the output
  return(output)

}#\end function


# USGS water gage data retrieval function
#﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋

# get daily mean water temperature for a site over a specified date range
gageGrab <- function(siteNo, start.date, end.date){

  # parameter codes for the kind of data we want
  pCode = "00010" # temperature 
  sCode = "00003" # mean 

  # request and retrieve the water gage data from USGS
  output <- read_waterdata_daily(monitoring_location_id = siteNo,
                                 parameter_code = pCode,
                                 statistic_id = sCode,
                                 time = c(start.date, end.date))

  # goodbye unnecessary information (and rename columns)
  output <- data.frame(date = output$time, temp = output$value)

  # return the data
  return(output)
}


# ---- STEP ONE ----------------------------------------------------------------

# get water and air temperatures for:
#  1.0  Thames river
#  1.1  Nipigon river
#  1.2  St Louis river
#  1.3  Genesee river
#  1.4  Vermillion river


#  T H A M E S  R I V E R
# ﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋

# 1.0 THAMES RIVER
#  ⤷ RETRIEVE WATER TEMPERATURES FROM GAGE

# load raw water temperature data from Springbank Dam, London, Ontario
rawData <- read.csv("https://woed-kiwis.thamesriver.on.ca/KiWIS/KiWIS?service=kisters&type=queryServices&request=getTimeseriesValues&datasource=0&format=csv&ts_id=304680042&period=complete", sep = ";")
rawData <- read.csv("../data/tsvalues.csv", sep = ";") # if the above doesn't work

# trim off top two rows of garbled metadata
rawData <- rawData[-c(1:2),] 

# split date column to trim unncessary time tag
rawDays <- do.call(rbind, strsplit(rawData[,1], "T"))

# put together date and temperature 
thames.water <- data.frame(date = rawDays[,1], temp = as.numeric(rawData[,2]))

# NOTE: dataset is already complete with 6110 rows, matches expected length from:
# seq(as.Date("2006-05-10"), as.Date("2023-01-30"), by = "days")
 
    # grab the range of years covered by the water temperature data 
    timeSpan <- strsplit( c( head(thames.water$date, 1), tail(thames.water$date, 1) ), "-")
    
    # isolate the years and make them numeric
    timeSpan <- as.numeric(c(timeSpan[[1]][1], timeSpan[[2]][1]))

# exclude data from before 2015
thames.water <- thames.water[(thames.water$date >= as.Date("2015-01-01")),]

# fix the date column
thames.water$date <- thames.water$date %>% as.Date

# reformat and change to date format to ordinal date
thames.water <- thames.water %>% dateFill(.,"2015-01-01", "2025-12-31") %>% add.jdate


# 1.0 THAMES RIVER
#  ⤷ RETRIEVE AIR TEMPERATURES FROM WEATHER STATION

# use stationID 10999 for London CS/climate station (it has mean air temp)
# retrieve air temperatures for the same years as the water temperatures 

# change the timespan of focus for simple baseline baseline years analysis
timeSpan <- c(2015,2025) 

# retrieve the data
thames.air <- hcd_daily(10999, timeSpan[1]:timeSpan[2])

# discard unnecessary information
thames.air <- subset(thames.air, select = c(Date, MeanTemp))

# standardize column names to match water temperature dataset
names(thames.air) <- c("date", "temp")

# input temperature data into complete date range template
# thames.air <- dateFill(thames.air, "2015-01-01", "2025-12-31")

# change to ordinal date format
thames.air <- thames.air %>% add.jdate


#  N I P I G O N  R I V E R
# ﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋

# 1.1 NIPIGON RIVER
#  ⤷ RETRIEVE WATER TEMPERATURES FROM GAGE

# NOTE: there is only the only DFO SLCC data available for now 

# load nipigon temperature data
nipigon.water <- read.csv('../data/nBaseline2.csv')[,-1]

# no missing data, thus imputation unnecessary in this case 
# nipigon.water <- nipigon.water %>% impute(., 1)


# 1.1 NIPIGON RIVER
#  ⤷ RETRIEVE AIR TEMPERATURES FROM WEATHER STATION

# best: cameron falls aut ID 27674, 17.9 km from nipigon, station #4031, (49.15, -88.34)
find_station(target = c(48.965433, -88.252240), recodes = T)
 
# Download air temp data
nipigon.air <- hcd_daily(27674, 1998:2023)

# isolate columns of interest, coerce to data frame for ease of use 
nipigon.air <- subset(nipigon.air, select = c(Date, MeanTemp)) 

# rename as per naming convention
names(nipigon.air) <- c("date", "temp")

# input temperature data into complete date range template
nipigon.air <- dateFill(nipigon.air, "1998-01-01", "2025-12-31")

# change to ordinal date format
nipigon.air <- nipigon.air %>% add.jdate


#  S T .  L O U I S  R I V E R
# ﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋

# 1.2 ST LOUIS RIVER
#  ⤷ RETRIEVE WATER TEMPERATURES FROM GAGE

# USGS National Water Information System (Station 04024000)
# lat long: 46.7032765 -92.4188018

# retrieve the mean daily water temperature data from the gage
stlouis.water <- gageGrab("USGS-04024000", "2000-01-01", "2025-12-31")

# perform date reformatting
stlouis.water <- stlouis.water %>% dateFill(.,"2011-01-01", "2025-12-31") %>% add.jdate

# trim off the first, incomplete year
stlouis.water <- stlouis.water[-1:-365,]

# perform imputation to fill missing data
stlouis.water <- stlouis.water %>% impute(., 1)

# export csv with complete data from 2012—2025
write.csv(stlouis.water, "../data/sBaseline2.csv", row.names = F )


# 1.2 ST LOUIS RIVER
#  ⤷ RETRIEVE AIR TEMPERATURES FROM WEATHER STATION

# search for suitable weather stations near the gage
stations <- meteo_nearby_stations(data.frame(id = "St. Louis",
                                             latitude = 46.7032765,
                                             longitude = -92.4188018)
)

# USR0000MSAG  SAGINAW MINNESOTA 46.84360 -92.46170 (distance 15.9)
stlouis.data <- ghcnd(stations$"St. Louis"$id[12])

# average daily temperatures
tavg.stlouis <- subset(stlouis.data, year %in% 2015:2025 & element == "TAVG") 

# extract the daily air temperature series 
stlouis.air <- airGrab(tavg.stlouis, "avg") 

    # ALTERNATIVELY:
    # max and min daily temperatures
    minmax.stlouis <- subset(stlouis.data, year %in% 2015:2025 & element %in% c("TMAX","TMIN") )
    
    # NOTE: gives a pretty different result from TAVG, only use it when you must
    stlouis.air2 <- airGrab(minmax.stlouis, "minmax")

# input temperature data into complete date range template
stlouis.air <- dateFill(stlouis.air, "2015-01-01", "2025-12-31")

# change to ordinal date format
stlouis.air <- stlouis.air %>% add.jdate


#  G E N E S E E  R I V E R
# ﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋

# 1.3 GENESEE RIVER
#  ⤷ RETRIEVE WATER TEMPERATURES FROM GAGE

# USGS National Water Information System (Station 04231600)
# lat long: 43.1417222, -77.6163056

# retrieve the mean daily water temperature data from the gage
genesee.water <- gageGrab("USGS-04231600", "2000-01-01", "2025-12-31")

# perform date reformatting
genesee.water <- genesee.water %>% dateFill(.,"2010-01-01", "2025-12-31") %>% add.jdate

# trim off the first, incomplete year
genesee.water <- genesee.water[-1:-365,]

# perform imputation to fill missing data
genesee.water <- genesee.water %>% impute(., 1)

# export csv with complete data from 2011—2025
write.csv(genesee.water, "../data/gBaseline2.csv", row.names = F )


# 1.3 GENESEE RIVER
#  ⤷ RETRIEVE AIR TEMPERATURES FROM WEATHER STATION

# get info on all available stations
stations <- ghcnd_stations()

# Find stations that are close to the genessee river gaging site
subset(stations, latitude == "43.1167" & longitude == "-77.6767" & element == "TAVG")$id
# [1] "USW00014768"

# the only one: Rochester International airport  
rochdat <- ghcnd("USW00014768")

# what years does it have for average temperature?  (1998 to 2005 & 2013-2022)
subset(rochdat, element == "TAVG", year) %>% unique

# average temperatures
tavg.roch <- subset(rochdat, year %in% 2015:2025 & element == "TAVG") 

# extract the daily air temperature series for Genesee 
genesee.air <- airGrab(tavg.roch) 

      # ALTERNATIVELY
      # working with temperature max and min
      minmax.roch <- subset(rochdat, year %in% 2015:2025 & element %in% c("TMAX","TMIN") )
      
      # WARNING: this is pretty different result from TAVG!
      genesee.air2 <- airGrab(minmax.roch, "minmax")

# input temperature data into complete date range template
genesee.air <- dateFill(genesee.air, "2015-01-01", "2025-12-31")

# change to ordinal date format
genesee.air <- genesee.air %>% add.jdate


#  V E R M I L L I O N  R I V E R
# ﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋

# 1.4 VERMILLION RIVER
#  ⤷ RETRIEVE WATER TEMPERATURES FROM GAGE

# USGS National Water (Station 04199500)
# lat long: 41.38199, -82.3168273	

# retrieve the mean daily water temperature data from the gage
vermillion.water <- gageGrab("USGS-04199500", "2010-01-01", "2025-12-31")

# perform date reformatting
vermillion.water <- vermillion.water %>% dateFill(.,"2011-01-01", "2025-12-31") %>% add.jdate

# trim off the first, incomplete year
vermillion.water <- vermillion.water[-1:-365,]

# perform imputation to fill missing data
vermillion.water <- vermillion.water %>% impute(., 1)

# export csv with complete data from 2012—2025
write.csv(vermillion.water, "../data/vBaseline2.csv", row.names = F )

 
# 1.4 VERMILLION RIVER
#  ⤷ RETRIEVE AIR TEMPERATURES FROM WEATHER STATION

# find weather stations nearest to the water gage
stations <- meteo_nearby_stations(data.frame(id = "Vermillion",
                                  latitude = 41.38199,
                                  longitude = -82.3168273)
)

# the best among different stations — #8 USW00004849 Elyria Lorain Co AP (distance:12.1 km) 
# lat long: 41.3461 -82.1794
vermillion.air <- ghcnd( stations$Vermillion$id[8] )

# check available years (1998 to 2023)
vermillion.air$year %>% unique

# check available data types (there is no mean temperature, only min and max)
vermillion.air$element %>% unique

# isolate years of interest and variables of interest 
vermillion.air <- subset(vermillion.air, year %in% 2015:2025 & element %in% c("TMAX","TMIN"))

# extract daily air temperatures 
vermillion.air <- airGrab(vermillion.air, "minmax")

# input temperature data into complete date range template
vermillion.air <- dateFill(vermillion.air, "2015-01-01", "2025-12-31")

# change to ordinal date format
vermillion.air <- vermillion.air %>% add.jdate


# D A T A  C O M B I N A T I O N
# ﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋

#  A I R  D A T A
air <- data.frame(j.date = thames.air$j.date,
                  thames = thames.air[,2],
                 nipigon = nipigon.air[,2],
                 stlouis = stlouis.air[,2],
                 genesee = genesee.air[,2],
              vermillion = vermillion.air[,2],
                    year = thames.air$year)

#  W A T E R  D A T A
water <- data.frame(j.date = thames.water$j.date,
                    thames = thames.water[,2],
                   nipigon = NA,
                   stlouis = stlouis.water[,2],
                   genesee = genesee.water[,2],
                vermillion = vermillion.water[,2],
                      year = vermillion.water$year)



# export these if desired
#write.csv(air, "../data/air.biasCheck.csv")
#write.csv(water, "../data/water.biasCheck.csv")


# ---- STEP TWO ----------------------------------------------------------------

# bias check: compare weather station VS GCM projected air temperature data

# I M P O R T  &  F O R M A T :  C L I M A T E  P R O J E C T I O N  D A T A 
# ﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋

# NOTE: code below temporarily borrowed from script #5:

# function to process that long gcm data into wide
format.gcm <- function(gcm.data){

  # vector of names to identify each climate model
  gcm.names <- unique(gcm.data$gcm)

  # vector of names to identify each tributary 
  trib.names <- unique(gcm.data$tributary)

  # prepare list to hold gcm data ordered by tributary
  trib.data <- sapply(trib.names, function(x) NULL)

  # order temperature series by tributary, each containing 5 gcms 
  for ( name in trib.names ){

    # subset data for one tributary at a time
    subData <- subset(gcm.data, 
                      tributary == name, 
                      select = c(year, j.date, gcm, tas))

    # cast the data into a wide shape
    castData <- dcast(subData, 
                      year + j.date ~ factor(gcm, levels = unique(gcm)), 
                      value.var = "tas")                 

    # put the data in its place within the list
    trib.data[[name]] <- castData 

  }

  # return the output
  return(trib.data)

}

# load the GCM data for all tributaries
gcm.long <- read.csv("../data/gcmOutputs.csv")

# reformat the data into a user-friendly wide format
gcm.air <- format.gcm(gcm.long)


#  A I R  C O M P A R I S O N S
# ﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋

# subset each tributary for individual comparisons with historical data 
 nip <- gcm.air$nip  %>% subset(., year %in% 2015:2025)
slou <- gcm.air$slou %>% subset(., year %in% 2015:2025)
 gen <- gcm.air$gen  %>% subset(., year %in% 2015:2025)
verm <- gcm.air$verm %>% subset(., year %in% 2015:2025)

#      ___________________________________

# PLOTTING GCM MEAN VERSUS WEATHER STATION DATA

# set up multiplot
par(mfrow = c(2,2),
    mgp = c(3,0.5,0))
# plot nipigon comparison
par(mar = c(0.5,3,3.5,0.5))
plot(1:4018, air$nipigon, col = "blue", pch = 16, 
     xlim = c(1,3287), ylim = c(-40,40), 
     ylab = '', xaxt = 'n', xlab = '',
     cex = 0.7, cex.axis = 0.8)
title("Nipigon", line = -1.25, adj = 0.1)
points(1:4018, nip[,3:7] %>% rowMeans, col = "red", pch = 15, cex = 0.5)
# plot stlouis comparison
par(mar = c(0.5,0,3.5,2.5))
plot(1:4018, air$stlouis, col = "blue", pch = 16, 
     xlim = c(1,3287), ylim = c(-40,40), 
     ylab = "n", yaxt="n", xlab = "n", xaxt = "n")
title("St. Louis", line = -1.25, adj = 0.1)
points(1:4018, slou[,3:7] %>% rowMeans, col = "red", pch = 15, cex = 0.5)
# plot genesee comparison
par(mar = c(3.5,3,0.5,0.5))
plot(1:4018, air$genesee, col = "blue", pch = 16,
     xlim = c(1,3287), ylim = c(-40,40), 
     xlab = "", ylab = "",
     cex = 0.7, cex.axis = 0.8)
title("Genesee", line = -1.25, adj = 0.1)
points(1:4018, gen[,3:7] %>% rowMeans, col = "red", pch = 15, cex = 0.5)
# plot vermillion comparison
par(mar = c(3.5,0,0.5,2.5))
plot(1:4018, air$vermillion, col = "blue", pch = 16, 
     xlim = c(1,3287), ylim = c(-40,40), 
     xlab = "", ylab = "", yaxt="n",
     cex = 0.7, cex.axis = 0.8)
title("Vermillion", line = -1.25, adj = 0.1)
points(1:4018, verm[,3:7] %>% rowMeans, col = "red", pch = 15, cex = 0.5)
legend(2090, -20, legend = c("GCM Mean", "Empirical"), 
       col = c("red", "blue"), 
       pch = c(19, 19),
       box.lty = 0
       #pt.cex = c(1, 1.4),
       #cex = 1.3
)
mtext("Days", side=1, line=2.5, at=-200, cex = 1)
mtext("Air Temperature (°C)", side=2, line=22.7, at=48, cex = 1)

# close off the plot
dev.off()

#              ___________________________________

# PLOTTING ALL INDIVIDUAL GCM DATA VERSUS WEATHER STATION DATA
 
# set up multiplot
par(mfrow = c(2,2),
    mgp = c(3,0.5,0))
par(mar = c(0.5,3,3.5,0.5))
# plot nipigon comparison
plot(1:4018, air$nipigon, col = "blue", pch = 16, cex = 0.5,
     xlim = c(1,3287), ylim = c(-40,40), 
     ylab = '', xaxt = 'n', xlab = '',
     cex.axis = 0.8)
title("Nipigon", line = -1.25, adj = 0.1)
points(1:4018, nip[,3],  col = "grey90", pch = 15, cex = 0.5)  # ecEarth3
points(1:4018, nip[,4],  col = "grey80", pch = 15, cex = 0.5)  # hadGEM3
points(1:4018, nip[,5],  col = "grey70", pch = 15, cex = 0.5)  # inmCM5
points(1:4018, nip[,6],  col = "grey60", pch = 15, cex = 0.5)  # mriESM2
points(1:4018, nip[,7],  col = "grey50", pch = 15, cex = 0.5)  # ukESM1
points(1:4018, air$nipigon, col = "blue", pch = 16, cex = 0.5) # weather station air 
# points(1:4018, nip[,3:7] %>% rowMeans, col = "red", pch = 15) # GCM mean
# plot stlouis comparison
par(mar = c(0.5,0,3.5,2.5))
plot(1:4018, air$stlouis, col = "blue", pch = 16, cex = 0.5,
     xlim = c(1,3287), ylim = c(-40,40), 
     ylab = "n", yaxt="n", xlab = "n", xaxt = "n")
title("St. Louis", line = -1.25, adj = 0.1)
points(1:4018, slou[,3],  col = "grey90", pch = 15, cex = 0.5)  # ecEarth3
points(1:4018, slou[,4],  col = "grey80", pch = 15, cex = 0.5)  # hadGEM3
points(1:4018, slou[,5],  col = "grey70", pch = 15, cex = 0.5)  # inmCM5
points(1:4018, slou[,6],  col = "grey50", pch = 15, cex = 0.5)  # mriESM2
points(1:4018, slou[,7],  col = "grey40", pch = 15, cex = 0.5)  # ukESM1
points(1:4018, air$stlouis, col = "blue", pch = 16, cex = 0.5)  # weather station air 
#  points(1:4018, slou[,3:7] %>% rowMeans, col = "red", pch = 15) # GCM mean
# plot genesee comparison
par(mar = c(3.5,3,0.5,0.5))
plot(1:4018, air$genesee, col = "blue", pch = 16, cex = 0.5,
     xlim = c(1,3287), ylim = c(-40,40), 
     xlab = "", ylab = "",
     cex.axis = 0.8)
title("Genesee", line = -1.25, adj = 0.1)
points(1:4018, gen[,3],  col = "grey90", pch = 15, cex = 0.5)  # ecEarth3
points(1:4018, gen[,4],  col = "grey80", pch = 15, cex = 0.5)  # hadGEM3
points(1:4018, gen[,5],  col = "grey70", pch = 15, cex = 0.5)  # inmCM5
points(1:4018, gen[,6],  col = "grey50", pch = 15, cex = 0.5)  # mriESM2
points(1:4018, gen[,7],  col = "grey40", pch = 15, cex = 0.5)  # ukESM1
points(1:4018, air$genesee, col = "blue", pch = 16, cex = 0.5) # weather station air 
#  points(1:4018, gen[,3:7] %>% rowMeans, col = "red", pch = 15) # GCM mean
# plot vermillion comparison
par(mar = c(3.5,0,0.5,2.5))
plot(1:4018, air$vermillion, col = "blue", pch = 16, cex = 0.5,
     xlim = c(1,3287), ylim = c(-40,40), 
     xlab = "", ylab = "", yaxt="n",
     cex.axis = 0.8)
title("Vermillion", line = -1.25, adj = 0.1)
points(1:4018, verm[,3],  col = "grey90", pch = 15, cex = 0.5)    # ecEarth3
points(1:4018, verm[,4],  col = "grey80", pch = 15, cex = 0.5)    # hadGEM3
points(1:4018, verm[,5],  col = "grey70", pch = 15, cex = 0.5)    # inmCM5
points(1:4018, verm[,6],  col = "grey50", pch = 15, cex = 0.5)    # mriESM2
points(1:4018, verm[,7],  col = "grey40", pch = 15, cex = 0.5)    # ukESM1
points(1:4018, air$vermillion, col = "blue", pch = 16, cex = 0.5) # weather station air 
#  points(1:4018, verm[,3:7] %>% rowMeans, col = "red", pch = 15)   # GCM mean
legend(1650, -20, legend = c( "ecEarth3", "hadGEM3", "inmCM5"),
       col = c("grey90", "grey80", "grey70"),
       pch = c(15,15,15),
       box.lty = 0
)
legend(2500, -20, legend = c("mriESM2", "ukESM1", "Empirical"), 
       col = c("grey50", "grey40", "blue"), 
       pch = c(15,15,19),
       box.lty = 0
)
mtext("Days", side=1, line=2.5, at=-200, cex = 1)
mtext("Air Temperature (°C)", side=2, line=22.7, at=48, cex = 1)

# close plot
dev.off()


# WEATHER STATION × CLIMATE MODEL REGRESSIONS (PER TRIBUTARY)

# TODO: get the thames GCM data in here too

# this function may not actually be worthwhile:
 
    # compares performance of linear regression to multiple regression
    regression <- function(empiricalVector, GCMvectors){
    
      # fit a linear regression then output stuff 
      model <- lm(empiricalVector ~ GCMvectors[,3:7] %>% rowMeans)
      slope <- model$coefficients[2]
      print(slope)
    
      # now do multiple regression
      slope <- lm(empiricalVector ~ GCMvectors[,3] + GCMvectors[,4] + 
                  GCMvectors[,5] + GCMvectors[,6] + 
                  GCMvectors[,7])$coefficients[-1] %>% sum
      print(slope)
    }
    
    # example usage:
    regression(air$nipigon, nip)


# linear regressions using averaged daily temperature across GCMs 
lm(air$nipigon ~ (nip[,3:7] %>% rowMeans))$coefficients[2]     %>% cat("   Nipigon =",.,"\n")
lm(air$stlouis ~ (slou[,3:7] %>% rowMeans))$coefficients[2]    %>% cat("  St Louis =",.,"\n")
lm(air$genesee ~ (gen[,3:7] %>% rowMeans))$coefficients[2]     %>% cat("   Genesee =",.,"\n")
lm(air$vermillion ~ (verm[,3:7] %>% rowMeans))$coefficients[2] %>% cat("Vermillion =",.,"\n")

# multiple regression version (shows negligible improvement) 
lm(air$nipigon ~ nip[,3] + nip[,4] + nip[,5] + nip[,6] + nip[,7])$coefficients[-1] %>% sum
lm(air$stlouis ~ slou[,3] + slou[,4] + slou[,5] + slou[,6] + slou[,7])$coefficients[-1] %>% sum
lm(air$genesee ~ gen[,3] + gen[,4] + gen[,5] + gen[,6] + gen[,7])$coefficients[-1] %>% sum
lm(air$vermillion ~ verm[,3] + verm[,4] + verm[,5] + verm[,6] + verm[,7])$coefficients[-1] %>% sum

# NOTE: the slopes are close to 1, so there isn't really any bias to correct as imagined... 

#                   ___________________________________

# plot predicted versus actual air temperature and overlay regression line

# set up multiplot
par(mfrow = c(2,2),
    mgp = c(3,0.5,0))
#
par(mar = c(0.5,3,3.5,0.5))
plot(air$nipigon ~ (nip[,3:7] %>% rowMeans), xlim = c(-35,35), ylim = c(-35,35),
ylab = '',
xaxt = 'n',
xlab = '',
cex.axis = 0.8
)
title("Nipigon", line = -1.25, adj = 0.1)
lm(air$nipigon ~ (nip[,3:7] %>% rowMeans)) %>% abline(., col = "red")
#
par(mar = c(0.5,0,3.5,2.5))
plot(air$stlouis ~ (slou[,3:7] %>% rowMeans), xlim = c(-35,35), ylim = c(-35,35),
ylab = 'n',
yaxt = 'n',
xlab = 'n',
xaxt = 'n',
cex.axis = 0.8
)
title("St. Louis", line = -1.25, adj = 0.1)
lm(air$stlouis ~ (slou[,3:7] %>% rowMeans)) %>% abline(., col="red")
#
par(mar = c(3.5,3,0.5,0.5))
plot(air$genesee ~ (gen[,3:7] %>% rowMeans), xlim = c(-35,35), ylim = c(-35,35),
ylab = '',
xlab = '',
cex.axis = 0.8
)
title("Genesee", line = -1.25, adj = 0.1)
lm(air$genesee ~ (gen[,3:7] %>% rowMeans)) %>% abline(., col = "red")
#
par(mar = c(3.5,0,0.5,2.5))
plot(air$vermillion ~ (verm[,3:7] %>% rowMeans), xlim = c(-35,35), ylim = c(-35,35),
ylab = '',
yaxt = 'n',
xlab = '',
cex.axis = 0.8
)
title("Vermillion", line = -1.25, adj = 0.1)
lm(air$vermillion ~ (verm[,3:7] %>% rowMeans)) %>% abline(., col = "red")
mtext('Projected Air Temperature (°C)', side=1, line=2.2, at=-37, cex = 1)
mtext('Actual Air Temperature (°C)', side=2, line=22.5, at=42, cex = 1)

dev.off()

# ---- STEP THREE --------------------------------------------------------------

#  A I R  T O  W A T E R  C O N V E R S I O N
# ﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋

# Developing temperature conversion function
 
# using imputation
#thames <- data.frame(j.date = thames.air$j.date[1:2952],
#                     air = impute(thames.air)[1:2952,"temp"],
#                     water = impute(thames.water)[1:2952,"temp"]
#)

# without imputation
thames <- data.frame(j.date = thames.air$j.date[1:2952],
                     air = thames.air[1:2952,"temp"],
                     water = thames.water[1:2952,"temp"]
)


# get the coefficients
conversionModel <- lm(thames.water$temp ~ thames.air$temp)

# reassign all negative air temperatures to 0
# thames$air[thames$air < 0] <- 0

# previous air2water:  y <- .927 * x + 2.952

# THE WINNER - least manipulation, yet still good, avoids low-temp problem
#              no impute, no zero 
#              R2 0.87, significant
#              0.76x + 6.03

# no impute, yes zero 
# R2 0.91, significant
# 0.96x + 2.97

# yes impute, no zero 
# R2 0.87, significant
# 0.77x + 6.00

# yes impute, yes zero 
# R2 0.91, significant
# 0.95x + 3.04


# look at the fit
plot(thames$air, thames$water, xlab = "Air Temperature (°C)", ylab = "Water Temperature (°C)")
abline(lm(water ~ air, thames), col = "red")

# look at the performance
plot(conversionModel)


#  W A T E R  C O M P A R I S O N S
#﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋


# was fit to Thames data, without the zero pre-conversion flattening 
# (now do the flattening post-temperature-conversion)
air2water <- function(x){ 
  y <- .761 * x + 6.028
  return(y)
}

# make a copy to convert into water temperatures 
gcm.water <- gcm.long

# apply function to convert air temperatures to water temperatures
gcm.water$tas <- air2water(gcm.water$tas)

# adjust negative air temperatures to zero (minimum possible water temperature)
gcm.water$tas[which(gcm.water$tas < 0)] <- 0

# change the converted GCM data from long to wide format
gcm.water <- format.gcm(gcm.water)

# add an average column
gcm.water <- lapply(gcm.water, \(x) cbind(x, mean = x[,3:7] %>% rowMeans))

# subset each tributary for individual comparisons with historical data 
 nip.w <- gcm.water$nip  %>% subset(., year %in% 2015:2025)
slou.w <- gcm.water$slou %>% subset(., year %in% 2015:2025)
 gen.w <- gcm.water$gen  %>% subset(., year %in% 2015:2025)
verm.w <- gcm.water$verm %>% subset(., year %in% 2015:2025)


# NOTE: NOW compare converted water temperatures against the gage and see how it looks
#      (same method as with the GCM air temperatures VS weather station, curves overlaid)

# write.csv(gcm.water, "newWater.csv")

#  C O M P A R I S O N  P L O T S
#﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋

#  MEAN GCM WATER VS EMPIRICAL

# set up multiplot
par(mfrow = c(2,2))
# plot nipigon comparison
par(mar = c(0.5,3,3.5,0.5))
plot(1:4018, water$nipigon, col = "blue", pch = 16, 
     ylab = '', xaxt = 'n', xlab = '', ylim = c(0,35)) 
title("Nipigon", line = -1, adj = 0.1)
points(1:4018, nip.w$mean, col = "red", pch = 15, cex = 0.5)
# plot stlouis comparison
par(mar = c(0.5,0,3.5,2.5))
plot(1:4018, water$stlouis, col = "blue", pch = 16, 
     ylab = 'n', yaxt = 'n', xlab = 'n', xaxt = 'n',
     ylim = c(0,35))
title("St. Louis", line = -1, adj = 0.1)
points(1:4018, slou.w$mean, col = "red", pch = 15, cex = 0.5)
# plot genesee comparison
par(mar = c(3.5,3,0.5,0.5))
plot(1:4018, water$genesee, col = "blue", pch = 16,
     ylab = '', xlab = '',
     ylim = c(0,35))
title("Genesee", line = -1, adj = 0.1)
points(1:4018, gen.w$mean, col = "red", pch = 15, cex = 0.5)
# plot vermillion comparison
par(mar = c(3.5,0,0.5,2.5))
plot(1:4018, water$vermillion, col = "blue", pch = 16, 
     ylab = '', yaxt = 'n', xlab = '',
     ylim = c(0,35))
title("Vermillion", line = -1, adj = 0.1)
points(1:4018, verm.w$mean, col = "red", pch = 15, cex = 0.5)
mtext('Days', side=1, line=2.2, at=-39, cex = 1.1)
mtext('Temperature (°C)', side=2, line=33, at=39, cex = 1.1)
mtext('GCM Mean', col = "red", side=1, line=-32.2, at=3780, cex = 1.1)
mtext('Empirical', col = "blue", side=1, line=-31, at=3720, cex = 1.1)


#  INDIVIDUAL GCM WATER VS EMPIRICAL

# set up multiplot
par(mfrow = c(2,2),
    mgp = c(3,0.5,0),
    xpd = TRUE)
# plot nipigon comparison
par(mar = c(0.5,3,3.5,0.5))
plot(1:4018, water$nipigon, 
     col = "blue", pch = 16, cex = 0.5, cex.axis = 0.8,
     ylab = '', xaxt = 'n', xlab = '', ylim = c(0,35))
title("Nipigon", line = -1.25, adj = 0.1)
points(1:4018, nip.w[,3],  col = "grey90", pch = 15, cex = 0.5)  # ecEarth3
points(1:4018, nip.w[,4],  col = "grey80", pch = 15, cex = 0.5)  # hadGEM3
points(1:4018, nip.w[,5],  col = "grey70", pch = 15, cex = 0.5)  # inmCM5
points(1:4018, nip.w[,6],  col = "grey60", pch = 15, cex = 0.5)  # mriESM2
points(1:4018, nip.w[,7],  col = "grey50", pch = 15, cex = 0.5)  # ukESM1
# points(1:4018, water$nipigon, col = "blue", pch = 16, cex = 0.5) # gage station water 
# IF YOU WANT TO LOOK AT BASELINE temperatures even though they don't match in years
# points(1:3768, nipigon.water$temp[-1:-155], col = "blue", pch = 5, cex = 0.5) 
#
# plot stlouis comparison
par(mar = c(0.5,0,3.5,2.5))
plot(1:4018, water$stlouis, col = "blue", pch = 16, cex = 0.5,cex.axis = 0.8,
     ylab = 'n', yaxt = 'n', xlab = 'n', xaxt = 'n', ylim = c(0,35))
title("St. Louis", line = -1.25, adj = 0.1)
points(1:4018, slou.w[,3],  col = "grey90", pch = 15, cex = 0.5)  # ecEarth3
points(1:4018, slou.w[,4],  col = "grey80", pch = 15, cex = 0.5)  # hadGEM3
points(1:4018, slou.w[,5],  col = "grey70", pch = 15, cex = 0.5)  # inmCM5
points(1:4018, slou.w[,6],  col = "grey50", pch = 15, cex = 0.5)  # mriESM2
points(1:4018, slou.w[,7],  col = "grey40", pch = 15, cex = 0.5)  # ukESM1
points(1:4018, water$stlouis, col = "blue", pch = 16, cex = 0.5)  # gage station water 
#
legend(2000, 47.5, legend = c( "ecEarth3", "hadGEM3", "inmCM5"),
       col = c("grey90", "grey80", "grey70"),
       pch = c(15,15,15),
       box.lty = 0,
       x.intersp = 0.7
)
legend(3100, 47.5, legend = c("mriESM2", "ukESM1", "Empirical"), 
       col = c("grey50", "grey40", "blue"), 
       pch = c(15,15,19),
       box.lty = 0,
       x.intersp = 0.7
)
#
# plot genesee comparison
par(mar = c(3.5,3,0.5,0.5))
plot(1:4018, water$genesee, col = "blue", pch = 16, cex = 0.5,cex.axis = 0.8,
     ylab = '', xlab = '',
     ylim = c(0,35))
title("Genesee", line = -1.25, adj = 0.1)
points(1:4018, gen.w[,3],  col = "grey90", pch = 15, cex = 0.5)  # ecEarth3
points(1:4018, gen.w[,4],  col = "grey80", pch = 15, cex = 0.5)  # hadGEM3
points(1:4018, gen.w[,5],  col = "grey70", pch = 15, cex = 0.5)  # inmCM5
points(1:4018, gen.w[,6],  col = "grey50", pch = 15, cex = 0.5)  # mriESM2
points(1:4018, gen.w[,7],  col = "grey40", pch = 15, cex = 0.5)  # ukESM1
points(1:4018, water$genesee, col = "blue", pch = 16, cex = 0.5) # gage station water 
#
# plot vermillion comparison
par(mar = c(3.5,0,0.5,2.5))
plot(1:4018, water$vermillion, col = "blue", pch = 16, cex = 0.5,cex.axis = 0.8,
     ylab = '', yaxt = 'n', xlab = '',
     ylim = c(0,35))
title("Vermillion", line = -1.25, adj = 0.1)
points(1:4018, verm.w[,3],  col = "grey90", pch = 15, cex = 0.5)    # ecEarth3
points(1:4018, verm.w[,4],  col = "grey80", pch = 15, cex = 0.5)    # hadGEM3
points(1:4018, verm.w[,5],  col = "grey70", pch = 15, cex = 0.5)    # inmCM5
points(1:4018, verm.w[,6],  col = "grey50", pch = 15, cex = 0.5)    # mriESM2
points(1:4018, verm.w[,7],  col = "grey40", pch = 15, cex = 0.5)    # ukESM1
points(1:4018, water$vermillion, col = "blue", pch = 16, cex = 0.5) # gage station water 
# final touches of text to the plot
mtext('Days', side=1, line=2.2, at=-180, cex = 1.1)
mtext('Water Temperature (°C)', side=2, line=22.5, at=39, cex = 1)



# close plot
dev.off()











# ---- STEP FOUR ---------------------------------------------------------------
# below is a quick investigation into whether individual regressions per tributary
# were better than using the one large Thames dataset to train a conversion function.
# Ultimately, it wasn't really an improvement.



# QUICK AND DIRTY - local regressions per tributary
# new and temporary code that will be deleted if this doesn't prove fruitful

# check and see if regression for nipigon alone is THAT significantly different from the Thames model


# prepping the data for regression (needs to be evenly lined up)

# making scrap variables
water.regress <- nipigon.water
air.regress <- nipigon.air %>% subset(., year %in% (1998:2010))


regress <- merge(air.regress, water.regress, by = c("year", "j.date"), all = T)
names(regress) <- c("year", "j.date", "air", "water")

# R square 0.68
lm(water ~ air, data = regress)


nip.regress <- function(x){ 
  y <- .442 * x + 5.68
  return(y)
}


# NOW checking performance of converting air temperatures to water
# is a Nipigon-specific regression better than the general Thames one?

# convert the air temperatures
regress$nipconvert <- regress$air %>% nip.regress
regress$nipconvert[regress$nipconvert < 0] <- 0

regress$thamesconvert <- regress$air %>% air2water
regress$thamesconvert[regress$thamesconvert < 0] <- 0


plot(1:4748, regress$water, col = "blue", pch = 16, 
     ylim = c(0,27), 
     xlab = "", ylab = "Temperature")
title("Nipigon", line = -1)
points(1:4748, regress$nipconvert, col = "green", pch = 15, cex = 0.5)
points(1:4748, regress$thamesconvert, col = "red", pch = 15, cex = 0.5)
points(1:4748, regress$water, col = "blue", pch = 15, cex = 0.5)

points(1:4748, 
cbind(regress$nipconvert, regress$thamesconvert) %>% rowMeans,
col = "magenta", pch = 15, cex = 0.5)


# thames model R2 is 3% better, but is too hot in summer, local nip model is too cold in summer
lm(regress$water~regress$thamesconvert)
lm(regress$water~regress$nipconvert)



# --------------------------------------------------------------------------------


# R2 = 0.77
lm(water$stlouis ~ air$stlouis)

slou.regress <- function(x){ 
  y <- 0.6603 * x + 5.9861      
  return(y)
}

# R2 = 0.85
lm(water$genesee ~ air$genesee)

gen.regress <- function(x){ 
  y <- 0.8105 * x + 3.8958             
  return(y)
}

# R2 = 0.88
lm(water$vermillion ~ air$vermillion)

verm.regress <- function(x){ 
  y <- 0.8742   * x + 3.4455                
  return(y)
}

# ————————————————————————————————————————————————————————————————————————————————

# ST LOUIS
stlouis <- data.frame(   air = air$stlouis,
                       water = water$stlouis )


# convert the air temperatures, using local and thames conversion function
stlouis$convert.slou <- stlouis$air %>% slou.regress
stlouis$convert.slou[stlouis$convert.slou < 0] <- 0

stlouis$convert.thames <- stlouis$air %>% air2water 
stlouis$convert.thames[stlouis$convert.thames < 0] <- 0

plot(1:4018, stlouis$water, col = "blue", pch = 16, 
     #xlim = c(1,3287), ylim = c(-40,40), 
     xlab = "", ylab = "Temperature")
title("St Louis", line = -1, adj = 0.1)
points(1:4018, stlouis$convert.slou, col = "green", pch = 15, cex = 0.5)
points(1:4018, stlouis$convert.thames, col = "red", pch = 15, cex = 0.5)
points(1:4018, stlouis$water, col = "blue", pch = 15, cex = 0.5)


lm(stlouis$water~stlouis$convert.slou)
lm(stlouis$water~stlouis$convert.thames)


# ————————————————————————————————————————————————————————————————————————————————

# GENESEE
genesee <- data.frame(   air = air$genesee,
                       water = water$genesee )


# convert the air temperatures, using local and thames conversion function
genesee$convert.gen <- genesee$air %>% gen.regress
genesee$convert.gen[genesee$convert.gen < 0] <- 0

genesee$convert.thames <- genesee$air %>% air2water 
genesee$convert.thames[genesee$convert.thames < 0] <- 0

plot(1:4018, genesee$water, col = "blue", pch = 16, 
     #xlim = c(1,3287), ylim = c(-40,40), 
     xlab = "", ylab = "Temperature")
title("Genesee", line = -1, adj = 0.1)
points(1:4018, genesee$convert.gen, col = "green", pch = 15, cex = 0.5)
points(1:4018, genesee$convert.thames, col = "red", pch = 15, cex = 0.5)
points(1:4018, genesee$water, col = "blue", pch = 15, cex = 0.5)

lm(genesee$water~genesee$convert.gen)
lm(genesee$water~genesee$convert.thames)


# ————————————————————————————————————————————————————————————————————————————————

# VERMILLION
vermillion <- data.frame(   air = air$vermillion,
                          water = water$vermillion )


# convert the air temperatures, using local and thames conversion function
vermillion$convert.verm <- vermillion$air %>% verm.regress
vermillion$convert.verm[vermillion$convert.verm < 0] <- 0

vermillion$convert.thames <- vermillion$air %>% air2water 
vermillion$convert.thames[vermillion$convert.thames < 0] <- 0

plot(1:4018, vermillion$water, col = "blue", pch = 16, 
     #xlim = c(1,3287), ylim = c(-40,40), 
     xlab = "", ylab = "Temperature")
title("Vermillion", line = -1, adj = 0.1)
points(1:4018, vermillion$convert.verm, col = "green", pch = 15, cex = 0.5)
points(1:4018, vermillion$convert.thames, col = "red", pch = 15, cex = 0.5)
points(1:4018, vermillion$water, col = "blue", pch = 15, cex = 0.5)

lm(vermillion$water~vermillion$convert.verm)
lm(vermillion$water~vermillion$convert.thames)


# ————————————————————————————————————————————————————————————————————————————————




# make a copy to convert into water temperatures 
gcm.water.local <- gcm.long

# apply function to convert air temperatures to water temperatures
gcm.water.local[which(gcm.water.local$tributary == "nip"), "tas"]  <- gcm.water.local[which(gcm.water.local$tributary == "nip"), "tas"]  %>% nip.regress
gcm.water.local[which(gcm.water.local$tributary == "slou"), "tas"] <- gcm.water.local[which(gcm.water.local$tributary == "slou"), "tas"] %>% slou.regress
gcm.water.local[which(gcm.water.local$tributary == "gen"), "tas"]  <- gcm.water.local[which(gcm.water.local$tributary == "gen"), "tas"]  %>% gen.regress
gcm.water.local[which(gcm.water.local$tributary == "verm"), "tas"] <- gcm.water.local[which(gcm.water.local$tributary == "verm"), "tas"] %>% verm.regress

# adjust negative air temperatures to zero (minimum possible water temperature)
gcm.water.local$tas[which(gcm.water.local$tas < 0)] <- 0

# change the converted GCM data from long to wide format
gcm.water.local <- format.gcm(gcm.water.local)

# add an average column
gcm.water.local <- lapply(gcm.water.local, \(x) cbind(x, mean = x[,3:7] %>% rowMeans))

# subset each tributary for individual comparisons with historical data 
 nip.l <- gcm.water.local$nip  %>% subset(., year %in% 2015:2025)
slou.l <- gcm.water.local$slou %>% subset(., year %in% 2015:2025)
 gen.l <- gcm.water.local$gen  %>% subset(., year %in% 2015:2025)
verm.l <- gcm.water.local$verm %>% subset(., year %in% 2015:2025)






# set up multiplot
par(mfrow = c(2,2))
# plot nipigon comparison
par(mar = c(0.5,3,3.5,0.5))
plot(1:4018, water$nipigon, col = "blue", pch = 16, cex = 0.5,
     ylab = '', xaxt = 'n', xlab = '', ylim = c(0,35))
title("Nipigon", line = -1, adj = 0.1)
points(1:4018, nip.w[,3],  col =  "violetred", pch = 15, cex = 0.5)  # ecEarth3
points(1:4018, nip.w[,4],  col = "violetred1", pch = 15, cex = 0.5)  # hadGEM3
points(1:4018, nip.w[,5],  col = "violetred2", pch = 15, cex = 0.5)  # inmCM5
points(1:4018, nip.w[,6],  col = "violetred3", pch = 15, cex = 0.5)  # mriESM2
points(1:4018, nip.w[,7],  col = "violetred4", pch = 15, cex = 0.5)  # ukESM1
#
points(1:4018, nip.l[,3],  col = "grey90", pch = 15, cex = 0.5)  # ecEarth3
points(1:4018, nip.l[,4],  col = "grey80", pch = 15, cex = 0.5)  # hadGEM3
points(1:4018, nip.l[,5],  col = "grey70", pch = 15, cex = 0.5)  # inmCM5
points(1:4018, nip.l[,6],  col = "grey60", pch = 15, cex = 0.5)  # mriESM2
points(1:4018, nip.l[,7],  col = "grey50", pch = 15, cex = 0.5)  # ukESM1
#
points(1:3768, nipigon.water$temp[-1:-155], col = "blue", pch = 16, cex = 0.5) # gage station water 
#
# plot stlouis comparison
par(mar = c(0.5,0,3.5,2.5))
plot(1:4018, water$stlouis, col = "blue", pch = 16, cex = .8,
     ylab = 'n', yaxt = 'n', xlab = 'n', xaxt = 'n', ylim = c(0,35))
title("St. Louis", line = -1, adj = 0.1)
points(1:4018, slou.w[,3],  col =  "violetred", pch = 15, cex = 0.5)  # ecEarth3
points(1:4018, slou.w[,4],  col = "violetred1", pch = 15, cex = 0.5)  # hadGEM3
points(1:4018, slou.w[,5],  col = "violetred2", pch = 15, cex = 0.5)  # inmCM5
points(1:4018, slou.w[,6],  col = "violetred3", pch = 15, cex = 0.5)  # mriESM2
points(1:4018, slou.w[,7],  col = "violetred4", pch = 15, cex = 0.5)  # ukESM1
#
points(1:4018, slou.l[,3],  col = "grey90", pch = 15, cex = 0.5)  # ecEarth3
points(1:4018, slou.l[,4],  col = "grey80", pch = 15, cex = 0.5)  # hadGEM3
points(1:4018, slou.l[,5],  col = "grey70", pch = 15, cex = 0.5)  # inmCM5
points(1:4018, slou.l[,6],  col = "grey50", pch = 15, cex = 0.5)  # mriESM2
points(1:4018, slou.l[,7],  col = "grey40", pch = 15, cex = 0.5)  # ukESM1
points(1:4018, water$stlouis, col = "blue", pch = 16, cex = 0.5)  # gage station water 
#
# plot genesee comparison
par(mar = c(3.5,3,0.5,0.5))
plot(1:4018, water$genesee, col = "blue", pch = 16, cex = 0.8,
     ylab = '', xlab = '',
     ylim = c(0,35))
title("Genesee", line = -1, adj = 0.1)
points(1:4018, gen.w[,3],  col =  "violetred", pch = 15, cex = 0.5)  # ecEarth3
points(1:4018, gen.w[,4],  col = "violetred1", pch = 15, cex = 0.5)  # hadGEM3
points(1:4018, gen.w[,5],  col = "violetred2", pch = 15, cex = 0.5)  # inmCM5
points(1:4018, gen.w[,6],  col = "violetred3", pch = 15, cex = 0.5)  # mriESM2
points(1:4018, gen.w[,7],  col = "violetred4", pch = 15, cex = 0.5)  # ukESM1
#
points(1:4018, gen.l[,3],  col = "grey90", pch = 15, cex = 0.5)  # ecEarth3
points(1:4018, gen.l[,4],  col = "grey80", pch = 15, cex = 0.5)  # hadGEM3
points(1:4018, gen.l[,5],  col = "grey70", pch = 15, cex = 0.5)  # inmCM5
points(1:4018, gen.l[,6],  col = "grey50", pch = 15, cex = 0.5)  # mriESM2
points(1:4018, gen.l[,7],  col = "grey40", pch = 15, cex = 0.5)  # ukESM1
points(1:4018, water$genesee, col = "blue", pch = 16, cex = 0.5) # gage station water 
#
# plot vermillion comparison
par(mar = c(3.5,0,0.5,2.5))
plot(1:4018, water$vermillion, col = "blue", pch = 16, cex = 0.5,
     ylab = '', yaxt = 'n', xlab = '',
     ylim = c(0,35))
title("Vermillion", line = -1, adj = 0.1)
#
points(1:4018, verm.w[,3],  col =  "violetred", pch = 15, cex = 0.5)    # ecEarth3
points(1:4018, verm.w[,4],  col = "violetred1", pch = 15, cex = 0.5)    # hadGEM3
points(1:4018, verm.w[,5],  col = "violetred2", pch = 15, cex = 0.5)    # inmCM5
points(1:4018, verm.w[,6],  col = "violetred3", pch = 15, cex = 0.5)    # mriESM2
points(1:4018, verm.w[,7],  col = "violetred4", pch = 15, cex = 0.5)    # ukESM1
#
points(1:4018, verm.l[,3],  col = "grey90", pch = 15, cex = 0.5)    # ecEarth3
points(1:4018, verm.l[,4],  col = "grey80", pch = 15, cex = 0.5)    # hadGEM3
points(1:4018, verm.l[,5],  col = "grey70", pch = 15, cex = 0.5)    # inmCM5
points(1:4018, verm.l[,6],  col = "grey50", pch = 15, cex = 0.5)    # mriESM2
points(1:4018, verm.l[,7],  col = "grey40", pch = 15, cex = 0.5)    # ukESM1
points(1:4018, water$vermillion, col = "blue", pch = 16, cex = 0.5) # gage station water 
# final touches of text to the plot
mtext('Days', side=1, line=2.2, at=-39, cex = 1.1)
mtext('Temperature (°C)', side=2, line=33, at=39, cex = 1.1)
mtext('GCM', col = "grey50", side=1, line=-32.2, at=3780, cex = 1.1)
mtext('Empirical', col = "blue", side=1, line=-31, at=3720, cex = 1.1)


# close plot
dev.off()


# now check the performance difference in the regressions 

# Nipigon water is 3923 long, starts 211, ends 118
# Thames slightly better 
lm(nipigon.water$temp[c(-1:-155, -3806:-3923)]~nip.w$mean[-1:-368])
lm(nipigon.water$temp[c(-1:-155, -3806:-3923)]~nip.l$mean[-1:-368])


# St Louis - Thames slightly better
lm(water$stlouis~slou.w$mean)
lm(water$stlouis~slou.l$mean)

# Genesee - Pretty much same (local slightly better)
lm(water$genesee~gen.w$mean)
lm(water$genesee~gen.l$mean)

# Vermillion - local is slightly better
lm(water$vermillion~verm.w$mean)
lm(water$vermillion~verm.l$mean)

