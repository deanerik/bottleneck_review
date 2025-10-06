#!/usr/bin/env Rscript

# 2. Format climate data.R
#    Licence: GNU GPLv3 - see LICENSE.txt for more details
 
#    Access the netCDF files containing NASA GCM data 
#    and format them for further use in R.
#    This script should be run after `1. Download climate data.sh`
#    has been executed to download GCM data.

#———————————————————————————————————————————————————————————————————————————————

# Initial settings
options(scipen = 100, digits = 4)
library(ncdf4)
library(magrittr)

# a list with all the files for years 2015-2100
filenames <- list( verm = c(paste0("v20",15:99,".nc"),"v2100.nc"), 
                   slou = c(paste0("s20",15:99,".nc"),"s2100.nc"), 
                    gen = c(paste0("g20",15:99,".nc"),"g2100.nc"), 
                    nip = c(paste0("n20",15:99,".nc"),"n2100.nc") )

# a list with all the model/directory names 
# following the shortnaming convention from acquireData.sh
modelnames  <- list("ecEarth3", "hadGEM3", "inmCM5", "mriESM2", "ukESM1")

# NOTE: the number of days per year can vary between GCMs
# (e.g. some don't include leap years)
# so if we used the data as is, we'd see these differences:

#   31411 rows  -  includes leap years             -  ecEarth3, mriESM2
#   31390 rows  -  365 days * 86 years (noleap)    -  inmCM5
#   30960 rows  -  360 days / year (Hadley style)  -  hadGEM3, ukESM1

# so to make the data uniform, 
# we'll need to fill in some missing days later on in this script

# these are all the leap years within our scope 
leapYears <- c(2016, 2020, 2024, 2028, 2032, 2036, 2040, 
               2044, 2048, 2052, 2056, 2060, 2064, 2068,
               2072, 2076, 2080, 2084, 2088, 2092, 2096)

# a function to interpolate a temperature value for Feb 29th
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

    }

    # return the augmented temperature series 
    return(series)

}

# ------------------------------------------------------------------------------

# make an empty list to store each GCM's outputs as a list item
output <- list()

# format all the data for each individual model via loop:
for (name in modelnames) {

    # get their directory name
    folder <- paste0("../data/",name,"/")

    # combine filenames and directories for paths to access files  
    filepaths  <- lapply(filenames, function(x){paste0(folder,x)})

    # empty data frame to store extracted temp series
    modDat <- list(verm = data.frame(),
                   slou = data.frame(),
                   gen = data.frame(),
                   nip = data.frame()
    )

    # loop to open files, extract TAS data, add dates, and bind years together
    for (index in seq_along(filepaths)){ # for the 4 tributary list items

        # empty data.frame for one tributary at a time
        dat <- data.frame()

        # initial year (+1) to start off labelling 
        year <- 2014 

        # loop to stitch data together for a single tributary at a time
        for (file in filepaths[[index]]) {
            
            year  <- year + 1  # increment year
            rawData <- nc_open(file) # open netCDF

            # extract temperature data & convert to celsius from kelvin
            tempSeries <- ncvar_get(rawData,"tas") - 273.15 

            # if it's a hadley-type model with 360 days per year...
            if (length(tempSeries) == 360) {

                # pick 5 days at random, leaving room for day+1 at the end
                newDays <- sample(1:359,5)

                # interpolate 5 days in the year 
                for (day in newDays) {

                    # average the random day and the following day
                    newTemp <- tempSeries[c(day,day+1)] %>% mean

                    # then insert it between those two days
                    tempSeries <- c(tempSeries[1:day],
                                    newTemp,
                                    tempSeries[-(1:day)])
                }
            }

            # if it's a known leap year and there are only 365 days...
            if (year %in% leapYears & length(tempSeries) == 365) {

                # add a leap day
                tempSeries <- addLeap(tempSeries)

            } 

            # add day and year columns, then append each year's data to the previous
            tempSeries <- cbind(j.date = 1:length(tempSeries), # days/year can vary 
                                temp = tempSeries,
                                year = year)
            dat <- rbind(dat, tempSeries)

            # Close netCDF to avoid "Too many open files" error & crashing
            nc_close(rawData)

        }

        # save that single tributary within the specific model's 4 tributary set
        modDat[[index]] <- dat

    }

    # wrap everything up nicely in one package
    output[[name]] <- cbind(modDat[[1]][,c("year","j.date")],
                    verm = modDat[[1]]$temp,
                    slou = modDat[[2]]$temp,
                    gen = modDat[[3]]$temp,
                    nip = modDat[[4]]$temp)
}

# ------------------------------------------------------------------------------
 
# load functions to reshape data for export 
library(reshape2)

# melt the results data into a long-format data frame for export
output <- melt(output, id = c("year", "j.date"), value.name = 'tas', variable.name = 'tributary', level = "gcm") 

# fix the levels column name
names(output)[5] <- "gcm"

# write it to csv
write.csv(output, paste0("../data/gcmOutputs.csv"), row.names = FALSE)
