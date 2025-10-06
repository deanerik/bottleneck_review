#!/usr/bin/env Rscript

# 7. Produce timelines graphic.R
#    Licence: GNU GPLv3 - see LICENSE.txt for more details

# for plotting results and overarching trends as a timeline (and some more)
 

#———————————————————————————————————————————————————————————————————————————————

# run the analyses and load their outputs
source("./5. Run model with climate data.R")

# load necessary functions
library(reshape2)
library(magrittr)
library(ggplot2)
library(ggridges)
library(viridis)
library(cowplot)
library(grid)
library(gridExtra)

# turns a data frame of results in long-format into a list of wide-format data
restoreList <- function(dataset){

    # empty list to be filled 
    dataList <- list()

    # for each tributary in the melted (long) dataset
    for( location in unique(dataset$tributary) ){

        # subset data for that location
        listItem <- subset(dataset, tributary == location, select = c(year, variable, value))

        # cast the data into wide format, and insert into the list
        dataList[[ location ]] <- dcast(listItem, year ~ variable)

    }

    # return the data in list format as output
    return(dataList)
}

# load data and reshape it back into list format
results <- restoreList(read.csv("../results/results.csv"))

#———————————————————————————————————————————————————————————————————————————————

# ---- FOR THE SCHEMATIC -------------------------------------------------------

# Date focused data for timeline schematic diagram

# PERIODS:   Baseline, 2050, and 2100
# STATISTIC: First spawn, last effective spawn, and last spawn
spawn.dates <- rbind(
                     # NIPIGON 
                     subset(results$nipigon, year < 2015)[c("spawnDaysRange1", "lastBday", "spawnDaysRange2")] %>% colMeans(na.rm = T) %>% round(0),
                     subset(results$nipigon, 2040 <= year & year <= 2050)[c("spawnDaysRange1", "lastBday", "spawnDaysRange2")] %>% colMeans %>% round(0),
                     subset(results$nipigon, 2090 <= year & year <= 2099)[c("spawnDaysRange1", "lastBday", "spawnDaysRange2")] %>% colMeans %>% round(0),

                     # ST LOUIS
                     subset(results$stlouis, year < 2015)[c("spawnDaysRange1", "lastBday", "spawnDaysRange2")] %>% colMeans(na.rm = T) %>% round(0),
                     subset(results$stlouis, 2040 <= year & year <= 2050)[c("spawnDaysRange1", "lastBday", "spawnDaysRange2")] %>% colMeans %>% round(0),
                     subset(results$stlouis, 2090 <= year & year <= 2099)[c("spawnDaysRange1", "lastBday", "spawnDaysRange2")] %>% colMeans %>% round(0),

                     # GENESEE
                     subset(results$genesee, year < 2015)[c("spawnDaysRange1", "lastBday", "spawnDaysRange2")] %>% colMeans(na.rm = T) %>% round(0),
                     subset(results$genesee, 2040 <= year & year <= 2050)[c("spawnDaysRange1", "lastBday", "spawnDaysRange2")] %>% colMeans %>% round(0),
                     subset(results$genesee, 2090 <= year & year <= 2099)[c("spawnDaysRange1", "lastBday", "spawnDaysRange2")] %>% colMeans %>% round(0),

                     # VERMILLION
                     subset(results$vermillion, year < 2015)[c("spawnDaysRange1", "lastBday", "spawnDaysRange2")] %>% colMeans(na.rm = T) %>% round(0),
                     subset(results$vermillion, 2040 <= year & year <= 2050)[c("spawnDaysRange1", "lastBday", "spawnDaysRange2")] %>% colMeans %>% round(0),
                     subset(results$vermillion, 2090 <= year & year <= 2099)[c("spawnDaysRange1", "lastBday", "spawnDaysRange2")] %>% colMeans %>% round(0)
)
# labels for tributaries
t.trib <- data.frame(trib = c("nipigon","nipigon","nipigon","stlouis","stlouis","stlouis","genesee","genesee","genesee","vermillion","vermillion","vermillion"),
                     year = c("base","2050","2100","base","2050","2100","base","2050","2100","base","2050","2100"))
# join them together 
spawn.dates <- cbind(spawn.dates, t.trib)



# — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — —
# processing GCM data and bundling it with historical data 

# read the raw air temperature projections
gcm.data <- read.csv("../data/gcmOutputs.csv")

# air-to-water conversion function, based on zeroed Thames River air temps
mod.temp2 <- function(x){ 
    y <- .927 * x + 2.952
    return(y)
}

# adjust negative air temperatures to zero
gcm.data$tas[which(gcm.data$tas < 0)] <- 0

# convert air temperatures to water temperatures
gcm.data$tas <- mod.temp2(gcm.data$tas)

# load data for baseline years
g.temp <- read.csv("../data/gBaseline.csv")
n.temp <- read.csv("../data/nBaseline.csv")
s.temp <- read.csv("../data/sBaseline.csv")
v.temp <- read.csv("../data/vBaseline.csv")

# wrap it all together
waterC <- rbind(
                data.frame(year = n.temp$year, j.date = n.temp$j.date, tributary = "nip",  tas = n.temp$temp, gcm = "base"),
                data.frame(year = s.temp$year, j.date = s.temp$j.date, tributary = "slou", tas = s.temp$temp, gcm = "base"),
                data.frame(year = g.temp$year, j.date = g.temp$j.date, tributary = "gen",  tas = g.temp$temp, gcm = "base"),
                data.frame(year = v.temp$year, j.date = v.temp$j.date, tributary = "verm", tas = v.temp$temp, gcm = "base"),
                gcm.data
)


# — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — —

# Get the ordinal date when the GDD signal has been accrued
get.gdd <- function(tempArray){

    # copy for GDD calculations 
    gddArray <- tempArray

    # exclude the days below the base GDD temperature (zero them)
    gddArray[gddArray[,2] < baseTemp, 2]  <- 0

    # Get the first ordinal date where enough degree days have accrued
    gdd.day <- gddArray[(gddArray[,2] %>% cumsum >= degreeDays),1] %>% head(1)

    # return that 
    return(gdd.day)

}

baseTemp = 15
degreeDays = 633

# ------------------------------------------------------------

# wrapper function to crunch GDD calcs from the large unified dataset 
 
whenGDD <- function(tempSeries, 
                    baseline,
                    trib,
                    years){

    # empty holder
    when <- NULL

    # access a single source of baseline data or 5 GCMs' projections 
    if (baseline == 1) {

        series <- "base" # no models, just empirical historical data here
        years  <- unique(tempSeries$year) # base years are specific to each trib

    } else if (baseline == 0) {

        # if not doing baseline, these are the GCMs to go through
        series <- c("ecEarth3", "hadGEM3", "inmCM5", "mriESM2", "ukESM1")

    }

    # for each individual model (or just baseline values)
    for (model in series) {

        # loop over each year in the period of interest
        for (each in years) {
            # accumulate GDD annual values for temperature series 
            # corresponding to a given year, river, and model
            when <- c(when, 
                      get.gdd( subset( tempSeries, 
                                      year == each & tributary == trib & gcm == model, 
                                      select = c("j.date", "tas") )
                      )
            )

        } 
    }
    # return the mean value
    return(mean(when))
}








# ——————————————————————————————————————————————————————————————————————————————

# EXAMPLE & COMPARISON:

# for BASELINE: 
 
# using the core function to get maturation date per annum 
get.gdd(subset(n.temp, year == 2000, select = c("j.date", "temp")))
get.gdd(subset(n.temp, year == 2001, select = c("j.date", "temp")))
get.gdd(subset(n.temp, year == 2002, select = c("j.date", "temp")))
get.gdd(subset(n.temp, year == 2003, select = c("j.date", "temp")))
get.gdd(subset(n.temp, year == 2004, select = c("j.date", "temp")))
get.gdd(subset(n.temp, year == 2005, select = c("j.date", "temp")))
get.gdd(subset(n.temp, year == 2006, select = c("j.date", "temp")))
get.gdd(subset(n.temp, year == 2007, select = c("j.date", "temp")))
get.gdd(subset(n.temp, year == 2008, select = c("j.date", "temp")))
get.gdd(subset(n.temp, year == 2009, select = c("j.date", "temp")))

# using the wrapper to streamline the process
whenGDD(waterC, 1, "nip")

# for FUTURE:

# Load the data
tribData <- read.csv("../data/tribData.csv")

# just working with one year at a time 
c(
 get.gdd(subset(tribData, year == 2015 & tributary == "verm", select = c( "j.date", "ecEarth3" ))),
 get.gdd(subset(tribData, year == 2015 & tributary == "verm", select = c( "j.date", "hadGEM3"  ))),
 get.gdd(subset(tribData, year == 2015 & tributary == "verm", select = c( "j.date", "inmCM5"   ))),
 get.gdd(subset(tribData, year == 2015 & tributary == "verm", select = c( "j.date", "mriESM2"  ))),
 get.gdd(subset(tribData, year == 2015 & tributary == "verm", select = c( "j.date", "ukESM1"   )))
)  

# and using the wrapper (gets the mean value)
whenGDD(waterC, 0, "verm", 2015)

# ——————————————————————————————————————————————————————————————————————————————

# baseline, 2050, 2100
 
# Nipigon
whenGDD(waterC, 1, "nip") %>% mean
whenGDD(waterC, 0, "nip", 2040:2050) %>% mean
whenGDD(waterC, 0, "nip", 2090:2099) %>% mean

# St Louis 
whenGDD(waterC, 1, "slou") %>% mean
whenGDD(waterC, 0, "slou", 2040:2050) %>% mean
whenGDD(waterC, 0, "slou", 2090:2099) %>% mean

# Genesee
whenGDD(waterC, 1, "gen") %>% mean
whenGDD(waterC, 0, "gen", 2040:2050) %>% mean
whenGDD(waterC, 0, "gen", 2090:2099) %>% mean

# Vermillion
whenGDD(waterC, 1, "verm") %>% mean
whenGDD(waterC, 0, "verm", 2040:2050) %>% mean
whenGDD(waterC, 0, "verm", 2090:2099) %>% mean


# — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — —

# copy of get.Range & best.Range that just gives the indices of winter start and end 
# from a pre-cut temp series (shoulder seasons plus winter)
 
get.winterDates <- function(tempArrayOG){

    # make a working copy
    tempArray <- tempArrayOG

    # find the index of the first cold day post equinox (Sept 23)
    startpoint <- (tempArray < winterTemp) %>% which %>% head(1) 
    # for the start of winter (which occurs at end of year)
    # guarding against cold spells making a false start of winter 

    # repeat until 5 days days following days also below the winter threshold
    # keep looping to find a correct stretch so long as all 5 are NOT winter temperatures 
    while( (tempArray < winterTemp)[startpoint:(startpoint+4)] %>% all != TRUE) {

        # if they aren't, get the five day range from the first cold day 
        range5 <- startpoint:(startpoint+4)

        # grab the last of the warm values
        checkpoint <- ((tempArray < winterTemp)[range5] != T) %>% which %>% tail(1)

        # then determine its position in the temp series
        # and the next cycle will check again starting from the next day 
        startpoint <- (range5)[checkpoint]+1

        # and if you hit the end of the series, finding nothing, then stop
        if(startpoint == length(tempArray)) break

    }

    # find the end of winter (which occurs at the start of the year)

    # get the index of the first warm day (crossing temperature threshold) 
    endpoint <- (startpoint-1) + (tempArray[startpoint:length(tempArray)] > winterTemp) %>% which %>% head(1)

    # if there's no start of winter, then log a negligble day
    if(identical(endpoint,numeric(0))){

        tempArray <- 0
        startpoint <- 1
        endpoint <- 2

    } else {

        # loop until all 5 days are above the winter threshold
        while( (tempArray > winterTemp)[endpoint:(endpoint+4)] %>% all != TRUE ) {

            # get the 5 day range
            range5 <- endpoint:(endpoint+4) 

            # grab the last of the cold values
            checkpoint <- ((tempArray > winterTemp)[range5] != T) %>% which %>% tail(1)

            # then determine its position in the temp series, and take the next spot
            endpoint <- (range5)[checkpoint]+1

        }
    }

    # the subtraction is necessary for this to work
    endpoint <- endpoint - 1

    # now with the true starting point of winter 
    # and the end (day before true start of growing) 
    # return the temperature series for the winter
    return(c(startpoint,endpoint))

}

# outputs 161 323
get.winterDates(trib.mods$verm[[1]][130:565,2])

# start ordinal example
130+161-1

# end ordinal (next year) example
130+323-365

# From the indices, extract the actual ordinal dates
# ALWAYS put the index for the equivalent of the 200th ordinal date
get.winterOrdinal <- function(tempData, begin){

    # grab the start and end index for the data
    indices <- get.winterDates(tempData[begin:(begin+365),2])

    startDay <- 200 + indices[1] - 1

    endDay <- 200 + indices[2] - 365

    # return the ordinal dates for start of winter and end of winter (next year)
    return(c(startDay,endDay))

}

# application: find the cooresponding index for ordinal day 200 in a given year
# then find the start and end of winter for that segment of time
# which begins on day 200, and goes into the next year past winter
aYear <- 2015
modelSeries <- trib.mods$verm[[1]] # n.temp # v.temp 
get.winterOrdinal(modelSeries, which(modelSeries$year == aYear)[200])


# ==== NEW WINTER ORDINAL DATE EXTRACTION ====

# uses the new methods to get the best (longest) winter range

get.Range2 <- function(tempArrayOG){

    # make a working copy
    tempArray <- tempArrayOG

    # find the index of the first cold day post equinox (Sept 23)
    startpoint <- (tempArray < winterTemp) %>% which %>% head(1) 
    # for the start of winter (which occurs at end of year)
    # guarding against cold spells making a false start of winter 

    # repeat until 5 days days following days also below the winter threshold
    # keep looping to find a correct stretch so long as all 5 are NOT winter temperatures 
    while( (tempArray < winterTemp)[startpoint:(startpoint+4)] %>% all != TRUE) {

        # if they aren't, get the five day range from the first cold day 
        range5 <- startpoint:(startpoint+4)

        # grab the last of the warm values
        checkpoint <- ((tempArray < winterTemp)[range5] != T) %>% which %>% tail(1)

        # then determine its position in the temp series
        # and the next cycle will check again starting from the next day 
        startpoint <- (range5)[checkpoint]+1

        # and if you hit the end of the series, finding nothing, then stop
        if(startpoint == length(tempArray)) break

    }

    # find the end of winter (which occurs at the start of the year)

    # get the index of the first warm day (crossing temperature threshold) 
    endpoint <- (startpoint-1) + (tempArray[startpoint:length(tempArray)] > winterTemp) %>% which %>% head(1)

    # if there's no start of winter, then log a negligble day
    if(identical(endpoint,numeric(0))){

        tempArray <- 0
        startpoint <- 1
        endpoint <- 2

    } else {

        # loop until all 5 days are above the winter threshold
        while( (tempArray > winterTemp)[endpoint:(endpoint+4)] %>% all != TRUE ) {

            # get the 5 day range
            range5 <- endpoint:(endpoint+4) 

            # grab the last of the cold values
            checkpoint <- ((tempArray > winterTemp)[range5] != T) %>% which %>% tail(1)

            # then determine its position in the temp series, and take the next spot
            endpoint <- (range5)[checkpoint]+1

        }
    }

    # the subtraction is necessary for this to work
    endpoint <- endpoint - 1


    # NOTE: New addition for the best.Range function (APR 2024).
    # This globally assigns an index for where to start 
    # performing additional checks (by running get.Range again)
    # for other stretches of winter temps within the original temp series
    nextStart <<- (endpoint + nextStart)

    # now with the true starting point of winter 
    # and the end (day before true start of growing) 
    # return the temperature series for the winter
    
    output <- list(temps = tempArray[startpoint:endpoint], dates = c(startpoint:endpoint))
    
    return(output)


}

#——————————————————————————————————————————————————————————————————————————

# Function that applies get.range multiple times to grab the longest winter stretch
# as there are sometimes false starts of 5 cold days that result in erroneously
# short winter temperature series

best.Range2 <- function(tempVectorOG){

    # make a working copy
    tempVector <- tempVectorOG

    # initalize variable (this needs to be globally scoped to work)
    nextStart <<- 0

    # get the first potential winter stretch 
    longRange <- get.Range2(tempVector) 

    # repeatedly check for other winter stretches after the previous detection 
    # but stop once there are not enough cold days left for winter to "start"
    while( 
          # ok... this make look like black magic, but here's how it works:
          # in the remaining temperature series
          # from the nextStart index to the end of the series
          # where you're looking for another potential winter stretch
          # this uses run length encoding to check whether
          # there are ANY points where 5 cold days occur in a row
          # (which requires slicing for TRUE values only)
          ((rle(tempVector[-(1:nextStart)] < winterTemp))$lengths[(rle(tempVector[-(1:nextStart)] < winterTemp))$values] >= 5) %>% any

          ){

        # add this so that the indices match the actual ordinal dates again
        dateFix <- nextStart
        # grab a subsequent range & correct the dates
        nextRange <- get.Range2(tempVector[-(1:nextStart)]) 
        nextRange$dates <- nextRange$dates + dateFix 

        # if this new range is longer than the previous
        if ( length(nextRange$temps) > length(longRange$temps) ) {

            # overwrite with the longest range detected so far 
            longRange <- nextRange
        }

    } # /end loop

    # return the longest series of winter temperatures
    return(longRange)

    # NOTE: This might be improved later
    # by having get.Range NOT output a global variable
    # but somehow calculate the nextRange point inside
    # best.Range after it uses get.Range
    # matching the outputted series with the source series
    # to define the index for the next start point in the source series

}

#——————————————————————————————————————————————————————————————————————————
 
# improved version that uses get.Range2 and best.Range2
get.winterOrdinal2 <- function(tempData, begin){

    # grab the start and end index for the data
    indices <- best.Range2(tempData[begin:(begin+365),2])$dates
    indices <- c(head(indices,1), tail(indices,1))

    startDay <- 200 + indices[1]

    endDay <- 200 + indices[2] - 365

    # return the ordinal dates for start of winter and end of winter (next year)
    return(c(startDay,endDay))

}

# ==== next section ====

# application: find the cooresponding index for ordinal day 200 in a given year
# then find the start and end of winter for that segment of time
# which begins on day 200, and goes into the next year past winter

# this works when running a single temp series at a time
# for GCM
aYear <- 2015
modelSeries <- trib.mods$nip[[1]] # n.temp # v.temp 
get.winterOrdinal2(modelSeries, which(modelSeries$year == aYear)[200])

# for baseline
aYear <- 2000
modelSeries <- n.temp # v.temp 
get.winterOrdinal2(modelSeries, which(modelSeries$year == aYear)[200])


# function with for loop to get average winter start and end over baseline years
w.dates.baseline <- function(modelSeries){

    years <- modelSeries$year %>% unique %>% head(-1)
    base.winters <- data.frame()
    # loop
    for (year in years) {
        base.winters <- rbind(base.winters, get.winterOrdinal2(modelSeries, which(modelSeries$year == year)[200]))
    }
    # fix the column names
    names(base.winters) <- c("w.start","w.end")
    # get the average
    output <- base.winters %>% colMeans %>% round

    # return the result
    return(output)

}

# results
w.dates.baseline(n.temp)
w.dates.baseline(s.temp)
w.dates.baseline(g.temp)
w.dates.baseline(v.temp)

#                                      ~ ~ ~                                    

# now try to do this for the 5 GCMS at once FOR a single year
aYear <- 2015
modelSeries <- trib.mods$nip
sapply( modelSeries, function(tribmodel){get.winterOrdinal(tribmodel, which(tribmodel$year == aYear)[200] )}) %>% rowMeans %>% round

#                                      ~ ~ ~                                    

# taking the above, do 5 gcms at once over a range of years and get averages
w.dates.future <- function(modelSeries, years){

    # empty storage for results
    future.winters <- data.frame()
    # loop
    for (year in years) {

        future.winters <- rbind(future.winters, sapply( modelSeries, function(tribmodel){get.winterOrdinal(tribmodel, which(tribmodel$year == year)[200] )}) %>% rowMeans %>% round)

    }

    # fix the column names
    names(future.winters) <- c("w.start","w.end")
    # get the average
    output <- future.winters %>% colMeans %>% round

    # return results
    return(output)
}

# ALL TOGETHER

w.dates <- rbind(
                 # nipigon
                 w.dates.baseline(n.temp),
                 w.dates.future(trib.mods$nip, 2040:2050),
                 w.dates.future(trib.mods$nip, 2090:2099),
                 # st louis
                 w.dates.baseline(s.temp),
                 w.dates.future(trib.mods$slou, 2040:2050),
                 w.dates.future(trib.mods$slou, 2090:2099),
                 # genesee
                 w.dates.baseline(g.temp),
                 w.dates.future(trib.mods$gen, 2040:2050),
                 w.dates.future(trib.mods$gen, 2090:2099),
                 # vermillion
                 w.dates.baseline(v.temp),
                 w.dates.future(trib.mods$verm, 2040:2050),
                 w.dates.future(trib.mods$verm, 2090:2099)
)


all.dates <- cbind(spawn.dates[,1:3], w.dates, t.trib)

# ---- Making the timeline plot ------------------------------------------------

library(viridis)

# for month axis to go at bottom of plot
month <- data.frame(name = month.abb[c(1:12,1:7)],
                    ordinal = c(1, 32, 60, 91, 121, 152, 
                                182, 213, 244, 274, 305, 
                                335, 366, 365+32, 365+60,
                                365+91, 365+121, 365+152, 365+182)
)


# put the extracted time points together for plotting
timeDat <- data.frame(
                      period    = c("0", "-1", "-2", "0", "-1", "-2"),
                      location  = c("nipigon", "nipigon", "nipigon", "vermillion", "vermillion", "vermillion"),
                      spawn1    = all.dates$spawnDaysRange1[all.dates$trib %in% c("nipigon", "vermillion")],
                      les       = all.dates$lastBday[all.dates$trib %in% c("nipigon", "vermillion")],
                      spawn2    = all.dates$spawnDaysRange2[all.dates$trib %in% c("nipigon", "vermillion")],
                      winter1   = all.dates$w.start[all.dates$trib %in% c("nipigon", "vermillion")],
                      winter2   = all.dates$w.end[all.dates$trib %in% c("nipigon", "vermillion")]+365
                      #o.winter2 = all.dates$w.end[all.dates$trib %in% c("nipigon", "vermillion")])
                      )


# configured for st.louis and genesee
timeDat.SG <- data.frame(
                      period    = c("0", "-1", "-2", "0", "-1", "-2"),
                      location  = c("stlouis", "stlouis", "stlouis", "genesee", "genesee", "genesee"),
                      spawn1    = all.dates$spawnDaysRange1[all.dates$trib %in% c("stlouis", "genesee")],
                      les       = all.dates$lastBday[all.dates$trib %in% c("stlouis", "genesee")],
                      spawn2    = all.dates$spawnDaysRange2[all.dates$trib %in% c("stlouis", "genesee")],
                      winter1   = all.dates$w.start[all.dates$trib %in% c("stlouis", "genesee")],
                      winter2   = all.dates$w.end[all.dates$trib %in% c("stlouis", "genesee")]+365
                      #o.winter2 = all.dates$w.end[all.dates$trib %in% c("nipigon", "vermillion")])
                      )


# — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — —
# for first year life history line 

# for this instance (genesee 2016 model 5)
# GDD is fully accrued before spawning temps are reached
# and not every day in the spawning period is suitable

modelSeries <- trib.mods$gen[[5]]
aYear <- 2016
get.spawning(modelSeries[which(modelSeries$year == aYear),])

# gdd ordinal date achieved
get.gdd(modelSeries[which(modelSeries$year == aYear),])
# [1] 144

# winter start and end ordinal
get.winterOrdinal(modelSeries[366:1096,], 200)
# [1] 334 104

# make a copy to get the gradient of GDD progress
gddArray <- (modelSeries[which(modelSeries$year == aYear),])

# zero out the temperatures below base
gddArray[gddArray[,2] < baseTemp, 2]  <- 0

# save the final GDD gradient, ending on the date of accrual
gddArray <- (gddArray[,2] %>% cumsum)[1:144]

# for plotting
gddArray <- data.frame(ordinal = 1:(gddArray %>% length), gdd = gddArray)

# tack on the rest of the time sereis as unchanged, for plotting
gddArray <- rbind(gddArray, data.frame(ordinal = 145:469, gdd = 639))

# — — — — — — — — — — — — — — — —
# alternatively: plot the temp series itself

library(scales)

# the temp series matching the right dates
gddArray <- modelSeries[366:(366+365+104),1:2]

# reset the index / row numbers
row.names(gddArray) <- NULL

# make the ordinals part of a continuous day count up
gddArray[366:470,1] <- gddArray[366:470,1] + 365

# fix column names
names(gddArray) <- c("ordinal", "temp")

# — — — — — — — — — — — — — — 
# alternative: use baseline data cause GCM data is too variable (looks bad)
# using genesee 2011 again 

dat <- s.temp
dat2 <- baseline[[3]]
gdd.dates <- get.winterOrdinal2(dat, 200)


# grab the whole year and into the next one
gddArray <- dat[1:(365+gdd.dates[2]+5),1:2]
names(gddArray)[1] <- "ordinal"
# make sure the ordinal dates keep coiunting up, instead of restarting after 365
gddArray[,1] <- 1:(365+gdd.dates[2]+5)

# make a copy
gdd.col <- gddArray

# zero out the temperatures below base
gdd.col[gdd.col[,2] < baseTemp, 2]  <- 0

# save the final GDD gradient, ending on the date of accrual
gdd.col <- (gdd.col[,2] %>% cumsum)[1:get.gdd(gddArray)]

# for plotting
gdd.col <- data.frame(ordinal = 1:(gdd.col %>% length), gdd = gdd.col)

# tack on the rest of the time series as unchanged, for plotting
gdd.col <- rbind(gdd.col, data.frame(ordinal = 1+length(gdd.col[,1]):(364+gdd.dates[2]+5),
                                     gdd = gdd.col[length(gdd.col[,1]),2]))

# combine the temp series with the gdd calculation
gddArray <- cbind(gddArray, gdd = gdd.col[,2])

# important dates
get.spawning(gddArray[,1:2])[1]
get.spawning(gddArray[,1:2]) %>% tail(1)

get.gdd(gddArray)

# winter dates
gdd.dates


# — — — — — — — — — — — — — — — —
# temp series with annotations later
# this is st louis baseline 2012
library(ggridges)

# force steady increase in GDD to see if it'll fix color gradient problem
gddArray[133:175,3] <- seq(15, 640, length.out = 43)
# plot(gddArray$gdd ~ gddArray$ordinal) # check that

# fixing the gdd values to have stable colours in rest of year 
gddArray[176:504,3] <- 650

# plot(gddArray$gdd ~ gddArray$ordinal)

fig1a <- ggplot(gddArray, aes(x = ordinal, height = temp, y = 0, fill = gdd)) + # color = gdd OR temp
    geom_ridgeline_gradient(gradient_lwd = 1, size = 0) +
    #ggplot(gddArray, aes(x = ordinal, y = temp,  color = gdd)) + # for non-ridgelines 
    #scale_color_gradient(high = "#D25471", low = "#171387") +
    scale_fill_gradient(high = "#D25471", low = "#171387", 
                        limits=c(0,640), oob = scales::squish,
                        name = "Degree days",
                        labels = c(0,200,400,633)) +
    #geom_line() +
    #geom_col(width = 1) + # tried this before but export had white line artifacts
    theme_classic() +
    theme(axis.line.y=element_blank(),
          axis.text.y=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.x =element_blank(),
          axis.ticks.x =element_blank(),
          axis.line.x =element_blank(),
          plot.margin = unit(c(0, -0.90, 0, -0.95), "cm"),
          legend.direction = "horizontal",
          legend.key.height = unit(0.2, 'cm'), #change legend key height
          legend.title = element_text(size=9), #change legend title font size
          legend.key.width = unit(0.6, 'cm'), 
          legend.background=element_blank(),
          legend.position = c(.11,.26)
          ) +
    guides(fill = guide_colorbar(title.position = "top")) +
    #ylim(0,150) +
    geom_segment(aes(x=0, xend=get.gdd(gddArray), y=15, yend=15), size = 0.3, linetype = "dashed", color = "white") +
    geom_segment(aes(x=get.spawning(gddArray)[1], xend=gdd.dates[1], y=18, yend=18), size = 0.3, linetype = "dashed", color = "white") +
    geom_segment(aes(x=(gdd.dates[1]-1), xend=gdd.dates[2]+370, y=10, yend=10), size = 0.3, linetype = "dashed", color = "black") +
    geom_segment(aes(x=get.spawning(gddArray)[1], xend=168, y=19, yend=24), size = 0.3, color = "gray50") +
    geom_segment(aes(x=133, xend=103, y=16, yend=23), size = 0.3, color = "gray50") +
    geom_segment(aes(x=168, xend=140, y=24, yend=28), size = 0.3, color = "gray50") +
    geom_segment(aes(x=175.5, xend=174, y=19, yend=24), size = 0.3, color = "gray50") +
    geom_segment(aes(x=174, xend=160, y=24, yend=30), size = 0.3, color = "gray50") +
    geom_segment(aes(x=176, xend=179, y=19.5, yend=33), size = 0.3, color = "gray50") +
    #geom_segment(aes(x=179, xend=199, y=26, yend=32), size = 0.3, color = "gray50") +
    geom_segment(aes(x=278, xend=280, y=11, yend=21), size = 0.3, color = "gray50") +
    #geom_segment(aes(x=278, xend=264, y=15, yend=30), size = 0.3, color = "gray50") +
    geom_segment(aes(x=440, xend=444, y=19, yend=15), size = 0.3, color = "gray50") +
    geom_segment(aes(x=497, xend=444, y=11, yend=15), size = 0.3, color = "gray50") +
    geom_text(label="15°C", aes(x=150, y=10), size = 3, color = "white") +
    geom_text(label="18°C", aes(x=215, y=14), size = 3, color = "white") +
    geom_text(label="10°C", aes(x=390, y=6), size = 3, color = "black") +
    geom_text(label="Maturation", aes(x=84, y=24), size = 3, color = "black") +
    geom_text(label="Spawning", aes(x=122, y=28), size = 3, color = "black") +
    geom_text(label="Larval drift", aes(x=152, y=33), size = 3, color = "black") +
    geom_text(label="Hatch, growth begins", aes(x=207, y=36), size = 3, color = "black") +
    geom_text(label="Growth ends, winter begins", aes(x=305, y=24), size = 3, color = "black") +
    geom_text(label="Winter ends, growth resumes", aes(x=450, y=23), size = 3, color = "black")

    #geom_vline(xintercept=get.spawning(gddArray)[1], color = "black", linetype = "solid", size=0.7) 
    #geom_vline(xintercept=gdd.dates[1], color = "white", linetype = "dotted", size=0.7) +
    #geom_vline(xintercept=dat2[1,"lastBday"], color = "white", linetype = "dotted", size=0.7)
    #geom_vline(xintercept=gdd.dates[2]+365, color = "white", linetype = "dotted", size=0.7) +
    #geom_vline(xintercept=get.spawning(gddArray[,1:2]) %>% tail(1), color = "white", linetype = "dotted", size=0.7) +


#ggsave(plot = fig1a, filename = "lifehist.png", path = "../graphics", bg = "white", width = 6, height = 3, dpi = 600)
     
# consider this for fixing the color gradient issue





# — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — —
# for OVERARCHING with management suggestions  


# Genesee 2011 as the example

# spawning days

library(reshape2)

timeDat.o <- data.frame(
                        period    = "0",
                        spawn1    = baseline[[1]][1, 6],
                        les       = baseline[[1]][1, 8],
                        spawn2    = baseline[[1]][1, 7],
                        winter1   = get.winterOrdinal2(g.temp, 200)[1],
                        winter2   = get.winterOrdinal2(g.temp, 200)[2]+365
) %>% melt

# add label (for the winter start ordinal date in the next year)
timeDat.o <- cbind(timeDat.o, label = c(173, 232, 271, 300, 76))

# !! below not actually needed because they seem to spawn every day within the period
# even if it's theoretically possible that they wouldn't, due to temp flux
#
# add on individual spawning days
#timeDat.o <- rbind(timeDat.o, data.frame(period = 0,
#                                         variable = "spawnDay",
#                                         value = get.spawning(g.temp[1:365,]),
#                                         label = NA
#                                         )
#)

top.time <- ggplot(timeDat.o,aes(x=value,y=as.numeric(period), col=variable, label=label)) +
    ylim(-0.20,2) + 
    xlim(-30,608) + 
    theme_classic() +
    theme(axis.line.y=element_blank(),
                 axis.text.y=element_blank(),
                 axis.title.x=element_blank(),
                 axis.title.y=element_blank(),
                 axis.ticks.y=element_blank(),
                 axis.text.x =element_blank(),
                 axis.ticks.x =element_blank(),
                 axis.line.x =element_blank(),
                 legend.title = element_blank(),
                 legend.key.height = unit(0.4, 'cm'), #change legend key height
                 legend.key.width = unit(1, 'cm'),
                 legend.text = element_text(size=8), #change legend text font size
#                 legend.position = "none",
                 plot.margin = unit(c(0, 0, 0, 0), "cm"),
                 legend.background = element_rect(fill='transparent'), #transparent legend bg
                 #legend.box.background = element_rect(fill='transparent'),
                 legend.position = c(.90,.88),
                 #legend.direction = "horizontal"
                ) +
    geom_hline(yintercept=0, color = "gray50", linetype = "dotted", size=0.4) + 
    geom_segment(aes(x=173,xend=271,y=0, yend=0), color = "#D25471") +
    geom_segment(aes(x=300,xend=441,y=0, yend=0), color = "#171387") +
    #geom_vline(aes(xintercept=365), linetype="dashed", size=0.75) +
    #geom_vline(aes(xintercept=152), linetype="dashed", size=0.75) +
    geom_point(aes(shape = variable), size=3) +
    scale_shape_manual(values=c("spawn1" = 20, "les" = 4, "spawn2" = 20,
                                "winter1" = 18, "winter2" = 18),
                       breaks = c("spawn1", "les", "winter1"),
                       labels=c("Spawning Period", "Last Effective Spawn", "Winter Period")) +
    scale_color_manual(values=c("spawn1" = "#D25471","les" = "#FAA543",
                                "spawn2" = "#D25471", "winter1" = "#171387",
                                "winter2" = "#171387"),
                       breaks = c("spawn1", "les", "winter1"),
                       labels=c("Spawning Period", "Last Effective Spawn", "Winter Period") ) +
    geom_text(aes(label = label, vjust=-0.95), size = 3, show_guide = F) +
    geom_text(data=month, aes(x=ordinal,y=-0.20,label=name),
              size=3, angle=90, color='black') +
    geom_segment(aes(x=-15, xend=90,y=1.5, yend=0.05), color = "gray50", size = 0.4) +
    geom_segment(aes(x=-15, xend=-15,y=1.5, yend=1.65), color = "gray50", size = 0.4) +
    geom_segment(aes(x=146, xend=166,y=0.12, yend=0.12), color = "gray50", size = 0.4) +
    geom_segment(aes(x=80, xend=146,y=1, yend=0.12), color = "gray50", size = 0.4) +
    geom_segment(aes(x=204, xend=224,y=0.12, yend=0.12), color = "gray50", size = 0.4) +
    geom_segment(aes(x=160, xend=204,y=0.75, yend=0.12), color = "gray50", size = 0.4) +
    geom_segment(aes(x=253, xend=263,y=0.12, yend=0.12), color = "gray50", size = 0.4) +
    geom_segment(aes(x=230, xend=253,y=0.5, yend=0.12), color = "gray50", size = 0.4) +
    geom_segment(aes(x=308, xend=323,y=0.12, yend=0.12), color = "gray50", size = 0.4) +
    geom_segment(aes(x=323, xend=370,y=0.12, yend=0.75), color = "gray50", size = 0.4) +
    geom_segment(aes(x=447, xend=457,y=0.12, yend=0.12), color = "gray50", size = 0.4) +
    geom_segment(aes(x=457, xend=520,y=0.12, yend=1), color = "gray50", size = 0.4) +
    geom_text(aes(x=-30,y=1.9,label="Pre-spawning period:\nMonitor for aggregations"),
              color='black', hjust = 0, size = 3.5, family = "serif") +
    geom_text(aes(x=40,y=1.5,label="Spawning begins:\nTarget aggregating adults, and youngest life stages"),
              color='black', hjust = 0, size = 3.5, family = "serif") +
    geom_text(aes(x=122,y=1,label="Last effective spawn:\nNot necessary to prevent further spawning"),
              color='black', hjust = 0, size = 3.5, family = "serif") +
    geom_text(aes(x=202,y=0.6,label="Spawning ends: target largest YOY"),
              color='black', hjust = 0, size = 3.5, family = "serif") +
    geom_text(aes(x=355,y=1,label="Winter begins:\nTarget adults"),
              color='black', hjust = 0, size = 3.5, family = "serif") +
    geom_text(aes(x=460,y=1.2,label="Winter ends:\nTarget surviving young-of-year"),
              color='black', hjust = 0, size = 3.5, family = "serif")
                        
    
#ggsave(plot = top.time, filename = "test.png", path = "../graphics", bg = "white", width = 9, height = 3)


# — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — —
# for NIPIGON
# reformatting for plotting, one place & period at a time
timeDat.n <- timeDat[1:3,] %>% melt
# add column for the ordinal date labels (for following year's winter end)
timeDat.n <- cbind(timeDat.n,
                 #label = c(228, 182, 160, NA, 208, 240, 246, 270, 288, 292, 291, 311, 168, 160, 102))
                   label = c(timeDat.n$value[-(13:15)], timeDat.n$value[13:15]-365) )
# plot position adjustment column
timeDat.n <- cbind(timeDat.n,
                 adjust = c(1,1,1,
                            -0.2,0.2,0.8,
                            0.2,0.8,0.8,
                            -0.1,-0.1,-0.1,
                            0.4,0.4,0.4
                            ))

# this theoretical point occurs BEFORE the spawning window so remove it (it looks too crowded anyway)
# it's the theoretical LES if conditions could allow them to spawn early enough
timeDat.n$value[4] <- NA
timeDat.n$label[4] <- NA

# !! NOTICE
# there are years where spawning doesn't happen at all
# 4 out of 9 years in nipigon baseline don't spawn
nip.time <- ggplot(timeDat.n,aes(x=value,y=as.numeric(period), col=variable, label=label)) +
    ylim(-3,1) + 
    theme_classic() +
    theme(axis.line.y=element_blank(),
                 axis.text.y=element_blank(),
                 axis.title.x=element_blank(),
                 axis.title.y=element_blank(),
                 axis.ticks.y=element_blank(),
                 axis.text.x =element_blank(),
                 axis.ticks.x =element_blank(),
                 axis.line.x =element_blank(),
                 legend.position = "none",
                 plot.margin = unit(c(0, 0.1, 0, 0), "cm")
                ) +
    geom_hline(yintercept=0, color = "gray50", linetype = "dotted", size=0.4) + 
    geom_hline(yintercept=-1, color = "gray50", linetype = "dotted", size=0.4) + 
    geom_hline(yintercept=-2, color = "gray50", linetype = "dotted", size=0.4) + 
    geom_segment(aes(x=timeDat.n$value[1],  xend=timeDat.n$value[7],   y=0,  yend=0),  color = "#D25471") +
    geom_segment(aes(x=timeDat.n$value[2],  xend=timeDat.n$value[8],   y=-1, yend=-1), color = "#D25471") +
    geom_segment(aes(x=timeDat.n$value[3],  xend=timeDat.n$value[9],   y=-2, yend=-2), color = "#D25471") +
    geom_segment(aes(x=timeDat.n$value[10], xend=timeDat.n$value[13], y=0,  yend=0),   color = "#171387") +
    geom_segment(aes(x=timeDat.n$value[11], xend=timeDat.n$value[14], y=-1, yend=-1),  color = "#171387") +
    geom_segment(aes(x=timeDat.n$value[12], xend=timeDat.n$value[15], y=-2, yend=-2),  color = "#171387") +
    geom_point(aes(shape = variable), size=3) +
    scale_shape_manual(values=c(20,4,20,18,18)) +
    scale_color_manual(values=c("#D25471","#FAA543","#D25471","#171387","#171387")) +
    #geom_vline(aes(xintercept=1), linetype="dashed", size=0.75) +
    #geom_vline(aes(xintercept=365), linetype="dashed", size=0.75) +
    geom_text(aes(label = label, hjust = adjust, vjust=-0.95), size = 3) +
    geom_text(data=month, aes(x=ordinal,y=-2.2,label=name),
              size=3, hjust=1, color='black', angle=90) +
     geom_text(aes(x=-2,y=0, hjust=0, vjust=-0.56, label = "Baseline"), color = "gray50", size = 3) +
     geom_text(aes(x=-2,y=-1, hjust=0, vjust=-0.5, label = "2050"), color = "gray50", size = 3) +
     geom_text(aes(x=-2,y=-2, hjust=0, vjust=-0.5, label = "2100"), color = "gray50", size = 3) 


# repeat for vermillion    
    
# reformatting for plotting, one place & period at a time
timeDat.v <- timeDat[4:6,] %>% melt
# add column for the ordinal date labels (for following year's winter end)
timeDat.v <- cbind(timeDat.v,
                 #label = c(148, 144, 124, 229, 258, 282, 270, 312, 331, 306, 321, 343, 468-365, 445-365, 409-365))
                   label = c(timeDat.v$value[-(13:15)], timeDat.v$value[13:15]-365) )
# plot position adjustment column
timeDat.v <- cbind(timeDat.v,
                 adjust = c(0.4,0.4,0.4,
                            1, 1, 1,
                            0.7, 1, 1,
                            -0.1, -0.1, 0.2,
                            0.4, 0.4, 0.1
                            ))
verm.time <- ggplot(timeDat.v,aes(x=value,y=as.numeric(period), col=variable, label=label)) +
    ylim(-3,1) + 
    theme_classic() +
    theme(axis.line.y=element_blank(),
                 axis.text.y=element_blank(),
                 axis.title.x=element_blank(),
                 axis.title.y=element_blank(),
                 axis.ticks.y=element_blank(),
                 axis.text.x =element_blank(),
                 axis.ticks.x =element_blank(),
                 axis.line.x =element_blank(),
                 legend.position = "none",
                 plot.margin = unit(c(0, 0, 0, 0.1), "cm")
                ) +
    geom_hline(yintercept=0, color = "gray50", linetype = "dotted", size=0.4) + 
    geom_hline(yintercept=-1, color = "gray50", linetype = "dotted", size=0.4) + 
    geom_hline(yintercept=-2, color = "gray50", linetype = "dotted", size=0.4) + 
    geom_segment(aes(x=timeDat.v$value[1],  xend=timeDat.v$value[7],  y=0, yend=0), color = "#D25471") +
    geom_segment(aes(x=timeDat.v$value[2],  xend=timeDat.v$value[8],  y=-1, yend=-1), color = "#D25471") +
    geom_segment(aes(x=timeDat.v$value[3],  xend=timeDat.v$value[9],  y=-2, yend=-2), color = "#D25471") +
    geom_segment(aes(x=timeDat.v$value[10], xend=timeDat.v$value[13], y=0, yend=0), color = "#171387") +
    geom_segment(aes(x=timeDat.v$value[11], xend=timeDat.v$value[14], y=-1, yend=-1), color = "#171387") +
    geom_segment(aes(x=timeDat.v$value[12], xend=timeDat.v$value[15], y=-2, yend=-2), color = "#171387") +
    geom_point(aes(shape = variable), size=3) +
    scale_shape_manual(values=c(20,4,20,18,18)) +
    scale_color_manual(values=c("#D25471","#FAA543","#D25471","#171387","#171387")) +
    #geom_vline(aes(xintercept=1), linetype="dashed", size=0.75) +
    #geom_vline(aes(xintercept=365), linetype="dashed", size=0.75) +
    geom_text(aes(label = label, hjust = adjust, vjust=-0.95), size = 3) +
    #geom_text(aes(x=1,y=0, hjust=0, vjust=-0.5, label = "Baseline"), color = "black", size = 5) +
    #geom_text(aes(x=1,y=-5, hjust=0, vjust=-0.5, label = "2050"), color = "black", size = 5) +
    #geom_text(aes(x=1,y=-10, hjust=0, vjust=-0.5, label = "2100"), color = "black", size = 5) +
    geom_text(data=month, aes(x=ordinal,y=-2.2,label=name),
              size=3, hjust=1, color='black', angle=90) 
    
    
# — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — —
# export


both.time <- plot_grid(nip.time, verm.time,
                       labels = c("Nipigon","Vermillion"),
                       label_size = 11,
                       label_fontface = "bold",
                       label_x = - -0.70, 
                       label_y = 1, 
                       align="h", 
                       ncol = 2)


all.time <- plot_grid(fig1a,
                      both.time,
                      top.time,
                      labels = c('a', 'b', 'c'),
                      ncol = 1,
                      rel_heights = c(0.25,0.35,0.4)
)

# save plot
ggsave(plot = all.time, filename = "timelines.png", path = "../graphics", bg = "white", width = 9, height = 5)


# as EPS
#ggsave(plot = all.time, filename = "timelines.eps", path = "../graphics", device = cairo_ps, bg = "white", width = 9, height = 6, fallback_resolution = 600)


# ===== making a two timeline plot for St. Louis and Genesee =====================
# as per revisions
 
# repeat for St Louis    
    
# reformatting for plotting, one place & period at a time
timeDat.s <- timeDat.SG[1:3,] %>% melt
# add column for the ordinal date labels (for following year's winter end)
timeDat.s <- cbind(timeDat.s,
                 #label = c(148, 144, 124, 229, 258, 282, 270, 312, 331, 306, 321, 343, 468-365, 445-365, 409-365))
                   label = c(timeDat.s$value[-(13:15)], timeDat.s$value[13:15]-365) )
# plot position adjustment column
timeDat.s <- cbind(timeDat.s,
                 adjust = c(1.2, 0.8, 0.4,
                            0.7, 1, 1,
                            0.7, 1, 1,
                            -0.1, -0.1, 0.2,
                            0.4, 0.4, 0.1
                            ))

# plot it
slou.time <- ggplot(timeDat.s,aes(x=value,y=as.numeric(period), col=variable, label=label)) +
    ylim(-3,1) + 
    theme_classic() +
    theme(axis.line.y=element_blank(),
                 axis.text.y=element_blank(),
                 axis.title.x=element_blank(),
                 axis.title.y=element_blank(),
                 axis.ticks.y=element_blank(),
                 axis.text.x =element_blank(),
                 axis.ticks.x =element_blank(),
                 axis.line.x =element_blank(),
                 legend.position = "none",
                 plot.margin = unit(c(0, 0, 0, 0.1), "cm")
                ) +
    geom_hline(yintercept=0, color = "gray50", linetype = "dotted", size=0.4) + 
    geom_hline(yintercept=-1, color = "gray50", linetype = "dotted", size=0.4) + 
    geom_hline(yintercept=-2, color = "gray50", linetype = "dotted", size=0.4) + 
    geom_segment(aes(x=timeDat.s$value[1],  xend=timeDat.s$value[7],  y=0, yend=0), color = "#D25471") +
    geom_segment(aes(x=timeDat.s$value[2],  xend=timeDat.s$value[8],  y=-1, yend=-1), color = "#D25471") +
    geom_segment(aes(x=timeDat.s$value[3],  xend=timeDat.s$value[9],  y=-2, yend=-2), color = "#D25471") +
    geom_segment(aes(x=timeDat.s$value[10], xend=timeDat.s$value[13], y=0, yend=0), color = "#171387") +
    geom_segment(aes(x=timeDat.s$value[11], xend=timeDat.s$value[14], y=-1, yend=-1), color = "#171387") +
    geom_segment(aes(x=timeDat.s$value[12], xend=timeDat.s$value[15], y=-2, yend=-2), color = "#171387") +
    geom_point(aes(shape = variable), size=3) +
    scale_shape_manual(values=c(20,4,20,18,18)) +
    scale_color_manual(values=c("#D25471","#FAA543","#D25471","#171387","#171387")) +
    #geom_vline(aes(xintercept=1), linetype="dashed", size=0.75) +
    #geom_vline(aes(xintercept=365), linetype="dashed", size=0.75) +
    geom_text(aes(label = label, hjust = adjust, vjust=-0.95), size = 3) +
    geom_text(data=month, aes(x=ordinal,y=-2.2,label=name),
              size=3, hjust=1, color='black', angle=90) +
     geom_text(aes(x=-2,y=0, hjust=0, vjust=-0.56, label = "Baseline"), color = "gray50", size = 3) +
     geom_text(aes(x=-2,y=-1, hjust=0, vjust=-0.5, label = "2050"), color = "gray50", size = 3) +
     geom_text(aes(x=-2,y=-2, hjust=0, vjust=-0.5, label = "2100"), color = "gray50", size = 3) 


# repeat for Genesee    
    
# reformatting for plotting, one place & period at a time
timeDat.g <- timeDat.SG[4:6,] %>% melt
# add column for the ordinal date labels (for following year's winter end)
timeDat.g <- cbind(timeDat.g,
                   label = c(timeDat.g$value[-(13:15)], timeDat.g$value[13:15]-365) )
# plot position adjustment column
timeDat.g <- cbind(timeDat.g,
                 adjust = c(0.4,0.4,0.4,
                            1, 1, 1,
                            0.7, 1, 1,
                            -0.1, -0.1, 0.2,
                            0.4, 0.4, 0.1
                            ))
# Plot it
gen.time <- ggplot(timeDat.g,aes(x=value,y=as.numeric(period), col=variable, label=label)) +
    ylim(-3,1) + 
    theme_classic() +
    theme(axis.line.y=element_blank(),
                 axis.text.y=element_blank(),
                 axis.title.x=element_blank(),
                 axis.title.y=element_blank(),
                 axis.ticks.y=element_blank(),
                 axis.text.x =element_blank(),
                 axis.ticks.x =element_blank(),
                 axis.line.x =element_blank(),
                 legend.position = "none",
                 plot.margin = unit(c(0, 0, 0, 0.1), "cm")
                ) +
    geom_hline(yintercept=0, color = "gray50", linetype = "dotted", size=0.4) + 
    geom_hline(yintercept=-1, color = "gray50", linetype = "dotted", size=0.4) + 
    geom_hline(yintercept=-2, color = "gray50", linetype = "dotted", size=0.4) + 
    geom_segment(aes(x=timeDat.g$value[1],  xend=timeDat.g$value[7],  y=0, yend=0), color = "#D25471") +
    geom_segment(aes(x=timeDat.g$value[2],  xend=timeDat.g$value[8],  y=-1, yend=-1), color = "#D25471") +
    geom_segment(aes(x=timeDat.g$value[3],  xend=timeDat.g$value[9],  y=-2, yend=-2), color = "#D25471") +
    geom_segment(aes(x=timeDat.g$value[10], xend=timeDat.g$value[13], y=0, yend=0), color = "#171387") +
    geom_segment(aes(x=timeDat.g$value[11], xend=timeDat.g$value[14], y=-1, yend=-1), color = "#171387") +
    geom_segment(aes(x=timeDat.g$value[12], xend=timeDat.g$value[15], y=-2, yend=-2), color = "#171387") +
    geom_point(aes(shape = variable), size=3) +
    scale_shape_manual(values=c(20,4,20,18,18)) +
    scale_color_manual(values=c("#D25471","#FAA543","#D25471","#171387","#171387")) +
    #geom_vline(aes(xintercept=1), linetype="dashed", size=0.75) +
    #geom_vline(aes(xintercept=365), linetype="dashed", size=0.75) +
    geom_text(aes(label = label, hjust = adjust, vjust=-0.95), size = 3) +
    #geom_text(aes(x=1,y=0, hjust=0, vjust=-0.5, label = "Baseline"), color = "black", size = 5) +
    #geom_text(aes(x=1,y=-5, hjust=0, vjust=-0.5, label = "2050"), color = "black", size = 5) +
    #geom_text(aes(x=1,y=-10, hjust=0, vjust=-0.5, label = "2100"), color = "black", size = 5) +
    geom_text(data=month, aes(x=ordinal,y=-2.2,label=name),
              size=3, hjust=1, color='black', angle=90) 


# put them together
both.time <- plot_grid(slou.time, gen.time,
                       labels = c("St. Louis","Genesee"),
                       label_size = 11,
                       label_fontface = "bold",
                       label_x = - -0.70, 
                       label_y = 0.9, 
                       align="h", 
                       ncol = 2)



# save plot
ggsave(plot = both.time, filename = "timelinesSG.png", path = "../graphics", bg = "white", width = 9, height = 3)



# ===== making a daily average temperature plot =====================
# as requested by reviewer

# first, getting the average daily temperatures
 
# for nipigon
nip.base <- aggregate(tas~j.date,
                      subset(waterC, gcm == "base" & tributary == "nip", select = -c(tributary, gcm, year)),
                      mean)

nip.mid <- aggregate(tas~j.date,
                     subset(waterC, year %in% 2040:2050 & tributary == "nip", select = -c(tributary, gcm, year)),
                     mean)

nip.late <- aggregate(tas~j.date,
                      subset(waterC, year %in% 2090:2099 & tributary == "nip", select = -c(tributary, gcm, year)),
                      mean)

# add a label to identify each period, and slap them together
nip.trio <- rbind(
                  nip.base <- cbind(nip.base, Period = "Baseline"),
                  nip.mid  <- cbind(nip.mid,  Period = "Mid-century"),
                  nip.late <- cbind(nip.late, Period = "Late-century")
)



# for st louis
slou.base <- aggregate(tas~j.date,
                      subset(waterC, gcm == "base" & tributary == "slou", select = -c(tributary, gcm, year)),
                      mean)
#
slou.mid <- aggregate(tas~j.date,
                     subset(waterC, year %in% 2040:2050 & tributary == "slou", select = -c(tributary, gcm, year)),
                     mean)
#
slou.late <- aggregate(tas~j.date,
                      subset(waterC, year %in% 2090:2099 & tributary == "slou", select = -c(tributary, gcm, year)),
                      mean)
#
# add a label to identify each period, and slap them together
slou.trio <- rbind(
                  slou.base <- cbind(slou.base, Period = "Baseline"),
                  slou.mid  <- cbind(slou.mid,  Period = "Mid-century"),
                  slou.late <- cbind(slou.late, Period = "Late-century")
)




# for genesee
gen.base <- aggregate(tas~j.date,
                      subset(waterC, gcm == "base" & tributary == "gen", select = -c(tributary, gcm, year)),
                      mean)
#
gen.mid <- aggregate(tas~j.date,
                     subset(waterC, year %in% 2040:2050 & tributary == "gen", select = -c(tributary, gcm, year)),
                     mean)
#
gen.late <- aggregate(tas~j.date,
                      subset(waterC, year %in% 2090:2099 & tributary == "gen", select = -c(tributary, gcm, year)),
                      mean)
#
# add a label to identify each period, and slap them together
gen.trio <- rbind(
                  gen.base <- cbind(gen.base, Period = "Baseline"),
                  gen.mid  <- cbind(gen.mid,  Period = "Mid-century"),
                  gen.late <- cbind(gen.late, Period = "Late-century")
)



# for vermillion
verm.base <- aggregate(tas~j.date,
                      subset(waterC, gcm == "base" & tributary == "verm", select = -c(tributary, gcm, year)),
                      mean)
#
verm.mid <- aggregate(tas~j.date,
                     subset(waterC, year %in% 2040:2050 & tributary == "verm", select = -c(tributary, gcm, year)),
                     mean)
#
verm.late <- aggregate(tas~j.date,
                      subset(waterC, year %in% 2090:2099 & tributary == "verm", select = -c(tributary, gcm, year)),
                      mean)
#
# add a label to identify each period, and slap them together
verm.trio <- rbind(
                  verm.base <- cbind(verm.base, Period = "Baseline"),
                  verm.mid  <- cbind(verm.mid,  Period = "Mid-century"),
                  verm.late <- cbind(verm.late, Period = "Late-century")
)



# nipigon subplot
tempFig.A <- ggplot(nip.trio, aes(x=j.date, y=tas, col=Period)) +
             ylim(-1,35) + 
             theme_classic() +
             theme(#axis.line.y=element_blank(),
                   #axis.text.y=element_blank(),
                   axis.title.x=element_blank(),
                   axis.title.y=element_blank(),
                   #axis.ticks.y=element_blank(),
                   axis.text.x =element_blank(),
                   #axis.ticks.x =element_blank(),
                   #axis.line.x =element_blank(),
                   legend.position = "none",
                   plot.margin = unit(c(0, 0, 0, 0.1), "cm")
                   ) +
             geom_line() +
             scale_color_manual(values=c("#FCA409FF", "#C43C4EFF", "#E45A32FF"))
# st louis 
tempFig.B <- ggplot(slou.trio, aes(x=j.date, y=tas, col=Period)) +
             ylim(-1,35) + 
             theme_classic() +
             theme(#axis.line.y=element_blank(),
                   #axis.text.y=element_blank(),
                   axis.title.x=element_blank(),
                   axis.title.y=element_blank(),
                   #axis.ticks.y=element_blank(),
                   axis.text.x =element_blank(),
                   #axis.ticks.x =element_blank(),
                   #axis.line.x =element_blank(),
                   legend.position = "none",
                   plot.margin = unit(c(0, 0, 0, 0.1), "cm")
                   ) +
             geom_line() +
             scale_color_manual(values=c("#FCA409FF", "#C43C4EFF", "#E45A32FF"))
# genesee
tempFig.C <- ggplot(gen.trio, aes(x=j.date, y=tas, col=Period)) +
             ylim(-1,35) + 
             theme_classic() +
             theme(#axis.line.y=element_blank(),
                   #axis.text.y=element_blank(),
                   axis.title.x=element_blank(),
                   axis.title.y=element_blank(),
                   #axis.ticks.y=element_blank(),
                   axis.text.x =element_blank(),
                   #axis.ticks.x =element_blank(),
                   #axis.line.x =element_blank(),
                   legend.position = "none",
                   plot.margin = unit(c(0, 0, 0, 0.1), "cm")
                   ) +
             geom_line() +
             scale_color_manual(values=c("#FCA409FF", "#C43C4EFF", "#E45A32FF"))
# vermillion
tempFig.D <- ggplot(verm.trio, aes(x=j.date, y=tas, col=Period)) +
             ylim(-1,35) + 
             theme_classic() +
             theme(#axis.line.y=element_blank(),
                   #axis.text.y=element_blank(),
                   axis.title.x=element_blank(),
                   axis.title.y=element_blank(),
                   #axis.ticks.y=element_blank(),
                   #axis.text.x =element_blank(),
                   #axis.ticks.x =element_blank(),
                   #axis.line.x =element_blank(),
                   legend.position = "none",
                   plot.margin = unit(c(0, 0, 0, 0.1), "cm")
                   ) +
             geom_line() +
             scale_color_manual(values=c("#FCA409FF", "#C43C4EFF", "#E45A32FF"))
# multiplot
fourTemps <- plot_grid(tempFig.A, tempFig.B, tempFig.C, tempFig.D,
                       labels = c("Nipigon", "St. Louis", "Genesee", "Vermillion"),
                       label_size = 11,
                       label_fontface = "bold",
                       label_x = 0.80, 
                       label_y = 0.9, 
                       align="v", 
                       ncol = 1,
                       nrow = 4)
# labels for the multiplot
y.grob <- textGrob("Water Temperature (°C)", vjust = 0.3, 
                   gp=gpar(col="black", fontsize=15), rot=90)
#
x.grob <- textGrob("Ordinal Date", vjust = .75,
                   gp=gpar(col="black", fontsize=15))
# and now put that all together
fourTemps_export <- grid.arrange(arrangeGrob(fourTemps, left = y.grob, bottom = x.grob))
# and save it
ggsave(plot = fourTemps_export, filename = "fourTemps.png", path = "../graphics", bg = "white", width = 6, height = 7)
