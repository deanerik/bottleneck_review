#!/usr/bin/env Rscript

# 4. Young-of-year fish model functions.R
#    Licence: GNU GPLv3 - see LICENSE.txt for more details

# Core functions and utilities to run simulations 
# of the age-0 Bighead Carp life cycle

#———————————————————————————————————————————————————————————————————————————————

# juliGreg      →  gregorian date from julian / ordinal date
# joGrow        →  daily growth increment from temperature
# get.spawning  →  all possible spawning dates for a given year
# get.cohort    ☞  all possible sizes for a given year

# get.Range     →  delineates start and end of a given winter 
# best.Range    →  checks for additional stretches winter & selects the longest one
# get.winter    ☞  gets winter temperature series for all years

# alpha         →  temperature-sensitive factor for energy consumption calculations
# ┗━get.cost    →  sum of daily energy costs over winter, per fish size
# get.reserves  →  get expendable energy from length, for each possible fish size
# get.outcome   ☞  get energy balance for each fish at end of a given winter 

# get.results   ☞  cohort sizes and overwinter survival outcomes for all years

# ---- Settings — scandalously global variables to direct all analyses ----

library(magrittr)

hatchSize  <- 7/10 # 7 mm, converted to cm
degreeDays <- 633
baseTemp   <- 15
spawnTemp  <- 18
winterTemp <- 10

# ---- Cohort Data — all possible fish sizes by onset of winter ----

# Julilan date to gregorian conversion (for intepreting and reporting)
# ** takes format DDD, YY **
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

#——————————————————————————————————————————————————————————————————————————
 
# jones growth increment in centimeters
joGrow <- function(temperature){
    increment <- -0.23 + 0.016 * temperature 
    return(increment)
}

#——————————————————————————————————————————————————————————————————————————

# takes a vector with date and temperature
get.spawning <- function(tempArray){

    # copy for GDD calculations 
    gddArray <- tempArray

    # exclude the days below the base GDD temperature (zero them)
    gddArray[gddArray[,2] < baseTemp, 2]  <- 0

    # Zero the days that don't have enough degree days, or high enough temperature
    gddArray[!(gddArray[,2] %>% cumsum >= degreeDays & gddArray[,2] >= spawnTemp), 2]  <- 0

    # spawning occurs on each of these days
    spawnDays <- gddArray[gddArray[,2] > 0,1]

    # return the spawning days 
    return(spawnDays)

}

#——————————————————————————————————————————————————————————————————————————
 
# takes a vector with ordinal date and temperature
get.cohort <- function(tempArray){

    # get spawning dates
    spawnDays <- get.spawning(tempArray)

    # growing can occur on these days
    growDays <- (tempArray[,2] > winterTemp) %>% which 

    # length of growing and spawning seasons
    n.growDays <- growDays %>% length 
    n.spawnDays <- spawnDays %>% length

    # amount of growth each day
    increments <- tempArray[growDays,2] %>% joGrow

    # Assumed minimum growth amount (no negative growth allowed)
    # joGrow has negative growth until 15C
    # so if starving happens below 10, then 10 to 15 assumed neutral
    increments[increments <= 0] <- 0 

    # if spawning can't happen, then log NA for some results
    if(identical(spawnDays, integer(0))){

        cohort <- data.frame(bday = NA, size = NA )

        output <- list(bdaySizes         = cohort,
                       num.growDays      = growDays %>% length,
                       growPercent_year  = round(n.growDays/365,2),
                       num.spawnDays     = 0,
                       spawnPercent_year = 0,
                       spawnDaysRange    = c(NA, NA),
                       gd                = c(0,0,0,0),
                       growArray         = growDays,
                       incrementArray    = increments
                       )

    # otherwise, proceed
    } else {

        # for each spawn date, 
        # sum growth from all growing days after spawning until winter starts
        eachGrowth <- sapply(spawnDays, function(spawnDate){
                                 increments[(growDays > spawnDate) %>% which] %>% sum }
                            ) 

        # cm sizes of all individuals and their birthday
        # hatchsize + growth increments (sizes)
        cohort <- data.frame(bday = spawnDays, size = (eachGrowth+hatchSize) )

        # first and last spawning days, julian date
        j.spawnRange <- c(spawnDays %>% head(1), spawnDays %>% tail(1))

        # same as previous, but converted to gregorian dates 
        g.spawnRange <- c( substr(juliGreg(j.spawnRange[1],10),6,10),
                          substr(juliGreg(j.spawnRange[2],10),6,10) )

        # change format so it can be put into a data frame 
        g.spawnRange <- strsplit(g.spawnRange, "-")  %>% unlist %>% as.integer
    
        # wrap that output into a list that includes:
        #   birthday and size 
        #   how many growing days
        #   how much of the year was suitable for growing (growing season fraction)
        #   how many spawning days
        #   and how much of the year was suitable for spawning
        #   first and last spawn days (julian then gregorian)
        output <- list(bdaySizes         = cohort,
                       num.growDays      = growDays %>% length,
                       growPercent_year  = round(n.growDays/365,2),
                       num.spawnDays     = spawnDays %>% length,
                       spawnPercent_year = round(n.spawnDays/365, 2),
                       spawnDaysRange    = j.spawnRange, 
                       gd                = g.spawnRange,
                       growArray         = growDays,
                       incrementArray    = increments
                       )
    } # \end else



    # return the list
    return(output)
}

# ---- Winter data: temperature series for seasons of variable duration ----

# Tool to grab the range of days that qualify for a given winter 
# (starting from the end of one calendar year, crossing through to the start of another)

# takes a temp array that is pre-cut to include pre- and post-winter days
# and narrows down to the specific winter days, and extracts their temp series
# so that it can be used for daily temp metabolism calculations
get.Range <- function(tempArrayOG){

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
    return(tempArray[startpoint:endpoint])

}

#——————————————————————————————————————————————————————————————————————————

# Function that applies get.range multiple times to grab the longest winter stretch
# as there are sometimes false starts of 5 cold days that result in erroneously
# short winter temperature series

best.Range <- function(tempVectorOG){

    # make a working copy
    tempVector <- tempVectorOG

    # initalize variable (this needs to be globally scoped to work)
    nextStart <<- 0

    # get the first potential winter stretch 
    longRange <- get.Range(tempVector) 

    # repeatedly check for other winter stretches after the previous detection 
    # but stop once there are not enough cold days left for winter to "start"
    while( 
          # ok... this might look like black magic, but here's how it works:
          # in the remaining temperature series
          # from the nextStart index to the end of the series
          # where you're looking for another potential winter stretch
          # this uses run length encoding to check whether
          # there are ANY points where 5 cold days occur in a row
          # (which requires slicing for TRUE values only)
          ((rle(tempVector[-(1:nextStart)] < winterTemp))$lengths[(rle(tempVector[-(1:nextStart)] < winterTemp))$values] >= 5) %>% any

          ){

        # grab a subsequent range
        nextRange <- get.Range(tempVector[-(1:nextStart)]) 

        # if this new range is longer than the previous
        if ( length(nextRange) > length(longRange) ) {

            # overwrite with the longest range detected so far 
            longRange <- nextRange
        }

    } # /end loop

    # return the longest series of winter temperatures
    return(longRange)

    # NOTE: This might be improved later
    # by having get.Range NOT output a global variable
    # (because globals are bad style, and could introduce vulnerability)
    # but somehow calculate the nextRange point inside
    # best.Range after it uses get.Range
    # matching the outputted series with the source series
    # to define the index for the next start point in the source series

}

#——————————————————————————————————————————————————————————————————————————

# grab the temperature series for all years and assemble
get.winter <- function(tempSeries){

    # determine how many years to operate on 
    years <- tempSeries$year %>% unique

    # empty container for the output
    winterArray <- list()

    # loop across years in the temperature series to link the DJF season
    for (index in seq_along(years)){

        # but stop the loop when you're at the last year
        if (index == length(years)) break 

        # mark the key dates so you can extract DJF from adjacent years:

        # first year: start of winter (day after last growing day)
        
        # cut out part of the years that should contain winter by merging: 
        # end portion of a year (June 21 to Dec 31)
        yearEnd <- tempSeries$temp[tempSeries$year == years[index]][172:365]

        # start portion of next year (Jan 1 to July 21) that should contain winter's end
        yearStart <- tempSeries$temp[tempSeries$year == years[index+1]][1:202]

        # combine it
        winterRun <- c(yearEnd,yearStart)

        # get the winter temperature series
        # insert that one winter season into a new list element 
        winterArray[[index]] <- best.Range(winterRun)

    } # \end loop

    # label by year and return the list of winter arrays 
    # (final year is excluded, as it only contributes Jan and Feb to the previous year)
    names(winterArray)  <- paste0("y",years[1:length(years)-1])
    return(winterArray)

}

# ---- Overwinter survival outcomes ----
 
# Shuter et al. (1980) calculation for alpha, which changes with temperature
alpha <- function(temperature){(0.6 * 10^-7)*2 ^ ((temperature-5)/10)}

#—————————————————————————————————————————————————————————————————————————

# make a function to get your total alpha
# gives you every fish size, and each day's total metabolic demand for that fish
# ** Takes vectors for size and winter temperature **
get.cost <- function(sizeArray,winterArray){
    bmrArray <- lapply(sizeArray, function(sizeinput){ alpha(winterArray) * (sizeinput*10)^2.4 })
    
    lapply(bmrArray,sum) %>% unlist

}

#——————————————————————————————————————————————————————————————————————————
 
# determine the amount of depletable energy stores (length in millimeters) 
# From Shuter et al. 1980
get.reserves <- function(sizeArray){

    ashfree.initial  <- 10^(-7.073 + 3.7263*log10(sizeArray*10))
    ashfree.critical <- 10^(-7.418+3.7263*log10(sizeArray*10))

    return(ashfree.initial - ashfree.critical)

}

#——————————————————————————————————————————————————————————————————————————

# get the result of overwinter survival
# from the sizes of the fish, get their energy stores
# and then see if it's greater than the energetic cost of winter
get.outcome <- function(sizeArray, winterArray){

    # if nobody was born, then survival outcome is 0
    if(is.na(sizeArray[1])){
        
        outcome <- 0 
    
    } else {

        # subract cumulative energy demand from the reserves
        outcome <- get.reserves(sizeArray) - get.cost(sizeArray, winterArray)
    }

    # total surviving days
    num.survive <- outcome[(outcome >= 0.01)]  %>% length

    # how long was the effective winter season (N-DJF-M <10C)
    n.winterDays <- length(winterArray)

    # don't proceed with most analysis if nobody survives
    if(num.survive == 0){

        # theoretical minimum gradient starting from 8 (likely always big enough to survive)
        min.size.range <- seq(8,1,-0.01)
        t.outcome <- get.reserves(min.size.range) - get.cost(min.size.range, winterArray)
        t.min.survive <- min.size.range[(t.outcome >= 0.01)] %>% tail(1) %>% round(2)

        # outputs that CAN still be done
        output <- list(cohortEnergyOutcome = NA,
                       surviveNum          =  0,
                       percentSpawnSurvive =  0,
                       minSize             = NA,
                       theoryMin           = t.min.survive,
                       wintDay             = n.winterDays, # num of days spent starving 
                       wintPer             = round(n.winterDays/365,2) ) # proportion of year spent starving
    } else {

        # smallest surviving size
        min.survive <- sizeArray[(outcome >= 0.01)] %>% tail(1) %>% round(2)

        # Theoretical minimum size to survive that winter
        # take the lowest surviving size from the simulation
        # theoretical downwards size gradient from the minimum simulated survival size
        min.size.range <- seq(min.survive, 1, -0.01)

        # repeat earlier steps to see the smallest surviving size possible 
        t.outcome <- get.reserves(min.size.range) - get.cost(min.size.range, winterArray)
        t.min.survive <- min.size.range[(t.outcome >= 0.01)] %>% tail(1) %>% round(2)

        # in case the minimum simulated is already the theoretical minimum
        if(identical(t.min.survive, numeric(0))){

            t.min.survive <- min.survive

        }

        # how many spawning days resulted in surviving sizes?
        surviveFraction <- (num.survive/length(outcome)) %>% round(2)

        # wrap all outputs into a named list
        output <- list(cohortEnergyOutcome = outcome, # energy balance per fish 
                       surviveNum          = num.survive, # num surviving fish
                       percentSpawnSurvive = surviveFraction, # % spawned that survive
                       minSize             = min.survive, # smallest surviving size simulated
                       theoryMin           = t.min.survive, # smallest possible surviving size
                       wintDay             = n.winterDays, # num of days spent starving 
                       wintPer             = round(n.winterDays/365,2) ) # proportion of year spent starving

    } # \end else

    # return the outputs 
    return(output)
}

# ---- Running the entire model ----

# takes dataframe columns: ordinal / temp / year
get.results <- function(temperatureData){

    # make a working copy of the temperature series
    temperatures <- temperatureData 

    # check what years we got
    tempYears <- temperatures$year %>% unique #; print(tempYears)

    # get all the winter starvation series 
    winterTemps <- get.winter(temperatures[,1:3])
     
    # initiate empty storage for results
    results <- data.frame()

    # loop across all years 
    for (num in seq_along(tempYears)){

        # but stop before running the final year (winters take 2 calendar years)
        if (num == length(tempYears) ) break

        # get the size-cohort for the year
        cohort <- get.cohort( temperatures[temperatures$year == tempYears[num],1:2] )

        # get the overwinter survival outcome
        outcome <- get.outcome(cohort$bdaySizes$size, winterTemps[[num]])

        # calculate last date of effective spawn

        # look at all theoretical growing days (could be before spawning)
        # reverse growth increments to go backwards in time from winter
        # look at the size increase by spawning each day earlier via cumsum 
        # add the hatch size to increments to get final size 
        # check to see if they're above the minimum
        # reverse it again to go back to the right index numbers
        # grab the last day that can still survive
         
        minIndex <- which((cohort$incrementArray %>% rev %>% cumsum %>% round(2) + hatchSize >= outcome$theoryMin) %>% rev) %>% tail(1)

        # In case sufficient growth just isn't possible, then assign an NA
        if(identical(minIndex, integer(0))){
        
            lastBday <- NA 
        
        } else {
            # the last effective spawn date needs adjustment by 1 index position
            lastBday <- cohort$growArray[[minIndex]] - 1
        }

        # combine previous outputs 
        newRow <- rbind(c(cohort[c(2:6)] %>% unlist, 
                          lastBday = lastBday,
                          outcome[c(2,3,5:7)] %>% unlist)) # with julian ones
        
        # alternate output
        #newRow <- rbind(c(cohort[c(2:5,7)] %>% unlist, outcome[c(2,3,5:7)] %>% unlist)) # with month/day spawn dates

        # add on each year's total output as a new row
        results <- rbind(results,newRow)

    } # \end loop
    
    # slap on a column of the years (excluding the last one)
    results <- cbind(year = tempYears[1:length(tempYears)-1], results)

    # output the results 
    return(results)
}
