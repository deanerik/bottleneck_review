#!/usr/bin/env Rscript

# 5. Run model with climate data.R
#    Licence: GNU GPLv3 - see LICENSE.txt for more details

# Loading and processing temperature series data

#———————————————————————————————————————————————————————————————————————————————

# load the necessary functions to run analyses
source("./4. Young-of-year fish model functions.R") 

# prevent outputs from being in scientific notation
options(scipen = 100, digits = 4)

#---- 1. Run model on baseline data --------------------------------------------

# load historical water temperature data from Jones et al. 2017
g.temp <- read.csv("../data/gBaseline.csv")
n.temp <- read.csv("../data/nBaseline.csv")
s.temp <- read.csv("../data/sBaseline.csv")
v.temp <- read.csv("../data/vBaseline.csv")
 
# package all the baseline results together
baseline <- list(genesee    = get.results(g.temp),
                 nipigon    = get.results(n.temp),
                 stlouis    = get.results(s.temp),
                 vermillion = get.results(v.temp))

# data source: 
#   Jones, L.A., Drake, D.A.R., Mandrak, N.E., 
#   Jerde, C.L., Wittmann, M.E., Lodge, D.M., 
#   van der Lee, A.S., Johnson, T.B., and Koops, M.A.  2017. 
#   Modelling Survival and Establishment of Grass Carp, 
#   Ctenopharyngodon idella, in the Great Lakes Basin. 
#   DFO Can. Sci. Advis. Sec.  Res. Doc. 2016/101. vi + 52 p.

#---- 2. Run model on GCM data -------------------------------------------------

# There are 5 climate models in the ensemble,
# and each model was used to make daily temperature projections 
# for each of the 4 tributaries, totalling 20 temperature series.
# The data came grouped by model — containing 4 series per tributary,
# but needs to be grouped by tributary — containing the 5 GCM series.
# Then the model can be run 5 times per tributary 
# using each climate model's temperature series,
# and the ensemble of results can be averaged for reporting. 

# First, we'll need some functions to process this data

# load necessary functions
library(reshape2)
library(magrittr)

# air-to-water conversion function, based on zeroed Thames River air temps
mod.temp2 <- function(x){ 
    y <- .927 * x + 2.952
    return(y)
}


#——————————————————————————————————————————————————————————————————————————

# load gcm data
gcm.data <- read.csv("../data/gcmOutputs.csv")

# vector of names to identify each climate model
gcm.names <- unique(gcm.data$gcm)

# vector of names to identify each tributary 
trib.names <- unique(gcm.data$tributary)

# adjust negative air temperatures to zero
gcm.data$tas[which(gcm.data$tas < 0)] <- 0

# convert air temperatures to water temperatures
gcm.data$tas <- mod.temp2(gcm.data$tas)


# — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — — —
 
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

# make a copy to modify for export
trib.export <- melt(trib.data, 
                    id = c("year","j.date", "ecEarth3", "hadGEM3", "inmCM5", "mriESM2", "ukESM1"),
                    level = "tributary" )

# fix the name for the level
names(trib.export)[8] <- "tributary"

# export reformatted gcm data 
write.csv(trib.export, "../data/tribData.csv", row.names = FALSE)
 
 

#——————————————————————————————————————————————————————————————————————————

# Reformat climate data into individual gcm datasets for use by another function

# make list of lists, 4 tributaries each containing 5 gcms 
trib.mods <- lapply(trib.data, function(tributary){

                        # empty placeholder for each tributary's 5 gcm series
                        sublist <- list()

                        # loop over each of the 5 models
                        for (model in gcm.names) {

                            # make data frame with 1 selected model
                            sublist[[model]] <- tributary[,c("j.date", model,"year")]
                            
                            # rename value column for consistency
                            names(sublist[[model]])[2] <- "temp"

                        }

                        # return the list for 1 trib (which gets done 4 times) 
                        return(sublist)
}
)

#——————————————————————————————————————————————————————————————————————————

# simulation results for each model's temperature series in each tributary
vermSummary <- lapply(trib.mods[[1]], get.results) 
slouSummary <- lapply(trib.mods[[2]], get.results) 
 genSummary <- lapply(trib.mods[[3]], get.results) 
 nipSummary <- lapply(trib.mods[[4]], get.results) 

# function to average results between GCM outputs per tributary
mean.gcm <- function(tributary.data) {
                         
    # data frame to filled with averaged results
    output <- data.frame(year = 2015:2099)

    # loop across results from 2nd to 13th columns (1st is just year) 
    for (column in 2:13) {

        # take column from each gcm (list item) and average by year (row)
        output[column] <- lapply(tributary.data, function(gcm) {
                                          gcm[column] }) %>% 
                                          as.data.frame %>% 
                                          rowMeans(na.rm = T)
    }

    # apply labels for the outputted data frame 
    names(output) <- c("year", "num.growDays", "growPercent_year", 
                       "num.spawnDays", "spawnPercent_year", "spawnDaysRange1",
                       "spawnDaysRange2", "lastBday", "surviveNum", 
                       "percentSpawnSurvive", "theoryMin", "wintDay", "wintPer")

    # return the data frame of averaged results
    return(output)
}

# compute the average results for each tributary & put together in a list
future <- list(genesee    = mean.gcm(genSummary), 
               nipigon    = mean.gcm(nipSummary),
               stlouis    = mean.gcm(slouSummary),
               vermillion = mean.gcm(vermSummary)
)

#---- 3. Reshape data for export ----------------------------------------------

# melt the results data into a long-format data frame for export
baselineOutput <- melt(baseline, id = "year", level = "tributary" ) 
futureOutput <- melt(future, id = "year", level = "tributary" ) 

# fix the levels column name
names(baselineOutput)[4] <- "tributary"
names(futureOutput)[4] <- "tributary"

# bind the two sets of results together
output <- rbind(baselineOutput,futureOutput)

# export results
write.csv(output, "../results/results.csv", row.names = FALSE)
