#!/usr/bin/env Rscript

# 3. Perform air-to-water temperature conversions.R
#    Licence: GNU GPLv3 - see LICENSE.txt for more details

# Various functions to perform the air to water temperature conversion
# necessary to turn GCM projected temperatures into something that can
# be used with the young-of-year life-history model & simulations


# ---- Thames River air and water temperature data -----------------------------

# if Thames river temperature data needs to be downloaded, do it
if (file.exists("../data/thames.csv")) {

     # but if you've already got it downloaded, just load it
     thames <- read.csv("../data/thames.csv")

} else {

      # RETRIEVE WATER TEMPERATURES FROM GAUGING STATION

      # load raw water temperature data from Springbank Dam, London, Ontario
      rawData <- read.csv("https://woed-kiwis.thamesriver.on.ca/KiWIS/KiWIS?service=kisters&type=queryServices&request=getTimeseriesValues&datasource=0&format=csv&ts_id=304680042&period=complete", sep = ";")

      # trim off top two rows of garbled metadata
      rawData <- rawData[-c(1:2),] 

      # split date column to trim unncessary time tag
      rawDays <- do.call(rbind, strsplit(rawData[,1], "T"))

      # put together date and temperature 
      w.thames <- data.frame(date = rawDays[,1], w.temp = as.numeric(rawData[,2]))

      # grab the range of years covered by the water temperature data 
      timeSpan <- strsplit( c( head(w.thames$date, 1), tail(w.thames$date, 1) ), "-")
      
      # isolate the years and make them numeric
      timeSpan <- as.numeric(c(timeSpan[[1]][1], timeSpan[[2]][1]))

      # ————————————————————————————————————————————————————————————————————————

      # RETRIEVE AIR TEMPERATURES FROM WEATHER STATION

      # load library for canadian weather station data (& install if missing)
      if(!require(canadaHCDx)){
            source("https://gitlab.com/ConorIA/canadaHCDx/raw/master/install_canadaHCDx.R")
            library(canadaHCDx)
      }

      # use stationID 10999 for London CS/climate station (it has mean air temp)
      # retrieve air temperatures for the same years as the water temperatures 
      a.thames <- hcd_daily(10999, timeSpan[1]:timeSpan[2])

      # trim the unnecessary columns, leaving only date and temperature
      a.thames <- subset(a.thames, select = c(Date, MeanTemp))

      # standardize column names to match water temperature dataset
      names(a.thames) <- c("date", "a.temp")

      # coerce date column into string format 
      a.thames$date <- as.character(a.thames$date) 

      # find which rows of air data match the dates in the water data 
      overlap <- which(a.thames$date %in% w.thames$date)

      # put corresponding air and water temperature series together
      thames <- cbind(w.thames, a.temp = a.thames$a.temp[overlap]) 

      # output the harmonized Thames river temperature series
      write.csv(thames,"../data/thames.csv", row.names = F)

}

# ——————————————————————————————————————————————————————————————————————————————

# establish a regression-based relationship between air & water temperatures

# find rows that have negative air temperatures
changeIndex <- which(thames$a.temp < 0)

# make copies of the data to manipulate negative temperatures differently 

# adjust them to zero 
thames0  <- thames
thames0$a.temp[changeIndex] <- 0  

# or adjust them to 4 
thames4  <- thames
thames4$a.temp[changeIndex] <- 4

# or remove them 
thamesNA <- thames
thamesNA$a.temp[changeIndex] <- NA  

# ——————————————————————————————————————

# make a default linear model to start off
model <- lm(w.temp ~ a.temp, thames)

# make a model with the zero-adjusted air temperatures
model0 <- lm(w.temp ~ a.temp, thames0)

# make a model with the 4C adjustments
model4 <- lm(w.temp ~ a.temp, thames4)

# make a model with negatives removed 
modelNA <- lm(w.temp ~ a.temp, thamesNA)

# ——————————————————————————————————————

# compare performance among models

# make a data frame of each model and it's coefficient of determination 
perform <- data.frame( model = c("model", "model0", "model4", "modelNA"),
                       adj.R2 = sapply( list(model, model0, model4, modelNA), 
                                        function(fit){ summary(fit)[[9]] } )
)

# ladies and gentlemen... the best model is...
best <- perform[which.max(perform$adj.R2), "model"]

# grab the intercept and slope 
b <- round( get(best)[[1]][[1]], 3)
m <- round( get(best)[[1]][[2]], 3)

# declare model parameters
paste("the best model is", best, "and it's formula is y =", m, "* x +", b)

# make function using the model to convert air to water temperatures
mod.temp <- function(x){ 
    y <- .927 * x + 2.952
    return(y)
}


# ——————————————————————————————————————


# residual analysis
plot(model)
plot(model0)
plot(model4)
plot(modelNA)

# see the data & throw on model fit
plot(w.temp ~ a.temp, thames0)
abline(model0, col = "red")
