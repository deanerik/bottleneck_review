#!/usr/bin/env Rscript

# 8. Produce summary statistics.R
#    Licence: GNU GPLv3 - see LICENSE.txt for more details

# For the various results reported in the manuscript

library(dataRetrieval) # USA water gage data

citation("dataRetrieval")

cite


# Change in first day of spawn
# ﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋

cat("\n\n Advanced onset of spawn in Nipigon by",
(baseline$nipigon$spawnDaysRange1 %>% mean(na.rm = T) - future$nipigon[future$nipigon$year %in% 2040:2050,"spawnDaysRange1"] %>% mean) %>% round, 
"and",                                                                                                              
(baseline$nipigon$spawnDaysRange1 %>% mean(na.rm = T) - future$nipigon[future$nipigon$year %in% 2090:2099,"spawnDaysRange1"] %>% mean) %>% round, 
"days by mid- and late-century compared to baseline")

cat("\n\n Advanced onset of spawn in St. Louis by",
(baseline$stlouis$spawnDaysRange1 %>% mean(na.rm = T) - future$stlouis[future$stlouis$year %in% 2040:2050,"spawnDaysRange1"] %>% mean) %>% round, 
"and",                                                                                                                                          
(baseline$stlouis$spawnDaysRange1 %>% mean(na.rm = T) - future$stlouis[future$stlouis$year %in% 2090:2099,"spawnDaysRange1"] %>% mean) %>% round, 
"days by mid- and late-century compared to baseline")

cat("\n\n Advanced onset of spawn in Genesee by",
(baseline$genesee$spawnDaysRange1 %>% mean(na.rm = T) - future$genesee[future$genesee$year %in% 2040:2050,"spawnDaysRange1"] %>% mean) %>% round, 
"and",                                                                                                                                         
(baseline$genesee$spawnDaysRange1 %>% mean(na.rm = T) - future$genesee[future$genesee$year %in% 2090:2099,"spawnDaysRange1"] %>% mean) %>% round, 
"days by mid- and late-century compared to baseline")

cat("\n\n Advanced onset of spawn in Vermillion by",
(baseline$vermillion$spawnDaysRange1 %>% mean(na.rm = T) - future$vermillion[future$vermillion$year %in% 2040:2050,"spawnDaysRange1"] %>% mean) %>% round, 
"and",                                                                                                                                          
(baseline$vermillion$spawnDaysRange1 %>% mean(na.rm = T) - future$vermillion[future$vermillion$year %in% 2090:2099,"spawnDaysRange1"] %>% mean) %>% round, 
"days by mid- and late-century compared to baseline")


# Change in harvest days (effective spawning period)
# ﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋


cat("\n\n Increase of harvest days in Nipigon by",
   ( 
    ( baseline$nipigon$lastBday %>% mean(na.rm = T) - baseline$nipigon$spawnDaysRange1 %>% mean(na.rm = T) ) - 
    (future$nipigon[future$nipigon$year %in% 2040:2050,"lastBday"] %>% mean) - (future$nipigon[future$nipigon$year %in% 2040:2050,"spawnDaysRange1"] %>% mean) 
   ), 
   "and",                                                                                                              
   (
    ( baseline$nipigon$lastBday %>% mean(na.rm = T) - baseline$nipigon$spawnDaysRange1 %>% mean(na.rm = T) ) - 
   (( future$nipigon[future$nipigon$year %in% 2090:2099,"lastBday"] %>% mean) - (future$nipigon[future$nipigon$year %in% 2090:2099,"spawnDaysRange1"] %>% mean))
), 
   "days by mid- and late-century compared to baseline")


cat("\n\n Increase of harvest days in st louis by",
   ( 
    (future$stlouis[future$stlouis$year %in% 2040:2050,"lastBday"] %>% mean) - (future$stlouis[future$stlouis$year %in% 2040:2050,"spawnDaysRange1"] %>% mean) 
    - 
    ( baseline$stlouis$lastBday %>% mean(na.rm = T) - baseline$stlouis$spawnDaysRange1 %>% mean(na.rm = T) ) 
   ), 
   "and",                                                                                                              
   (
   (( future$stlouis[future$stlouis$year %in% 2090:2099,"lastBday"] %>% mean) - (future$stlouis[future$stlouis$year %in% 2090:2099,"spawnDaysRange1"] %>% mean))
    - 
    ( baseline$stlouis$lastBday %>% mean(na.rm = T) - baseline$stlouis$spawnDaysRange1 %>% mean(na.rm = T) ) 
), 
   "days by mid- and late-century compared to baseline")
#
cat("\n\n Increase of harvest days in genesee by",
   ( 
    (future$genesee[future$genesee$year %in% 2040:2050,"lastBday"] %>% mean) - (future$genesee[future$genesee$year %in% 2040:2050,"spawnDaysRange1"] %>% mean) 
    - 
    ( baseline$genesee$lastBday %>% mean(na.rm = T) - baseline$genesee$spawnDaysRange1 %>% mean(na.rm = T) ) 
   ), 
   "and",                                                                                                              
   (
   (( future$genesee[future$genesee$year %in% 2090:2099,"lastBday"] %>% mean) - (future$genesee[future$genesee$year %in% 2090:2099,"spawnDaysRange1"] %>% mean))
    - 
    ( baseline$genesee$lastBday %>% mean(na.rm = T) - baseline$genesee$spawnDaysRange1 %>% mean(na.rm = T) ) 
), 
   "days by mid- and late-century compared to baseline")
#
cat("\n\n Increase of harvest days in vermillion by",
   ( 
    (future$vermillion[future$vermillion$year %in% 2040:2050,"lastBday"] %>% mean) - (future$vermillion[future$vermillion$year %in% 2040:2050,"spawnDaysRange1"] %>% mean) 
    - 
    ( baseline$vermillion$lastBday %>% mean(na.rm = T) - baseline$vermillion$spawnDaysRange1 %>% mean(na.rm = T) ) 
   ), 
   "and",                                                                                                              
   (
   (( future$vermillion[future$vermillion$year %in% 2090:2099,"lastBday"] %>% mean) - (future$vermillion[future$vermillion$year %in% 2090:2099,"spawnDaysRange1"] %>% mean))
    - 
    ( baseline$vermillion$lastBday %>% mean(na.rm = T) - baseline$vermillion$spawnDaysRange1 %>% mean(na.rm = T) ) 
), 
   "days by mid- and late-century compared to baseline")


# LONGEST increase of harvest days is in GENESEE

cat("\n\n By 2100, Genesee will have a",
(
 (( future$genesee[future$genesee$year %in% 2090:2099,"lastBday"] %>% mean) - (future$genesee[future$genesee$year %in% 2090:2099,"spawnDaysRange1"] %>% mean))
 /
 ( baseline$genesee$lastBday %>% mean(na.rm = T) - baseline$genesee$spawnDaysRange1 %>% mean(na.rm = T) )
),
    "times increase in the number of harvest days, increasing from",
 ( baseline$genesee$lastBday %>% mean(na.rm = T) - baseline$genesee$spawnDaysRange1 %>% mean(na.rm = T) ),
 "under baseline to", 
 (( future$genesee[future$genesee$year %in% 2090:2099,"lastBday"] %>% mean) - (future$genesee[future$genesee$year %in% 2090:2099,"spawnDaysRange1"] %>% mean)),
 "by the end of century"
)

# Bias investigation
# ﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋

results$genesee[, c("year","theoryMin", "percentSpawnSurvive")]

# check if baseline to 2025 water temperatures have a big jump or not


n.temp
g.temp
v.temp




bias.verm <- rbind(
                   cbind(v.temp, type = "base"),
                   cbind(trib.mods[[1]][[1]][trib.mods[[1]][[1]]$year %in% 2015:2030, ], type = "gcm"),
                   cbind(trib.mods[[1]][[2]][trib.mods[[1]][[2]]$year %in% 2015:2030, ], type = "gcm"),
                   cbind(trib.mods[[1]][[3]][trib.mods[[1]][[3]]$year %in% 2015:2030, ], type = "gcm"),
                   cbind(trib.mods[[1]][[4]][trib.mods[[1]][[4]]$year %in% 2015:2030, ], type = "gcm"),
                   cbind(trib.mods[[1]][[5]][trib.mods[[1]][[5]]$year %in% 2015:2030, ], type = "gcm")
)




plot(v.temp$temp, col = "blue", xlim = c(1, 5844), ylim = c(0, 40))

points(trib.mods[[1]][[1]][trib.mods[[1]][[1]]$year %in% 2015:2030, "temp"], color = "gray40")
points(trib.mods[[1]][[2]][trib.mods[[1]][[2]]$year %in% 2015:2030, "temp"], color = "gray50")
points(trib.mods[[1]][[3]][trib.mods[[1]][[3]]$year %in% 2015:2030, "temp"], color = "gray60")
points(trib.mods[[1]][[4]][trib.mods[[1]][[4]]$year %in% 2015:2030, "temp"], color = "gray70")
points(trib.mods[[1]][[5]][trib.mods[[1]][[5]]$year %in% 2015:2030, "temp"], color = "gray80")





# NOTE: for Genesee and Vermillion, that the min appears more in line, so this bias is from averaging


# extract each survival percentage value from individual GCMs (for all tributaries)
all.nip <- nipSummary %>% sapply(.,function(x){data.frame(percent= x$percentSpawnSurvive)}) 
all.nip <- all.nip %>% as.data.frame %>% cbind(year = 2015:2099)
#
all.slou <- slouSummary %>% sapply(.,function(x){data.frame(percent= x$percentSpawnSurvive)}) 
all.slou <- all.slou %>% as.data.frame %>% cbind(year = 2025:2099)
#
all.gen <- genSummary %>% sapply(.,function(x){data.frame(percent= x$percentSpawnSurvive)}) 
all.gen <- all.gen %>% as.data.frame %>% cbind(year = 2025:2099)
#
all.verm <- vermSummary %>% sapply(.,function(x){data.frame(percent= x$percentSpawnSurvive)}) 
all.verm <- all.verm %>% as.data.frame %>% cbind(year = 2025:2099)

# 1 
# 2 
# 3
# 4
     


# 2 x 2 multiplot with redundant garnish removed 
par(mfrow = c(2,2),
    mgp = c(3,0.5,0))
# NIPIGON
par(mar = c(0.5,3,3.5,0.5))
plot(2015:2099, future$nipigon[["percentSpawnSurvive"]],
     xlim = c(2000,2100), ylim = c(0,1), 
     pch = 15, col = "gray70", cex = 0.7,
     ylab = '', xaxt = 'n', xlab = '',
     cex.axis = 0.8)
title("Nipigon", line = -1.25, adj = 0.2)
# baseline GCM values
points(2000:2008,baseline$nipigon[,"percentSpawnSurvive"], pch = 20)
# min and max GCM values
points(2015:2099, all.nip[,-6] %>% apply(.,1,min), pch = 25, col = "gray40", bg = "gray40", cex = 0.5 )
points(2015:2099, all.nip[,-6] %>% apply(.,1,max), pch = 24, col = "gray90", bg = "gray90", cex = 0.5 )
# trendlines for min, mean, max connected to the baseline
lm( c( baseline$nipigon[,"percentSpawnSurvive"], future$nipigon[,"percentSpawnSurvive"]) ~ c(2000:2008,2015:2099) ) %>% abline(col = "gray70") 
lm( c( baseline$nipigon[,"percentSpawnSurvive"], all.nip[,-6] %>% apply(.,1,min)) ~ c(2000:2008,2015:2099) ) %>% abline(col = "gray40")
lm( c( baseline$nipigon[,"percentSpawnSurvive"], all.nip[,-6] %>% apply(.,1,max)) ~ c(2000:2008,2015:2099) ) %>% abline(col = "gray90")
abline(v=2009.5, lty = 3 , col = 'black')
# ST. LOUIS
par(mar = c(0.5,0,3.5,2.5))
plot(2025:2099, future$stlouis[["percentSpawnSurvive"]],
     xlim = c(2000,2100), ylim = c(0,1), 
     pch = 15, col = "gray70", cex = 0.7,
     ylab = 'n', yaxt = 'n', xlab = 'n', xaxt = 'n',
     cex.axis = 0.8)
title("St. Louis", line = -1.25, adj = 0.05)
# baseline GCM values
points(2012:2024, baseline$stlouis[,"percentSpawnSurvive"], pch = 20)
# min and max GCM values
points(2025:2099, all.slou[,-6] %>% apply(.,1,min), pch = 25, col = "gray40", bg = "gray40", cex = 0.5 )
points(2025:2099, all.slou[,-6] %>% apply(.,1,max), pch = 24, col = "gray90", bg = "gray90", cex = 0.5 )
# trendlines for min, mean, max connected to the baseline
lm( c( baseline$stlouis[,"percentSpawnSurvive"], future$stlouis[,"percentSpawnSurvive"]) ~ c(2012:2099) ) %>% abline(col = "gray70") 
lm( c( baseline$stlouis[,"percentSpawnSurvive"], all.slou[,-6] %>% apply(.,1,min)) ~ c(2012:2099) ) %>% abline(col = "gray40")
lm( c( baseline$stlouis[,"percentSpawnSurvive"], all.slou[,-6] %>% apply(.,1,max)) ~ c(2012:2099) ) %>% abline(col = "gray90")
abline(v=2024.5, lty = 3 , col = 'black')
# LEGEND
legend(2067, 0.35, legend = c(  "Max", "Mean", "Min","Baseline"), 
       col = c("gray90", "gray70","gray40","black"), 
       pch = c(24, 15, 25, 20),
       pt.bg = c("gray90", "gray70","gray40","black"),  
#       pch = c("▲", "■", "▼", "●"),
       box.lty = 0,
       pt.cex = c(1, 1.4, 1, 1.7),
       cex = 1.3
)
# GENESEE
par(mar = c(3.5,3,0.5,0.5))
plot(2025:2099, future$genesee[["percentSpawnSurvive"]],
     xlim = c(2000,2100), ylim = c(0,1), 
     pch = 15, col = "gray70", cex = 0.7,
     ylab = '', xlab = '',
     cex.axis = 0.8)
title("Genesee", line = -1.25, adj = 0.05)
# baseline GCM values
points(2011:2024, baseline$genesee[,"percentSpawnSurvive"], pch = 20)
# min and max GCM values
points(2025:2099, all.gen[,-6] %>% apply(.,1,min), pch = 25, col = "gray40", bg = "gray40", cex = 0.5 )
points(2025:2099, all.gen[,-6] %>% apply(.,1,max), pch = 24, col = "gray90", bg = "gray90", cex = 0.5 )
# trendlines for min, mean, max connected to the baseline
lm( c( baseline$genesee[,"percentSpawnSurvive"], future$genesee[,"percentSpawnSurvive"]) ~ c(2011:2099) ) %>% abline(col = "gray70") 
lm( c( baseline$genesee[,"percentSpawnSurvive"], all.gen[,-6] %>% apply(.,1,min)) ~ c(2011:2099) ) %>% abline(col = "gray40")
lm( c( baseline$genesee[,"percentSpawnSurvive"], all.gen[,-6] %>% apply(.,1,max)) ~ c(2011:2099) ) %>% abline(col = "gray90")
abline(v=2024.5, lty = 3 , col = 'black')
# VERMILLION
par(mar = c(3.5,0,0.5,2.5))
plot(2025:2099, future$vermillion[["percentSpawnSurvive"]],
     xlim = c(2000,2100), ylim = c(0,1), 
     pch = 15, col = "gray70", cex = 0.7,
     ylab = '', yaxt = 'n', xlab = '',
     cex.axis = 0.8)
title("Vermillion", line = -1.25, adj = 0.03)
# baseline GCM values
points(2012:2024, baseline$vermillion[,"percentSpawnSurvive"], pch = 20)
# min and max GCM values
points(2025:2099, all.verm[,-6] %>% apply(.,1,min), pch = 25, col = "gray40", bg = "gray40", cex = 0.5 )
points(2025:2099, all.verm[,-6] %>% apply(.,1,max), pch = 24, col = "gray90", bg = "gray90", cex = 0.5 )
# NOTE: that the min appears more in line, so this bias is from averaging
#
# trendlines for min, mean, max connected to the baseline
lm( c( baseline$vermillion[,"percentSpawnSurvive"], future$vermillion[,"percentSpawnSurvive"]) ~ c(2012:2099) ) %>% abline(col = "gray70") 
lm( c( baseline$vermillion[,"percentSpawnSurvive"], all.verm[,-6] %>% apply(.,1,min)) ~ c(2012:2099) ) %>% abline(col = "gray40")
lm( c( baseline$vermillion[,"percentSpawnSurvive"], all.verm[,-6] %>% apply(.,1,max)) ~ c(2012:2099) ) %>% abline(col = "gray90")
abline(v=2024.5, lty = 3 , col = 'black')
#
mtext("Year", side=1, line=2.5, at=1995, cex = 1)
mtext( "Effective Spawning Proportion", side=2, line=22.7, at=1.1, cex = 1)
#




















# NOTE:  compare if the plot still has a huge jump if you plot only the minimums?



all.nip  <- melt(all.nip, value.name = "percent", id = "year" )[,-2]
names(all.nip)[3] <- "type"
all.nip$type  <- "Nipigon"
#
all.slou <- slouSummary %>% lapply(.,function(x){data.frame(year = 2025:2099, percent= x$percentSpawnSurvive)}) 
all.slou  <- melt(all.slou, value.name = "percent", id = "year" )[,-2]
names(all.slou)[3] <- "type"
all.slou$type  <- "St. Louis"
#
all.gen <- genSummary %>% lapply(.,function(x){data.frame(year = 2025:2099, percent= x$percentSpawnSurvive)}) 
all.gen  <- melt(all.gen, value.name = "percent", id = "year" )[,-2]
names(all.gen)[3] <- "type"
all.gen$type  <- "Genesee"
#
all.verm <- vermSummary %>% lapply(.,function(x){data.frame(year = 2025:2099, percent= x$percentSpawnSurvive)}) 
all.verm  <- melt(all.verm, value.name = "percent", id = "year" )[,-2]
names(all.verm)[3] <- "type"
all.verm$type  <- "Vermillion"




# Rate of LES change per year 
# ﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋


lm( LES ~ year, 
   data.frame( LES = future$nipigon$lastBday, year = 2015:2099 )
   )

lm( LES ~ year, 
   data.frame( LES = c(baseline$stlouis$lastBday, future$stlouis$lastBday), year = 2012:2099 )
   )


lm( LES ~ year, 
   data.frame( LES = c(baseline$genesee$lastBday, future$genesee$lastBday), year = 2011:2099 )
   )


lm( LES ~ year, 
   data.frame( LES = c(baseline$vermillion$lastBday, future$vermillion$lastBday), year = 2012:2099 )
   )


# By end of century, how many days did spawning end later, versus start earlier?
# ﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋

# Nipigon
cat("NIPIGON \n\n The onset of spawn shifted earlier by",
(future$nipigon$spawnDaysRange1 %>% mean) -
(baseline$nipigon$spawnDaysRange1 %>% mean(na.rm = T)),
"days and the end of spawn shifter later by",
(future$nipigon$spawnDaysRange2 %>% mean) -
(baseline$nipigon$spawnDaysRange2 %>% mean (na.rm = T)),
"days.\n\n\n"
)
# St. Louis
cat("ST LOUIS \n\n The onset of spawn shifted earlier by",
(future$stlouis$spawnDaysRange1 %>% mean) -
(baseline$stlouis$spawnDaysRange1 %>% mean(na.rm = T)),
"days and the end of spawn shifter later by",
(future$stlouis$spawnDaysRange2 %>% mean) -
(baseline$stlouis$spawnDaysRange2 %>% mean (na.rm = T)),
"days.\n\n\n"
)
# Genesee
cat("GENESEE \n\n The onset of spawn shifted earlier by",
(future$genesee$spawnDaysRange1 %>% mean) -
(baseline$genesee$spawnDaysRange1 %>% mean(na.rm = T)),
"days and the end of spawn shifter later by",
(future$genesee$spawnDaysRange2 %>% mean) -
(baseline$genesee$spawnDaysRange2 %>% mean (na.rm = T)),
"days.\n\n\n"
)
# Vermillion
cat("VERMILLION \n\n The onset of spawn shifted earlier by",
(future$vermillion$spawnDaysRange1 %>% mean) -
(baseline$vermillion$spawnDaysRange1 %>% mean(na.rm = T)),
"days and the end of spawn shifter later by",
(future$vermillion$spawnDaysRange2 %>% mean) -
(baseline$vermillion$spawnDaysRange2 %>% mean (na.rm = T)),
"days.\n\n\n"
)




# How often did temperatures between 0-12 occur before winter?
# ﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋

# For the issue raised in section 3.2 Growth — about growth equation 

# for a temp series data set, using the average winter start ordinal for a trib & time period
# check how many days before winter have temperatures that are 12 or below
negativeGrowth_check <- function(dataset, winterStart){

   # init empty storage 
   daysBelow <- vector()

   # get temp column, despite being named after various GCMs 
   tempName <- names(dataset[names(dataset) != "j.date" & names(dataset) != "year"])

   for (year in dataset$year %>% unique) {

      daysBelow <- c(daysBelow, 
                     (dataset[dataset$year == year, tempName][250:winterStart] <= 12) %>% sum)

   }

   output <- daysBelow %>% mean

   return(output)
}



# for reference
data.frame(w.start = w.dates[,1],
          w.end = w.dates[,2],
          id = c("n.base", "n.50", "n.100", 
             "s.base", "s.50", "s.100",
             "g.base", "g.50", "g.100",
             "v.base", "v.50", "v.100"))

# one vector to store them all
allChecks <- c(

# baseline checks
negativeGrowth_check(n.temp, w.dates[1]),
negativeGrowth_check(s.temp, w.dates[4]),
negativeGrowth_check(g.temp, w.dates[7]),
negativeGrowth_check(v.temp, w.dates[10]),

# 2050 checks

trib.data$nip[trib.data$nip$year %in% 2040:2050, c(1,2,3)] %>% negativeGrowth_check(., w.dates[2]),
trib.data$nip[trib.data$nip$year %in% 2040:2050, c(1,2,4)] %>% negativeGrowth_check(., w.dates[2]),
trib.data$nip[trib.data$nip$year %in% 2040:2050, c(1,2,5)] %>% negativeGrowth_check(., w.dates[2]),
trib.data$nip[trib.data$nip$year %in% 2040:2050, c(1,2,6)] %>% negativeGrowth_check(., w.dates[2]),
trib.data$nip[trib.data$nip$year %in% 2040:2050, c(1,2,7)] %>% negativeGrowth_check(., w.dates[2]),
#
trib.data$slou[trib.data$slou$year %in% 2040:2050, c(1,2,3)] %>% negativeGrowth_check(., w.dates[5]),
trib.data$slou[trib.data$slou$year %in% 2040:2050, c(1,2,4)] %>% negativeGrowth_check(., w.dates[5]),
trib.data$slou[trib.data$slou$year %in% 2040:2050, c(1,2,5)] %>% negativeGrowth_check(., w.dates[5]),
trib.data$slou[trib.data$slou$year %in% 2040:2050, c(1,2,6)] %>% negativeGrowth_check(., w.dates[5]),
trib.data$slou[trib.data$slou$year %in% 2040:2050, c(1,2,7)] %>% negativeGrowth_check(., w.dates[5]),
#
trib.data$gen[trib.data$gen$year %in% 2040:2050, c(1,2,3)] %>% negativeGrowth_check(., w.dates[8]),
trib.data$gen[trib.data$gen$year %in% 2040:2050, c(1,2,4)] %>% negativeGrowth_check(., w.dates[8]),
trib.data$gen[trib.data$gen$year %in% 2040:2050, c(1,2,5)] %>% negativeGrowth_check(., w.dates[8]),
trib.data$gen[trib.data$gen$year %in% 2040:2050, c(1,2,6)] %>% negativeGrowth_check(., w.dates[8]),
trib.data$gen[trib.data$gen$year %in% 2040:2050, c(1,2,7)] %>% negativeGrowth_check(., w.dates[8]),
#
trib.data$verm[trib.data$verm$year %in% 2040:2050, c(1,2,3)] %>% negativeGrowth_check(., w.dates[11]),
trib.data$verm[trib.data$verm$year %in% 2040:2050, c(1,2,4)] %>% negativeGrowth_check(., w.dates[11]),
trib.data$verm[trib.data$verm$year %in% 2040:2050, c(1,2,5)] %>% negativeGrowth_check(., w.dates[11]),
trib.data$verm[trib.data$verm$year %in% 2040:2050, c(1,2,6)] %>% negativeGrowth_check(., w.dates[11]),
trib.data$verm[trib.data$verm$year %in% 2040:2050, c(1,2,7)] %>% negativeGrowth_check(., w.dates[11]),

# 2100 checks

trib.data$nip[trib.data$nip$year %in% 2040:2050, c(1,2,3)] %>% negativeGrowth_check(., w.dates[3]),
trib.data$nip[trib.data$nip$year %in% 2040:2050, c(1,2,4)] %>% negativeGrowth_check(., w.dates[3]),
trib.data$nip[trib.data$nip$year %in% 2040:2050, c(1,2,5)] %>% negativeGrowth_check(., w.dates[3]),
trib.data$nip[trib.data$nip$year %in% 2040:2050, c(1,2,6)] %>% negativeGrowth_check(., w.dates[3]),
trib.data$nip[trib.data$nip$year %in% 2040:2050, c(1,2,7)] %>% negativeGrowth_check(., w.dates[3]),

trib.data$slou[trib.data$slou$year %in% 2040:2050, c(1,2,3)] %>% negativeGrowth_check(., w.dates[6]),
trib.data$slou[trib.data$slou$year %in% 2040:2050, c(1,2,4)] %>% negativeGrowth_check(., w.dates[6]),
trib.data$slou[trib.data$slou$year %in% 2040:2050, c(1,2,5)] %>% negativeGrowth_check(., w.dates[6]),
trib.data$slou[trib.data$slou$year %in% 2040:2050, c(1,2,6)] %>% negativeGrowth_check(., w.dates[6]),
trib.data$slou[trib.data$slou$year %in% 2040:2050, c(1,2,7)] %>% negativeGrowth_check(., w.dates[6]),

trib.data$gen[trib.data$gen$year %in% 2040:2050, c(1,2,3)] %>% negativeGrowth_check(., w.dates[9]),
trib.data$gen[trib.data$gen$year %in% 2040:2050, c(1,2,4)] %>% negativeGrowth_check(., w.dates[9]),
trib.data$gen[trib.data$gen$year %in% 2040:2050, c(1,2,5)] %>% negativeGrowth_check(., w.dates[9]),
trib.data$gen[trib.data$gen$year %in% 2040:2050, c(1,2,6)] %>% negativeGrowth_check(., w.dates[9]),
trib.data$gen[trib.data$gen$year %in% 2040:2050, c(1,2,7)] %>% negativeGrowth_check(., w.dates[9]),

trib.data$verm[trib.data$verm$year %in% 2040:2050, c(1,2,3)] %>% negativeGrowth_check(., w.dates[12]),
trib.data$verm[trib.data$verm$year %in% 2040:2050, c(1,2,4)] %>% negativeGrowth_check(., w.dates[12]),
trib.data$verm[trib.data$verm$year %in% 2040:2050, c(1,2,5)] %>% negativeGrowth_check(., w.dates[12]),
trib.data$verm[trib.data$verm$year %in% 2040:2050, c(1,2,6)] %>% negativeGrowth_check(., w.dates[12]),
trib.data$verm[trib.data$verm$year %in% 2040:2050, c(1,2,7)] %>% negativeGrowth_check(., w.dates[12])
)

# the lowest and highest number of sub-12 pre-winter days that ever occur in a given simulation
allChecks %>% na.omit %>% min
allChecks %>% na.omit %>% max




# Shift in LES by end of century - how much later can they now spawn effectively?
# ﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋

# look at the minimum and maximum from the future LES
futureLES <- c(
               future$nipigon[future$genesee$year %in% 2090:2099,"lastBday"] %>% mean,
               future$stlouis[future$genesee$year %in% 2090:2099,"lastBday"] %>% mean,
               future$genesee[future$genesee$year %in% 2090:2099,"lastBday"] %>% mean,
               future$vermillion[future$genesee$year %in% 2090:2099,"lastBday"] %>% mean
)

# and from the baseline values
baseLES <- c(
             baseline$nipigon$lastBday %>% mean,
             baseline$stlouis$lastBday %>% mean,
             baseline$genesee$lastBday %>% mean,
             baseline$vermillion$lastBday %>% mean
)

# output sentence
cat(" By the end of century, fish could be spawned\n",
((futureLES %>% min) - (baseLES %>% na.omit %>% min))/7,
"to",
((futureLES %>% max) - (baseLES %>% na.omit %>% max))/7,
"weeks later compared to baseline and still survive")


# Miscellaneous 
# ﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋

# who has the greatest decrease in winter days? SEE TABLE 3

# by what year does survival become consistent in Nipigon?
baseline$stlouis[, c("year","surviveNum","lastBday","percentSpawnSurvive" )] %>% colMeans
baseline$genesee[, c("year","surviveNum","lastBday","percentSpawnSurvive" )] %>% colMeans
baseline$vermillion[, c("year","surviveNum","lastBday","percentSpawnSurvive" )] %>% colMeans
future$nipigon[, c("year","surviveNum","lastBday","percentSpawnSurvive" )]


# by 2051 it is capable of survival performance similar to St Louis' baseline mean
# and by 2058 it's consistently comparable to other tributaries' performance at baseline
future$nipigon$year[future$nipigon$surviveNum >= (baseline$stlouis$surviveNum %>% mean)]


# Rate of change (slope) over time for metrics in each tributary 
# ﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋


# effective spawn days 
lm(future$nipigon$surviveNum ~ future$nipigon$year)
lm(future$stlouis$surviveNum ~ future$stlouis$year)
lm(future$genesee$surviveNum ~ future$genesee$year)
lm(future$vermillion$surviveNum ~ future$vermillion$year)

# date of LES
lm(future$nipigon$lastBday ~ future$nipigon$year)
lm(future$stlouis$lastBday ~ future$stlouis$year)
lm(future$genesee$lastBday ~ future$genesee$year)
lm(future$vermillion$lastBday ~ future$vermillion$year)

# onset of spawn 
lm(future$nipigon$spawnDaysRange1 ~ future$nipigon$year)
lm(future$stlouis$spawnDaysRange1 ~ future$stlouis$year)
lm(future$genesee$spawnDaysRange1 ~ future$genesee$year)
lm(future$vermillion$spawnDaysRange1 ~ future$vermillion$year)

# l crit
lm(future$nipigon$theoryMin ~ future$nipigon$year)
lm(future$stlouis$theoryMin ~ future$stlouis$year)
lm(future$genesee$theoryMin ~ future$genesee$year)
lm(future$vermillion$theoryMin ~ future$vermillion$year)


# winter days
lm(future$nipigon$wintDay ~ future$nipigon$year)
lm(future$stlouis$wintDay ~ future$stlouis$year)
lm(future$genesee$wintDay ~ future$genesee$year)
lm(future$vermillion$wintDay ~ future$vermillion$year)




# Reassurance that the simulation didn't go into extreme temperatures
# ﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋

# temperatures greater than 26.9°C and 38.8°C occur in what percent of the data?

((gcm.data$tas > 26.9) %>% sum) / (gcm.data$tas %>% length)

((gcm.data$tas >= 38.8) %>% sum) / (gcm.data$tas %>% length)




# Table 1 - percentage of spawn period that can survive (changes per river per period)
# ﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋

# (example)

#              Baseline         2050            2100
# Nipigon        0%             38% (+38%)
# St Louis
# Genesee
# Vermillion


# NIPIGON
cat(
    (baseline$nipigon$percentSpawnSurvive %>% mean)*100,
"%     ",
    (future$nipigon[future$nipigon$year %in% 2040:2050,"percentSpawnSurvive"] %>% mean)*100 ,
"%     (+",
( -1 + (future$nipigon[future$nipigon$year %in% 2040:2050,"percentSpawnSurvive"] %>% mean ) /
        (baseline$nipigon$percentSpawnSurvive %>% mean ) 
     )*100,
"%)     ",
(future$nipigon[future$nipigon$year %in% 2090:2099,"percentSpawnSurvive"] %>% mean)*100 ,
"%     (+",
    ( -1 + 
     (future$nipigon[future$nipigon$year %in% 2090:2099,"percentSpawnSurvive"] %>% mean ) /
        (baseline$nipigon$percentSpawnSurvive %>% mean ) 
     )*100 ,
"%)\n"
)

# St Louis
cat(
    (baseline$stlouis$percentSpawnSurvive %>% mean)*100,
"%     ",
    (future$stlouis[future$stlouis$year %in% 2040:2050,"percentSpawnSurvive"] %>% mean)*100 ,
"%     (+",
( -1 + (future$stlouis[future$stlouis$year %in% 2040:2050,"percentSpawnSurvive"] %>% mean ) /
        (baseline$stlouis$percentSpawnSurvive %>% mean ) 
     )*100,
"%)     ",
(future$stlouis[future$stlouis$year %in% 2090:2099,"percentSpawnSurvive"] %>% mean)*100 ,
"%     (+",
    ( -1 + 
     (future$stlouis[future$stlouis$year %in% 2090:2099,"percentSpawnSurvive"] %>% mean ) /
        (baseline$stlouis$percentSpawnSurvive %>% mean ) 
     )*100 ,
"%)\n"
)

# Genesee
cat(
    (baseline$genesee$percentSpawnSurvive %>% mean)*100,
"%     ",
    (future$genesee[future$genesee$year %in% 2040:2050,"percentSpawnSurvive"] %>% mean)*100 ,
"%     (+",
( -1 + (future$genesee[future$genesee$year %in% 2040:2050,"percentSpawnSurvive"] %>% mean ) /
        (baseline$genesee$percentSpawnSurvive %>% mean ) 
     )*100,
"%)     ",
(future$genesee[future$genesee$year %in% 2090:2099,"percentSpawnSurvive"] %>% mean)*100 ,
"%     (+",
    ( -1 + 
     (future$genesee[future$genesee$year %in% 2090:2099,"percentSpawnSurvive"] %>% mean ) /
        (baseline$genesee$percentSpawnSurvive %>% mean ) 
     )*100 ,
"%)\n"
)


# Vermillion
cat(
    (baseline$vermillion$percentSpawnSurvive %>% mean)*100,
"%     ",
    (future$vermillion[future$vermillion$year %in% 2040:2050,"percentSpawnSurvive"] %>% mean)*100 ,
"%     (+",
( -1 + (future$vermillion[future$vermillion$year %in% 2040:2050,"percentSpawnSurvive"] %>% mean ) /
        (baseline$vermillion$percentSpawnSurvive %>% mean ) 
     )*100,
"%)     ",
(future$vermillion[future$vermillion$year %in% 2090:2099,"percentSpawnSurvive"] %>% mean)*100 ,
"%     (+",
    ( -1 + 
     (future$vermillion[future$vermillion$year %in% 2090:2099,"percentSpawnSurvive"] %>% mean ) /
        (baseline$vermillion$percentSpawnSurvive %>% mean ) 
     )*100 ,
"%)\n"
)







# Table 2 - Lcrit, spawn onset, and LES across each time period, and percent change
# ﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋

#                                  (EXAMPLE)
#
#                Baseline       2050       Δ       2100       Δ
#
#  Nipigon       lcrit
#                spawn onset 
#                LES
#  
#  St Louis
#  
#  
#  Genesee
#  
#  
#  Vermillion


# NIPIGON
# L CRIT
cat(
    baseline$nipigon$theoryMin %>% mean *10,
    "mm     ",
    future$nipigon[future$nipigon$year %in% 2040:2050,"theoryMin"] %>% mean *10,
    "mm     ",
    ( -1 + (future$nipigon[future$nipigon$year %in% 2040:2050,"theoryMin"] %>% mean *10 ) /
        (baseline$nipigon$theoryMin %>% mean *10 ) 
     )*100 ,
    "%     ",
    future$nipigon[future$nipigon$year %in% 2090:2099,"theoryMin"] %>% mean *10,
    "mm     ",
    ( -1 + (future$nipigon[future$nipigon$year %in% 2090:2099,"theoryMin"] %>% mean *10 ) /
        (baseline$nipigon$theoryMin %>% mean *10 ) 
     )*100 ,
    "%\n"
)

# SPAWN ONSET
cat(
    baseline$nipigon$spawnDaysRange1 %>% mean(na.rm = T),
    "(", 
baseline$nipigon$spawnDaysRange1 %>% mean(na.rm = T) %>% juliGreg(., 09) %>% as.character,
    ")     ",
future$nipigon[future$nipigon$year %in% 2040:2050,"spawnDaysRange1"] %>% mean,
    "(", 
future$nipigon[future$nipigon$year %in% 2040:2050,"spawnDaysRange1"] %>% mean %>% juliGreg(., 50) %>% as.character,
    ")     ",
( -1 + ( (future$nipigon[future$nipigon$year %in% 2040:2050,"spawnDaysRange1"] %>% mean) /
    (baseline$nipigon$spawnDaysRange1 %>% mean(na.rm = T))
 )
) *100,
    "%     ",
future$nipigon[future$nipigon$year %in% 2090:2099,"spawnDaysRange1"] %>% mean,
    "(", 
future$nipigon[future$nipigon$year %in% 2090:2099,"spawnDaysRange1"] %>% mean %>% juliGreg(., 50) %>% as.character,
    ")     ",
( -1 + ( (future$nipigon[future$nipigon$year %in% 2090:2099,"spawnDaysRange1"] %>% mean) /
    (baseline$nipigon$spawnDaysRange1 %>% mean(na.rm = T))
 )
) *100,
    "%\n"
)

# LES
cat(
    baseline$nipigon$lastBday %>% mean(na.rm = T),
    "(", 
baseline$nipigon$lastBday %>% mean(na.rm = T) %>% juliGreg(., 09) %>% as.character,
    ")     ",
future$nipigon[future$nipigon$year %in% 2040:2050,"lastBday"] %>% mean,
    "(", 
future$nipigon[future$nipigon$year %in% 2040:2050,"lastBday"] %>% mean %>% juliGreg(., 50) %>% as.character,
    ")     ",
( -1 + ( (future$nipigon[future$nipigon$year %in% 2040:2050,"lastBday"] %>% mean) /
    (baseline$nipigon$lastBday %>% mean(na.rm = T))
 )
) *100,
    "%     ",
future$nipigon[future$nipigon$year %in% 2090:2099,"lastBday"] %>% mean,
    "(", 
future$nipigon[future$nipigon$year %in% 2090:2099,"lastBday"] %>% mean %>% juliGreg(., 50) %>% as.character,
    ")     ",
( -1 + ( (future$nipigon[future$nipigon$year %in% 2090:2099,"lastBday"] %>% mean) /
    (baseline$nipigon$lastBday %>% mean(na.rm = T))
 )
) *100,
    "%\n"
)


#———————————————————————————————————————————————————————————————————————————————

# ST LOUIS
# L CRIT
cat(
    baseline$stlouis$theoryMin %>% mean *10,
    "mm     ",
    future$stlouis[future$stlouis$year %in% 2040:2050,"theoryMin"] %>% mean *10,
    "mm     ",
    ( -1 + (future$stlouis[future$stlouis$year %in% 2040:2050,"theoryMin"] %>% mean *10 ) /
        (baseline$stlouis$theoryMin %>% mean *10 ) 
     )*100 ,
    "%     ",
    future$stlouis[future$stlouis$year %in% 2090:2099,"theoryMin"] %>% mean *10,
    "mm     ",
    ( -1 + (future$stlouis[future$stlouis$year %in% 2090:2099,"theoryMin"] %>% mean *10 ) /
        (baseline$stlouis$theoryMin %>% mean *10 ) 
     )*100 ,
    "%"
)

# SPAWN ONSET
cat(
    baseline$stlouis$spawnDaysRange1 %>% mean(na.rm = T),
    "(", 
baseline$stlouis$spawnDaysRange1 %>% mean(na.rm = T) %>% juliGreg(., 09) %>% as.character,
    ")     ",
future$stlouis[future$stlouis$year %in% 2040:2050,"spawnDaysRange1"] %>% mean,
    "(", 
future$stlouis[future$stlouis$year %in% 2040:2050,"spawnDaysRange1"] %>% mean %>% juliGreg(., 50) %>% as.character,
    ")     ",
( -1 + ( (future$stlouis[future$stlouis$year %in% 2040:2050,"spawnDaysRange1"] %>% mean) /
    (baseline$stlouis$spawnDaysRange1 %>% mean(na.rm = T))
 )
) *100,
    "%     ",
future$stlouis[future$stlouis$year %in% 2090:2099,"spawnDaysRange1"] %>% mean,
    "(", 
future$stlouis[future$stlouis$year %in% 2090:2099,"spawnDaysRange1"] %>% mean %>% juliGreg(., 50) %>% as.character,
    ")     ",
( -1 + ( (future$stlouis[future$stlouis$year %in% 2090:2099,"spawnDaysRange1"] %>% mean) /
    (baseline$stlouis$spawnDaysRange1 %>% mean(na.rm = T))
 )
) *100,
    "%"
)

# LES
cat(
    baseline$stlouis$lastBday %>% mean(na.rm = T),
    "(", 
baseline$stlouis$lastBday %>% mean(na.rm = T) %>% juliGreg(., 09) %>% as.character,
    ")     ",
future$stlouis[future$stlouis$year %in% 2040:2050,"lastBday"] %>% mean,
    "(", 
future$stlouis[future$stlouis$year %in% 2040:2050,"lastBday"] %>% mean %>% juliGreg(., 50) %>% as.character,
    ")     ",
( -1 + ( (future$stlouis[future$stlouis$year %in% 2040:2050,"lastBday"] %>% mean) /
    (baseline$stlouis$lastBday %>% mean(na.rm = T))
 )
) *100,
    "%     ",
future$stlouis[future$stlouis$year %in% 2090:2099,"lastBday"] %>% mean,
    "(", 
future$stlouis[future$stlouis$year %in% 2090:2099,"lastBday"] %>% mean %>% juliGreg(., 50) %>% as.character,
    ")     ",
( -1 + ( (future$stlouis[future$stlouis$year %in% 2090:2099,"lastBday"] %>% mean) /
    (baseline$stlouis$lastBday %>% mean(na.rm = T))
 )
) *100,
    "%"
)

#———————————————————————————————————————————————————————————————————————————————

# GENESEE
# L CRIT
cat(
    baseline$genesee$theoryMin %>% mean *10,
    "mm     ",
    future$genesee[future$genesee$year %in% 2040:2050,"theoryMin"] %>% mean *10,
    "mm     ",
    ( -1 + (future$genesee[future$genesee$year %in% 2040:2050,"theoryMin"] %>% mean *10 ) /
        (baseline$genesee$theoryMin %>% mean *10 ) 
     )*100 ,
    "%     ",
    future$genesee[future$genesee$year %in% 2090:2099,"theoryMin"] %>% mean *10,
    "mm     ",
    ( -1 + (future$genesee[future$genesee$year %in% 2090:2099,"theoryMin"] %>% mean *10 ) /
        (baseline$genesee$theoryMin %>% mean *10 ) 
     )*100 ,
    "%"
)

# SPAWN ONSET
cat(
    baseline$genesee$spawnDaysRange1 %>% mean(na.rm = T),
    "(", 
baseline$genesee$spawnDaysRange1 %>% mean(na.rm = T) %>% juliGreg(., 09) %>% as.character,
    ")     ",
future$genesee[future$genesee$year %in% 2040:2050,"spawnDaysRange1"] %>% mean,
    "(", 
future$genesee[future$genesee$year %in% 2040:2050,"spawnDaysRange1"] %>% mean %>% juliGreg(., 50) %>% as.character,
    ")     ",
( -1 + ( (future$genesee[future$genesee$year %in% 2040:2050,"spawnDaysRange1"] %>% mean) /
    (baseline$genesee$spawnDaysRange1 %>% mean(na.rm = T))
 )
) *100,
    "%     ",
future$genesee[future$genesee$year %in% 2090:2099,"spawnDaysRange1"] %>% mean,
    "(", 
future$genesee[future$genesee$year %in% 2090:2099,"spawnDaysRange1"] %>% mean %>% juliGreg(., 50) %>% as.character,
    ")     ",
( -1 + ( (future$genesee[future$genesee$year %in% 2090:2099,"spawnDaysRange1"] %>% mean) /
    (baseline$genesee$spawnDaysRange1 %>% mean(na.rm = T))
 )
) *100,
    "%"
)

# LES
cat(
    baseline$genesee$lastBday %>% mean(na.rm = T),
    "(", 
baseline$genesee$lastBday %>% mean(na.rm = T) %>% juliGreg(., 09) %>% as.character,
    ")     ",
future$genesee[future$genesee$year %in% 2040:2050,"lastBday"] %>% mean,
    "(", 
future$genesee[future$genesee$year %in% 2040:2050,"lastBday"] %>% mean %>% juliGreg(., 50) %>% as.character,
    ")     ",
( -1 + ( (future$genesee[future$genesee$year %in% 2040:2050,"lastBday"] %>% mean) /
    (baseline$genesee$lastBday %>% mean(na.rm = T))
 )
) *100,
    "%     ",
future$genesee[future$genesee$year %in% 2090:2099,"lastBday"] %>% mean,
    "(", 
future$genesee[future$genesee$year %in% 2090:2099,"lastBday"] %>% mean %>% juliGreg(., 50) %>% as.character,
    ")     ",
( -1 + ( (future$genesee[future$genesee$year %in% 2090:2099,"lastBday"] %>% mean) /
    (baseline$genesee$lastBday %>% mean(na.rm = T))
 )
) *100,
    "%"
)


#———————————————————————————————————————————————————————————————————————————————

# VERMILLION
# L CRIT
cat(
    baseline$vermillion$theoryMin %>% mean *10,
    "mm     ",
    future$vermillion[future$vermillion$year %in% 2040:2050,"theoryMin"] %>% mean *10,
    "mm     ",
    ( -1 + (future$vermillion[future$vermillion$year %in% 2040:2050,"theoryMin"] %>% mean *10 ) /
        (baseline$vermillion$theoryMin %>% mean *10 ) 
     )*100 ,
    "%     ",
    future$vermillion[future$vermillion$year %in% 2090:2099,"theoryMin"] %>% mean *10,
    "mm     ",
    ( -1 + (future$vermillion[future$vermillion$year %in% 2090:2099,"theoryMin"] %>% mean *10 ) /
        (baseline$vermillion$theoryMin %>% mean *10 ) 
     )*100 ,
    "%"
)

# SPAWN ONSET
cat(
    baseline$vermillion$spawnDaysRange1 %>% mean(na.rm = T),
    "(", 
baseline$vermillion$spawnDaysRange1 %>% mean(na.rm = T) %>% juliGreg(., 09) %>% as.character,
    ")     ",
future$vermillion[future$vermillion$year %in% 2040:2050,"spawnDaysRange1"] %>% mean,
    "(", 
future$vermillion[future$vermillion$year %in% 2040:2050,"spawnDaysRange1"] %>% mean %>% juliGreg(., 50) %>% as.character,
    ")     ",
( -1 + ( (future$vermillion[future$vermillion$year %in% 2040:2050,"spawnDaysRange1"] %>% mean) /
    (baseline$vermillion$spawnDaysRange1 %>% mean(na.rm = T))
 )
) *100,
    "%     ",
future$vermillion[future$vermillion$year %in% 2090:2099,"spawnDaysRange1"] %>% mean,
    "(", 
future$vermillion[future$vermillion$year %in% 2090:2099,"spawnDaysRange1"] %>% mean %>% juliGreg(., 50) %>% as.character,
    ")     ",
( -1 + ( (future$vermillion[future$vermillion$year %in% 2090:2099,"spawnDaysRange1"] %>% mean) /
    (baseline$vermillion$spawnDaysRange1 %>% mean(na.rm = T))
 )
) *100,
    "%"
)

# LES
cat(
    baseline$vermillion$lastBday %>% mean(na.rm = T),
    "(", 
baseline$vermillion$lastBday %>% mean(na.rm = T) %>% juliGreg(., 09) %>% as.character,
    ")     ",
future$vermillion[future$vermillion$year %in% 2040:2050,"lastBday"] %>% mean,
    "(", 
future$vermillion[future$vermillion$year %in% 2040:2050,"lastBday"] %>% mean %>% juliGreg(., 50) %>% as.character,
    ")     ",
( -1 + ( (future$vermillion[future$vermillion$year %in% 2040:2050,"lastBday"] %>% mean) /
    (baseline$vermillion$lastBday %>% mean(na.rm = T))
 )
) *100,
    "%     ",
future$vermillion[future$vermillion$year %in% 2090:2099,"lastBday"] %>% mean,
    "(", 
future$vermillion[future$vermillion$year %in% 2090:2099,"lastBday"] %>% mean %>% juliGreg(., 50) %>% as.character,
    ")     ",
( -1 + ( (future$vermillion[future$vermillion$year %in% 2090:2099,"lastBday"] %>% mean) /
    (baseline$vermillion$lastBday %>% mean(na.rm = T))
 )
) *100,
    "%"
)




# Table 3 - changes in duration of periods (winter, spawning, effective spawning)
# ﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋

# (example)     
#                          Winter            Spawning         Effective
#              Nipigon    # days (% Δ)
#   PERIOD     St Louis
#    2050      Genesee
#              Vermillion

# run code chunks by paragraph to get whole-column outputs
# (percentage outputs will still need to be changed from decimal format)


cat("2050 WINTER\n\n Days (%)")
# NIPIGON
cat("\n",
( 
 (future$nipigon[future$nipigon$year %in% 2040:2050,"wintDay"] %>% mean %>% round) -
 (baseline$nipigon$wintDay %>% mean %>% round)
),
# 
paste0("(",
     -1 + ( 
           (future$nipigon[future$nipigon$year %in% 2040:2050,"wintDay"] %>% mean %>% round) /
              (baseline$nipigon$wintDay %>% mean %>% round)
           ) %>% round(2),
    "%)")
)
# ST LOUIS
cat("\n",
( 
 (future$stlouis[future$stlouis$year %in% 2040:2050,"wintDay"] %>% mean %>% round) -
 (baseline$stlouis$wintDay %>% mean %>% round)
),
#
    paste0("(",
     -1 + ( 
           (future$stlouis[future$stlouis$year %in% 2040:2050,"wintDay"] %>% mean %>% round) /
              (baseline$stlouis$wintDay %>% mean %>% round)
           ) %>% round(2),
           "%)"
    )
)
# GENESEE
cat("\n",
( 
 (future$genesee[future$genesee$year %in% 2040:2050,"wintDay"] %>% mean %>% round) -
 (baseline$genesee$wintDay %>% mean %>% round)
),
#
    paste0("(",
     -1 + ( 
           (future$genesee[future$genesee$year %in% 2040:2050,"wintDay"] %>% mean %>% round) /
              (baseline$genesee$wintDay %>% mean %>% round)
           ) %>% round(2),
           "%)"
    )
)
# VERMILLION
cat("\n",
( 
 (future$vermillion[future$vermillion$year %in% 2040:2050,"wintDay"] %>% mean %>% round) -
 (baseline$vermillion$wintDay %>% mean %>% round)
),
#
    paste0("(",
     -1 + ( 
           (future$vermillion[future$vermillion$year %in% 2040:2050,"wintDay"] %>% mean %>% round) /
              (baseline$vermillion$wintDay %>% mean %>% round)
           ) %>% round(2),
           "%)"
    )
)
#
cat("\n\n2100 WINTER\n\n Days (%)")
# NIPIGON
cat("\n",
( 
 (future$nipigon[future$nipigon$year %in% 2090:2099,"wintDay"] %>% mean %>% round) -
 (baseline$nipigon$wintDay %>% mean %>% round)
),
# 
    paste0("(",
     -1 + ( 
           (future$nipigon[future$nipigon$year %in% 2090:2099,"wintDay"] %>% mean %>% round) /
              (baseline$nipigon$wintDay %>% mean %>% round)
           ) %>% round(2),
           "%)"
    )
)
# ST LOUIS
cat("\n",
( 
 (future$stlouis[future$stlouis$year %in% 2090:2099,"wintDay"] %>% mean %>% round) -
 (baseline$stlouis$wintDay %>% mean %>% round)
),
#
    paste0("(",
     -1 + ( 
           (future$stlouis[future$stlouis$year %in% 2090:2099,"wintDay"] %>% mean %>% round) /
              (baseline$stlouis$wintDay %>% mean %>% round)
           ) %>% round(2),
           "%)"
    )
)
# GENESEE
cat("\n",
( 
 (future$genesee[future$genesee$year %in% 2090:2099,"wintDay"] %>% mean %>% round) -
 (baseline$genesee$wintDay %>% mean %>% round)
),
#
    paste0("(",
     -1 + ( 
           (future$genesee[future$genesee$year %in% 2090:2099,"wintDay"] %>% mean %>% round) /
              (baseline$genesee$wintDay %>% mean %>% round)
           ) %>% round(2),
           "%)"
    )
)
# VERMILLION
cat("\n",
( 
 (future$vermillion[future$vermillion$year %in% 2090:2099,"wintDay"] %>% mean %>% round) -
 (baseline$vermillion$wintDay %>% mean %>% round)
),
#
    paste0("(",
     -1 + ( 
           (future$vermillion[future$vermillion$year %in% 2090:2099,"wintDay"] %>% mean %>% round) /
              (baseline$vermillion$wintDay %>% mean %>% round)
           ) %>% round(2),
           "%)"
    )
)


cat("2050 SPAWNING\n\n Days (%)")
# NIPIGON
cat("\n","+",
( 
 (future$nipigon[future$nipigon$year %in% 2040:2050,"num.spawnDays"] %>% mean %>% round) -
 (baseline$nipigon$num.spawnDays %>% mean %>% round)
),
#
    paste0("(+",
     -1 + ( 
           (future$nipigon[future$nipigon$year %in% 2040:2050,"num.spawnDays"] %>% mean %>% round) /
              (baseline$nipigon$num.spawnDays %>% mean %>% round)
           ) %>% round(2),
           "%)"
    )
)
# ST LOUIS
cat("\n","+",
( 
 (future$stlouis[future$stlouis$year %in% 2040:2050,"num.spawnDays"] %>% mean %>% round) -
 (baseline$stlouis$num.spawnDays %>% mean %>% round)
),
#
    paste0("(+",
     -1 + ( 
           (future$stlouis[future$stlouis$year %in% 2040:2050,"num.spawnDays"] %>% mean %>% round) /
              (baseline$stlouis$num.spawnDays %>% mean %>% round)
           ) %>% round(2),
           "%)"
    )
)
# GENESEE
cat("\n","+",
( 
 (future$genesee[future$genesee$year %in% 2040:2050,"num.spawnDays"] %>% mean %>% round) -
 (baseline$genesee$num.spawnDays %>% mean %>% round)
),
#
    paste0("(+",
     -1 + ( 
           (future$genesee[future$genesee$year %in% 2040:2050,"num.spawnDays"] %>% mean %>% round) /
              (baseline$genesee$num.spawnDays %>% mean %>% round)
           ) %>% round(2),
           "%)"
    )
)
# VERMILLION
cat("\n","+",
( 
 (future$vermillion[future$vermillion$year %in% 2040:2050,"num.spawnDays"] %>% mean %>% round) -
 (baseline$vermillion$num.spawnDays %>% mean %>% round)
),
#
    paste0("(+",
     -1 + ( 
           (future$vermillion[future$vermillion$year %in% 2040:2050,"num.spawnDays"] %>% mean %>% round) /
              (baseline$vermillion$num.spawnDays %>% mean %>% round)
           ) %>% round(2),
           "%)"
    )
)
#
cat("\n\n2100 SPAWNING\n\n Days (%)")
# NIPIGON
cat("\n","+",
( 
 (future$nipigon[future$nipigon$year %in% 2090:2099,"num.spawnDays"] %>% mean %>% round) -
 (baseline$nipigon$num.spawnDays %>% mean %>% round)
),
#
    paste0("(+",
     -1 + ( 
           (future$nipigon[future$nipigon$year %in% 2090:2099,"num.spawnDays"] %>% mean %>% round) /
              (baseline$nipigon$num.spawnDays %>% mean %>% round)
           ) %>% round(2),
           "%)"
    )
)
# ST LOUIS
cat("\n","+",
( 
 (future$stlouis[future$stlouis$year %in% 2090:2099,"num.spawnDays"] %>% mean %>% round) -
 (baseline$stlouis$num.spawnDays %>% mean %>% round)
),
#
    paste0("(+",
     -1 + ( 
           (future$stlouis[future$stlouis$year %in% 2090:2099,"num.spawnDays"] %>% mean %>% round) /
              (baseline$stlouis$num.spawnDays %>% mean %>% round)
           ) %>% round(2),
           "%)"
    )
)
# GENESEE
cat("\n","+",
( 
 (future$genesee[future$genesee$year %in% 2090:2099,"num.spawnDays"] %>% mean %>% round) -
 (baseline$genesee$num.spawnDays %>% mean %>% round)
),
#
    paste0("(+",
     -1 + ( 
           (future$genesee[future$genesee$year %in% 2090:2099,"num.spawnDays"] %>% mean %>% round) /
              (baseline$genesee$num.spawnDays %>% mean %>% round)
           ) %>% round(2),
           "%)"
    )
)
# VERMILLION
cat("\n","+",
( 
 (future$vermillion[future$vermillion$year %in% 2090:2099,"num.spawnDays"] %>% mean %>% round) -
 (baseline$vermillion$num.spawnDays %>% mean %>% round)
),
#
    paste0("(+",
     -1 + ( 
           (future$vermillion[future$vermillion$year %in% 2090:2099,"num.spawnDays"] %>% mean %>% round) /
              (baseline$vermillion$num.spawnDays %>% mean %>% round)
           ) %>% round(2),
           "%)"
    )
)


cat("2050 EFFECTIVE\n\n Days (%)")
# NIPIGON
cat("\n","+",
( 
 (future$nipigon[future$nipigon$year %in% 2040:2050,"surviveNum"] %>% mean %>% round) -
 (baseline$nipigon$surviveNum %>% mean %>% round)
),
#
    paste0("(+",
     -1 + ( 
           (future$nipigon[future$nipigon$year %in% 2040:2050,"surviveNum"] %>% mean %>% round) /
              (baseline$nipigon$surviveNum %>% mean %>% round)
           ) %>% round(2),
           "%)"
    )
)
# ST LOUIS
cat("\n","+",
( 
 (future$stlouis[future$stlouis$year %in% 2040:2050,"surviveNum"] %>% mean %>% round) -
 (baseline$stlouis$surviveNum %>% mean %>% round)
),
#
    paste0("(+",
     -1 + ( 
           (future$stlouis[future$stlouis$year %in% 2040:2050,"surviveNum"] %>% mean %>% round) /
              (baseline$stlouis$surviveNum %>% mean %>% round)
           ) %>% round(2),
           "%)"
    )
)
# GENESEE
cat("\n","+",
( 
 (future$genesee[future$genesee$year %in% 2040:2050,"surviveNum"] %>% mean %>% round) -
 (baseline$genesee$surviveNum %>% mean %>% round)
),
#
    paste0("(+",
     -1 + ( 
           (future$genesee[future$genesee$year %in% 2040:2050,"surviveNum"] %>% mean %>% round) /
              (baseline$genesee$surviveNum %>% mean %>% round)
           ) %>% round(2),
           "%)"
    )
)
# VERMILLION
cat("\n","+",
( 
 (future$vermillion[future$vermillion$year %in% 2040:2050,"surviveNum"] %>% mean %>% round) -
 (baseline$vermillion$surviveNum %>% mean %>% round)
),
#
    paste0("(+",
     -1 + ( 
           (future$vermillion[future$vermillion$year %in% 2040:2050,"surviveNum"] %>% mean %>% round) /
              (baseline$vermillion$surviveNum %>% mean %>% round)
           ) %>% round(2),
           "%)"
    )
)
#
cat("\n\n2100 EFFECTIVE\n\n Days (%)")
# NIPIGON
cat("\n","+",
( 
 (future$nipigon[future$nipigon$year %in% 2090:2099,"surviveNum"] %>% mean %>% round) -
 (baseline$nipigon$surviveNum %>% mean %>% round)
),
#
    paste0("(+",
     -1 + ( 
           (future$nipigon[future$nipigon$year %in% 2090:2099,"surviveNum"] %>% mean %>% round) /
              (baseline$nipigon$surviveNum %>% mean %>% round)
           ) %>% round(2),
           "%)"
    )
)
# ST LOUIS
cat("\n","+",
( 
 (future$stlouis[future$stlouis$year %in% 2090:2099,"surviveNum"] %>% mean %>% round) -
 (baseline$stlouis$surviveNum %>% mean %>% round)
),
#
    paste0("(+",
     -1 + ( 
           (future$stlouis[future$stlouis$year %in% 2090:2099,"surviveNum"] %>% mean %>% round) /
              (baseline$stlouis$surviveNum %>% mean %>% round)
           ) %>% round(2),
           "%)"
    )
)
# GENESEE
cat("\n","+",
( 
 (future$genesee[future$genesee$year %in% 2090:2099,"surviveNum"] %>% mean %>% round) -
 (baseline$genesee$surviveNum %>% mean %>% round)
),
#
    paste0("(+",
     -1 + ( 
           (future$genesee[future$genesee$year %in% 2090:2099,"surviveNum"] %>% mean %>% round) /
              (baseline$genesee$surviveNum %>% mean %>% round)
           ) %>% round(2),
           "%)"
    )
)
# VERMILLION
cat("\n","+",
( 
 (future$vermillion[future$vermillion$year %in% 2090:2099,"surviveNum"] %>% mean %>% round) -
 (baseline$vermillion$surviveNum %>% mean %>% round)
),
#
    paste0("(+",
     -1 + ( 
           (future$vermillion[future$vermillion$year %in% 2090:2099,"surviveNum"] %>% mean %>% round) /
              (baseline$vermillion$surviveNum %>% mean %>% round)
           ) %>% round(2),
           "%)"
    )
)



















# Table 6 - proportion of spawning season that's before LES (percent survivable)
# ﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋

cat("   NIPIGON \n",
 nipSummary[[1]][(nipSummary[[1]]$year %in% 2040:2050), "percentSpawnSurvive"] %>% mean %>% round(2),
 nipSummary[[1]][(nipSummary[[1]]$year %in% 2090:2099), "percentSpawnSurvive"] %>% mean %>% round(2),
 "\n",
 nipSummary[[2]][(nipSummary[[2]]$year %in% 2040:2050), "percentSpawnSurvive"] %>% mean %>% round(2),
 nipSummary[[2]][(nipSummary[[2]]$year %in% 2090:2099), "percentSpawnSurvive"] %>% mean %>% round(2),
 "\n",
 nipSummary[[3]][(nipSummary[[3]]$year %in% 2040:2050), "percentSpawnSurvive"] %>% mean %>% round(2),
 nipSummary[[3]][(nipSummary[[3]]$year %in% 2090:2099), "percentSpawnSurvive"] %>% mean %>% round(2),
 "\n",
 nipSummary[[4]][(nipSummary[[4]]$year %in% 2040:2050), "percentSpawnSurvive"] %>% mean %>% round(2),
 nipSummary[[4]][(nipSummary[[4]]$year %in% 2090:2099), "percentSpawnSurvive"] %>% mean %>% round(2),
 "\n",
 nipSummary[[5]][(nipSummary[[5]]$year %in% 2040:2050), "percentSpawnSurvive"] %>% mean %>% round(2),
 nipSummary[[5]][(nipSummary[[5]]$year %in% 2090:2099), "percentSpawnSurvive"] %>% mean %>% round(2),
 "\n\n"
)
#
cat("   ST LOUIS \n",
 slouSummary[[1]][(slouSummary[[1]]$year %in% 2040:2050), "percentSpawnSurvive"] %>% mean %>% round(2),
 slouSummary[[1]][(slouSummary[[1]]$year %in% 2090:2099), "percentSpawnSurvive"] %>% mean %>% round(2),
 "\n",
 slouSummary[[2]][(slouSummary[[2]]$year %in% 2040:2050), "percentSpawnSurvive"] %>% mean %>% round(2),
 slouSummary[[2]][(slouSummary[[2]]$year %in% 2090:2099), "percentSpawnSurvive"] %>% mean %>% round(2),
 "\n",
 slouSummary[[3]][(slouSummary[[3]]$year %in% 2040:2050), "percentSpawnSurvive"] %>% mean %>% round(2),
 slouSummary[[3]][(slouSummary[[3]]$year %in% 2090:2099), "percentSpawnSurvive"] %>% mean %>% round(2),
 "\n",
 slouSummary[[4]][(slouSummary[[4]]$year %in% 2040:2050), "percentSpawnSurvive"] %>% mean %>% round(2),
 slouSummary[[4]][(slouSummary[[4]]$year %in% 2090:2099), "percentSpawnSurvive"] %>% mean %>% round(2),
 "\n",
 slouSummary[[5]][(slouSummary[[5]]$year %in% 2040:2050), "percentSpawnSurvive"] %>% mean %>% round(2),
 slouSummary[[5]][(slouSummary[[5]]$year %in% 2090:2099), "percentSpawnSurvive"] %>% mean %>% round(2),
 "\n\n"
)
#
cat("   GENESEE \n",
 genSummary[[1]][(genSummary[[1]]$year %in% 2040:2050), "percentSpawnSurvive"] %>% mean %>% round(2),
 genSummary[[1]][(genSummary[[1]]$year %in% 2090:2099), "percentSpawnSurvive"] %>% mean %>% round(2),
 "\n",
 genSummary[[2]][(genSummary[[2]]$year %in% 2040:2050), "percentSpawnSurvive"] %>% mean %>% round(2),
 genSummary[[2]][(genSummary[[2]]$year %in% 2090:2099), "percentSpawnSurvive"] %>% mean %>% round(2),
 "\n",
 genSummary[[3]][(genSummary[[3]]$year %in% 2040:2050), "percentSpawnSurvive"] %>% mean %>% round(2),
 genSummary[[3]][(genSummary[[3]]$year %in% 2090:2099), "percentSpawnSurvive"] %>% mean %>% round(2),
 "\n",
 genSummary[[4]][(genSummary[[4]]$year %in% 2040:2050), "percentSpawnSurvive"] %>% mean %>% round(2),
 genSummary[[4]][(genSummary[[4]]$year %in% 2090:2099), "percentSpawnSurvive"] %>% mean %>% round(2),
 "\n",
 genSummary[[5]][(genSummary[[5]]$year %in% 2040:2050), "percentSpawnSurvive"] %>% mean %>% round(2),
 genSummary[[5]][(genSummary[[5]]$year %in% 2090:2099), "percentSpawnSurvive"] %>% mean %>% round(2),
 "\n\n"
)
#
cat("   VERMILLION \n",
 vermSummary[[1]][(vermSummary[[1]]$year %in% 2040:2050), "percentSpawnSurvive"] %>% mean %>% round(2),
 vermSummary[[1]][(vermSummary[[1]]$year %in% 2090:2099), "percentSpawnSurvive"] %>% mean %>% round(2),
 "\n",
 vermSummary[[2]][(vermSummary[[2]]$year %in% 2040:2050), "percentSpawnSurvive"] %>% mean %>% round(2),
 vermSummary[[2]][(vermSummary[[2]]$year %in% 2090:2099), "percentSpawnSurvive"] %>% mean %>% round(2),
 "\n",
 vermSummary[[3]][(vermSummary[[3]]$year %in% 2040:2050), "percentSpawnSurvive"] %>% mean %>% round(2),
 vermSummary[[3]][(vermSummary[[3]]$year %in% 2090:2099), "percentSpawnSurvive"] %>% mean %>% round(2),
 "\n",
 vermSummary[[4]][(vermSummary[[4]]$year %in% 2040:2050), "percentSpawnSurvive"] %>% mean %>% round(2),
 vermSummary[[4]][(vermSummary[[4]]$year %in% 2090:2099), "percentSpawnSurvive"] %>% mean %>% round(2),
 "\n",
 vermSummary[[5]][(vermSummary[[5]]$year %in% 2040:2050), "percentSpawnSurvive"] %>% mean %>% round(2),
 vermSummary[[5]][(vermSummary[[5]]$year %in% 2090:2099), "percentSpawnSurvive"] %>% mean %>% round(2),
 "\n\n"
)








# Table 7 - l crit (mm) per GCM model for mid (left) & late century (right) periods
# ﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋

cat("   NIPIGON \n",
 nipSummary[[1]][(nipSummary[[1]]$year %in% 2040:2050), "theoryMin"] %>% mean *10 %>% round(2),
 nipSummary[[1]][(nipSummary[[1]]$year %in% 2090:2099), "theoryMin"] %>% mean *10 %>% round(2),
 "\n",
 nipSummary[[2]][(nipSummary[[2]]$year %in% 2040:2050), "theoryMin"] %>% mean *10 %>% round(2),
 nipSummary[[2]][(nipSummary[[2]]$year %in% 2090:2099), "theoryMin"] %>% mean *10 %>% round(2),
 "\n",
 nipSummary[[3]][(nipSummary[[3]]$year %in% 2040:2050), "theoryMin"] %>% mean *10 %>% round(2),
 nipSummary[[3]][(nipSummary[[3]]$year %in% 2090:2099), "theoryMin"] %>% mean *10 %>% round(2),
 "\n",
 nipSummary[[4]][(nipSummary[[4]]$year %in% 2040:2050), "theoryMin"] %>% mean *10 %>% round(2),
 nipSummary[[4]][(nipSummary[[4]]$year %in% 2090:2099), "theoryMin"] %>% mean *10 %>% round(2),
 "\n",
 nipSummary[[5]][(nipSummary[[5]]$year %in% 2040:2050), "theoryMin"] %>% mean *10 %>% round(2),
 nipSummary[[5]][(nipSummary[[5]]$year %in% 2090:2099), "theoryMin"] %>% mean *10 %>% round(2),
 "\n\n"
)
#
cat("   ST LOUIS \n",
 slouSummary[[1]][(slouSummary[[1]]$year %in% 2040:2050), "theoryMin"] %>% mean *10 %>% round(2),
 slouSummary[[1]][(slouSummary[[1]]$year %in% 2090:2099), "theoryMin"] %>% mean *10 %>% round(2),
 "\n",
 slouSummary[[2]][(slouSummary[[2]]$year %in% 2040:2050), "theoryMin"] %>% mean *10 %>% round(2),
 slouSummary[[2]][(slouSummary[[2]]$year %in% 2090:2099), "theoryMin"] %>% mean *10 %>% round(2),
 "\n",
 slouSummary[[3]][(slouSummary[[3]]$year %in% 2040:2050), "theoryMin"] %>% mean *10 %>% round(2),
 slouSummary[[3]][(slouSummary[[3]]$year %in% 2090:2099), "theoryMin"] %>% mean *10 %>% round(2),
 "\n",
 slouSummary[[4]][(slouSummary[[4]]$year %in% 2040:2050), "theoryMin"] %>% mean *10 %>% round(2),
 slouSummary[[4]][(slouSummary[[4]]$year %in% 2090:2099), "theoryMin"] %>% mean *10 %>% round(2),
 "\n",
 slouSummary[[5]][(slouSummary[[5]]$year %in% 2040:2050), "theoryMin"] %>% mean *10 %>% round(2),
 slouSummary[[5]][(slouSummary[[5]]$year %in% 2090:2099), "theoryMin"] %>% mean *10 %>% round(2),
 "\n\n"
)
#
cat("   GENESEE \n",
 genSummary[[1]][(genSummary[[1]]$year %in% 2040:2050), "theoryMin"] %>% mean *10 %>% round(2),
 genSummary[[1]][(genSummary[[1]]$year %in% 2090:2099), "theoryMin"] %>% mean *10 %>% round(2),
 "\n",
 genSummary[[2]][(genSummary[[2]]$year %in% 2040:2050), "theoryMin"] %>% mean *10 %>% round(2),
 genSummary[[2]][(genSummary[[2]]$year %in% 2090:2099), "theoryMin"] %>% mean *10 %>% round(2),
 "\n",
 genSummary[[3]][(genSummary[[3]]$year %in% 2040:2050), "theoryMin"] %>% mean *10 %>% round(2),
 genSummary[[3]][(genSummary[[3]]$year %in% 2090:2099), "theoryMin"] %>% mean *10 %>% round(2),
 "\n",
 genSummary[[4]][(genSummary[[4]]$year %in% 2040:2050), "theoryMin"] %>% mean *10 %>% round(2),
 genSummary[[4]][(genSummary[[4]]$year %in% 2090:2099), "theoryMin"] %>% mean *10 %>% round(2),
 "\n",
 genSummary[[5]][(genSummary[[5]]$year %in% 2040:2050), "theoryMin"] %>% mean *10 %>% round(2),
 genSummary[[5]][(genSummary[[5]]$year %in% 2090:2099), "theoryMin"] %>% mean *10 %>% round(2),
 "\n\n"
)
#
cat("   VERMILLION \n",
 vermSummary[[1]][(vermSummary[[1]]$year %in% 2040:2050), "theoryMin"] %>% mean *10 %>% round(2),
 vermSummary[[1]][(vermSummary[[1]]$year %in% 2090:2099), "theoryMin"] %>% mean *10 %>% round(2),
 "\n",
 vermSummary[[2]][(vermSummary[[2]]$year %in% 2040:2050), "theoryMin"] %>% mean *10 %>% round(2),
 vermSummary[[2]][(vermSummary[[2]]$year %in% 2090:2099), "theoryMin"] %>% mean *10 %>% round(2),
 "\n",
 vermSummary[[3]][(vermSummary[[3]]$year %in% 2040:2050), "theoryMin"] %>% mean *10 %>% round(2),
 vermSummary[[3]][(vermSummary[[3]]$year %in% 2090:2099), "theoryMin"] %>% mean *10 %>% round(2),
 "\n",
 vermSummary[[4]][(vermSummary[[4]]$year %in% 2040:2050), "theoryMin"] %>% mean *10 %>% round(2),
 vermSummary[[4]][(vermSummary[[4]]$year %in% 2090:2099), "theoryMin"] %>% mean *10 %>% round(2),
 "\n",
 vermSummary[[5]][(vermSummary[[5]]$year %in% 2040:2050), "theoryMin"] %>% mean *10 %>% round(2),
 vermSummary[[5]][(vermSummary[[5]]$year %in% 2090:2099), "theoryMin"] %>% mean *10 %>% round(2),
 "\n\n"
)

# Checking if decreases are greater baseline to mid-century (GCM vs gage bias?)
# ﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋

# baseline difference to mid century
(baseline[[1]]$theoryMin %>% mean) - (future[[1]]$theoryMin[16:26] %>% mean)
(baseline[[2]]$theoryMin %>% mean) - (future[[2]]$theoryMin[16:26] %>% mean)
(baseline[[3]]$theoryMin %>% mean) - (future[[3]]$theoryMin[16:26] %>% mean)
(baseline[[4]]$theoryMin %>% mean) - (future[[4]]$theoryMin[16:26] %>% mean)

# baseline difference to late century
(baseline[[1]]$theoryMin %>% mean) - (future[[1]]$theoryMin[66:75] %>% mean)
(baseline[[2]]$theoryMin %>% mean) - (future[[2]]$theoryMin[66:75] %>% mean)
(baseline[[3]]$theoryMin %>% mean) - (future[[3]]$theoryMin[66:75] %>% mean)
(baseline[[4]]$theoryMin %>% mean) - (future[[4]]$theoryMin[66:75] %>% mean)

# mid century difference to late century
(future[[1]]$theoryMin[16:26] %>% mean) - (future[[1]]$theoryMin[66:75] %>% mean)
(future[[2]]$theoryMin[16:26] %>% mean) - (future[[2]]$theoryMin[66:75] %>% mean)
(future[[3]]$theoryMin[16:26] %>% mean) - (future[[3]]$theoryMin[66:75] %>% mean)
(future[[4]]$theoryMin[16:26] %>% mean) - (future[[4]]$theoryMin[66:75] %>% mean)

baseline[[2]]$theoryMin 
future[[2]]$theoryMin 







#
# ﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋





#
# ﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋




