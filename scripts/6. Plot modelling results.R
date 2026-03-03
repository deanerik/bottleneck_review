#!/usr/bin/env Rscript

# 6. Plot modelling results.R
#    Licence: GNU GPLv3 - see LICENSE.txt for more details

# Plotting results from the first year of life simulations
 
#———————————————————————————————————————————————————————————————————————————————

# run the analyses and load their outputs
source("./5. Run model with climate data.R")

# load plotting libraries
library(ggplot2)
library(ggridges)
library(viridis)
library(cowplot)
library(grid)
library(gridExtra)

# if you want to export plots in EPS format
#library("cairo")

# index of relevant years
y2050s <- future[[2]]$year %in% 2040:2050
y2100s <- future[[2]]$year %in% 2090:2099

# make a folder for graphical outputs if one doesn't already exist 
if (dir.exists("../graphics")) {
    cat("/graphics directory ready to receive")
} else {
    dir.create("../graphics")
    cat("/graphics directory created")
}

#———————————————————————————————————————————————————————————————————————————————

# Function to extract the cohort size data for density plots
get.plotdata <- function(tempSeries){

    # make a copy
    temperatures <- tempSeries

    # make plot data container
    plotdata <- data.frame()

    # figure out what years you're running
    tempYears <- temperatures$year %>% unique 

    # loop through years
    for (num in seq_along(tempYears)){

        # get the daily-length-cohorts and stack them up
        cohort <- get.cohort( temperatures[temperatures$year == tempYears[num],1:2] )

        plotdata <- rbind(plotdata, cohort[[1]])

    }

    return(plotdata)
}

#———————————————————————————————————————————————————————————————————————————————

# Size cohort distribution plots 
baselinePlot <- list(genesee    = get.plotdata(g.temp),
                     nipigon    = get.plotdata(n.temp),
                     stlouis    = get.plotdata(s.temp),
                     vermillion = get.plotdata(v.temp))


# ---- Vermillion -------------------------------------------------------------

# plotting results: cohort sizes
verm50 <- lapply(trib.mods[["verm"]],function(x){

                    get.plotdata(  x[x$year %in% (2040:2050),])
})

verm100 <- lapply(trib.mods[["verm"]],function(x){

                    get.plotdata(  x[x$year %in% (2090:2100),])
})


# stack up each model's temp series
verm50 <- rbind(verm50[[1]],
                verm50[[2]],
                verm50[[3]],
                verm50[[4]],
                verm50[[5]])

# stack up each model's temp series
verm100 <- rbind(verm100[[1]],
                 verm100[[2]],
                 verm100[[3]],
                 verm100[[4]],
                 verm100[[5]])

# add the column to signify time period type
vermillion <- rbind( cbind(baselinePlot$vermillion,  type = "Baseline"),
                     cbind(verm50, type = "2050"),
                     cbind(verm100, type = "2100")
)


# plot to test
ggplot(vermillion, aes(x=size, fill=factor(type, levels = c("Baseline","2050","2100")))) + geom_density(alpha=0.6, color=NA, show.legend = F) 

# ---- Genesee ----------------------------------------------------------------

# repeat as above for each tributary

gen50 <- lapply(trib.mods[["gen"]],function(x){
                    get.plotdata(  x[x$year %in% (2040:2050),])
})

gen100 <- lapply(trib.mods[["gen"]],function(x){
                    get.plotdata(  x[x$year %in% (2090:2100),])
})

gen50 <- rbind(gen50[[1]],
                gen50[[2]],
                gen50[[3]],
                gen50[[4]],
                gen50[[5]])

gen100 <- rbind(gen100[[1]],
                 gen100[[2]],
                 gen100[[3]],
                 gen100[[4]],
                 gen100[[5]])

genesee <- rbind( cbind(baselinePlot$genesee,  type = "Baseline"),
                     cbind(gen50, type = "2050"),
                     cbind(gen100, type = "2100")
)

# plot to test
ggplot(genesee, aes(x=size, fill=factor(type, levels = c("Baseline","2050","2100")))) + geom_density(alpha=0.6, color=NA, show.legend = F) 

# ---- St. Louis --------------------------------------------------------------

# repeat as above for each tributary

slou50 <- lapply(trib.mods[["slou"]],function(x){
                    get.plotdata(  x[x$year %in% (2040:2050),])
})

slou100 <- lapply(trib.mods[["slou"]],function(x){
                    get.plotdata(  x[x$year %in% (2090:2100),])
})

slou50 <- rbind(slou50[[1]],
                slou50[[2]],
                slou50[[3]],
                slou50[[4]],
                slou50[[5]])

slou100 <- rbind(slou100[[1]],
                 slou100[[2]],
                 slou100[[3]],
                 slou100[[4]],
                 slou100[[5]])

stlouis <- rbind( cbind(baselinePlot$stlouis,  type = "Baseline"),
                     cbind(slou50, type = "2050"),
                     cbind(slou100, type = "2100")
)

# plot to test
ggplot(stlouis, aes(x=size, fill=factor(type, levels = c("Baseline","2050","2100")))) + geom_density(alpha=0.6, color=NA, show.legend = F) 


# ---- Nipigon ----------------------------------------------------------------

# repeat as above for each tributary

nip50 <- lapply(trib.mods[["nip"]],function(x){
                    get.plotdata(  x[x$year %in% (2040:2050),])
})

nip100 <- lapply(trib.mods[["nip"]],function(x){
                    get.plotdata(  x[x$year %in% (2090:2100),])
})

nip50 <- rbind( nip50[[1]],
                nip50[[2]],
                nip50[[3]],
                nip50[[4]],
                nip50[[5]])

nip100 <- rbind( nip100[[1]],
                 nip100[[2]],
                 nip100[[3]],
                 nip100[[4]],
                 nip100[[5]])

nipigon <- rbind( cbind(baselinePlot$nipigon,  type = "Baseline"),
                     cbind(nip50, type = "2050"),
                     cbind(nip100, type = "2100")
)

# plot to test
ggplot(nipigon, aes(x=size, fill=factor(type, levels = c("Baseline","2050","2100")))) + geom_density(alpha=0.6, color=NA, show.legend = F) 


# ---- Density Plots -----------------------------------------------------------

# color values
pre <- inferno(100)[80]
post50 <- inferno(100)[63]
post100 <- inferno(100)[53]
preL <- 'gray70'
post50L <- 'gray55' 
post100L <- 'gray40'


# 101, [1:989,]
nipP <- ggplot(nipigon,    aes(x=size, fill=factor(type, levels = c("Baseline","2050","2100")))) +
    # middle chunk same for everyone
    geom_density(alpha=0.6, color=NA, show.legend = F) +
    xlim(-4,53) + ylim(0,0.40) + 
    scale_y_continuous(labels = function(x) format(x, nsmall = 2), limits = c(0,.6) ) + # force 2 decimals
    theme_classic() + 
        theme(text = element_text(size=12), 
              #axis.text.x = element_text(size = 20),
              axis.text.x = element_blank(),
              #axis.ticks.x = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank()) +
                 labs(x = "Total Length (cm)", y = "Probability Density") +
                 scale_fill_manual(values=c(pre, post50, post100)) +
                 geom_vline(aes(xintercept=baseline$nipigon$theoryMin %>% mean), color=preL, linetype="dashed", size=0.75) +
                 geom_vline(aes(xintercept=future$nipigon[future$nipigon$year %in% 2040:2050,"theoryMin"] %>% mean), color=post50L, linetype="dashed", size=0.75) +
                 geom_vline(aes(xintercept=future$nipigon[future$nipigon$year %in% 2090:2099,"theoryMin"] %>% mean), color=post100L, linetype="dashed", size=0.75)
#307, 1766
genP <- ggplot(genesee,    aes(x=size, fill=factor(type, levels = c("Baseline","2050","2100")))) +
    # middle chunk same for everyone
    geom_density(alpha=0.6, color=NA, show.legend = T) +
    xlim(-4,53) + ylim(0,0.11) + 
    #xlim(-4,53) + ylim(0,0.58) + # to match nip
    theme_classic() + 
    theme(text = element_text(size=12), 
          #axis.text.x = element_text(size = 20),
          #axis.text.y = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = c(0.90, 0.5),
          legend.title = element_blank(),
          ) +
                 labs(x = "Total Length (cm)", y = "Probability Density") +
                 scale_fill_manual(values=c(pre, post50, post100)) +
                 geom_vline(aes(xintercept=baseline$genesee$theoryMin %>% mean), color=preL, linetype="dashed", size=0.75) +
                 geom_vline(aes(xintercept=future$genesee[future$genesee$year %in% 2040:2050,"theoryMin"] %>% mean), color=post50L, linetype="dashed", size=0.75) +
                 geom_vline(aes(xintercept=future$genesee[future$genesee$year %in% 2090:2099,"theoryMin"] %>% mean), color=post100L, linetype="dashed", size=0.75) +
                 geom_segment(aes(x=41.2, xend=41.2, y=0.06,  yend=.07), color=preL, linetype="dashed", size=0.55) +
                 geom_segment(aes(x=41.2, xend=41.2, y=0.048, yend=.059),  color=post50L, linetype="dashed", size=0.55) +
                 geom_segment(aes(x=41.2, xend=41.2, y=0.037,  yend=.048), color=post100L, linetype="dashed", size=0.55)
# 225, 1495
louP <- ggplot(stlouis,    aes(x=size, fill=factor(type, levels = c("Baseline","2050","2100")))) +
    # middle chunk same for everyone
    geom_density(alpha=0.6, color=NA, show.legend = F) +
    xlim(-4,53) + ylim(0,0.11) + 
    #xlim(-4,53) + ylim(0,0.58) + # to match nip
    # ylim(0,0.60)
    theme_classic() + 
    theme(text = element_text(size=12),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
                 labs(x = "Total Length (cm)", y = "Probability Density") +
                 scale_fill_manual(values=c(pre, post50, post100)) +
                 geom_vline(aes(xintercept=baseline$stlouis$theoryMin %>% mean), color=preL, linetype="dashed", size=0.75) +
                 geom_vline(aes(xintercept=future$stlouis[future$stlouis$year %in% 2040:2050,"theoryMin"] %>% mean), color=post50L, linetype="dashed", size=0.75) +
                 geom_vline(aes(xintercept=future$stlouis[future$stlouis$year %in% 2090:2099,"theoryMin"] %>% mean), color=post100L, linetype="dashed", size=0.75)
# 326, 2075
vermP <- ggplot(vermillion,    aes(x=size, fill=factor(type, levels = c("Baseline","2050","2100")))) +
    # middle chunk same for everyone
    geom_density(alpha=0.6, color=NA, show.legend = F) +
    xlim(-4,53) + ylim(0,0.11) + 
    #xlim(-4,53) + ylim(0,0.58) + # to match nip
    theme_classic() + 
    theme(text = element_text(size=12), 
          #axis.text.y = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()
          ) +
                 labs(x = "Total Length (cm)", y = "Probability Density") +
                 scale_fill_manual(values=c(pre, post50, post100)) +
                 geom_vline(aes(xintercept=baseline$vermillion$theoryMin %>% mean), color=preL, linetype="dashed", size=0.75) +
                 geom_vline(aes(xintercept=future$vermillion[future$vermillion$year %in% 2040:2050,"theoryMin"] %>% mean), color=post50L, linetype="dashed", size=0.75) +
                 geom_vline(aes(xintercept=future$vermillion[future$vermillion$year %in% 2090:2099,"theoryMin"] %>% mean), color=post100L, linetype="dashed", size=0.75)
#
# common axis labels for multiplot
y.grob <- textGrob("Probability Density", 
                   gp=gpar(col="black", fontsize=15), rot=90)
#
x.grob <- textGrob("Total Length (cm)", vjust = 0.15,
                   gp=gpar(col="black", fontsize=15))
#
# all 4 together
all4 <- plot_grid(nipP, genP, louP, vermP, 
                  labels = c("Nipigon","Genesee","St. Louis","Vermillion"),
                  label_size = 14,
                  label_fontface = "plain",
                  label_x = .65, 
                  label_y = .9, 
                  align="v", 
                  nrow = 2)
#
outPlot <- grid.arrange(arrangeGrob(all4, left = y.grob, bottom = x.grob))
#
# save plot
ggsave(plot = outPlot, filename = "all4.png", path = "../graphics", bg = "white", width = 9, height = 6)


# as EPS
#ggsave(plot = outPlot, filename = "all4.eps", path = "../graphics", device = cairo_ps, bg = "white", width = 9, height = 6, fallback_resolution = 600)


#  ---- Spawn date and last effective spawn density plot -----------------------

nipP <- ggplot(nipigon,    aes(x=bday, fill=factor(type, levels = c("Baseline","2050","2100")))) +
    # middle chunk same for everyone
    geom_density(alpha=0.6, color=NA, show.legend = T) +
    xlim(1,365) + ylim(0,0.04) + 
    scale_x_continuous(limits = c(0,365), breaks = c(1,53,105,157,209,261,313,365)) +
    #scale_y_continuous(labels = function(x) format(x, nsmall = 2), limits = c(0,.6) ) + # force 2 decimals
    theme_classic() + 
        theme(text = element_text(size=12), 
              #axis.text.x = element_text(size = 20),
              axis.text.x = element_blank(),
              #axis.ticks.x = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              legend.position = c(0.90, 0.5),
              legend.title = element_blank(),
              ) + labs(x = "Ordinal spawn date", y = "Probability Density") +
                 scale_fill_manual(values=c(pre, post50, post100)) +
                 geom_vline(aes(xintercept=baseline$nipigon$lastBday %>% mean), color=preL, linetype="dashed", size=0.75) +
                 geom_vline(aes(xintercept=future$nipigon[future$nipigon$year %in% 2040:2050,"lastBday"] %>% mean), color=post50L, linetype="dashed", size=0.75) +
                 geom_vline(aes(xintercept=future$nipigon[future$nipigon$year %in% 2090:2099,"lastBday"] %>% mean), color=post100L, linetype="dashed", size=0.75) +
                 guides(color = guide_legend(override.aes = list(size = 0.5))) +
                 geom_segment(aes(x=303, xend=303, y=0.022,  yend=.0266), color=preL, linetype="dashed", size=0.5) +
                 geom_segment(aes(x=303, xend=303, y=0.017, yend=.021),  color=post50L, linetype="dashed", size=0.5) +
                 geom_segment(aes(x=303, xend=303, y=0.0115,  yend=.0165), color=post100L, linetype="dashed", size=0.5)
#
genP <- ggplot(genesee,    aes(x=bday, fill=factor(type, levels = c("Baseline","2050","2100")))) +
    # middle chunk same for everyone
    geom_density(alpha=0.6, color=NA, show.legend = F) +
    #xlim(1,365) + ylim(0,0.015) + 
    xlim(1,365) + ylim(0,0.015) + 
    scale_x_continuous(limits = c(0,365), breaks = c(1,53,105,157,209,261,313,365)) +
    theme_classic() + 
    theme(text = element_text(size=12), 
          #axis.text.x = element_text(size = 20),
          #axis.text.y = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()
          ) +
                 labs(x = "Ordinal spawn date", y = "Probability Density") +
                 scale_fill_manual(values=c(pre, post50, post100)) +
                 geom_vline(aes(xintercept=baseline$genesee$lastBday %>% mean), color=preL, linetype="dashed", size=0.75) +
                 geom_vline(aes(xintercept=future$genesee[future$genesee$year %in% 2040:2050,"lastBday"] %>% mean), color=post50L, linetype="dashed", size=0.75) +
                 geom_vline(aes(xintercept=future$genesee[future$genesee$year %in% 2090:2099,"lastBday"] %>% mean), color=post100L, linetype="dashed", size=0.75)
#
louP <- ggplot(stlouis,    aes(x=bday, fill=factor(type, levels = c("Baseline","2050","2100")))) +
    # middle chunk same for everyone
    geom_density(alpha=0.6, color=NA, show.legend = F) +
    #xlim(0,365) + ylim(0,0.015) + 
    xlim(1,365) + ylim(0,0.015) + 
    scale_x_continuous(limits = c(0,365), breaks = c(1,53,105,157,209,261,313,365)) +
    theme_classic() + 
    theme(text = element_text(size=12),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_blank()) +
                 labs(x = "Ordinal spawn date", y = "Probability Density") +
                 scale_fill_manual(values=c(pre, post50, post100)) +
                 geom_vline(aes(xintercept=baseline$stlouis$lastBday %>% mean), color=preL, linetype="dashed", size=0.75) +
                 geom_vline(aes(xintercept=future$stlouis[future$stlouis$year %in% 2040:2050,"lastBday"] %>% mean), color=post50L, linetype="dashed", size=0.75) +
                 geom_vline(aes(xintercept=future$stlouis[future$stlouis$year %in% 2090:2099,"lastBday"] %>% mean), color=post100L, linetype="dashed", size=0.75)
#
vermP <- ggplot(vermillion,    aes(x=bday, fill=factor(type, levels = c("Baseline","2050","2100")))) +
    # middle chunk same for everyone
    geom_density(alpha=0.6, color=NA, show.legend = F) +
    #xlim(0,365) + ylim(0,0.015) + 
    xlim(1,365) + ylim(0,0.015) + 
    scale_x_continuous(limits = c(0,365), breaks = c(1,53,105,157,209,261,313,365)) +
    theme_classic() + 
    theme(text = element_text(size=12), 
          #axis.text.y = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()
          ) +
                 labs(x = "Ordinal spawn date", y = "Probability Density") +
                 scale_fill_manual(values=c(pre, post50, post100)) +
                 geom_vline(aes(xintercept=baseline$vermillion$lastBday %>% mean), color=preL, linetype="dashed", size=0.75) +
                 geom_vline(aes(xintercept=future$vermillion[future$vermillion$year %in% 2040:2050,"lastBday"] %>% mean), color=post50L, linetype="dashed", size=0.75) +
                 geom_vline(aes(xintercept=future$vermillion[future$vermillion$year %in% 2090:2099,"lastBday"] %>% mean), color=post100L, linetype="dashed", size=0.75)
#
#
# common axis labels for multiplot
y.grob <- textGrob("Probability Density", 
                   gp=gpar(col="black", fontsize=15), rot=90)
#
x.grob <- textGrob("Ordinal spawn date", vjust = 0.15,
                   gp=gpar(col="black", fontsize=15))
#
# all 4 together
all4 <- plot_grid(nipP, louP, genP, vermP, 
                  labels = c("Nipigon","St. Louis","Genesee","Vermillion"),
                  label_size = 14,
                  label_fontface = "plain",
                  label_x = .75, 
                  label_y = .9, 
                  align="v", 
                  nrow = 4)
#
outPlot <- grid.arrange(arrangeGrob(all4, left = y.grob, bottom = x.grob))
#
# save plot
ggsave(plot = outPlot, filename = "allBday.png", path = "../graphics", bg = "white", width = 6, height = 9)

# as EPS
#ggsave(plot = outPlot, filename = "allBday.eps", path = "../graphics", device = cairo_ps, bg = "white", width = 6, height = 9, fallback_resolution = 600)


#  ---- Percent cohort survival plot --------------------------------

# arrange survival stats for plotting
perSpaSurv <- rbind(
                    cbind(percent = future[[2]][["percentSpawnSurvive"]], year = 2015:2099, type = "Nipigon"),
                    cbind(percent = future[[3]][["percentSpawnSurvive"]], year = 2025:2099, type = "St. Louis"),
                    cbind(percent = future[[1]][["percentSpawnSurvive"]], year = 2025:2099, type = "Genesee"),
                    cbind(percent = future[[4]][["percentSpawnSurvive"]], year = 2025:2099, type = "Vermillion"),
                    # add in baseline values
                    cbind(percent = baseline$nipigon$percentSpawnSurvive, year = 2000:2008, type = "Nipigon"),
                    cbind(percent = baseline$stlouis$percentSpawnSurvive, year = 2012:2024, type = "St. Louis"),
                    cbind(percent = baseline$genesee$percentSpawnSurvive, year = 2011:2024, type = "Genesee"),
                    cbind(percent = baseline$vermillion$percentSpawnSurvive, year = 2012:2024, type = "Vermillion")
                    ) %>% as.data.frame

# coerce data back to numeric type 
perSpaSurv[,1] <- perSpaSurv[,1] %>% as.numeric
perSpaSurv[,2] <- perSpaSurv[,2] %>% as.numeric

# Set the order of types so the legend, and color scheme, goes north to south
perSpaSurv$type <- factor(perSpaSurv$type, levels = c("Nipigon", "St. Louis", "Genesee", "Vermillion"))

# scatterplot of trends with regression lines
surPlot <- ggplot(perSpaSurv, aes(x = year, y = percent, color = type, shape = type)) +
           geom_point(show.legend = T) +
           ylim(0,.91) +
           scale_fill_discrete(breaks = c("Nipigon","St. Louis", "Genesee", "Vermillion")) +
           scale_shape_manual(values=c(16,15,18,17)) +
           scale_color_viridis(option = "C", begin = 0.01, end = 0.8, discrete = T) +
           # this line has the exact color codes, if needed
           #scale_color_manual(values = rev(c("#FFAC5A", "#D9456E", "#8A00A2", "#101186"))) +
           geom_smooth(method = "lm", se=F, show.legend = F, linewidth = 0.5, linetype = 1) +
           theme_classic() + 
           theme(axis.text.x = element_text(size = 9),
                 axis.text.y = element_text(size = 9),
                 axis.title.x = element_text(size = 13),
                 axis.title.y = element_text(size = 13, vjust = 2),
                 legend.position = c(0.85, 0.3),
                 legend.title = element_blank(),
                 legend.text = element_text(size = 10)
                 ) +
           labs(x = "Year", y = "Effective spawning proportion") +
           #geom_vline(aes(xintercept=2024.5), color="gray45", linetype="dashed", size=0.45, ) +
           geom_segment(aes(x=2024.5, xend=2024.5, y=0.34, yend=.9), color="gray45", linetype="dashed", size=0.45) +
           geom_segment(aes(x=2009, xend=2009, y=0, yend=.22), color="gray45", linetype="dashed", size=0.45) +
           geom_segment(aes(x=2009, xend=2024.5, y=0.22, yend=.34), color="gray45", linetype="dashed", size=0.45) +
           guides(color = guide_legend(override.aes = list(size = 3))) +
           annotate("text", x = 2010.5, y = .89, label ="Baseline period", size = 3, color = "gray45")
          
# save plot
ggsave(plot = surPlot, filename = "allSur4.png", path = "../graphics", bg = "white", width = 5, height = 6)

# as eps
#ggsave(plot = surPlot, filename = "allSur4.eps", path = "../graphics", device = cairo_ps, bg = "white", width = 5, height = 6, fallback_resolution = 600)











#  ---- ALTERNATE: EACH INDIVIDUAL GCM VERSION of "Percent cohort survival plot" --------------------------------

# NOTE: not really any good
 
# extract individual GCM survival percentage values
all.nip <- nipSummary %>% lapply(.,function(x){data.frame(year = 2015:2099, percent= x$percentSpawnSurvive)}) 
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

# arrange survival stats for plotting
perSpaSurv2 <- rbind(all.nip, all.slou, all.gen, all.verm, 
                     # and baseline values
                     cbind(percent = baseline$nipigon$percentSpawnSurvive, year = 2000:2008, type = "Nipigon"),
                     cbind(percent = baseline$stlouis$percentSpawnSurvive, year = 2012:2024, type = "St. Louis"),
                     cbind(percent = baseline$genesee$percentSpawnSurvive, year = 2011:2024, type = "Genesee"),
                     cbind(percent = baseline$vermillion$percentSpawnSurvive, year = 2012:2024, type = "Vermillion")
                     ) %>% as.data.frame


# coerce data back to numeric type 
perSpaSurv2[,1] <- perSpaSurv2[,1] %>% as.numeric
perSpaSurv2[,2] <- perSpaSurv2[,2] %>% as.numeric

# Set the order of types so the legend, and color scheme, goes north to south
perSpaSurv2$type <- factor(perSpaSurv2$type, levels = c("Nipigon", "St. Louis", "Genesee", "Vermillion"))

# scatterplot of trends with regression lines
surPlot2 <- ggplot(perSpaSurv2, aes(x = year, y = percent, color = type, shape = type)) +
           geom_point(show.legend = T) +
           ylim(0,.91) +
           scale_fill_discrete(breaks = c("Nipigon","St. Louis", "Genesee", "Vermillion")) +
           scale_shape_manual(values=c(16,15,18,17)) +
           scale_color_viridis(option = "C", begin = 0.01, end = 0.8, discrete = T) +
           # this line has the exact color codes, if needed
           #scale_color_manual(values = rev(c("#FFAC5A", "#D9456E", "#8A00A2", "#101186"))) +
           geom_smooth(method = "lm", se=F, show.legend = F, linewidth = 0.5, linetype = 1) +
           theme_classic() + 
           theme(axis.text.x = element_text(size = 9),
                 axis.text.y = element_text(size = 9),
                 axis.title.x = element_text(size = 13),
                 axis.title.y = element_text(size = 13, vjust = 2),
                 legend.position = c(0.85, 0.3),
                 legend.title = element_blank(),
                 legend.text = element_text(size = 10)
                 ) +
           labs(x = "Year", y = "Effective spawning proportion") +
           #geom_vline(aes(xintercept=2024.5), color="gray45", linetype="dashed", size=0.45, ) +
           geom_segment(aes(x=2024.5, xend=2024.5, y=0.34, yend=.9), color="gray45", linetype="dashed", size=0.45) +
           geom_segment(aes(x=2009, xend=2009, y=0, yend=.22), color="gray45", linetype="dashed", size=0.45) +
           geom_segment(aes(x=2009, xend=2024.5, y=0.22, yend=.34), color="gray45", linetype="dashed", size=0.45) +
           guides(color = guide_legend(override.aes = list(size = 3))) +
           annotate("text", x = 2010.5, y = .89, label ="Baseline period", size = 3, color = "gray45")




