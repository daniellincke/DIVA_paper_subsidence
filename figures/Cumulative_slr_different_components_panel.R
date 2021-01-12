library(ggplot2)
library(scales)
library(dplyr)
library(reshape2)

#
# read the result data
#
data <- read.csv("../results/cls_slrrates_2015.csv")

data$delta <- data$deltasubsidence != 0
data$city <- data$citysubsidence_controlled_high != 0

#
# split up into high slr estimate data and low slr estimate data
#
data_high <- data[,c("locationid","length","pop_below_10p0","rslr_climate","rslr_climate_gia","rslr_climate_gia_delta","rslr_high","delta","city")]
data_low <- data[,c("locationid","length","pop_below_10p0","rslr_climate","rslr_climate_gia","rslr_climate_gia_delta","rslr_low","delta","city")]

#
# do some reasonable renaming
#
names(data_high)[names(data_high)=="rslr_high"] <- "rslr_climate_gia_delta_city"
names(data_low)[names(data_low)=="rslr_low"] <- "rslr_climate_gia_delta_city"

#
# compute cummulative values of length/pop - rlsr for high estimate dataset
#
data_high  <- data_high[with(data_high, order(data_high$rslr_climate)), ]
data_high  <- data_high %>% mutate(length_sum_rslr_climate = cumsum(length), pop_sum_rslr_climate = cumsum(pop_below_10p0))
data_high$index_rslr_climate  <- 1:nrow(data)
data_high$index_rel_rslr_climate <- data_high$index_rslr_climate * 100 / max(data_high$index_rslr_climate)
data_high$length_rel_rslr_climate <- data_high$length_sum_rslr_climate * 100 / max(data_high$length_sum_rslr_climate)
data_high$pop_rel_rslr_climate <- data_high$pop_sum_rslr_climate * 100 / max(data_high$pop_sum_rslr_climate)
#
# climate+gia
#
data_high  <- data_high[with(data_high, order(data_high$rslr_climate_gia)), ]
data_high  <- data_high %>% mutate(length_sum_rslr_climate_gia = cumsum(length), pop_sum_rslr_climate_gia = cumsum(pop_below_10p0))
data_high$index_rslr_climate_gia  <- 1:nrow(data)
data_high$index_rel_rslr_climate_gia <- data_high$index_rslr_climate_gia * 100 / max(data_high$index_rslr_climate_gia)
data_high$length_rel_rslr_climate_gia <- data_high$length_sum_rslr_climate_gia * 100 / max(data_high$length_sum_rslr_climate_gia)
data_high$pop_rel_rslr_climate_gia <- data_high$pop_sum_rslr_climate_gia * 100 / max(data_high$pop_sum_rslr_climate_gia)
#
# climate+gia+delta
#
data_high  <- data_high[with(data_high, order(data_high$rslr_climate_gia_delta)), ]
data_high  <- data_high %>% mutate(length_sum_rslr_climate_gia_delta = cumsum(length), pop_sum_rslr_climate_gia_delta = cumsum(pop_below_10p0))
data_high$index_rslr_climate_gia_delta  <- 1:nrow(data)
data_high$index_rel_rslr_climate_gia_delta <- data_high$index_rslr_climate_gia_delta * 100 / max(data_high$index_rslr_climate_gia_delta)
data_high$length_rel_rslr_climate_gia_delta <- data_high$length_sum_rslr_climate_gia_delta * 100 / max(data_high$length_sum_rslr_climate_gia_delta)
data_high$pop_rel_rslr_climate_gia_delta <- data_high$pop_sum_rslr_climate_gia_delta * 100 / max(data_high$pop_sum_rslr_climate_gia_delta)
data_high$subsidence <- "upper estimate"
#
# climate+gia+delta+city
#
data_high  <- data_high[with(data_high, order(data_high$rslr_climate_gia_delta_city)), ]
data_high  <- data_high %>% mutate(length_sum_rslr_climate_gia_delta_city = cumsum(length), pop_sum_rslr_climate_gia_delta_city = cumsum(pop_below_10p0))
data_high$index_rslr_climate_gia_delta_city  <- 1:nrow(data)
data_high$index_rel_rslr_climate_gia_delta_city <- data_high$index_rslr_climate_gia_delta_city * 100 / max(data_high$index_rslr_climate_gia_delta_city)
data_high$length_rel_rslr_climate_gia_delta_city <- data_high$length_sum_rslr_climate_gia_delta_city * 100 / max(data_high$length_sum_rslr_climate_gia_delta_city)
data_high$pop_rel_rslr_climate_gia_delta_city <- data_high$pop_sum_rslr_climate_gia_delta_city * 100 / max(data_high$pop_sum_rslr_climate_gia_delta_city)
data_high$subsidence <- "upper estimate"

#
# compute cummulative values of length/pop - rlsr for low estimate dataset
#
data_low  <- data_low[with(data_low, order(data_low$rslr_climate)), ]
data_low  <- data_low %>% mutate(length_sum_rslr_climate = cumsum(length), pop_sum_rslr_climate = cumsum(pop_below_10p0))
data_low$index_rslr_climate  <- 1:nrow(data)
data_low$index_rel_rslr_climate <- data_low$index_rslr_climate * 100 / max(data_low$index_rslr_climate)
data_low$length_rel_rslr_climate <- data_low$length_sum_rslr_climate * 100 / max(data_low$length_sum_rslr_climate)
data_low$pop_rel_rslr_climate <- data_low$pop_sum_rslr_climate * 100 / max(data_low$pop_sum_rslr_climate)
#
# climate+gia
#
data_low  <- data_low[with(data_low, order(data_low$rslr_climate_gia)), ]
data_low  <- data_low %>% mutate(length_sum_rslr_climate_gia = cumsum(length), pop_sum_rslr_climate_gia = cumsum(pop_below_10p0))
data_low$index_rslr_climate_gia  <- 1:nrow(data)
data_low$index_rel_rslr_climate_gia <- data_low$index_rslr_climate_gia * 100 / max(data_low$index_rslr_climate_gia)
data_low$length_rel_rslr_climate_gia <- data_low$length_sum_rslr_climate_gia * 100 / max(data_low$length_sum_rslr_climate_gia)
data_low$pop_rel_rslr_climate_gia <- data_low$pop_sum_rslr_climate_gia * 100 / max(data_low$pop_sum_rslr_climate_gia)
#
# climate+gia+delta
#
data_low  <- data_low[with(data_low, order(data_low$rslr_climate_gia_delta)), ]
data_low  <- data_low %>% mutate(length_sum_rslr_climate_gia_delta = cumsum(length), pop_sum_rslr_climate_gia_delta = cumsum(pop_below_10p0))
data_low$index_rslr_climate_gia_delta  <- 1:nrow(data)
data_low$index_rel_rslr_climate_gia_delta <- data_low$index_rslr_climate_gia_delta * 100 / max(data_low$index_rslr_climate_gia_delta)
data_low$length_rel_rslr_climate_gia_delta <- data_low$length_sum_rslr_climate_gia_delta * 100 / max(data_low$length_sum_rslr_climate_gia_delta)
data_low$pop_rel_rslr_climate_gia_delta <- data_low$pop_sum_rslr_climate_gia_delta * 100 / max(data_low$pop_sum_rslr_climate_gia_delta)
#
# climate+gia+delta+city
#
data_low  <- data_low[with(data_low, order(data_low$rslr_climate_gia_delta_city)), ]
data_low  <- data_low %>% mutate(length_sum_rslr_climate_gia_delta_city = cumsum(length), pop_sum_rslr_climate_gia_delta_city = cumsum(pop_below_10p0))
data_low$index_rslr_climate_gia_delta_city  <- 1:nrow(data)
data_low$index_rel_rslr_climate_gia_delta_city <- data_low$index_rslr_climate_gia_delta_city * 100 / max(data_low$index_rslr_climate_gia_delta_city)
data_low$length_rel_rslr_climate_gia_delta_city <- data_low$length_sum_rslr_climate_gia_delta_city * 100 / max(data_low$length_sum_rslr_climate_gia_delta_city)
data_low$pop_rel_rslr_climate_gia_delta_city <- data_low$pop_sum_rslr_climate_gia_delta_city * 100 / max(data_low$pop_sum_rslr_climate_gia_delta_city)
data_low$subsidence <- "lower estimate"

#
# re-unite the data and do renaming for the legend
#
data <- rbind(data_low,data_high)
names(data)[names(data)=="length_rel"] <- "Coastal relative sea level"
names(data)[names(data)=="pop_rel"] <- "Population weighted coastal relative sea level"

#
# reorganise data in a order to plot rslr vs. coastal lenght and rslr vs. coastal population in different colors
# and throw out the ones we don't use
#
dataToPlot <- melt(data, id=c("locationid","subsidence","rslr_climate","rslr_climate_gia","rslr_climate_gia_delta","rslr_climate_gia_delta_city","delta","city")) 
dataToPlot <- dataToPlot[dataToPlot$variable!="length_sum_rslr_climate",]
dataToPlot <- dataToPlot[dataToPlot$variable!="index_rslr_climate",]
dataToPlot <- dataToPlot[dataToPlot$variable!="index_rel_rslr_climate",]
dataToPlot <- dataToPlot[dataToPlot$variable!="pop_sum_rslr_climate",]
dataToPlot <- dataToPlot[dataToPlot$variable!="length_sum_rslr_climate_gia",]
dataToPlot <- dataToPlot[dataToPlot$variable!="index_rslr_climate_gia",]
dataToPlot <- dataToPlot[dataToPlot$variable!="index_rel_rslr_climate_gia",]
dataToPlot <- dataToPlot[dataToPlot$variable!="pop_sum_rslr_climate_gia",]
dataToPlot <- dataToPlot[dataToPlot$variable!="length_sum_rslr_climate_gia_delta",]
dataToPlot <- dataToPlot[dataToPlot$variable!="index_rslr_climate_gia_delta",]
dataToPlot <- dataToPlot[dataToPlot$variable!="index_rel_rslr_climate_gia_delta",]
dataToPlot <- dataToPlot[dataToPlot$variable!="pop_sum_rslr_climate_gia_delta",]
dataToPlot <- dataToPlot[dataToPlot$variable!="length_sum_rslr_climate_gia_delta_city",]
dataToPlot <- dataToPlot[dataToPlot$variable!="index_rslr_climate_gia_delta_city",]
dataToPlot <- dataToPlot[dataToPlot$variable!="index_rel_rslr_climate_gia_delta_city",]
dataToPlot <- dataToPlot[dataToPlot$variable!="pop_sum_rslr_climate_gia_delta_city",]

names(dataToPlot)[names(dataToPlot)=="value"] <- "value_y"


dataToPlotLengthClimate <- dataToPlot[dataToPlot$variable %in% c("length_rel_rslr_climate"),]
names(dataToPlotLengthClimate)[names(dataToPlotLengthClimate)=="rslr_climate"] <- "value_x"
dataToPlotLengthClimate <- dataToPlotLengthClimate[,c("subsidence","variable","value_x","value_y","delta","city")]
dataToPlotLengthClimate$variable <- "Climate Induced SLR"
dataToPlotLengthClimate$wm <- "Coastal length-weighted"

dataToPlotLengthClimateGia <- dataToPlot[dataToPlot$variable %in% c("length_rel_rslr_climate_gia"),]
names(dataToPlotLengthClimateGia)[names(dataToPlotLengthClimateGia)=="rslr_climate_gia"] <- "value_x"
dataToPlotLengthClimateGia <- dataToPlotLengthClimateGia[,c("subsidence","variable","value_x","value_y","delta","city")]
dataToPlotLengthClimateGia$variable <- "Climate Induced SLR + GIA"
dataToPlotLengthClimateGia$wm <- "Coastal length-weighted"

dataToPlotLengthClimateGiaDelta <- dataToPlot[dataToPlot$variable %in% c("length_rel_rslr_climate_gia_delta"),]
names(dataToPlotLengthClimateGiaDelta)[names(dataToPlotLengthClimateGiaDelta)=="rslr_climate_gia_delta"] <- "value_x"
dataToPlotLengthClimateGiaDelta <- dataToPlotLengthClimateGiaDelta[,c("subsidence","variable","value_x","value_y","delta","city")]
dataToPlotLengthClimateGiaDelta$variable <- "Climate Induced SLR + GIA + Delta"
dataToPlotLengthClimateGiaDelta$wm <- "Coastal length-weighted"

dataToPlotLengthClimateGiaDeltaCity <- dataToPlot[dataToPlot$variable %in% c("length_rel_rslr_climate_gia_delta_city"),]
names(dataToPlotLengthClimateGiaDeltaCity)[names(dataToPlotLengthClimateGiaDeltaCity)=="rslr_climate_gia_delta_city"] <- "value_x"
dataToPlotLengthClimateGiaDeltaCity <- dataToPlotLengthClimateGiaDeltaCity[,c("subsidence","variable","value_x","value_y","delta","city")]
dataToPlotLengthClimateGiaDeltaCity$variable <- "Climate Induced SLR + GIA + Delta + City"
dataToPlotLengthClimateGiaDeltaCity$wm <- "Coastal length-weighted"

dataToPlotPopulationClimate <- dataToPlot[dataToPlot$variable %in% c("pop_rel_rslr_climate"),]
names(dataToPlotPopulationClimate)[names(dataToPlotPopulationClimate)=="rslr_climate"] <- "value_x"
dataToPlotPopulationClimate <- dataToPlotPopulationClimate[,c("subsidence","variable","value_x","value_y","delta","city")]
dataToPlotPopulationClimate$variable <- "Climate Induced SLR"
dataToPlotPopulationClimate$wm <- "Coastal population-weighted"

dataToPlotPopulationClimateGia <- dataToPlot[dataToPlot$variable %in% c("pop_rel_rslr_climate_gia"),]
names(dataToPlotPopulationClimateGia)[names(dataToPlotPopulationClimateGia)=="rslr_climate_gia"] <- "value_x"
dataToPlotPopulationClimateGia <- dataToPlotPopulationClimateGia[,c("subsidence","variable","value_x","value_y","delta","city")]
dataToPlotPopulationClimateGia$variable <- "Climate Induced SLR + GIA"
dataToPlotPopulationClimateGia$wm <- "Coastal population-weighted"

dataToPlotPopulationClimateGiaDelta <- dataToPlot[dataToPlot$variable %in% c("pop_rel_rslr_climate_gia_delta"),]
names(dataToPlotPopulationClimateGiaDelta)[names(dataToPlotPopulationClimateGiaDelta)=="rslr_climate_gia_delta"] <- "value_x"
dataToPlotPopulationClimateGiaDelta <- dataToPlotPopulationClimateGiaDelta[,c("subsidence","variable","value_x","value_y","delta","city")]
dataToPlotPopulationClimateGiaDelta$variable <- "Climate Induced SLR + GIA + Delta"
dataToPlotPopulationClimateGiaDelta$wm <- "Coastal population-weighted"

dataToPlotPopulationClimateGiaDeltaCity <- dataToPlot[dataToPlot$variable %in% c("pop_rel_rslr_climate_gia_delta_city"),]
names(dataToPlotPopulationClimateGiaDeltaCity)[names(dataToPlotPopulationClimateGiaDeltaCity)=="rslr_climate_gia_delta_city"] <- "value_x"
dataToPlotPopulationClimateGiaDeltaCity <- dataToPlotPopulationClimateGiaDeltaCity[,c("subsidence","variable","value_x","value_y","delta","city")]
dataToPlotPopulationClimateGiaDeltaCity$variable <- "Climate Induced SLR + GIA + Delta + City"
dataToPlotPopulationClimateGiaDeltaCity$wm <- "Coastal population-weighted"

dataToPlot <- rbind(dataToPlotLengthClimate,dataToPlotLengthClimateGia,dataToPlotLengthClimateGiaDelta,dataToPlotLengthClimateGiaDeltaCity,dataToPlotPopulationClimate,dataToPlotPopulationClimateGia,dataToPlotPopulationClimateGiaDelta,dataToPlotPopulationClimateGiaDeltaCity)

dataToPlot <- dataToPlot %>% group_by(variable,subsidence,wm) %>% arrange(value_x)
dataToPlot <- dataToPlot[order(dataToPlot$variable,dataToPlot$subsidence,dataToPlot$wm),]

dataToPlot[dataToPlot$value_x > 21.2,]$value_x <- 21.16

#
# plot
#
rsbPalette  <- c(rgb(255/255, 150/255, 150/255),rgb(50/255, 150/255, 220/255),rgb(0/255, 175/255, 0/255),rgb(0/255, 0/255, 0/255))
dataToPlot$variable <- as.factor(dataToPlot$variable)
dataToPlot$variable=factor(dataToPlot$variable,levels=rev(levels(dataToPlot$variable)))
dataToPlot$letter <- as.factor(dataToPlot$wm)
levels(dataToPlot$letter)[levels(dataToPlot$letter)=="Coastal population-weighted"] <- "(b)"
levels(dataToPlot$letter)[levels(dataToPlot$letter)=="Coastal length-weighted"] <- "(a)"

ggplot(data=dataToPlot) +
    geom_path(aes(y = value_y, x = value_x, color=variable, linetype=subsidence), size=1.0) +
    geom_text(aes(-10,95,label = letter), size=7, color="black") +
    xlab("Annual relative change [mm/yr]") + 
    ylab("Cummulative frequency [%]") + theme_bw(20) + 
    facet_grid(wm ~ .) + 
    scale_y_continuous(expand=c(0,0),breaks=c(0,25,50,75,100),limits=c(-0.5,100.5),label=comma) +
    scale_x_continuous(expand=c(0,0),breaks=c(-10,0,10,20),limits=c(-11,21.17),label=comma) +
    theme(plot.margin = unit(c(0.5,1,0.5,0.5), "cm")) +
    scale_colour_manual(values=rsbPalette, guide = guide_legend(title = "", override.aes = list(size = 10), order = 0, nrow=2,byrow=TRUE), breaks=c("Climate Induced SLR","Climate Induced SLR + GIA","Climate Induced SLR + GIA + Delta","Climate Induced SLR + GIA + Delta + City")) +
    theme(legend.position="bottom", legend.box = "vertical", legend.title= element_blank(), legend.spacing = unit(-0.3,"cm"))
ggsave("./jpg/FigS1_cummulative_slr_allcomponents.jpg", width = 9.5, height = 10, dpi = 600)
ggsave("./eps/FigS1_cummulative_slr_allcomponents.eps", width = 8.8, height = 10)


