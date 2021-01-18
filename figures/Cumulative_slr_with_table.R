library(ggplot2)
library(scales)
library(dplyr)
library(reshape2)
library(gridExtra)
library(gtable)

#
# read the result data
#
data <- read.csv("../results/cls_slrrates_2015.csv")

#
# split up into high slr estimate data and low slr estimate data
#
data_high <- data[,c("locationid","rslr_high","length","pop_below_10p0")]
data_low <- data[,c("locationid","rslr_low","length","pop_below_10p0")]

#
# do some reasonable renaming
#
names(data_high)[names(data_high)=="rslr_high"] <- "rslr"
names(data_low)[names(data_low)=="rslr_low"] <- "rslr"

#
# print the extremes
#
print(min(data_high$rslr))
print(max(data_high$rslr))
print(min(data_low$rslr))
print(max(data_low$rslr))

#
# compute cummulative values of length/pop - rlsr for high estimate dataset
#
data_high  <- data_high[with(data_high, order(data_high$rslr)), ]
data_high  <- data_high %>% mutate(length_sum = cumsum(length), pop_sum = cumsum(pop_below_10p0))
data_high$index  <- 1:nrow(data)
data_high$index_rel <- data_high$index * 100 / max(data_high$index)
data_high$length_rel <- data_high$length_sum * 100 / max(data_high$length_sum)
data_high$pop_rel <- data_high$pop_sum * 100 / max(data_high$pop_sum)
data_high$subsidence <- "upper estimate"

#
# Print some statistics
#
data_high_neg_slr <- data_high[data_high$rslr<0,]
print("negative SLR (high subsidence estimates):")
print("segments:")
print(nrow(data_high_neg_slr))
print("length:") 
print(data_high_neg_slr[nrow(data_high_neg_slr):nrow(data_high_neg_slr),]$length_sum)
print(data_high_neg_slr[nrow(data_high_neg_slr):nrow(data_high_neg_slr),]$length_rel)
print("population:") 
print(data_high_neg_slr[nrow(data_high_neg_slr):nrow(data_high_neg_slr),]$pop_sum)
print(sum(data_high_neg_slr$pop_below_10p0))
print(data_high_neg_slr[nrow(data_high_neg_slr):nrow(data_high_neg_slr),]$pop_rel)

print(">= 10mm/yr - low city subsidence")
print("================================")
data_high_catstrophic_slr <- data_high[data_high$rslr>=10,]
print("segments:")
print(nrow(data_high_catstrophic_slr))
print("length:") 
print(sum(data_high_catstrophic_slr[1:nrow(data_high_catstrophic_slr),]$length))
print(sum(data_high_catstrophic_slr[1:nrow(data_high_catstrophic_slr),]$length)*100/sum(data_high$length))
print("population:") 
print(sum(data_high_catstrophic_slr[1:nrow(data_high_catstrophic_slr),]$pop_below_10p0))
print(sum(data_high_catstrophic_slr[1:nrow(data_high_catstrophic_slr),]$pop_below_10p0)*100/sum(data_high$pop_below_10p0))


#
# compute cummulative values of length/pop - rlsr for low estimate dataset
#
data_low  <- data_low[with(data_low, order(data_low$rslr)), ]
data_low  <- data_low %>% mutate(length_sum = cumsum(length), pop_sum = cumsum(pop_below_10p0))
data_low$index  <- 1:nrow(data)
data_low$index_rel <- data_low$index * 100 / max(data_low$index)
data_low$length_rel <- data_low$length_sum * 100 / max(data_low$length_sum)
data_low$pop_rel <- data_low$pop_sum * 100 / max(data_low$pop_sum)
data_low$subsidence <- "lower estimate"

print(">= 10mm/yr - high city subsidence")
print("=================================")
data_low_catstrophic_slr <- data_low[data_low$rslr>=10,]
print("segments:")
print(nrow(data_low_catstrophic_slr))
print("length:") 
print(sum(data_low_catstrophic_slr[1:nrow(data_low_catstrophic_slr),]$length))
print(sum(data_low_catstrophic_slr[1:nrow(data_low_catstrophic_slr),]$length)*100/sum(data_low$length))
print("population:") 
print(sum(data_low_catstrophic_slr[1:nrow(data_low_catstrophic_slr),]$pop_below_10p0))
print(sum(data_low_catstrophic_slr[1:nrow(data_low_catstrophic_slr),]$pop_below_10p0)*100/sum(data_low$pop_below_10p0))

#
# re-unite the data and do renaming for the legend
#
data <- rbind(data_low,data_high)
names(data)[names(data)=="length_rel"] <- "Length-weighted coastal relative sea level"
names(data)[names(data)=="pop_rel"] <- "Population-weighted coastal relative sea level"

#
# reorganise data in a order to plot rslr vs. coastal lenght and rslr vs. coastal population in different colors
# and throw out the ones we don't use
#
data <- melt(data, id=c("locationid","subsidence","rslr","length","pop_below_10p0")) 
data <- data[data$variable!="length_sum",]
data <- data[data$variable!="index",]
data <- data[data$variable!="index_rel",]
data <- data[data$variable!="pop_sum",]

#
# plot
#
rsbPalette  <- c(rgb(255/255, 0/255, 0/255), rgb(0/255, 0/255, 255/255))
rsbPaletteGreyFriendly  <- c(rgb(255/255, 0/255, 0/255), rgb(50/255, 140/255, 255/255))

table1_data <- read.csv("../tables/table1.csv")
print(table1_data)
table1_data$sum_length_low <- round(table1_data$satellite...length,1) + round(table1_data$uplift...length,1) + round(table1_data$delta...length,1) + round(table1_data$city..low....length,1)
table1_data$sum_length_high <- round(table1_data$satellite...length,1) + round(table1_data$uplift...length,1) + round(table1_data$delta...length,1) + round(table1_data$city..high....length,1)
city_length_string <- if (round(table1_data$city..low....length,1)==round(table1_data$city..high....length,1)) as.character(round(table1_data$city..low....length,1)) else paste(as.character(round(table1_data$city..low....length,1)),as.character(round(table1_data$city..high....length,1)),sep=" to ") 
sum_length_string <- if (round(table1_data$sum_length_low,1)==round(table1_data$sum_length_high,1)) as.character(round(table1_data$sum_length_low,1)) else paste(as.character(round(table1_data$sum_length_low,1)),as.character(round(table1_data$sum_length_high,1)),sep=" to ") 

table1_data$sum_pop_low <- round(table1_data$satellite...pop,1) + round(table1_data$uplift...pop,1) + round(table1_data$delta...pop,1) + round(table1_data$city..low....pop,1)
table1_data$sum_pop_high <- round(table1_data$satellite...pop,1) + round(table1_data$uplift...pop,1) + round(table1_data$delta...pop,1) + round(table1_data$city..high....pop,1)
city_pop_string <- if (round(table1_data$city..low....pop,1)==round(table1_data$city..high....pop,1)) as.character(round(table1_data$city..low....pop,1)) else paste(as.character(round(table1_data$city..low....pop,1)),as.character(round(table1_data$city..high....pop,1)),sep=" to ") 
sum_pop_string <- if (round(table1_data$sum_pop_low,1)==round(table1_data$sum_pop_high,1)) as.character(round(table1_data$sum_pop_low,1)) else paste(as.character(round(table1_data$sum_pop_low,1)),as.character(round(table1_data$sum_pop_high,1)),sep=" to ") 

satellite_length_perc <- if (round(table1_data$sum_length_low,1)==round(table1_data$sum_length_high,1)) as.character(round((table1_data$satellite...length * 100)/table1_data$sum_length_low)) else paste(as.character(round((table1_data$satellite...length * 100)/table1_data$sum_length_low)),as.character(round((table1_data$satellite...length * 100)/table1_data$sum_length_high)),sep=" to ") 
uplift_length_perc <- if (round(table1_data$sum_length_low,1)==round(table1_data$sum_length_high,1)) as.character(round((table1_data$uplift...length * 100)/table1_data$sum_length_low)) else paste(as.character(round((table1_data$uplift...length * 100)/table1_data$sum_length_low)),as.character(round((table1_data$uplift...length * 100)/table1_data$sum_length_high)),sep=" to ") 
delta_length_perc <- if (round(table1_data$sum_length_low,1)==round(table1_data$sum_length_high,1)) as.character(round((table1_data$delta...length * 100)/table1_data$sum_length_low)) else paste(as.character(round((table1_data$delta...length * 100)/table1_data$sum_length_low)),as.character(round((table1_data$delta...length * 100)/table1_data$sum_length_high)),sep=" to ") 
city_length_perc <- if (round(table1_data$city..low....length,1)==round(table1_data$city..high....length,1)) as.character(round((table1_data$city..low....length * 100)/table1_data$sum_length_low)) else paste(as.character(round((table1_data$city..low....length * 100)/table1_data$sum_length_low)),as.character(round((table1_data$city..high....length * 100)/table1_data$sum_length_high)),sep=" to ") 

satellite_pop_perc <- if (round(table1_data$sum_pop_low,1)==round(table1_data$sum_pop_high,1)) as.character(round((table1_data$satellite...pop * 100)/table1_data$sum_pop_low)) else paste(as.character(round((table1_data$satellite...pop * 100)/table1_data$sum_pop_high)),as.character(round((table1_data$satellite...pop * 100)/table1_data$sum_pop_low)),sep=" to ") 
uplift_pop_perc <- if (round((table1_data$uplift...pop * 100)/table1_data$sum_pop_high)==round((table1_data$uplift...pop * 100)/table1_data$sum_pop_low)) as.character(round((table1_data$uplift...pop * 100)/table1_data$sum_pop_low)) else paste(as.character(round((table1_data$uplift...pop * 100)/table1_data$sum_pop_high)),as.character(round((table1_data$uplift...pop * 100)/table1_data$sum_pop_low)),sep=" to ") 
delta_pop_perc <- if (round((table1_data$delta...pop * 100)/table1_data$sum_pop_high)==round((table1_data$delta...pop * 100)/table1_data$sum_pop_low)) as.character(round((table1_data$delta...pop * 100)/table1_data$sum_pop_low)) else paste(as.character(round((table1_data$delta...pop * 100)/table1_data$sum_pop_high)),as.character(round((table1_data$delta...pop * 100)/table1_data$sum_pop_low)),sep=" to ") 
city_pop_perc <- if (round(table1_data$city..low....pop,1)==round(table1_data$city..high....pop,1)) as.character(round((table1_data$city..low....pop * 100)/table1_data$sum_pop_low)) else paste(as.character(round((table1_data$city..low....pop * 100)/table1_data$sum_pop_low)),as.character(round((table1_data$city..high....pop * 100)/table1_data$sum_pop_high)),sep=" to ") 

mydata <- data.frame(
"mm/yr"=c(as.character(round(table1_data$satellite...length,1)),as.character(round(table1_data$uplift...length,1)),as.character(round(table1_data$delta...length,1)),city_length_string,sum_length_string), 
"%"=c(satellite_length_perc,uplift_length_perc,delta_length_perc,city_length_perc," "), 
"mm/yr"=c(as.character(round(table1_data$satellite...pop,1)),as.character(round(table1_data$uplift...pop,1)),as.character(round(table1_data$delta...pop,1)),city_pop_string,sum_pop_string), 
"%"=c(satellite_pop_perc,uplift_pop_perc,delta_pop_perc,city_pop_perc," "), check.names = FALSE)

mytabledata <- cbind("Relative SLR component"=c("Climate induced SLR (1993 to 2015)","Glacial Isostatic Adjustment","Delta Subsidence","City Subsidence", "Global-mean sum"),mydata)

mydatatable <- tableGrob(mydata, rows=NULL)
myfirstcolumntable <- tableGrob(data.frame("Relative SLR component"=c("","","Climate induced SLR (1993 to 2015)","Glacial Isostatic Adjustment","Delta Subsidence","City Subsidence", "Global-mean sum"), check.names = FALSE), rows=NULL)

myheader1 <- tableGrob(mytabledata[1, 1:2], rows=NULL, cols=c("Coastal length weighted", "Coastal population weighted")) 
myheader2 <- tableGrob(mytabledata[1, 1:1], rows=NULL, cols=c("Contribution to relative sea-level change")) 

mycombined_header <- gtable_combine(myheader2[1,], myheader1[1,], along=2)
mycombined_table <- gtable_combine(mycombined_header[1:2,], mydatatable, along=2)

mycombined_table$layout[1:2, c("l","r")] <- list(c(1), c(4))
mycombined_table$layout[3, c("l","r")] <- list(c(1), c(2))
mycombined_table$layout[4, c("l","r")] <- list(c(3), c(4))
mycombined_table$layout[5, c("l","r")] <- list(c(1), c(2))
mycombined_table$layout[6, c("l","r")] <- list(c(3), c(4))
mycombined_table$widths <- rep(max(mydatatable$widths)*1.4, length(mycombined_table$widths))


ggplot(data=data) +
    geom_path(aes(y = value, x = rslr, color=variable, linetype=subsidence),size=1.5) +
    xlab("Annual relative change [mm/yr]") + 
    ylab("Cummulative frequency [%]") + theme_bw(20) + 
    scale_y_continuous(expand=c(0,0),breaks=c(0,25,50,75,100),limits=c(0,100),label=comma) +
    scale_x_continuous(expand=c(0,0),breaks=c(-10,0,10,100),limits=c(-11,110),label=comma) +
    theme(plot.margin = unit(c(0.5,1,0.5,0.5), "cm")) +
    scale_colour_manual(values=rsbPaletteGreyFriendly, guide = guide_legend(title = "", override.aes = list(size = 10), order = 0)) +
    theme(legend.position="bottom", legend.box = "vertical", legend.title= element_blank(), legend.spacing = unit(-0.3,"cm")) + 
    annotation_custom(mycombined_table, xmin=48, xmax=76, ymin=39, ymax=39.5) +
    annotation_custom(myfirstcolumntable, xmin=30.7, xmax=30.5, ymin=39, ymax=39.5) +
    annotate("text", x = 19, y = 12, label = "The contemporary global average components of relative sea-level rise around the world's coasts in terms\nof absolute and relative contribution. The averages are weighted by coastal length and coastal population (based\non the Low Elevation Coastal Zone (LECZ below 10m)). A positive value indicates a relative rise in sea level.", hjust = 0)
ggsave("./jpg/Fig1_cummulative_slr_with_table.jpg", width = 16, height = 8, dpi = 400)

ggplot(data=data) +
    geom_path(aes(y = value, x = rslr, color=variable, linetype=subsidence),size=1.5) +
    xlab("Annual relative change [mm/yr]") + 
    ylab("Cummulative frequency [%]") + theme_bw(20) + 
    scale_y_continuous(expand=c(0,0),breaks=c(0,25,50,75,100),limits=c(0,100),label=comma) +
    scale_x_continuous(expand=c(0,0),breaks=c(-10,0,10,100),limits=c(-11,110),label=comma) +
    theme(plot.margin = unit(c(0.5,1,0.5,0.5), "cm")) +
    scale_colour_manual(values=rsbPaletteGreyFriendly, guide = guide_legend(title = "", override.aes = list(size = 10), order = 0)) +
    theme(legend.position="bottom", legend.box = "vertical", legend.title= element_blank(), legend.spacing = unit(-0.3,"cm")) + 
    annotation_custom(mycombined_table, xmin=71, xmax=76, ymin=39, ymax=39.5) +
    annotation_custom(myfirstcolumntable, xmin=30.7, xmax=30.5, ymin=39, ymax=39.5) +
    annotate("text", x = 15, y = 12, label = "The contemporary global average components of relative sea-level rise around the world's coasts in terms\nof absolute and relative contribution. The averages are weighted by coastal length and coastal population (based\non the Low Elevation Coastal Zone (LECZ below 10m)). A positive value indicates a relative rise in sea level.", hjust = 0)
ggsave("./eps/Fig1_cummulative_slr_with_table.eps", width = 12, height = 9)
write.csv(data,"./source_data/Fig1_cummulative_slr_source_data.csv",row.names=F)


ggplot(data=data) +
    geom_path(aes(y = value, x = rslr, color=variable, linetype=subsidence),size=1.5) +
    xlab("Annual relative change [mm/yr]") + 
    ylab("Cummulative frequency [%]") + theme_bw(20) + 
    scale_y_continuous(expand=c(0,0),breaks=c(0,25,50,75,100),limits=c(0,100),label=comma) +
    scale_x_continuous(expand=c(0,0),breaks=c(-10,0,10,100),limits=c(-11,110),label=comma) +
    theme(plot.margin = unit(c(0.5,1,0.5,0.5), "cm")) +
    scale_colour_manual(values=rsbPaletteGreyFriendly, guide = guide_legend(title = "", override.aes = list(size = 10), order = 0)) +
    theme(legend.position="bottom", legend.box = "vertical", legend.title= element_blank(), legend.spacing = unit(-0.3,"cm"))

ggsave("./jpg/Fig1_cummulative_slr.jpg", width = 12, height = 9, dpi = 600)
ggsave("./eps/Fig1_cummulative_slr.eps", width = 12, height = 9)

df <- data.frame(x = c(0,12), y = c(5,5.5))
base <- ggplot(df, aes(x, y)) +
  geom_blank() +
  theme_void()

base + 
  annotation_custom(mycombined_table, xmin=6, xmax=11) +
  annotation_custom(myfirstcolumntable, xmin=2, xmax=5) 

ggsave("./jpg/Table1_cummulative_slr.jpg", width = 18, height = 8, dpi = 600)
ggsave("./eps/Table1_cummulative_slr.eps", width = 10)
