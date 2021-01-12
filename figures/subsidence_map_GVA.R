library(ggplot2)
library(rgdal)
library(plyr)
library(maptools)
library(gridExtra)
library(colorspace)

source ("layout.r")

data <- read.csv("../results/cls_slrrates_2015.csv")
datacls <- read.csv("../input/cls_input_33.csv")
datacls <- datacls[,c("locationid","adminid")]
data <- merge(data, datacls, by=c("locationid"),all.x=T)
dataadmin <- read.csv("../input/admin_input_regions_gva.csv")
dataadmin <- dataadmin[,c("locationid","regionid")]
names(dataadmin)[names(dataadmin)=="locationid"] <- "adminid"
data <- merge(data, dataadmin, by=c("adminid"),all.x=T)

data[is.na(data)] <- 0

data$citysubsidence_controlled_high[data$citysubsidence_controlled_high < 0] <- 0
data$citysubsidence_controlled_low[data$citysubsidence_controlled_low < 0] <- 0
data$citysubsidence_uncontrolled_high[data$citysubsidence_uncontrolled_high < 0] <- 0
data$citysubsidence_uncontrolled_low[data$citysubsidence_uncontrolled_low < 0] <- 0

data$rslr_high <- data$sat_slr + data$uplift + data$deltasubsidence + data$citysubsidence_uncontrolled_high
data$rslr_low <- data$sat_slr + data$uplift + data$deltasubsidence + data$citysubsidence_uncontrolled_low
data$rslr_high_length <- data$rslr_high * data$length
data$rslr_high_pop <- data$rslr_high * data$pop_below_10p0
data$rslr_low_length <- data$rslr_low * data$length
data$rslr_low_pop <- data$rslr_low * data$pop_below_10p0
data$popdens_below_10p0 <- data$pop_below_10p0/data$area_below_10p0

gvaResults <- ddply(data, .(regionid), summarise, 
    sum.length=sum(length),
    sum.pop=sum(pop_below_10p0),
    sum.sat_slr_length = sum(sat_length), 
    sum.sat_slr_pop = sum(sat_pop),
    sum.uplift_length = sum(uplift_length), 
    sum.uplift_pop = sum(uplift_pop),
    sum.deltasubsidence_length = sum(deltasubsidence_length), 
    sum.deltasubsidence_pop = sum(deltasubsidence_pop),
    sum.citysubsidence_controlled_high_length = sum(citysubsidence_controlled_high_length), 
    sum.citysubsidence_controlled_high_pop = sum(citysubsidence_controlled_high_pop),
    sum.citysubsidence_uncontrolled_high_length = sum(citysubsidence_uncontrolled_high_length), 
    sum.citysubsidence_uncontrolled_high_pop = sum(citysubsidence_uncontrolled_high_pop),
    sum.citysubsidence_controlled_low_length = sum(citysubsidence_controlled_low_length), 
    sum.citysubsidence_controlled_low_pop = sum(citysubsidence_controlled_low_pop),
    sum.citysubsidence_uncontrolled_low_length = sum(citysubsidence_uncontrolled_low_length), 
    sum.citysubsidence_uncontrolled_low_pop = sum(citysubsidence_uncontrolled_low_pop),
    sum.rslr_low_length = sum(rslr_low_length), 
    sum.rslr_low_pop = sum(rslr_low_pop),
    sum.rslr_high_length = sum(rslr_high_length), 
    sum.rslr_high_pop = sum(rslr_high_pop)
)

gvaResults$rslr_low_length <- round(gvaResults$sum.rslr_low_length / gvaResults$sum.length, digits=4)
gvaResults$rslr_high_length <- round(gvaResults$sum.rslr_high_length / gvaResults$sum.length, digits=4)
gvaResults$rslr_low_pop <- round(gvaResults$sum.rslr_low_pop / gvaResults$sum.pop, digits=4)
gvaResults$rslr_high_pop <- round(gvaResults$sum.rslr_high_pop / gvaResults$sum.pop, digits=4)

gvaResultsCompact <- gvaResults[,c("regionid","rslr_high_length","rslr_high_pop")]
clsResultsCompact <- data[,c("locationid","regionid")]
clsResultsCompact <- merge(clsResultsCompact, gvaResultsCompact, by=c("regionid"),all.x=T)

cls <- readOGR(dsn="../input/gis/cls_p32.shp", layer="cls_p32")
cls@data$id = rownames(cls@data)
cls.points = fortify(cls, region="id")
cls_df = join(cls.points, cls@data, by="id")

plotData <- join(cls_df, clsResultsCompact, by = c("locationid"), type = "full")


#group = cut(plotData$rslr_high_length, c(-Inf,2,4,6,8,10,15,20,Inf), right=FALSE)
#plotData$rslr_high_length_cat <- as.factor(group)
#group = cut(plotData$rslr_high_pop, c(-Inf,2,4,6,8,10,15,20,Inf), right=FALSE)
#plotData$rslr_high_pop_cat <- as.factor(group)
#levels(plotData$rslr_high_length_cat) <- c("<= 2 mm/yr", "2-4 mm/yr", "4-6 mm/yr", "6-8 mm/yr", "8-10 mm/yr", "10-15 mm/yr", "15-20 mm/yr", ">20 mm/yr")
#levels(plotData$rslr_high_pop_cat) <- c("<= 2 mm/yr", "2-4 mm/yr", "4-6 mm/yr", "6-8 mm/yr", "8-10 mm/yr", "10-15 mm/yr", "15-20 mm/yr", ">20 mm/yr")

group = cut(plotData$rslr_high_length, c(-Inf,2,5,10,15,20,Inf), right=FALSE)
plotData$rslr_high_length_cat <- as.factor(group)
group = cut(plotData$rslr_high_pop, c(-Inf,2,5,10,15,20,Inf), right=FALSE)
plotData$rslr_high_pop_cat <- as.factor(group)
levels(plotData$rslr_high_length_cat) <- c("<= 2 mm/yr", "2-5 mm/yr", "5-10 mm/yr", "10-15 mm/yr", "15-20 mm/yr", ">20 mm/yr")
levels(plotData$rslr_high_pop_cat) <- c("<= 2 mm/yr", "2-5 mm/yr", "5-10 mm/yr", "10-15 mm/yr", "15-20 mm/yr", ">20 mm/yr")

plotLengthW <- ggplot(data=plotData,aes(long,lat,group=group)) +
    geom_path(lwd=1.2,aes(color=rslr_high_length_cat)) +
#    scale_color_discrete_sequential(palette = "Red-Blue", drop=FALSE, nmax = 8) +
    scale_color_manual(values=subsidencePaletteGreyFriendly, drop=FALSE) + 
    theme_minimal(20) +
    annotate("text", x = -170, y = 80, label = "(a)", size = 10) +
    legendMap + 
    axis +
    panel + 
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0.02,0)) + 
    labs(x=NULL, y=NULL, title=NULL) +
    theme(plot.margin = rep(unit(0,"null"),4)) +
    guides(color = FALSE, fill = FALSE)

plotPopW_legend <- ggplot(data=plotData,aes(long,lat,group=group)) +
    geom_path(lwd=1.2,aes(color=rslr_high_pop_cat)) +
#    scale_color_discrete_sequential(palette = "Red-Blue", drop=FALSE, nmax = 8) +
    scale_color_manual(values=subsidencePaletteGreyFriendly, drop=FALSE) + 
    theme_minimal(20) +
    annotate("text", x = -170, y = 80, label = "(b)", size = 10) +
    legendMap + 
    axis +
    panel + 
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0.02,0)) + 
    labs(x=NULL, y=NULL, title=NULL) +
    theme(plot.margin = rep(unit(0,"null"),4)) +  
    guides(colour = guide_legend(title=NULL, nrow=2, byrow=TRUE))

p <- grid.arrange(plotLengthW, plotPopW_legend, layout_matrix = rbind(c(1), c(2)))

ggsave("./jpg/Fig2_map_subsidence_GVA_publication.jpg", p, width = 13, height = 14, dpi = 600)
ggsave("./eps/Fig2_map_subsidence_GVA_publication.eps", p, width = 13, height = 14)
