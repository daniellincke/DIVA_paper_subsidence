library(ggplot2)
library(scales)

source("layout.r")

data <- read.csv("../results/global_output.csv")
data <- data[data$ada=="No adaptation",]
data$pop_below_h100 <- data$pop_below_h100 / 1000
data$assets_below_h100 <- data$assets_below_h100 / 1000
data <- data[data$time <= 2050,]

dataNO <- data[data$sub=="No subsidence",]
dataNO$subassumption <- "Middle Estimate"
dataGO <- data[data$sub=="GIA only",]
dataGO$subassumption <- "Middle Estimate"
dataGD <- data[data$sub=="GIA and Delta",]
dataGD$subassumption <- "Middle Estimate"

dataGDC <- data[((data$sub=="GIA, Delta and City (unmitigated)") |  (data$sub=="GIA, Delta and City (mitigated)") ),]

dataNO  <- dataNO[,c("time","area_below_h100","assets_below_h100","pop_below_h100","rcp","sub","ssp","subassumption")]
dataGO  <- dataGO[,c("time","area_below_h100","assets_below_h100","pop_below_h100","rcp","sub","ssp","subassumption")]
dataGD  <- dataGD[,c("time","area_below_h100","assets_below_h100","pop_below_h100","rcp","sub","ssp","subassumption")]
dataGDC <- dataGDC[,c("time","area_below_h100","assets_below_h100","pop_below_h100","rcp","sub","ssp","subassumption")]

dataREST <- rbind(dataNO,dataGO)
dataREST <- rbind(dataREST,dataGD)

data <- rbind(dataREST,dataGDC)
data$sub <- factor(data$sub, levels=c("No subsidence","GIA only","GIA and Delta","GIA and City (mitigated)","GIA and City (unmitigated)","GIA, Delta and City (unmitigated)","GIA, Delta and City (mitigated)"))
data$rcp <- factor(data$rcp, levels=c("No climate induced sea-level rise","RCP 2.6 (Medium ice melting)","RCP 4.5 (Medium ice melting)","RCP 8.5 (Medium ice melting)"))

levels(data$sub)[levels(data$sub)=="No subsidence"] <- "No vertical land movement"
levels(data$sub)[levels(data$sub)=="GIA and City (mitigated)"] <- "GIA and City (controlled)"
levels(data$sub)[levels(data$sub)=="GIA and City (unmitigated)"] <- "GIA and City (uncontrolled)"

levels(data$sub)[levels(data$sub)=="GIA, Delta and City (mitigated)"] <- "GIA, Delta and City (controlled)"
levels(data$sub)[levels(data$sub)=="GIA, Delta and City (unmitigated)"] <- "GIA, Delta and City (uncontrolled)"

levels(data$subassumption)[levels(data$subassumption)=="Lower"] <- "Lower estimate"
levels(data$subassumption)[levels(data$subassumption)=="Upper"] <- "Upper estimate"

data$letter <- data$sub
levels(data$letter) <- c("(a)","(b)","(c)","","","(d)","(e)")

data <- data[data$time >= 2015,]

dataSSP2 <- data[data$ssp=="SSP2",]

ggplot(dataSSP2) + 
    geom_line(aes(x=time, y=pop_below_h100, color=rcp, linetype=subassumption),size=0.8) +
    geom_text(aes(2019,387,label = letter), size=7, color="black") +
    facet_grid(. ~ sub) +
    xlab("Year") + 
    ylab("Population below 1-in-100-years waterlevel [Million]") +
    theme_bw(14) +
    scale_y_continuous(labels = comma, limits = c(220, 401), breaks=c(225,250,275,300,325,350,375,400), expand = c(0,0)) + 
    scale_x_continuous(breaks=c(2020, 2030, 2040, 2050), limits = c(2015, 2050), expand = c(0,0)) +
    scale_colour_manual(values=subsidencePalette) +
    scale_fill_manual(values=subsidencePalette) +
    scale_linetype_manual(values=c("dotdash", "solid", "twodash")) +
    legend + 
    theme(legend.title=element_blank()) +
    theme(axis.text.x=element_text(angle=90, vjust=0.5)) + 
    theme(strip.text.x = element_text(size = 10)) +
    guides(color=guide_legend(nrow=2,byrow=TRUE))

ggsave("./jpg/Fig3_subsidence_exposure_population_2015_2050_onlySSP2_lines.jpg", width = 12, height = 6, dpi = 600)
ggsave("./eps/Fig3_subsidence_exposure_population_2015_2050_onlySSP2_lines.eps", width = 12, height = 6)

data[data$ssp!="SSP1",]$letter <- ""

ggplot(data) + 
    geom_line(aes(x=time, y=pop_below_h100, color=rcp, linetype=subassumption),size=0.8) +
    geom_text(aes(2019,380,label = letter), size=7, color="black") +
    facet_grid(ssp ~ sub) +
    xlab("Year") + 
    ylab("Population below 1-in-100-years waterlevel [Million]") +
    theme_bw(14) +
    scale_y_continuous(labels = comma, limits = c(220, 401), breaks=c(225,250,275,300,325,350,375,400), expand = c(0,0)) + 
    scale_x_continuous(breaks=c(2020, 2030, 2040, 2050), limits = c(2015, 2050), expand = c(0,0)) +
    scale_colour_manual(values=subsidencePalette) +
    scale_fill_manual(values=subsidencePalette) +
    scale_linetype_manual(values=c("dotdash", "solid", "twodash")) +
    legend + 
    theme(legend.title=element_blank()) +
    theme(axis.text.x=element_text(angle=90, vjust=0.5)) + 
    theme(strip.text.x = element_text(size = 10)) +
    guides(color=guide_legend(nrow=2,byrow=TRUE))

ggsave("./jpg/FigS3_subsidence_exposure_population_2015_2050.jpg", width = 12, height = 10, dpi = 600)
ggsave("./eps/FigS3_subsidence_exposure_population_2015_2050.eps", width = 12, height = 10)

