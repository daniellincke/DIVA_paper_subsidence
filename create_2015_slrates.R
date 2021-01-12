datacls <- read.csv("./input/cls_input_33.csv")
datacls <- datacls[,c("locationid","cityid","deltaid","length")]

datacls_gia <- read.csv("./input/cls_input_uplift_ICE-6G.csv")
datacls_gia <- datacls_gia[,c("locationid","uplift")]
datacls <- merge(datacls, datacls_gia, by=c("locationid"),all.x=T)

datasat <- read.csv("./input/satelitte_SLRrates1p0deg.csv")
names(datasat)[names(datasat)=="segmentID"] <- "locationid"
names(datasat)[names(datasat)=="avg_SLRrates1p0deg"] <- "sat_slr"

datacls <- merge(datacls, datasat, by=c("locationid"),all.x=T)

# for one selected case we need to extract 2015 cls population exposure 
dataclsout <- read.csv("./input/cls_output.csv")
dataclsout <- dataclsout[dataclsout$time==2015,]
dataclsout <- dataclsout[,c("locationid","pop_below_10p0","pop_below_h100","area_below_10p0")]

datadelta <- read.csv("./input/delta_input_1mm.csv")
datadelta <- datadelta[,c("locationid","subsidence_ericson_total","subsidence_non_ericson")]
datadelta$subsidence <- datadelta$subsidence_ericson_total + datadelta$subsidence_non_ericson
names(datadelta)[names(datadelta)=="locationid"] <- "deltaid"
names(datadelta)[names(datadelta)=="subsidence"] <- "deltasubsidence"

datacity_high <- read.csv("./input/city_subsidence_uncontrolled_high.csv")
datacity_high <- datacity_high[,c("locationid","subsidence")]
names(datacity_high)[names(datacity_high)=="locationid"] <- "cityid"
names(datacity_high)[names(datacity_high)=="subsidence"] <- "citysubsidence_uncontrolled_high"

datacity_low <- read.csv("./input/city_subsidence_uncontrolled_low.csv")
datacity_low <- datacity_low[,c("locationid","subsidence")]
names(datacity_low)[names(datacity_low)=="locationid"] <- "cityid"
names(datacity_low)[names(datacity_low)=="subsidence"] <- "citysubsidence_uncontrolled_low"

datacitycontrolled_high <- read.csv("./input/city_subsidence_controlled_high.csv")
datacitycontrolled_high <- datacitycontrolled_high[,c("locationid","subsidence")]
names(datacitycontrolled_high)[names(datacitycontrolled_high)=="locationid"] <- "cityid"
names(datacitycontrolled_high)[names(datacitycontrolled_high)=="subsidence"] <- "citysubsidence_controlled_high"

datacitycontrolled_low <- read.csv("./input/city_subsidence_controlled_low.csv")
datacitycontrolled_low <- datacitycontrolled_low[,c("locationid","subsidence")]
names(datacitycontrolled_low)[names(datacitycontrolled_low)=="locationid"] <- "cityid"
names(datacitycontrolled_low)[names(datacitycontrolled_low)=="subsidence"] <- "citysubsidence_controlled_low"

data <- merge(datacls, dataclsout, by=c("locationid"),all.x=T)
data <- merge(data, datadelta, by=c("deltaid"),all.x=T)
data <- merge(data, datacitycontrolled_high, by=c("cityid"),all.x=T)
data <- merge(data, datacitycontrolled_low, by=c("cityid"),all.x=T)
data <- merge(data, datacity_high, by=c("cityid"),all.x=T)
data <- merge(data, datacity_low, by=c("cityid"),all.x=T)
data <- data[!duplicated(data), ]

data[is.na(data)] <- 0
data$citysubsidence_controlled_high[data$citysubsidence_controlled_high < 0] <- 0
data$citysubsidence_controlled_low[data$citysubsidence_controlled_low < 0] <- 0
data$citysubsidence_uncontrolled_high[data$citysubsidence_uncontrolled_high < 0] <- 0
data$citysubsidence_uncontrolled_low[data$citysubsidence_uncontrolled_low < 0] <- 0

data$sat_length = data$sat_slr * data$length
data$sat_pop = data$sat_slr * data$pop_below_10p0
data$uplift_length = data$uplift * data$length
data$uplift_pop = data$uplift * data$pop_below_10p0
data$deltasubsidence_length = data$deltasubsidence * data$length
data$deltasubsidence_pop = data$deltasubsidence * data$pop_below_10p0
data$citysubsidence_controlled_high_length = data$citysubsidence_controlled_high * data$length
data$citysubsidence_controlled_high_pop = data$citysubsidence_controlled_high * data$pop_below_10p0
data$citysubsidence_controlled_low_length = data$citysubsidence_controlled_low * data$length
data$citysubsidence_controlled_low_pop = data$citysubsidence_controlled_low * data$pop_below_10p0

data$citysubsidence_uncontrolled_high_length = data$citysubsidence_uncontrolled_high * data$length
data$citysubsidence_uncontrolled_high_pop = data$citysubsidence_uncontrolled_high * data$pop_below_10p0
data$citysubsidence_uncontrolled_low_length = data$citysubsidence_uncontrolled_low * data$length
data$citysubsidence_uncontrolled_low_pop = data$citysubsidence_uncontrolled_low * data$pop_below_10p0

data$rslr_high <- data$sat_slr + data$uplift + data$deltasubsidence + data$citysubsidence_uncontrolled_high
data$rslr_low <- data$sat_slr + data$uplift + data$deltasubsidence + data$citysubsidence_uncontrolled_low

data$rslr_climate                                  <- data$sat_slr
data$rslr_climate_gia                              <- data$sat_slr + data$uplift
data$rslr_climate_gia_delta                        <- data$sat_slr + data$uplift + data$deltasubsidence
data$rslr_climate_gia_delta_city_controlled_low    <- data$sat_slr + data$uplift + data$deltasubsidence + data$citysubsidence_controlled_low
data$rslr_climate_gia_delta_city_controlled_high   <- data$sat_slr + data$uplift + data$deltasubsidence + data$citysubsidence_controlled_high
data$rslr_climate_gia_delta_city_uncontrolled_low  <- data$sat_slr + data$uplift + data$deltasubsidence + data$citysubsidence_uncontrolled_low
data$rslr_climate_gia_delta_city_uncontrolled_high <- data$sat_slr + data$uplift + data$deltasubsidence + data$citysubsidence_uncontrolled_high

write.csv(data,"./results/cls_slrrates_2015.csv",row.names=F)

