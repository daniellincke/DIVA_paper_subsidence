adaSubstring <- function(s) {
    if(length(grep("_AD",s))>0) "Adaptation"
    else "No adaptation"
}

rcpSubstring <- function(s) {
    if(length(grep("rcp85_high",s))>0) "RCP 8.5 (High ice melting)"
    else if(length(grep("rcp85_med",s))>0) "RCP 8.5 (Medium ice melting)"
    else if(length(grep("rcp45_med",s))>0) "RCP 4.5 (Medium ice melting)"
    else if(length(grep("rcp26_med",s))>0) "RCP 2.6 (Medium ice melting)"
    else if(length(grep("rcp26_low",s))>0) "RCP 2.6 (Low ice melting)"
    else if(length(grep("1mm_slr",s))>0) "SLR 1mm / year"
    else if(length(grep("3mm_slr",s))>0) "SLR 3mm / year"
    else if(length(grep("5mm_slr",s))>0) "SLR 5mm / year"
    else if(length(grep("7mm_slr",s))>0) "SLR 7mm / year"
    else if(length(grep("9mm_slr",s))>0) "SLR 9mm / year"
    else "No climate induced sea-level rise"
}

sspSubstring <- function(s) {
    if(length(grep("ssp1",s))>0) "SSP1"
    else if(length(grep("ssp2",s))>0) "SSP2"
    else if(length(grep("ssp3",s))>0) "SSP3"
    else if(length(grep("ssp4",s))>0) "SSP4"
    else if(length(grep("ssp5",s))>0) "SSP5"
    else "Constant socio-economics"
}

subSubstring <- function(s) {
    if(length(grep("Jakarta",s))>0) "All + Jakarta sinking"
    else if(length(grep("DELTA+CITY-SUB_unmittigated",s,fixed=TRUE))>0) "GIA, Delta and City (unmitigated)"
    else if(length(grep("DELTA+CITY-SUB_mittigated",s,fixed=TRUE))>0) "GIA, Delta and City (mitigated)"
    else if(length(grep("GIA+CITY-SUB_mittigated",s,fixed=TRUE))>0) "GIA and City (mitigated)"
    else if(length(grep("GIA+CITY-SUB_unmittigated",s,fixed=TRUE))>0) "GIA and City (unmitigated)"
    else if(length(grep("DELTA-SUB",s))>0) "GIA and Delta"
    else if(length(grep("GIA-SUB",s))>0) "GIA only"
    else "No subsidence"
}

subAssumptionSubstring <- function(s) {
    if(length(grep("_highend",s))>0) "Upper"
    else if(length(grep("_lowend",s))>0) "Lower"
    else "Upper"
}


labeldata <- function(data) {
    data$rcp <- sapply(data$caseid,rcpSubstring)
    data$ada <- sapply(data$caseid,adaSubstring)
    data$ssp <- sapply(data$caseid,sspSubstring)
    data$sub <- sapply(data$caseid,subSubstring)
    data$subassumption <- sapply(data$caseid,subAssumptionSubstring)
    data
}

data <- read.csv("global_output.csv")
data <- labeldata(data)
data <- data[,c("DIVA_version","caseid","locationid","locationname","time","area_below_1p0","area_below_2p0","area_below_10p0","area_below_h1","area_below_h10","area_below_h100","area_below_h1000","assets_below_1p0","assets_below_2p0","assets_below_10p0","assets_below_h1","assets_below_h10","assets_below_h100","assets_below_h1000","coastlength","gdp","gdpc","h1","h10","h100","h1000","pop_below_1p0","pop_below_2p0","pop_below_10p0","pop_below_h1","pop_below_h10","pop_below_h100","pop_below_h1000","rcp","ada","ssp","sub","subassumption")]
write.csv(data,"global_output.csv",row.names=F)
