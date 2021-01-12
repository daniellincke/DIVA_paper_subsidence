library(ggplot2)
library(scales)
library(reshape2)

lmp <- function (modelobject) {
    if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
    f <- summary(modelobject)$fstatistic
    p <- pf(f[1],f[2],f[3],lower.tail=F)
    attributes(p) <- NULL
    return(p)
}


data <- read.csv("../results/cls_slrrates_2015.csv")

data[is.na(data)] <- 0

data$citysubsidence_uncontrolled_high[data$citysubsidence_uncontrolled_high < 0] <- 0
data$citysubsidence_uncontrolled_low[data$citysubsidence_uncontrolled_low < 0] <- 0

data$rslr_high <- data$sat_slr + data$uplift + data$deltasubsidence + data$citysubsidence_uncontrolled_high
data$rslr_low <- data$sat_slr + data$uplift + data$deltasubsidence + data$citysubsidence_uncontrolled_low

data$rslr_high_length <- data$rslr_high * data$length
data$rslr_high_pop <- data$rslr_high * data$pop_below_10p0

data$rslr_low_length <- data$rslr_low * data$length
data$rslr_low_pop <- data$rslr_low * data$pop_below_10p0

data$popdens_below_10p0 <- data$pop_below_10p0/data$area_below_10p0
data <- data[data$pop_below_10p0!=0,]

model1 <- lm(formula = data$sat_slr ~ log10(data$popdens_below_10p0))
model2 <- lm(formula = data$uplift ~ log10(data$popdens_below_10p0))
model3 <- lm(formula = data$citysubsidence_uncontrolled_high ~ log10(data$popdens_below_10p0))
model4 <- lm(formula = data$deltasubsidence + data$citysubsidence_uncontrolled_high ~ log10(data$popdens_below_10p0))
model5 <- lm(formula = data$rslr_high ~ log10(data$popdens_below_10p0))

p1 <- lmp(lm(formula = data$sat_slr ~ log10(data$popdens_below_10p0)))
p2 <- lmp(lm(formula = data$uplift ~ log10(data$popdens_below_10p0)))
p3 <- lmp(lm(formula = data$citysubsidence_uncontrolled_high ~ log10(data$popdens_below_10p0)))
p4 <- lmp(lm(formula = data$deltasubsidence + data$citysubsidence_uncontrolled_high ~ log10(data$popdens_below_10p0)))
p5 <- lmp(lm(formula = data$rslr_high ~ log10(data$popdens_below_10p0)))

r1 <- summary(model1)$r.squared
r2 <- summary(model2)$r.squared
r3 <- summary(model3)$r.squared
r4 <- summary(model4)$r.squared
r5 <- summary(model5)$r.squared

data_popdens <- data[,c("popdens_below_10p0","locationid")]
data_text <- data.frame(
    label = c(
	paste(paste("Slope = ",round(coef(model1)["log10(data$popdens_below_10p0)"],4)),paste(paste("\nR² = ",round(r1,4),paste("\np = ",round(p1,4))))),
	paste(paste("Slope = ",round(coef(model2)["log10(data$popdens_below_10p0)"],4)),paste(paste("\nR² = ",round(r2,4),paste("\np = ",round(p2,4))))),
	paste(paste("Slope = ",round(coef(model3)["log10(data$popdens_below_10p0)"],4)),paste(paste("\nR² = ",round(r3,4),paste("\np = ",round(p3,4))))),
	paste(paste("Slope = ",round(coef(model4)["log10(data$popdens_below_10p0)"],4)),paste(paste("\nR² = ",round(r4,4),paste("\np = ",round(p4,4))))),
	paste(paste("Slope = ",round(coef(model5)["log10(data$popdens_below_10p0)"],4)),paste(paste("\nR² = ",round(r5,4),paste("\np = ",round(p5,4)))))  ),
    variable = c("sat_slr","uplift","citysubsidence_uncontrolled_high","deltasubsidence","rslr_high")
)

data <- data[,c("sat_slr","rslr_high","uplift","deltasubsidence","citysubsidence_uncontrolled_high","locationid")]
data <- melt(data,by=c("locationid"))
data <- merge(data,data_popdens,by=c("locationid"))
data <- merge(data,data_text,by=c("variable"))

data$variable <- factor(data$variable, levels=c("sat_slr","uplift","citysubsidence_uncontrolled_high","deltasubsidence","rslr_high"))
levels(data$variable) <- c("Only climate induced SLR","GIA","City subsidence","Delta subsidence","All SLR components")

data$letter <- as.factor(data$variable)
levels(data$letter)[levels(data$letter)=="Only climate induced SLR"] <- "(a)"
levels(data$letter)[levels(data$letter)=="GIA"] <- "(b)"
levels(data$letter)[levels(data$letter)=="City subsidence"] <- "(c)"
levels(data$letter)[levels(data$letter)=="Delta subsidence"] <- "(d)"
levels(data$letter)[levels(data$letter)=="All SLR components"] <- "(e)"

ggplot(data) + 
    geom_point(aes(x=popdens_below_10p0,y=value)) + 
    geom_smooth(aes(x=popdens_below_10p0,y=value),method = lm) + 
    facet_grid(. ~ variable) +
    geom_text(aes(0.001,104,label = label), size=7, hjust = 0, lineheight = .85, color="black") +
    geom_text(aes(0.001,117,label = letter), size=7, hjust = 0, lineheight = .85, color="black") +
    xlab("Coastal population density [people/km²]") + 
    ylab("Local relative sea-level change [mm/yr]") +
    scale_x_log10(labels = trans_format("log10", math_format(10^.x))) + 
    scale_y_continuous(limits = c(-14, 121), breaks=c(-10,0,10,20,30,40,50,60,70,80,90,100,110,120), expand = c(0,0)) +
    theme_bw(20) +
    theme(plot.margin = unit(c(0.4,1.2,0.4,0.4), "cm"))

ggsave("./jpg/FigS2_subsidence_scatterplots_population_density.jpg", width = 16, height = 8, dpi = 600)
ggsave("./eps/FigS2_subsidence_scatterplots_population_density.eps", width = 16, height = 8)

