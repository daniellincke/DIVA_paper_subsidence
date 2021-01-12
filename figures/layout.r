library(ggplot2)

legend <- theme(legend.position="bottom", 
    legend.direction = "horizontal",
    legend.text=element_text(size=10),
    legend.title=element_text(size=10)
    ) 

axis <- theme(axis.text.y=element_blank(),panel.spacing = unit(c(0,0,0,0), "lines"),
    axis.text.x=element_blank(),
    axis.ticks=element_blank(),
    axis.ticks.length = unit(0,"null"),
    axis.title.x=element_blank(),
    axis.title.y=element_blank()
    )

blank_axis_x <- theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
blank_axis_y <- theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())

panel <- theme(panel.grid.major = element_blank(),
    panel.grid = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(0,"null")
)

legendMap <- theme(
    legend.position="bottom", 
    legend.direction = "horizontal",
    legend.box="horizontal",
    legend.text=element_text(size=12, margin = margin(r = 30, unit = "pt")),
    legend.title=element_text(size=16),
    legend.key.width = unit(1.0, "cm"),
    legend.spacing.x = unit(0.33, 'cm')
) 

#panel <- theme(
#    panel.grid.major = element_blank(),
#    panel.grid = element_blank()
#)


subsidencePalette  <- c(rgb(180/255, 180/255, 180/255), rgb(0/255, 0/255, 255/255), rgb(121/255, 188/255, 255/255), rgb(255/255, 0/255, 0/255))
subsidencePaletteRCP2685  <- c(rgb(180/255, 180/255, 180/255), rgb(0/255, 0/255, 255/255), rgb(255/255, 0/255, 0/255))

subsidencePaletteGreyFriendly  <- c(rgb(0/255, 50/255, 255/255), rgb(0/255, 120/255, 200/255), rgb(60/255, 0/255, 120/255), rgb(120/255, 0/255, 80/255), rgb(185/255, 0/255, 40/255), rgb(255/255, 0/255, 0/255))

