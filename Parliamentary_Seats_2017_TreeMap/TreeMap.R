## Treemap for Parliamentary Seats and Etnicity

library(treemap)
library(ggplot2)
library(gridExtra)
library(extrafont)

setwd("C:/Users/Jason/Google Drive/R/Parliamentary_Seats_2017")

seats <- read.csv("parliamentary_seats_ethnicity_party_2017.csv",header=TRUE)


png(filename = "TreeMap_ParliamentarySeats2017.png",height=7,width=9,
    bg = "white",units='in', res = 400, family = "",  type = "cairo-png")

party.col <- c("#092781","yellow","#E5342C","#00833c","#9C0E0B","#7dc6e6")

treemap(seats, 
        index=c("Ethnicity","Numbers"),
        vSize="Numbers",  
        vColor="Party",
        type="categorical",
        palette= party.col,
        fontsize.title = 15, #Change the font size of the title
        title="2017 Parliamentary Seat Holders by Ethnicity and Party\n", #Customize your title
        fontfamily.title = "Arial", fontfamily.labels = "Arial Narrow",fontfamily.legend="Arial Narrow",
        align.labels=list(c("center", "center"), c("right", "bottom"),c("left", "top")),
        force.print.labels=TRUE,
        lowerbound.cex.labels=0,
        overlap.labels=1,
        fontsize.labels=c(20, 12, 15),
        fontsize.legend=12,
        bg.labels = 0,
        border.col =c("black","grey30"),
        border.lwds = c(6,1), # defines line width
        inflate.labels=FALSE)


dev.off()
