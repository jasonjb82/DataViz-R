########################################################################################################
#### Code to plot migration flows into Malaysia (2005 - 2010) ##########################################
#### Code by Jason Jon Benedict, 28th May 2017 #########################################################
#### Data: Quantifying Global International Migration Flows - Guy J.	Abel, Nikola	Sander,2014 ########
########################################################################################################

# Load libraries

library(RColorBrewer)
library(rnaturalearthdata)
library(rnaturalearth)
library(ggplot2)
library(ggrepel)
library(viridis)
library(RColorBrewer)
library(maptools)
library(rgdal)
library(raster)
library(mapdata)
library(readxl)
library(plyr)
library(dplyr)
library(ggalt)
library(ggmap)
library(rgeos)

# Set working directory

setwd("C:/Jason/User/Google Drive/R/Migration")

# Read migration flow data from csv file

flow <- read_excel("Abel-Database-s2.xlsx", sheet ="2005-10",skip=2)

# Reformat data

flow_my <- flow[,c("X__3","MYS")]

colnames(flow_my) <- c("origin","number")

flow_my$destination <- "MYS"

# Get centroid coordinates for Malaysia

mys_coord <- geocode("Malaysia")

flow_my$dest_lat <- mys_coord$lat
flow_my$dest_long <- mys_coord$lon

# Read country shapefiles

countries <- readOGR(dsn=".", layer="TM_WORLD_BORDERS-0.3",encoding="UTF-8")

countries_df <- subset(countries, NAME != "Antarctica")

# Get country centroids

country_centres <- SpatialPointsDataFrame(gCentroid(countries, byid=TRUE), 
                                      countries@data, match.ID=FALSE)

country_df <- as.data.frame(country_centres)

country_df$NAME <- as.character(country_df$NAME)

# Rename countries from long names to shorter names

country_df$NAME[country_df$NAME=="Korea, Republic of"]<- "South Korea"
country_df$NAME[country_df$NAME=="Brunei Darussalam"]<- "Brunei"
country_df$NAME[country_df$NAME=="Iran (Islamic Republic of)"]<- "Iran"

country_coords_df <- country_df[,c("ISO3","REGION","NAME","x","y")]

combined.df <- merge(x=flow_my, y=country_coords_df[,c("ISO3","x","y","NAME","REGION")], by.x ="origin", by.y = "ISO3", all.x =TRUE)

combined.df.final <- combined.df[combined.df$number!=0 & combined.df$origin != "TOTAL",] 

# Create country labels

labels <- combined.df.final[row.names(unique(combined.df.final[,c("NAME","x","y")])),]

# Define theme for flow map

theme_mig<- theme(text = element_text(family = "Officina Sans ITC Book", color = "grey40"),
                  panel.background = element_rect(fill="black"), 
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  axis.line = element_blank(),
                  axis.text.x = element_blank(),
                  axis.text.y = element_blank(),
                  axis.ticks = element_blank(),
                  axis.title.x = element_blank(),
                  axis.title.y = element_blank(),
                  legend.position = c(0.5, 0.1),
                  legend.direction = 'horizontal',
                  legend.key.height = unit(5, "pt"),
                  legend.key.width = unit(40, "pt"),
                  legend.text = element_text(size = 8),
                  legend.title = element_text(size = 10),
                  legend.title.align = 0.5,
                  legend.background = element_blank())


plot <-

ggplot() + geom_polygon(data=countries_df, aes(long,lat,group=group), fill="grey35")+ 
geom_curve(data=combined.df.final, aes(x = x, y = y, xend = dest_long, yend = dest_lat,
colour=number,size=number),alpha=0.65,curvature = .3,angle=20,lineend = "round") +
coord_fixed()+
geom_text_repel(data=labels, aes(x = x, y = y, label = NAME), col = "grey70", size = 1.75, force=2, segment.color = NA,
family = "Arial Narrow",box.padding = unit(0.1, "lines")) +
scale_alpha_continuous(guide=FALSE)+
scale_size_continuous(guide=FALSE,range=c(0.4,2))+
scale_colour_gradientn(colours=c("lightgoldenrod","goldenrod","orange","orangered","firebrick","red4","darkred"),limits = c(0,400000),breaks=c(0,100000,200000,300000,400000),labels=c("0","100k","200k","300k","400k")) +
theme_mig +
labs(title='Migrant flow into Malaysia',
subtitle='from 2005 - 2010',
caption='Data: Quantifying Global International Migration Flows - Guy J.	Abel, Nikola	Sander,2014')+
guides(color = guide_colorbar(title = "Estimated no of people", raster = F, title.position = "top"))
  
plot

# Save flow map to png file

ggsave(plot,file="Migration.png",dpi=400,w=9,h=4.5,type="cairo-png")
