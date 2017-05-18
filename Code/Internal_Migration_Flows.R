############################################################################
#### Code to plot Malaysian internal migration flows (2005 - 2010) #########
#### Code by Jason Jon Benedict, 17th May 2017 #############################
#### Data from WorldPop ####################################################
############################################################################


# Load libraries

library(ggmap)
library(ggplot2)
library(ggrepel)
library(viridis)
library(RColorBrewer)
library(maptools)
library(rgdal)
library(raster)

# Set working directory

setwd("C:/Users/User/Google Drive/R/Migration")

# Read migration flow data from csv file

mig <- read.csv("MYS_5yrs_InternalMigFlows_2010.csv", stringsAsFactors = FALSE)

# Recode states from numbers into actual state name

mig$NODEI[mig$NODEI=="1"]<- as.character("Johor")
mig$NODEJ[mig$NODEJ=="1"]<- as.character("Johor")

mig$NODEI[mig$NODEI=="2"]<- as.character("Kedah")
mig$NODEJ[mig$NODEJ=="2"]<- as.character("Kedah")

mig$NODEI[mig$NODEI=="3"]<- as.character("Kelantan")
mig$NODEJ[mig$NODEJ=="3"]<- as.character("Kelantan")

mig$NODEI[mig$NODEI=="4"]<- as.character("Melaka")
mig$NODEJ[mig$NODEJ=="4"]<- as.character("Melaka")

mig$NODEI[mig$NODEI=="5"]<- as.character("N.Sembilan")
mig$NODEJ[mig$NODEJ=="5"]<- as.character("N.Sembilan")

mig$NODEI[mig$NODEI=="6"]<- as.character("Pahang")
mig$NODEJ[mig$NODEJ=="6"]<- as.character("Pahang")

mig$NODEI[mig$NODEI=="7"]<- as.character("Penang")
mig$NODEJ[mig$NODEJ=="7"]<- as.character("Penang")

mig$NODEI[mig$NODEI=="8"]<- as.character("Perak")
mig$NODEJ[mig$NODEJ=="8"]<- as.character("Perak")

mig$NODEI[mig$NODEI=="9"]<- as.character("Perlis")
mig$NODEJ[mig$NODEJ=="9"]<- as.character("Perlis")

mig$NODEI[mig$NODEI=="10"]<- as.character("Selangor")
mig$NODEJ[mig$NODEJ=="10"]<- as.character("Selangor")

mig$NODEI[mig$NODEI=="11"]<- as.character("Terengganu")
mig$NODEJ[mig$NODEJ=="11"]<- as.character("Terengganu")

mig$NODEI[mig$NODEI=="12"]<- as.character("Sabah")
mig$NODEJ[mig$NODEJ=="12"]<- as.character("Sabah")

mig$NODEI[mig$NODEI=="13"]<- as.character("Sarawak")
mig$NODEJ[mig$NODEJ=="13"]<- as.character("Sarawak")

mig$NODEI[mig$NODEI=="14"]<- as.character("K.Lumpur")
mig$NODEJ[mig$NODEJ=="14"]<- as.character("K.Lumpur")

mig$NODEI[mig$NODEI=="15"]<- as.character("Labuan")
mig$NODEJ[mig$NODEJ=="15"]<- as.character("Labuan")


# Define theme for flow map

theme_mig<- theme(text = element_text(family = "Officina Sans ITC Book", color = "#3A3F4A"),
      panel.background = element_rect(fill="#EFF2F4"), 
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

# Create state labels

labels <- mig[row.names(unique(mig[,c("NODEI","LATFR","LONFR")])),]

# Get MYS administrative boundaries from GADM

mys <-getData("GADM", country="MY", level=0)

# Plot flow map

plot<-
ggplot() + geom_polygon(data=mys, aes(long,lat,group=group), fill="grey40")+ 
geom_curve(data=mig, aes(x = LONFR, y = LATFR, xend = LONTO, yend = LATTO, alpha=PrdMIG,colour=PrdMIG,size=PrdMIG),curvature = .2) + 
geom_point(data=labels, aes(x = LONFR, y = LATFR), col = "grey20",size=0.25) + 
geom_text_repel(data=labels, aes(x = LONFR, y = LATFR, label = NODEI), col = "grey8", size = 2.5, segment.color = NA,family = "Arial Narrow") +
xlim(99.5, 119.5) + ylim(-0.5, 8) + coord_fixed()+
#scale_colour_viridis(option="plasma",limits = c(0,21000),breaks=c(0,21000),labels=c("Lowest","Highest"))+
scale_color_gradientn(colours = rainbow(5),limits = c(0,21000),breaks=c(0,21000),labels=c("Lowest","Highest"))+
scale_alpha_continuous(guide=FALSE)+
scale_size_continuous(guide=FALSE,trans="sqrt",range=c(0.5,2))+
labs(title='Malaysia internal migration flows',
subtitle='2010 - 2015',
caption='Data: WorldPop, www.worldpop.org')+
guides(color = guide_colorbar(title = "Estimated internal migration flows", raster = F, title.position = "top"))+

plot

# Save flow map to png file

ggsave(plot,file="MYS Internal Migration.png",dpi=400,w=9,h=4.5,type="cairo-png")
