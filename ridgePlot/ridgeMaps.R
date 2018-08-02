library(rgdal)     
library(ggplot2)   
library(ggmap)
library(raster)
library(ggridges)
library(ggjoy)
library(extrafont)

setwd("D:/R/DataViz-R/ridgePlot/")

# Load font
windowsFonts(Avenir="Avenir Next Medium")
windowsFonts(Roboto="Roboto Medium")
windowsFonts(KDK="Komika Display Kaps")

map <- raster("png_topo_rsmpl01.tif") #elevation data of sefi

#convert the raster to points for plotting
map.p <- rasterToPoints(map)

#Make the points a dataframe for ggplot
df <- data.frame(map.p)

#Make appropriate column headings
colnames(df) <- c("Longitude", "Latitude", "MAP")

df$MAP <- df$MAP*1.2

line.map <- ggplot() + 
geom_ridgeline(data=df, aes(x=Longitude, y=Latitude, height = MAP, group = Latitude),
size=0.25,min_height=0.5,fill=NA,color="white",na.rm = FALSE)+
coord_fixed(0.9)+
theme_bw()+
annotate("text", x = 639341.408, y = 578096.596, label = "PENANG ISLAND",size=3.5,colour="white",family="KDK") +
theme(text = element_text(family = "Avenir", color = "#3A3F4A"),
        panel.background = element_rect(fill = "cadetblue4"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank())

line.map

ggsave(line.map,file="ridge_map.png",dpi=700,w=3.75,h=5)

