library(rgdal)     
library(ggplot2)   
library(ggmap)
library(raster)
library(ggridges)
library(extrafont)

setwd("D:/R/DataViz-R/ridgePlot/")

# Load font
windowsFonts(Berlin="Berlin Sans FB")
windowsFonts(KDK="Komika Display Kaps")

map <- raster("rsmpl04.tif") #elevation data of sefi

#convert the raster to points for plotting
map.p <- rasterToPoints(map)

#Make the points a dataframe for ggplot
df <- data.frame(map.p)

#Make appropriate column headings
colnames(df) <- c("Longitude", "Latitude", "MAP")

df$MAP <- df$MAP*4

line.map <- ggplot() + 
geom_ridgeline(data=df, aes(x=Longitude, y=Latitude, height = MAP, group = Latitude),
size=0.25,min_height=10,fill=NA,color="white",na.rm = FALSE)+
coord_fixed(1)+
theme_bw()+
annotate("text", x = 655209.512 , y =  806136.090, label = "Sabah, Malaysia",size=6,colour="white",family="KDK") +
theme(text = element_text(family = "KDK", color = "#3A3F4A"),
        panel.background = element_rect(fill = "darkslateblue"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank())

line.map

ggsave(line.map,file="ridge_map01.png",dpi=700,w=5.5,h=4.5)

