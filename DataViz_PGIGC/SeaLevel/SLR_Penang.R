# Load required libraries

library(ggplot2)
library(scales)
library(grid)
library(plyr)
library(lubridate)
library(zoo)
library(viridis)


windowsFonts(Roboto="Roboto Medium")

# Set working directory

setwd("C:/Users/User/Google Drive/R/SeaLevel")

# Read csv file

sl<-read.csv("rqd0144a.csv",header=FALSE)

# Rename columns

colnames(sl)<-c("year","month","day","sl_mm")

# Format date columns

sl$date <- as.Date(paste(sl$year,sl$month,sl$day),format="%Y%m%d")
sl$month <- as.numeric(format(sl$date,"%m"))
sl$year <- as.numeric(format(sl$date,"%Y"))
sl$monthf <- factor(sl$month,levels=as.character(1:12),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE)
sl$mday <- strptime(sl$date, "%Y-%m-%d")$mday
sl$jday <- strptime(sl$date, "%Y-%m-%d")$yday+1
sl$daymth <- as.character(paste(sl$month,sl$day,sep="-"))
sl$daymth <-as.Date(sl$daymth,format="%m-%d")


## Plot Sea Level

hp_sl <- ggplot(sl, aes(date, sl_mm,colour=sl_mm))+
         #geom_line(size=0.5)+
         geom_point(shape=5,size=0.5)+
         geom_smooth(method="lm",size=0.5,col="red")+
         scale_x_date(name="",labels=date_format("%Y"),breaks = date_breaks("2 years"))+
         ylab("Milimetres (mm)\n")+
         xlab("\nYear")+
         scale_color_viridis(option="D",guide=FALSE)+
         theme_bw()+
         theme(text = element_text(family = "Roboto", color = "#3A3F4A"),
         panel.border = element_rect(colour = "grey30",fill=F,size=1),
         panel.grid.major = element_line(colour = "grey30",size=0.25,linetype='dotted'),
         panel.grid.minor = element_blank(),
         axis.title.y=element_text(size=11,colour="grey30"),
         axis.title.x=element_text(size=9,colour="grey30"),
         plot.title = element_text(lineheight=1.2, face="bold",size = 18, colour = "grey30"),
         plot.subtitle = element_text(face="bold",size = 15, colour = "grey30"),
         plot.caption=element_text(hjust=0,size=8.5,colour="grey30",lineheight = 1.2),
         panel.background = element_rect(fill = NA,colour = "grey30"))+
         labs(title='Sea Level at Penang, Malaysia',
         subtitle='1985 - 2013\n',
         caption='Data source : Daily sea level measurements provided to the University of Hawaii Sea Level Centre by Malaysian Dept. of Survey and Mapping (JUPEM) - http://uhslc.soest.hawaii.edu/data/?rq\nGraphic produced by authors of The Jason & Doug Blog - www.jason-doug-climate.blogspot.com on August 2017')
         

hp_sl

# Get regression and slope value

reg_fun<-lm(formula=sl$sl_mm~sl$year) 

slope<-round(coef(reg_fun)[2],3)  

hp_sl_final <- hp_sl + annotate(geom="text",x=as.Date("1986-01-01"),
               y=3100,label="Slope = 3.68 mm/year",family="Roboto")

hp_sl_final

# Save plot to png

ggsave(hp_sl_final, file="Penang_SeaLevel_Plot_1985-2013.png", width=12, height=7,dpi=400,unit="in",type="cairo")
