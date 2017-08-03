###########################################
#### Produce visibility 'heatmap' plot ####
###########################################

# Load libraries

library(ggplot2)
library(RColorBrewer)
library(openair)
library(weatherData)
library(mgcv)
library(scales)
library(plyr)
library(reshape2)
library(circular)
library(gridExtra)
library(lubridate)
library(weathermetrics)
library(zoo)
library(viridis)

# Setting work directory

setwd("C:/Users/User/Google Drive/R/Visibility")

# Use station code below to get required plot data and parameters

station.id="WMKP"

s<-getStationCode(station.id)

s1<-(strsplit(s,split= " "))[[1]]

station.name<-paste(s1[4],s1[5])

### Getting summarized weather data for WU

ws2001<-getSummarizedWeather(station.id, "2001-01-01", "2001-12-31", opt_custom_columns=T,
                             custom_columns=c(1:23), opt_verbose=T)
ws2002<-getSummarizedWeather(station.id, "2002-01-01", "2002-12-31", opt_custom_columns=T,
                             custom_columns=c(1:23), opt_verbose=T)
ws2003<-getSummarizedWeather(station.id, "2003-01-01", "2003-12-31", opt_custom_columns=T,
                             custom_columns=c(1:23), opt_verbose=T)
ws2004<-getSummarizedWeather(station.id, "2004-01-01", "2004-12-31", opt_custom_columns=T,
                             custom_columns=c(1:23), opt_verbose=T)
ws2005<-getSummarizedWeather(station.id, "2005-01-01", "2005-12-31", opt_custom_columns=T,
                             custom_columns=c(1:23), opt_verbose=T)
ws2006<-getSummarizedWeather(station.id, "2006-01-01", "2006-12-31", opt_custom_columns=T,
                             custom_columns=c(1:23), opt_verbose=T)
ws2007<-getSummarizedWeather(station.id, "2007-01-01", "2007-12-31", opt_custom_columns=T,
                             custom_columns=c(1:23), opt_verbose=T)
ws2008<-getSummarizedWeather(station.id, "2008-01-01", "2008-12-31", opt_custom_columns=T,
                             custom_columns=c(1:23), opt_verbose=T)
ws2009<-getSummarizedWeather(station.id, "2009-01-01", "2009-12-31", opt_custom_columns=T,
                             custom_columns=c(1:23), opt_verbose=T)
ws2010<-getSummarizedWeather(station.id, "2010-01-01", "2010-12-31", opt_custom_columns=T,
                             custom_columns=c(1:23), opt_verbose=T)
ws2011<-getSummarizedWeather(station.id, "2011-01-01", "2011-12-31", opt_custom_columns=T,
                             custom_columns=c(1:23), opt_verbose=T)
ws2012<-getSummarizedWeather(station.id, "2012-01-01", "2012-12-31", opt_custom_columns=T,
                             custom_columns=c(1:23), opt_verbose=T)
ws2013<-getSummarizedWeather(station.id, "2013-01-01", "2013-12-31", opt_custom_columns=T,
                             custom_columns=c(1:23), opt_verbose=T)
ws2014<-getSummarizedWeather(station.id, "2014-01-01", "2014-12-31", opt_custom_columns=T,
                             custom_columns=c(1:23), opt_verbose=T)
ws2015<-getSummarizedWeather(station.id, "2015-01-01", "2015-12-31", opt_custom_columns=T,
                             custom_columns=c(1:23), opt_verbose=T)
ws2016<-getSummarizedWeather(station.id, "2016-01-01", "2016-12-31", opt_custom_columns=T,
                             custom_columns=c(1:23), opt_verbose=T)

ws<-rbind(ws2001,ws2002,ws2003,ws2004,ws2005,ws2006,ws2007,ws2008,ws2009,ws2010,ws2011,ws2012,ws2013,ws2014,ws2015,ws2016) 

summary(ws)

str(ws)

### List of variables from Wunderground ###

# [1] "MYT"                        "Max_TemperatureC"           "Mean_TemperatureC"          "Min_TemperatureC"          
# [5] "Dew_PointC"                 "MeanDew_PointC"             "Min_DewpointC"              "Max_Humidity"              
# [9] "Mean_Humidity"              "Min_Humidity"               "Max_Sea_Level_PressurehPa"  "Mean_Sea_Level_PressurehPa"
# [13] "Min_Sea_Level_PressurehPa"  "Max_VisibilityKm"           "Mean_VisibilityKm"          "Min_VisibilitykM"          
# [17] "Max_Wind_SpeedKm_h"         "Mean_Wind_SpeedKm_h"        "Max_Gust_SpeedKm_h"         "Precipitationmm"           
# [21] "CloudCover"                 "Events"                     "WindDirDegrees"            

colnames(ws)<-c("date","date1","maxtemp","meantemp","mintemp","dewp","meandewp","maxdewp","maxhum","meanhum","minhum","maxslp","meanslp",
                "minslp","maxvsb","meanvsb","minvsb","maxwspd","meanwspd","maxgust","prcp","cc","events","wd")

## Adding date columns

ws$dates <- as.Date(ws$date)

ws$year <- as.numeric(as.POSIXlt(ws$dates)$year+1900)
ws$month <- as.numeric(as.POSIXlt(ws$dates)$mon+1)
ws$monthf <- factor(ws$month,levels=as.character(1:12),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE)
ws$weekday <- as.POSIXlt(ws$dates)$wday
ws$weekdayf <- factor(ws$weekday,levels=rev(0:6),labels=rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")),ordered=TRUE)
ws$yearmonth <- as.yearmon(ws$date)
ws$yearmonthf <- factor(ws$yearmonth)
ws$week <- as.numeric(format(as.Date(ws$dates),"%W"))
ws$weekf<- factor(ws$week)
ws$jday<-yday(ws$dates)

ws_subset <- subset(ws,week > 0 & week < 53)

# Define colour palette

col<-c("black","grey10","grey20","grey50","white")

# Plot 'heatmap'

v <- ggplot(data=ws_subset,aes(x=week,y=year,fill=meanvsb))+ geom_tile(colour="white",size=0.1)+ theme_bw()+
scale_fill_viridis(option="B",name="Visibility\n(km)")+coord_equal(ratio=1)+
ylab("")+
xlab("")+
scale_y_continuous(expand = c(0,0),breaks = seq(2000, 2016, 1)) +
scale_x_discrete(expand = c(0,0),breaks = seq(0,52,2))+
labs(title='Average Weekly Visibility for Penang',
subtitle='2001 - 2016\n',
caption='Data source: Weather Underground (www.wunderground.com) on 31st July 2017\nGraphic produced by the authors of The Jason & Doug Blog - www.jason-doug-climate.blogspot.com')+
theme(text = element_text(family = "Roboto", color = "#3A3F4A"),
panel.background=element_rect(fill="transparent"),
panel.border=element_blank(),
axis.title.y=element_text(size=10,colour="grey30"),
axis.title.x=element_text(size=10,colour="grey30"),
axis.text.y=element_text(size=10,colour="grey30",face="bold"),
axis.text.x=element_text(size=10,colour="grey30",face="bold"),
axis.ticks=element_blank(),
plot.title = element_text(lineheight=1.2, face="bold",size = 13, colour = "grey30"),
plot.subtitle = element_text(face="bold",size = 10, colour = "grey30"),
plot.caption=element_text(hjust=0,size=8,colour="grey30",lineheight = 1.2),
panel.grid.major = element_blank(),
legend.title=element_text(size=8),
legend.title.align=1,
legend.text=element_text(size=8),
legend.position="bottom",
legend.key.size=unit(0.2, "cm"),
legend.key.width=unit(1, "cm"))

v

# Save plot to png

ggsave(v, file="Penang_Weekly_Average_Visibility_2001-2016.png", width=10, height=5,dpi=400,type = "cairo-png")

