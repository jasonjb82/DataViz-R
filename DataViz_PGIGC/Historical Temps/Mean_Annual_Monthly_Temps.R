#### Long Term Monthly Average Temperature Graph using data from GHCNv3
#### Jason Benedict, Feb 13 2014
#####################################################################################

library(ggplot2)
library(splines2)
library(scales)
library(grid)
library(dplyr)
library(extrafont)

windowsFonts(Roboto="Roboto Medium")

#Setting work directory

setwd("C:/Users/User/Google Drive/R/Historical_Temps")

list.files()

# Importing csv file into R

dataset = read.csv("CISL GHCN v3 50548601000 monthly.csv", header=TRUE,sep=",",skip=2,na.strings="")

head(dataset)

colnames(dataset)<-c("date","date1","temp","prcp")

# Reformatting data

temp<-as.numeric(dataset$temp)

year<-as.numeric(substr(dataset$date,1,4))
year[1]
min.year <- min(year)
max.year <- max(year)
max.year


mon<-as.numeric(substr(dataset$date,6,7))

day<-01

summary(mon)

date<-paste(year,mon,day,sep="/")

dataset$dates <- as.Date(date, "%Y/%m/%d") 

min.date <- min(dataset$dates)
max.date <- max(dataset$dates)

dataset$year <- as.numeric(format(dataset$dates,"%Y"))
dataset$month <- as.numeric(format(dataset$dates,"%m"))
dataset$monthf <- factor(dataset$month,levels=as.character(1:12),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE)

dataset_sub <- subset(dataset, year > 1945)

dataset_yr <- aggregate(temp ~  year, dataset_sub, FUN = mean)

colnames(dataset_yr) <- c("year","yr_temp")


# Calculate the min and max values, which.min returns the first (like your example):
mins <- group_by(dataset_sub) %>% slice(which.min(temp))
maxs <- group_by(dataset_sub) %>% slice(which.max(temp))
#ends <- group_by(dataset_sub) %>% filter(year == max(year))

# Plotting average temperatures with ggplot2 and saving to png format

p<-ggplot(data=dataset_sub, aes(x=date1, y=temp,colour="temp")) +
  geom_line(size=0.5) +
  geom_line(data=dataset_yr,aes(x=year,y=yr_temp,colour="yr_temp"),size=1,alpha=0.75)+
  geom_point(data=dataset_yr,aes(x=year,y=yr_temp),colour="#d73027",size=1.75)+
  scale_y_continuous() +
  scale_colour_manual("", breaks = c("temp", "yr_temp"),
  values = c("grey80", "#d73020"),
  labels = c("Monthly average temperature","Yearly average temperature")) + 
  scale_x_continuous(breaks = seq(1946, 2016,5)) + theme_bw()+
  theme(text = element_text(family = "Roboto", color = "#3A3F4A"),
  panel.grid.major = element_line(colour = "#4575b4",size=0.25,linetype='dotted'),
  panel.grid.minor = element_blank(),
  panel.border=element_rect(size=1,colour="grey30"),
  legend.position=c(0.15, 0.9),
  legend.key=element_blank(),
  legend.title=element_text(size=12),
  legend.text=element_text(size=12),
  legend.background=element_blank(),
  axis.title.y=element_text(size=12,colour="grey30"),
  axis.title.x=element_text(size=12,colour="grey30"),
  axis.text.y=element_text(size=12,colour="grey30",face="bold"),
  axis.text.x=element_text(size=12,colour="grey30",face="bold"),
  plot.title = element_text(lineheight=1.2, face="bold",size = 18, colour = "grey30"),
  plot.subtitle = element_text(face="bold",size = 15, colour = "grey30"),
  plot.caption=element_text(hjust=0,size=10,colour="grey30",lineheight = 1.2))+
  geom_point(data = mins, aes(x=date1,y=temp), col = 'blue',size=2) +
  geom_text(data = mins, aes(label = paste(monthf,"",year,":",round(temp,1)),x=year,y=temp), vjust = 1, hjust = -0.25, family = "Roboto",color = "grey30", size=3.5) +
  geom_point(data = maxs, aes(x=date1,y=temp), col = 'red',size=2) +
  geom_text(data = maxs, aes(label = paste(monthf,"",year,":",round(temp,1)),x=year,y=temp), vjust = -1, family = "Roboto",color = "grey30", size=3.5) +
  #geom_point(data = ends, aes(x=year,y=temp), col = 'green',size=2) +
  #geom_text(data = ends, aes(label = paste(year,":",round(temp,1)),x=year,y=temp), vjust = -1,family = "Lato Semibold", color = "grey40", size=3) +
  xlab("") + ylab(as.expression(expression( paste("Temperature (", degree,"C)") ))) +
  labs(title='Average Yearly and Monthly Temperatures in Penang',
  subtitle='1946 - 2016\n',
  caption='Data source : Global Historical Climatology Network v3 temperature data version ghcnm.tavg.v3.3.0.20170707\nGraphic produced by the authors of The Jason & Doug Blog - www.jason-doug-climate.blogspot.com on August 2017')

p

# Exporting file to png

ggsave(p, file="Penang_CISL_Data_Temperature_Plot.png", width=12, height=8)
