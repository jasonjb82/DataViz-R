###########################################################################################
### Code to import Dengue Hotspot data from Open Data Malaysia and create map animation ###
### Produced by Jason Jon Benedict on 7th June 2017 #######################################
###########################################################################################

library(ggplot2)
library(readxl)
library(animation)
library(ISOweek)
library(RDSTK)
library(lubridate)
library(zoo)
library(dplyr)
library(ggmap)
library(magrittr)
library(raster)
library(viridis)
library(ggrepel)


# Set working directory

setwd("C:/Users/Jason/Google Drive/R/Dengue")

# Read data from spreadsheet and reformatting data

denggi <- read_excel("mohdengguehotspot20102014v3.xlsx", sheet ="HOT SPOTS",skip=1)

colnames(denggi) <- c("year","week","state","district","locality","no_cases","days")

denggi_data <- denggi[,c("year","week","state","district","locality","no_cases","days")]

# Extracting data for Selangor and year 2014

dat <- subset(denggi_data, state == "Selangor" & year == 2014)

dat$weeknew <- sprintf("%02d",dat$week)

dat$weekyear <- as.character(paste(dat$year, dat$weeknew, sep = "-"))

dat$otherdates <- sub("(\\d{4}-)(\\d{2})", "\\1W\\2-1", dat$weekyear)

dat$dates <- ISOweek2date(dat$otherdates)
dat$dates2  = dat$dates  + days(2)

dat$month <- as.numeric(as.POSIXlt(dat$dates2)$mon+1)

dat$locality <- gsub("\\(|\\)", "", dat$locality)

# Cleaning up addresses

dat$address <- as.character(paste(dat$locality,dat$district,dat$state,sep= " "))
dat$address <- gsub('Jln', 'Jalan', dat$address)
dat$address <- gsub('Tmn', 'Taman', dat$address)
dat$address <- gsub('Kg', 'Kampung', dat$address)
dat$address <- gsub('Sg', 'Sungai', dat$address)
dat$address <- gsub('Btg', 'Batang', dat$address)
dat$address <- gsub('Apt', 'Apartment', dat$address)
dat$address <- gsub('Appt', 'Apartment', dat$address)
dat$address <- gsub('Bdr', 'Bandar', dat$address)
dat$address <- gsub('Bch', 'Bandar Country Homes', dat$address)
dat$address <- gsub('Ppr', 'Program Perumahan Rakyat', dat$address)

write.csv(dat,file="denggi_selangor.csv",row.names=FALSE)

## Split data frames for geocoding

dat1 <- dat[1:1000,]
dat2 <- dat[1001:2000,]
dat3 <- dat[2001:3000,]
dat4 <- dat[3001:4000,]
dat5 <- dat[4001:4076,]

#write.csv(dat1,file="denggi_selangor1.csv",row.names=FALSE)

# Loop through the addresses to get the latitude and longitude of each address and add it to the data frames
# Export data frames to csv files

# Data frame 1

for(i in 1:nrow(dat1))
{
  # Print("Working...")
  result <- geocode(dat1$address[i], output = "latlona", source = "google")
  dat1$lon[i] <- as.numeric(result[1])
  dat1$lat[i] <- as.numeric(result[2])
}

write.csv(dat1,file="dat1.csv",row.names=FALSE)

# Data frame 2

for(i in 1:nrow(dat2))
{
  # Print("Working...")
  result <- geocode(dat2$address[i], output = "latlona", source = "google")
  dat2$lon[i] <- as.numeric(result[1])
  dat2$lat[i] <- as.numeric(result[2])
}

write.csv(dat2,file="dat2.csv",row.names=FALSE)

# Data frame 3

for(i in 1:nrow(dat3))
{
  # Print("Working...")
  result <- geocode(dat3$address[i], output = "latlona", source = "google")
  dat3$lon[i] <- as.numeric(result[1])
  dat3$lat[i] <- as.numeric(result[2])
}

write.csv(dat3,file="dat3.csv",row.names=FALSE)


# Data frame 4

for(i in 1:nrow(dat4))
{
  # Print("Working...")
  result <- geocode(dat4$address[i], output = "latlona", source = "google")
  dat4$lon[i] <- as.numeric(result[1])
  dat4$lat[i] <- as.numeric(result[2])
}

write.csv(dat4,file="dat4.csv",row.names=FALSE)


# Data frame 5

for(i in 1:nrow(dat5))
{
  # Print("Working...")
  result <- geocode(dat5$address[i], output = "latlona", source = "google")
  dat5$lon[i] <- as.numeric(result[1])
  dat5$lat[i] <- as.numeric(result[2])
}

write.csv(dat5,file="dat5.csv",row.names=FALSE)


# Plot hotspots

# Read data from csv files

files <- list.files(pattern = "^dat(.*)csv$")

df <- do.call(rbind,lapply(files,read.csv))

df$yearmonth <- as.yearmon(df$dates2)

# Define theme for map

theme_map <- theme(text = element_text(family = "Akrobat Black", color = "#3A3F4A"),
                  panel.background = element_blank(), 
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  axis.line = element_blank(),
                  axis.text.x = element_blank(),
                  axis.text.y = element_blank(),
                  axis.ticks = element_blank(),
                  axis.title.x = element_blank(),
                  axis.title.y = element_blank(),
                  plot.caption=element_text(hjust=1,size=6,colour="grey30",lineheight = 0.5),
                  plot.subtitle=element_text(face="italic",size=12,colour="grey40"),
                  plot.title=element_text(size=18,face="bold"),
                  legend.position = c(0.10, 0.15),
                  legend.direction = 'vertical',
                  legend.key.height = unit(10, "pt"),
                  legend.key.width = unit(10, "pt"),
                  legend.text = element_text(size = 8),
                  legend.title = element_text(size = 10),
                  legend.title.align = 0.5,
                  legend.background = element_blank())

# Get MYS administrative boundaries from GADM

states    <- c('Selangor')


# Get location of major cities

labels <- data.frame(city=c("Kuala Lumpur", "Shah Alam","Klang", "Subang Jaya", "Petaling Jaya",
                            "Kajang","Kuala Selangor","Putrajaya","Puchong","Batang Kali","Gombak",
                            "Banting","Rawang","Sungai Besar","Hulu Langat","Kuala Kubu Bahru"))
labels <- cbind(geocode(as.character(labels$city)), labels)


# Extracting basemap from GADM

mys <-getData("GADM", country="MY", level=1)
map <- mys[mys$NAME_1 %in% states,]

# Plotting hexbin map using ggplot

summary.hexmap <- 
ggplot()+
geom_polygon(data=map, aes(x=long,y=lat,group=group),colour="grey90",fill="grey90",size=1.2)+ 
geom_polygon(data=map, aes(x=long,y=lat,group=group),colour="grey50",fill="grey50",size=0.001)+ 
stat_summary_hex(aes(x = lon, y = lat, z = no_cases,
fill = cut(..value.., c(0, 100, 250, 500, 1000,
1500, 2000, 2500, Inf))),fun = sum,
colour = NA, bins = 50, alpha = 0.75,
data = df)+
geom_text_repel(data=labels, aes(x = lon, y = lat, label = city), col = "black", size = 2.25, segment.color = NA,family = "Arial Narrow") +
coord_equal()+
scale_fill_brewer(palette = "YlOrRd",
labels = c("<100", "100-250", "250-500",
"500-1000", "1000-1500","1500-2000", "2000-2500",">2500")) +
xlim(100.8, 102) + ylim(2.5, 4)+
labs(title='Dengue hotspots',
subtitle='for Selangor, Malaysia in year 2014',
caption='Notes: Only approx 85% of dengue case locations were geocoded due to erronous addresses in the dataset\n
Data published by Ministry of Health Malaysia and provided to Open Data Malaysia - data.gov.my @jasonjb82')+ theme_map +
guides(fill = guide_legend(title = "No of cases", raster = F, title.position = "top"))

summary.hexmap

# Save map to png file

ggsave(summary.hexmap,file="Dengue_Selangor_Map_2014.png",dpi=400,w=4.5,h=6,type="cairo-png")
