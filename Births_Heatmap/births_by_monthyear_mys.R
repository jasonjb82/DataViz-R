# Load required libraries

library(ggplot2)
library(reshape)
library(lubridate)
library(scales)
library(grid)
library(viridis)
library(zoo)

# Load font
windowsFonts(Akrobat="Akrobat-ExtraBold")

# Set working directory
setwd("D:/R/DataViz-R/Births_Heatmap")

# Read csv file
df <- read.csv(file="UNdata_Export_20180428_034557857.csv",header=TRUE)

# Rename columns
colnames(df) <- c("country","year","area","month","record_type","reliability","source_yr","value","value_fn")

# Remove unwanted columns
df <- df[-c(3,5,6,7,9)]

# Remove rows with NA's
bdf <- dplyr::filter(df,  !is.na(value))

# Remove rows with totals
bdf <- subset(bdf, month!="Total")

# Reformat dates
bdf$date <- (paste(bdf$month,"-",bdf$year))
bdf$date <- as.yearmon(bdf$date, "%B - %Y")
bdf$month_new <- format(bdf$date,"%B")
bdf$monthf = factor(bdf$month, levels = rev(month.name))

# Subset years 2000 onwards
bdf$year <- as.numeric(as.character(bdf$year))
bdf_2000 <- subset(bdf,year >=2000)

# Create plot using ggplot2
v <-  ggplot(data=bdf_2000,aes(x=year,y=monthf))+
      geom_tile(aes(fill=value), colour="grey95",size=0.3)+ theme_bw()+
      scale_fill_viridis(option="B",name="No. of births\n",breaks=c(min(bdf_2000$value),max(bdf_2000$value)),labels=c("Low","High"))+
      ylab("")+
      xlab("")+
      #guides(fill = guide_legend(title = "Births", title.position = "top"))+
      scale_x_continuous(expand = c(0,0),breaks = seq(2000,2015,1)) + coord_equal(ratio=0.4)+
      labs(
      title='What was the most common birth month in Malaysia',
      subtitle='2000 - 2015\n',
      caption='Data source: United Nations Statistics Division, 2017\nGraphic produced by Jason J Benedict - www.jason-doug-climate.blogspot.com')+
      theme(text = element_text(family = "Akrobat", color = "#3A3F4A"),
      panel.background=element_rect(fill="transparent"),
      panel.border=element_blank(),
      axis.title.y=element_text(size=12,colour="grey30"),
      axis.title.x=element_text(size=12,colour="grey30"),
      axis.text.y=element_text(size=12,colour="grey30",face="bold"),
      axis.text.x=element_text(size=12,colour="grey30",face="bold"),
      axis.ticks=element_blank(),
      plot.title = element_text(lineheight=1.2, face="bold",size = 16, colour = "grey30"),
      plot.subtitle = element_text(face="bold",size = 13, colour = "grey30"),
      plot.caption=element_text(hjust=0,size=9,colour="grey30",lineheight = 1.2),
      panel.grid.major = element_blank(),
      legend.title=element_text(size=10),
      legend.title.align=0.5,
      legend.text=element_text(size=10),
      legend.position="bottom",
      legend.key.size=unit(0.3, "cm"),
      legend.key.width=unit(1, "cm"))

v

# Exporting file to png
ggsave(v, file="birth_monthyear_mys.png", width=11, height=5)
