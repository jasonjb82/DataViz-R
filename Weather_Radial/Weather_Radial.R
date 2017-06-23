############################################################################
### Code to read data from Weather Underground and produce radial plots ####
############################################################################

# Load required libraries

library(dplyr)
library(lubridate)
library(weatherData)
library(ggplot2)
library(viridis)
library(scales)
library(grid)
library(gtable)

# Set working directory

setwd("C:/Users/Jason/Google Drive/R")


# Theme for weather radial

theme_wr <- theme_minimal()+
        theme(text = element_text(family = "Officina Sans ITC Book", color = "#3A3F4A"),
        panel.grid = element_line(linetype = 'solid', color = 'black', size = 0.75),
        axis.title = element_blank(),
        axis.text.x = element_text(size = 16, vjust =10),
        axis.text.y = element_blank(),
        legend.position = c(0.5, 0.47),
        legend.direction = 'horizontal',
        legend.key.height = unit(4, "pt"),
        legend.key.width = unit(40, "pt"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 15),
        legend.title.align = 0.5,
        plot.background = element_rect(fill = "#EFF2F4",colour = NA),
        plot.margin = unit(c(1, 1, 1, 1), "cm"))


# Produce temperature labels

lbls = data.frame(x = rep(as.Date('2016-07-01'),5), y = seq(20,40,5), label = seq(20,40,5))

# Download data from Weather Underground using the weatherData package and reformat data accordingly

df_2016 <- getWeatherForDate("WMKP", "2016-01-01", end_date="2016-12-31",opt_all_columns = TRUE)

df_2016$date <- as.Date(df_2016$Date)

df_2016$Precipitationmm[df_2016$Precipitationmm == "0"] <- NA

# Plot using ggplot2

p2 <- 
ggplot(df_2016)+
geom_hline(yintercept = 0, color = 'gray', size = 0.5)+
geom_linerange(aes(x = date, ymin = Min_TemperatureC, ymax = Max_TemperatureC, color = Mean_TemperatureC), size = 1)+
geom_text(aes(x = as.Date('2016-01-01'), y = 12, label = 'Penang Island',
family = 'Officina Sans ITC Book'), size = 13, color = "#3A3F4A")+
geom_text(aes(x = as.Date('2016-01-01'), y = 8, label = 'Weather in 2016',
family = 'Officina Sans ITC Book'), size = 10, color = "#3A3F4A")+
geom_text(aes(x = as.Date('2016-07-01'), y = 16, label = 'Data: Weather Underground',
family = 'Officina Sans ITC Book'), size = 4.5, color = "#3A3F4A")+
geom_point(data=df_2016,aes(x=date, y=38, size=Precipitationmm,fill=Precipitationmm),alpha=0.25,colour="deepskyblue2")+
scale_fill_gradient(guide=FALSE)+
scale_size_area(max_size = 15, breaks = seq(0,60,20),guide = guide_legend(title = "Precipitation in mm's"))+
scale_color_viridis(option = 'plasma', end = 1, 
limits = c(20,40), breaks = seq(20,40,5), labels = as.character(seq(20,40,5)))+
scale_y_continuous(limits = c(0,40),breaks = seq(20,30,40), expand = c(0, 0))+
scale_x_date(date_breaks = '1 month', labels = c('January', 'February', 'March', 'April', 'May', 'June',
'July', 'August', 'September', 'October', 'November', 'December', ''))+
coord_polar()+
geom_text(data = lbls, aes(x = x, y = y, label = label,family = "Officina Sans ITC Book"), size = 4, color = "#3A3F4A", hjust = 0.5)+
guides(color = guide_colorbar(title = expression("Temperature in"*~degree*C), raster = F, title.position = "top"),
size = guide_legend(title="Precipitation in mm's",title.position = "top"))+
theme_wr
  
p2

# Save to png file

ggsave(p2,file="Weather Radial.png",dpi=400,w=12,h=12,type="cairo-png")


