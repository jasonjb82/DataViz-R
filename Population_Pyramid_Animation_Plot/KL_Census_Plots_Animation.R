##################################################################################################
## Code to read and population data from data.gov.my and plot population pyramids using ggplot2 ##
## and produce animation using ImageMagick #######################################################
## Code by Jason Jon Benedict, 27th June 2017 ####################################################
## Data from Open Data Malaysia portal - www.data.gov.my #########################################
##################################################################################################

# Load libraries

library(ggplot2)
library(gtools)
library(zoo)
library(scales)
library(reshape2)
library(data.table)
library(tidyr)
library(dplyr)
library(magick)
library(purrr)
library(animation)

# Set working directory

setwd("C:/Users/Jason/Google Drive/R/Population/KL_POP")

# Read data from multiple csv files

files <- list.files(pattern = "^bppd(.*)csv$")

df <- do.call(rbind,lapply(files,read.csv))

# Rename column headers

colnames(df) <- c("year","age_group","total","male","female")

df <- df[-c(3)]

# Reformat data

df %<>% 
tidyr::gather(sex, number, -year, -age_group) %>%  
mutate(age_group = factor(age_group,
                           ordered = TRUE,
                           levels = c(" 0 - 4"," 5 - 9"," 10 - 14"," 15 - 19"," 20 - 24"," 25 - 29",
                                      " 30 - 34"," 35 - 39"," 40 - 44"," 45 - 49"," 50 - 54",
                                    " 55 - 59"," 60 - 64"," 65 - 69"," 70 - 74"," 75 - 79"," 80 - 84"," 85+")),
          number = ifelse(sex == "female", number*-1, number*1)) %>% 
          filter(year %in% c(1995:2010))

# Drop NA's

df_plot <- df %>% drop_na()

# Run loop to produce individual years plot files for creating animation
  
  for (i in c(seq(1995,2010,1))) {
    
    title <- as.character(i)
    
    mys <- filter(df_plot, year == i)

pop_plot <-
ggplot(mys, aes(x = age_group, color = sex,frame=year))+
geom_linerange(data = mys[mys$sex=="female",], 
aes(ymin = -20, ymax = -20+number), size = 3.5, alpha = 0.8)+
geom_linerange(data = mys[mys$sex=="male",], 
aes(ymin = 20, ymax = 20+number), size = 3.5, alpha = 0.8)+
geom_label(aes(x = age_group, y = 0, label = age_group, family = "Akrobat Black"), 
inherit.aes = F,size = 3.5, label.padding = unit(0.0, "lines"), label.size = 0,
label.r = unit(0.0, "lines"), fill = "#EFF2F4", alpha = 0.9, color = "#5D646F")+
scale_y_continuous(breaks = c(c(-100, -75, -50, -25, 0) + -20, c(0, 25, 50, 75, 100)+20),
labels = c("100", "75", "50", "25", "0", "0", "25", "50", "75", "100"))+
facet_wrap(~year, ncol = 1,scales = "fixed")+
coord_flip( ylim=c(-120,120))+ 
labs(title = "Population by sex and age group (in thousands)",
subtitle = "Kuala Lumpur, Malaysia",
caption = "\n\nData from Intercensal Mid-Year Population estimates published by Department of Statistics Malaysia and provided to Open Data Malaysia - data.gov.my \nVisualization by @jasonjb82")+
scale_color_manual(name = "", values = c(female = "steelblue", male = "indianred"),
labels = c("Female", "Male"))+
theme_minimal(base_family = "Akrobat Black")+
theme(text = element_text(color = "#3A3F4A"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
        axis.title = element_blank(),
        plot.title = element_text(face = "bold", size = 25, margin = margin(b = 10), hjust = 0.020),
        plot.subtitle = element_text(size = 18, margin = margin(b = 10), hjust = 0.020),
        plot.caption = element_text(size = 9, margin = margin(b = 10), hjust=0, lineheight = 1,color = "#5D646F", family="Akrobat Semibold"),
        axis.text.x = element_text(size = 12, color = "#5D646F"),
        axis.text.y = element_blank(),
        strip.text = element_text(color = "#5D646F", size = 18, face = "bold", hjust = 0.030),
        plot.background = element_rect(fill = "white"),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        legend.position = "top",
        legend.spacing  = unit(0.1, "lines"),
        legend.text  = element_text(family = "Akrobat Black", size = 14),
        legend.text.align = 0)

pop_plot

print(pop_plot)

ggsave(pop_plot,filename = paste0("plot_",i,".png"),
       width = 9,height=6,dpi = 400,type="cairo-png")

  }

# Make a GIF with ImageMagick

path.to.convert <- paste0(shortPathName("C:/Program Files/ImageMagick-6.9.3-Q16)"),"convert.exe")
ani.options(convert=path.to.convert, interval=0.5)

files <- Sys.glob("*.png")
im.convert(files, output = 'KL_POP_1995-2010.gif')

