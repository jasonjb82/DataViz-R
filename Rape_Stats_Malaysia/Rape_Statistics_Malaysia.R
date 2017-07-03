# Load libraries

library(ggplot2)
library(reshape2)
library(ggthemes)
library(directlabels)
library(stringr)

# Set working directory

setwd("C:/Users/User/Google Drive/R/Crime/")

# Define data

rapes <- data.frame(age_group = c('Below 6', '6-9', '10-12', '13-15', '16-18', '18+'),t2000 = c(26,36,64,477,221,393),t2014 = c(15,45,97,1086,586,520))

# Reformat data

rapes <- melt(rapes, id.vars = "age_group", variable.name = "year", value.name = "rapes")
rapes$year <- as.numeric(substr(rapes$year, 2,5))


# Plot using ggplot

g <- ggplot(data = rapes, aes(x = year, y = rapes, color = age_group)) + geom_line(size = 1.2, alpha = 0.7) + geom_point(size=3)+
scale_color_manual(values = c("Below 6" = "deepskyblue1", '6-9' = "deepskyblue2", '10-12' = "deepskyblue3",'13-15' = "firebrick2", '16-18' = "deepskyblue4", '18+' = "dodgerblue1"))+
scale_x_continuous(breaks = c(2000, 2014), limits = c(2000, 2014.5), expand = c(0, 1)) + 
scale_y_continuous(breaks = c(250,500,750,1000)) + 
annotate("text", x = 2000, y = 1200, label = "Rape cases reported ", fontface = "bold",hjust = 0.2,family = "Officina Sans ITC Book", size=4,color = "#3A3F4A") +
annotate("text", x = 2013.5, y = 1200, label = "Age Group", fontface = "bold", hjust = .2,family = "Officina Sans ITC Book",size=4,color = "#3A3F4A") +
theme(legend.position = "none", plot.background = element_rect(fill = "white"), panel.background = element_rect(fill = "white")) +
geom_dl(aes(label = age_group, x = year + 0.25), method = "last.qp", cex = 0.25) +
labs(title="Rape statistics by age group for Malaysia",
subtitle="Rape statistics according to age (2000-2015) from Malaysian police provided by\nHome Minister in parliamentary reply to MP Kasthuriraani a/p Patto.
2015 data is partial.\n
Displayed below is the change in numbers from 2000 to 2014 only\n",
caption='\nData sourced from Sinar Project, http://sinarproject.org',family = "Officina Sans ITC Book",size=13)  +
  
  
# Define theme
  
theme(
text = element_text(family = "Officina Sans ITC Book", color = "#3A3F4A"),
panel.grid.major.y=element_line(size=0.25,linetype = "dashed",color = "#3A3F4A"),
panel.background = element_rect(fill = "snow",colour = NA),
plot.background = element_rect(fill = "snow",colour = NA),
axis.ticks.x = element_line(size = 1,color = "#3A3F4A"),
axis.ticks.y = element_blank(),
axis.ticks.length=unit(0.3,"cm"),
axis.text.x = element_text(family = "Officina Sans ITC Book",size=12,color = "#3A3F4A"),
axis.text.y = element_text(family = "Officina Sans ITC Book",size=12,color = "#3A3F4A"),
axis.line.x = element_line(colour="#3A3F4A",size=1.2),
axis.line.y=element_blank(),
panel.grid.major = element_blank(),
panel.grid.minor = element_blank(),
axis.title.x = element_blank(),
axis.title.y = element_blank()
)
  
g

# Save file to png format

ggsave(g,file="Rape Malaysia 2000 to 2014.png",dpi=400,w=5,h=6,type="cairo-png")
