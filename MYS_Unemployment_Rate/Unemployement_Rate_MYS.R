## Plot to create small multiples of unemployment rate for states in Malaysia compared
## to national average rates

# Load required libraries

library(data.table)
library(quantmod)
library(tidyverse)
library(lubridate)
library(scales)
library(ggthemes)
library(dplyr)

# Set working directory

setwd("C:/Users/User/Google Drive/R/Unemployment")

# Read unemployment rate data

ur <- read.csv("bptms-Unemployment_Rate.csv", stringsAsFactors = FALSE)

# Reformat data

colnames(ur) <- c("year","region","rate")

ur$year <- ymd(sprintf("%d-01-01",ur$year))

ur.my <- subset(ur,region == "Malaysia")

# create variables for use in ribbon chart

ur.dt <- as.data.table(ur)
ur.my.dt <- as.data.table(ur.my)

# Merge state and country data

ur.comb<-merge(ur.dt,ur.my.dt, by="year")

colnames(ur.comb) <- c("year","region","ur","country","ur.my")

# create variables for use in ribbon chart

ur.comb[,up:=ifelse(ur > ur.my, ur, ur.my)]
ur.comb[,down:=ifelse(ur < ur.my, ur, ur.my)]

# Drop Malaysia and states with limited data (so we can have 50 plots in small multiple)

ur.plot <- ur.comb[! region %in% c("Malaysia","W.P.Putrajaya")]

# Drop years before 1994 since there are gaps in the data

ur.plot.df <- with(ur.plot, ur.plot[(year >= "1994-01-01"),])

ends <- ur.plot.df %>%  group_by(region) %>% summarise(value=last(ur))

# Create Small Multiples

plot_ur <- ggplot(data=ur.plot.df,aes(x=year,y=ur))+
geom_line(color="black")+
geom_line(linetype=2,aes(y=ur.my))+
geom_ribbon(aes(ymin=ur,ymax=down),fill="#f2003c",alpha=0.5)+
geom_ribbon(aes(ymin=ur,ymax=up),fill="#0093af",alpha=0.5)+
scale_x_date(breaks = seq(as.Date("1995-01-01"), as.Date("2015-01-01"), by="5 years"), labels=date_format("%Y"))+
facet_wrap(~region,ncol=5,scales="free_x")+
theme_hc()+
theme(
text = element_text(family = "Lato Semibold", color = "grey20"),
axis.ticks.x = element_blank(),
strip.background = element_blank(),
strip.text = element_text(hjust = 0),
panel.grid.major = element_line(colour="grey50",size=0.35),
panel.grid.minor = element_blank(),
panel.spacing = unit(2, "lines"),
plot.margin=unit(c(0,1,0,1),"cm"),
legend.position="top",
plot.caption=element_text(hjust=1,size=9,colour="grey30"),
plot.subtitle=element_text(face="italic",size=12,colour="grey40"),
plot.title=element_text(size=18,face="bold"))+
labs(x="",y="",
title="\nThe state of Malaysia's jobs: 1995 - 2015",
subtitle="Solid line is state unemployment rate, dotted line is Malaysia's average unemployment rate\nRed (blue) indicates the state level is higher (lower) than the national average",
caption="Notes: Excludes W.P Putrajaya since data was available from 2011 onwards only. Data used here was taken from 1995 onwards as there were data gaps for years 1991 and 1994.\nSource: Department of Statistics Malaysia provided to Malaysia's Open Data Site (data.gov.my) @jasonjb82\nViz based on 'Working on a Workout' by @lenkiefer\n")+
geom_rug(aes(color=ifelse(ur>ur.my,"Worse than national average","Better than national average")),sides="b")+
scale_color_manual(values=c("#4575b4","#d73027"),name="")

plot_ur

# Save plot to png file

ggsave(plot_ur,file="MYS Unemployment Rate.png",dpi=400,w=13,h=8,type="cairo-png")
