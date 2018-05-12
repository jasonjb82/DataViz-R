# Load libraries
library(rgdal)
library(ggplot2)
library(grid)
library(geogrid)
library(maptools)
library(viridis)
library(readxl)
library(stringr)
library(tidyr)
library(gridExtra)
library(tigris)
library(scales)
library(extrafont)

# Set working directory
setwd("D:/R/DataViz-R/PRU14")
# Load font
windowsFonts(Font="Segui UI")

# Import results from excel sheet and csv and clean up data
results <- read_excel("Election-Results-2018.xlsx", sheet = "Parlimen_Result_By_Seat")

colnames(results) <- c("par_code","name","mp","party","win_votes","pc_total_votes","majority_tot","total_votes_cand",
                       "total_votes","number_cand")

results$par_code <- str_replace_all(results$par_code, "P.", "P")

results$won_party_2018[results$party == "BN" ] <-'BN'
results$won_party_2018[results$party == "PKR" | results$party == "DAP" ] <-'Opposition'

old_results <- read.csv("parliaments_votes.csv",header=TRUE)

results_2013 <- subset(old_results,year=="2013")

results_2013$won_party_2013[results_2013$won_party_code == "UMNO" | results_2013$won_party_code == "MIC" |  results_2013$won_party_code == "MCA"| results_2013$won_party_code == "GERAKAN"] <-'BN'
results_2013$won_party_2013[results_2013$won_party_code == "DAP" | results_2013$won_party_code == "PKR" ] <-'Opposition'

# Convert wide to long
data_long <- gather(results, results, values, win_votes:total_votes, factor_key=TRUE)

# Read shapefile into R
input_file <- "D:/R/DataViz-R/PRU14/malaysia_par_2015_demographics/johor.shp"

original_shapes <- read_polygons(input_file)

raw <- read_polygons(input_file)
raw@data$xcentroid <- sp::coordinates(raw)[,1]
raw@data$ycentroid <- sp::coordinates(raw)[,2]

clean <- function(shape) {
  shape@data$id = rownames(shape@data)
  shape.points = fortify(shape, region="id")
  shape.df = merge(shape.points, shape@data, by="id")
}

# Join shapefile and tabular data
results_sdf <- geo_join(raw, results, 'par_code', 'par_code', how = 'inner')
results_comb_sdf <- geo_join(results_sdf, results_2013, 'par_code', 'parl_code', how = 'inner')

# Function to create hexagon bins from shapefile
par(mfrow = c(2, 3), mar = c(0, 0, 2, 0))
for (i in 1:6) {
  new_cells <- calculate_grid(shape = results_sdf, learning_rate = 0.01, grid_type = "hexagonal", seed = i)
  plot(new_cells, main = paste("Seed", i, sep = " "))
}

new_cells_hex <- calculate_grid(shape = results_comb_sdf, grid_type = "hexagonal", seed = 4)
resulthex <- assign_polygons(results_comb_sdf, new_cells_hex)

result_df_hex <- clean(resulthex)

result_df_hex$parliament <- str_replace_all(result_df_hex$parliament, " ", "\n")

# Generate plots
p1 <- ggplot(result_df_hex) +
      geom_polygon(aes(x = long, y = lat, fill = win_votes, group = group),colour="#f5f5f5") +
      geom_text(aes(V1, V2+0.03, label = substr(parliament, 1, 20)), size = 2, color = "grey25") +
      geom_text(aes(V1,V2-0.04,label = comma(win_votes)), size = 3, color = "grey10") +
      scale_y_continuous(label = comma)+
      scale_fill_distiller(palette="YlOrBr", na.value="#7f7f7f",name="Total",direction=1)+
      #scale_fill_viridis(option="C",direction=-1) +
      coord_equal() +
      guides(fill = FALSE) +
      theme_void()+
      labs(
      title='Number of votes of winning candidate in GE14')+
      theme(text = element_text(family="Calibri", color = "#3A3F4A",size=13,face="bold"),
      plot.title = element_text(lineheight=1.2, face="bold",size = 12, colour = "grey30",hjust = 0),
      plot.subtitle = element_text(face="bold",size = 10, colour = "grey30"))

p1

p2 <- ggplot(result_df_hex) +
      geom_polygon(aes(x = long, y = lat, fill = pc_total_votes, group = group),colour="#f5f5f5") +
      geom_text(aes(V1, V2+0.04, label = substr(parliament, 1, 20)), size = 2, color = "black") +
      geom_text(aes(V1, V2-0.04, label = paste0(round(pc_total_votes*100,0),"%")), size = 3, color = "black") +
      #scale_fill_viridis(option="D") +
      scale_fill_distiller(palette="Greens", na.value="#7f7f7f",name="Percent (%)",direction=1)+
      coord_equal() +
      guides(fill = FALSE) +
      theme_void()+
      labs(
      title='\nPercent of total votes of winning candidate in GE14')+
      theme(text = element_text(family="Akrobat", color = "#3A3F4A",size=13,face="bold"),
      plot.title = element_text(lineheight=1.2, face="bold",size = 14, colour = "grey30",hjust = 0),
      plot.subtitle = element_text(face="bold",size = 11, colour = "grey30"))

p2

p3 <- ggplot(result_df_hex) +
      geom_polygon(aes(x = long, y = lat, fill = majority_tot, group = group),colour="#f5f5f5") +
      geom_text(aes(V1, V2+0.04, label = substr(parliament, 1, 20)), size = 2, color = "black") +
      geom_text(aes(V1,V2-0.04,label = comma(majority_tot)), size = 3, color = "black") +
      scale_y_continuous(label = comma)+
      #scale_fill_viridis(option="B",direction=-1) +
      scale_fill_distiller(palette="OrRd", na.value="#7f7f7f",name="Total",direction=1)+
      coord_equal() +
      guides(fill = FALSE) +
      theme_void() +
      labs(
      title='\nNumber of majority votes of winning candidate in GE14')+
      theme(text = element_text(family="Akrobat", color = "#3A3F4A",size=13,face="bold"),
      plot.title = element_text(lineheight=1.2, face="bold",size = 14, colour = "grey30",hjust = 0),
      plot.subtitle = element_text(face="bold",size = 11, colour = "grey30"))

p3

p4 <- ggplot(result_df_hex) +
      geom_polygon(aes(x = long, y = lat, fill = total_votes_cand, group = group),colour="#f5f5f5") +
      geom_text(aes(V1, V2+0.04, label = substr(parliament, 1, 20)), size = 2, color = "grey25") +
      geom_text(aes(V1, V2-0.04, label = substr(total_votes_cand, 1, 20)), size = 3, color = "grey25") +
      #scale_fill_viridis(option="C",direction=-1) +
      scale_fill_distiller(palette="BuPu", na.value="#7f7f7f",name="Total",direction=1)+
      coord_equal() +
      guides(fill = FALSE) +
      theme_void()+
      labs(
      title='Total votes for candidates in GE14')+
      theme(text = element_text(family="Calibri", color = "#3A3F4A",size=13,face="bold"),
      plot.title = element_text(lineheight=1.2, face="bold",size = 12, colour = "grey30",hjust = 0),
      plot.subtitle = element_text(face="bold",size = 10, colour = "grey30"))
p4

## Plot and export to png file
#font_import()

cols <- c("BN" = "steelblue2", "Opposition" = "violetred1", "PAS" = "mediumseagreen")

# Set theme

p5 <- ggplot(result_df_hex) +
      geom_polygon(aes(x = long, y = lat, fill = won_party_2013, group = group),colour="#f5f5f5") +
      geom_text(aes(V1, V2, label = substr(parliament, 1, 20)), size = 2, color = "grey20") +
      scale_fill_manual(values=cols,name="") +
      #scale_fill_distiller(palette="RdPu", na.value="#7f7f7f",name="Total")+
      coord_equal() +
      #guides(fill = FALSE) +
      theme_void()+
      labs(
      title='Winning Party (coalition) in GE13',
      subtitle= "(each hexagon represents one parliament seat)")+
      theme(text = element_text(family="Akrobat", color = "#3A3F4A",size=13,face="bold"),
      plot.title = element_text(lineheight=1.2, face="bold",size = 14, colour = "grey30",hjust = 0),
      plot.subtitle = element_text(face="bold",size = 11, colour = "grey30"),
      legend.position="bottom")

p5

p6 <- ggplot(result_df_hex) +
      geom_polygon(aes(x = long, y = lat, fill = won_party_2018, group = group),colour="#f5f5f5") +
      geom_text(aes(V1, V2, label = substr(parliament, 1, 20)), size = 2, color = "grey20") +
      scale_fill_manual(values=cols,name="") +
      #scale_fill_distiller(palette="RdPu", na.value="#7f7f7f",name="Total")+
      coord_equal() +
      #guides(fill = FALSE) +
      theme_void()+
      labs(
      title='Winning Party (coalition) in GE14',
      subtitle= "(each hexagon represents one parliament seat)")+
      theme(text = element_text(family = "Akrobat", color = "#3A3F4A",size=13,face="bold"),
      plot.title = element_text(lineheight=1.2, face="bold",size = 14, colour = "grey30",hjust = 0),
      plot.subtitle = element_text(face="bold",size = 11, colour = "grey30"),
      legend.position="bottom")

p6

png(file="PRU14_Johor_Results.png",width = 10.5, height = 10.5, units = "in",
    bg = "grey95", res = 400, restoreConsole = TRUE,
    type = "cairo")

grid.arrange(p5,p6,p2,p3,nrow=2,
top = textGrob("\nWinds of change in UMNO's fort\nGE14 Parliament seat results in Johor\n",just="centre",gp=gpar(fontsize=22,fontfamily = "Akrobat",fontface="bold",col="gray30")),
bottom = textGrob("\nSource: SPR / Keith Rozario (2018) - Graphic produced by @jasonjb82\n",gp=gpar(fontsize=11, fontfamily = "Akrobat",fontface="bold",col="gray30")))
dev.off()
