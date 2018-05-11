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

setwd("D:/R/DataViz-R/PRU14")
# Load font
windowsFonts(Font="Segui UI")

# Import results from excel sheet

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

results_sdf <- geo_join(raw, results, 'par_code', 'par_code', how = 'inner')
results_comb_sdf <- geo_join(results_sdf, results_2013, 'par_code', 'parl_code', how = 'inner')

par(mfrow = c(2, 3), mar = c(0, 0, 2, 0))
for (i in 1:6) {
  new_cells <- calculate_grid(shape = results_sdf, learning_rate = 0.01, grid_type = "hexagonal", seed = i)
  plot(new_cells, main = paste("Seed", i, sep = " "))
}

new_cells_hex <- calculate_grid(shape = results_comb_sdf, grid_type = "hexagonal", seed = 4)
resulthex <- assign_polygons(results_comb_sdf, new_cells_hex)

result_df_hex <- clean(resulthex)

nrow(results_sdf)

# Generate plots

p1 <- ggplot(result_df_hex) +
      geom_polygon(aes(x = long, y = lat, fill = win_votes, group = group),colour="black") +
      geom_text(aes(V1, V2+0.03, label = substr(parliament, 1, 20)), size = 2, color = "white") +
      geom_text(aes(V1, V2-0.03, label = substr(win_votes, 1, 20)), size = 3, color = "white") +
      #scale_fill_distiller(palette="BuGn", na.value="#7f7f7f",name="Total")+
      scale_fill_viridis(option="D",direction=-1) +
      coord_equal() +
      guides(fill = FALSE) +
      theme_void()+
      labs(
      title='  Votes of winning candidate in PRU14')+
      theme(text = element_text(family = "Font", color = "#3A3F4A"))


p1

p2 <- ggplot(result_df_hex) +
      geom_polygon(aes(x = long, y = lat, fill = pc_total_votes, group = group),colour="black") +
      geom_text(aes(V1, V2+0.03, label = substr(parliament, 1, 20)), size = 2, color = "white") +
      geom_text(aes(V1, V2-0.03, label = substr(pc_total_votes, 1, 20)), size = 3, color = "white") +
      #scale_fill_viridis() +
      scale_fill_distiller(palette="RdYlGn", na.value="#7f7f7f",name="Percent (%)")+
      coord_equal() +
      guides(fill = FALSE) +
      theme_void()+
      labs(
      title='  Percent of total votes in PRU14')+
      theme(text = element_text(family = "Font", color = "#3A3F4A"))

p2


p3 <- ggplot(result_df_hex) +
      geom_polygon(aes(x = long, y = lat, fill = majority_tot, group = group),colour="black") +
      geom_text(aes(V1, V2+0.03, label = substr(parliament, 1, 20)), size = 2, color = "white") +
      geom_text(aes(V1, V2-0.03, label = substr(majority_tot, 1, 20)), size = 3, color = "white") +
      scale_fill_viridis(option="B",direction=-1) +
      #scale_fill_distiller(palette="YlOrBr", na.value="#7f7f7f",name="Total")+
      coord_equal() +
      guides(fill = FALSE) +
      theme_void() +
      labs(
      title=' Majority votes of winning candidate in PRU14')+
      theme(text = element_text(family = "Font", color = "#3A3F4A"))


p3

p4 <- ggplot(result_df_hex) +
      geom_polygon(aes(x = long, y = lat, fill = total_votes_cand, group = group),colour="black") +
      geom_text(aes(V1, V2+0.03, label = substr(parliament, 1, 20)), size = 2, color = "white") +
      geom_text(aes(V1, V2-0.03, label = substr(total_votes_cand, 1, 20)), size = 3, color = "white") +
      scale_fill_viridis(option="C",direction=-1) +
      #scale_fill_distiller(palette="RdPu", na.value="#7f7f7f",name="Total")+
      coord_equal() +
      guides(fill = FALSE) +
      theme_void()+
      labs(
      title='  Total votes for candidates in PRU14')+
      theme(text = element_text(family = "Font", color = "#3A3F4A"))


p4


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


## Plot and export to png file

png(file="PRU14_Perak_Results.png",width = 10, height = 11, units = "in",
    bg = "white", res = 400, family = "", restoreConsole = TRUE,
    type = "cairo")

grid.arrange(p1,p2,p4,p3,nrow=2,
top = textGrob("\nPRU14 PARLIAMENT RESULTS - PERAK\n",gp=gpar(fontsize=20,font=1)))

dev.off()

cols <- c("BN" = "steelblue2", "Opposition" = "violetred1", "PAS" = "mediumseagreen")

p5 <- ggplot(result_df_hex) +
      geom_polygon(aes(x = long, y = lat, fill = won_party_2013, group = group),colour="black") +
      geom_text(aes(V1, V2, label = substr(parliament, 1, 20)), size = 2, color = "white") +
      #geom_text(aes(V1, V2-0.03, label = substr(total_votes_cand, 1, 20)), size = 3, color = "white") +
      scale_fill_manual(values=cols,name="") +
      #scale_fill_distiller(palette="RdPu", na.value="#7f7f7f",name="Total")+
      coord_equal() +
      #guides(fill = FALSE) +
      theme_void()+
      labs(
      title='  Winning party (coalition) in PRU13')+
      theme(text = element_text(family = "Font", color = "#3A3F4A"),
      legend.position="bottom")

p5

p6 <- ggplot(result_df_hex) +
      geom_polygon(aes(x = long, y = lat, fill = won_party_2018, group = group),colour="black") +
      geom_text(aes(V1, V2, label = substr(parliament, 1, 20)), size = 2, color = "white") +
      #geom_text(aes(V1, V2-0.03, label = substr(total_votes_cand, 1, 20)), size = 3, color = "white") +
      scale_fill_manual(values=cols,name="") +
      #scale_fill_distiller(palette="RdPu", na.value="#7f7f7f",name="Total")+
      coord_equal() +
      #guides(fill = FALSE) +
      theme_void()+
      labs(
      title='  Winning party (coalition) in PRU14')+
      theme(text = element_text(family = "Font", color = "#3A3F4A"),
      legend.position="bottom")


p6


png(file="PRU14_Johor_Results.png",width = 10, height = 11, units = "in",
    bg = "white", res = 400, restoreConsole = TRUE,
    type = "cairo")

grid.arrange(p5,p6,p1,p3,nrow=2,
top = textGrob("\nThe winds of change in Johor\nThe numbers behind the Oppositions capture of the UMNO heartland\n",gp=gpar(fontsize=20,fontfamily = "Font")),
bottom = textGrob("\nSource: SPR / Keith Rozario (2018) - Graphic produced by @jasonjb82\n",gp=gpar(fontsize=10,fontfamily = "Font",font=3)))
dev.off()

