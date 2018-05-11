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

setwd("D:/R/DataViz-R/PRU14")
# Load font
windowsFonts(Font="Open Sans")

input_file <- "D:/R/DataViz-R/PRU14/malaysia_par_2015_demographics/perak.shp"

original_shapes <- read_polygons(input_file)

raw <- read_polygons(input_file)
raw@data$xcentroid <- sp::coordinates(raw)[,1]
raw@data$ycentroid <- sp::coordinates(raw)[,2]

clean <- function(shape) {
  shape@data$id = rownames(shape@data)
  shape.points = fortify(shape, region="id")
  shape.df = merge(shape.points, shape@data, by="id")
}

result_df_raw <- clean(results_sdf)
rawplot <- ggplot(result_df_raw) +
  geom_polygon(aes(x = long, y = lat, fill = cartodb_id, group = group)) +
  #geom_text(aes(xcentroid, ycentroid, label = substr(NAME, 1, 4)), size = 2,color = "white") +
  coord_equal() +
  scale_fill_viridis() +
  guides(fill = FALSE) +
  theme_void()

rawplot

par(mfrow = c(2, 3), mar = c(0, 0, 2, 0))
for (i in 1:6) {
  new_cells <- calculate_grid(shape = results_sdf, learning_rate = 0.01, grid_type = "hexagonal", seed = i)
  plot(new_cells, main = paste("Seed", i, sep = " "))
}

new_cells_hex <- calculate_grid(shape = results_sdf, grid_type = "hexagonal", seed = 2)
resulthex <- assign_polygons(results_sdf, new_cells_hex)

result_df_hex <- clean(resulthex)

# Import results from excel sheet

results <- read_excel("Election-Results-2018.xlsx", sheet = "Parlimen_Result_By_Seat")

colnames(results) <- c("par_code","name","mp","party","win_votes","pc_total_votes","majority_tot","total_votes_cand",
                       "total_votes","number_cand")

results$par_code <- str_replace_all(results$par_code, "P.", "P")

# Convert wide to long

data_long <- gather(results, results, values, win_votes:total_votes, factor_key=TRUE)

library(writexl)
outfile = "D:/File.xlsx"
write_xlsx(data_long,path="D:/File.xlsx")
write_xlsx(result_df_hex,path="D:/File1.xlsx")

library(tigris)
results_sdf <- geo_join(raw, results, 'par_code', 'par_code', how = 'inner')


nrow(results_sdf)

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
      title='Votes of Winning Candidate')+
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
      title='Percent of Total Votes')+
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
      title='Majority Total')+
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
      title='Total Votes for Candidates')+
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

cols <- c("BN" = "darkblue", "PKR" = "red", "PAS" = "darkgreen")

p5 <- ggplot(result_df_hex) +
      geom_polygon(aes(x = long, y = lat, fill = party, group = group),colour="black") +
      geom_text(aes(V1, V2+0.03, label = substr(parliament, 1, 20)), size = 2, color = "white") +
      geom_text(aes(V1, V2-0.03, label = substr(total_votes_cand, 1, 20)), size = 3, color = "white") +
      scale_fill_manual(values=cols) +
      #scale_fill_distiller(palette="RdPu", na.value="#7f7f7f",name="Total")+
      coord_equal() +
      guides(fill = FALSE) +
      theme_void()+
      labs(
      title='Total Votes for Candidates')+
      theme(text = element_text(family = "Font", color = "#3A3F4A"))


p5
