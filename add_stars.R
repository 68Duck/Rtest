library(ggplot2)
library(rnaturalearth)
library(sf)
library(ggnewscale)
library(ggstar)
source("C:\\Users\\Joshu\\Documents\\R\\Rtest\\cloropleth.R")


# df <- ne_countries(scale = "large", type = "countries", continent = "south america")
# map <- cloropleth(df, df$pop_rank, "Population rank")

add_stars <- function(map, df, width, height, star_size) {

  values <- sample(1:5, nrow(df), replace=T)
  for (i in 1:nrow(df)) {
    data <- data.frame(
        x = c(1, 2, 3, 4, 5),
        y = c(1, 1, 1, 1, 1),
        colours = c(rep("yellow", each = values[i]),
         rep("grey", each = (5 - values[i])))
    )

    map <- build_star_layer(map, df[i, , drop = FALSE], data, width, height, star_size)
  }
    
  return(map)
}

build_star_layer <- function(map, df, data, width, height, star_size) {
  
  points <- ggplot(data, aes(x = x, y = y, color = colours, fill = colours)) +
    geom_star(stat = "identity", size=star_size) +
    scale_fill_manual(values = c("yellow" = "yellow", "grey" = "grey")) +
    scale_color_manual(values = c("yellow" = "yellow", "grey" = "grey")) +
    theme_minimal() +
    theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),legend.position="none",
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank())
  
  point_grob <- ggplotGrob(points)

  map <- map + annotation_custom(grob = point_grob, xmin = df$label_x - width / 2, xmax = df$label_x + width / 2, ymin = df$label_y - height / 2, ymax = df$label_y + height / 2)
  return(map)
}
  
# width <- 10
# height <- 8
# star_size <- 3

# map <- add_stars(map, df, width, height, star_size)
# print(map)
