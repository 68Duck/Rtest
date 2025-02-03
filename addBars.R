library(ggplot2)
library(cowplot)
library(ggnewscale)
source("./Rtest\\cloropleth.R")

test <- ne_countries(scale=110, type="countries", continent="africa")
map <- cloropleth(test, test$pop_rank, "Population rank")



addBars <- function(graph, bars, x, y, measure, legend_title, scale=0.5, width=2) {
    graph <- graph + ggnewscale::new_scale_fill() +
        geom_rect(data=data.frame(bars), aes(xmin = x - width / 2,
         xmax = x + width / 2, ymin = y,
          ymax = y + measure * scale, fill = measure)) 
    graph <- graph + scale_fill_viridis_c(option = "A")
    graph$labels$fill <- legend_title
    return(graph)
}


map <- addBars(map, test, test$label_x, test$label_y, test$name_len, "Name Length")
print(map)
