library(ggplot2)
library(cowplot)
source("./Rtest\\cloropleth.R")

test <- ne_countries(scale=110, type="countries", continent="africa")
map <- cloropleth(test, test$pop_rank, "Population rank")

addBars <- function(graph, bars, x, y, measure, legend_title, scale=0.5, width=2) {
    # print(cloropleth(bars, measure, legend_title))
    graph <- graph + 
        geom_rect(data=bars, aes(xmin = x - width / 2,
         xmax = x + width / 2, ymin = y,
          ymax = y + measure * scale, fill = measure, colour=measure))
    graph$labels$colour <- legend_title
    return(graph)
}


# map <- addBars(map, test, test$label_x, test$label_y, test$name_len, "Name Length")
# print(map)
