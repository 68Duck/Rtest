library(ggplot2)
library(scatterpie)
library(ggnewscale)
source("C:\\Users\\Joshu\\Documents\\R\\Rtest\\cloropleth.R")


test <- ne_countries(scale=110, type="countries", continent="africa")
map <- cloropleth(test, test$pop_rank, "Population rank")

addPie <- function(graph, data, x, y, measures, legend_title, pie_scale) {
    data <- as.data.frame(data)
    graph <- graph + new_scale("fill") + 
        geom_scatterpie(aes_string(x = x, y = y),
         data = data, cols=measures, pie_scale = pie_scale) + 
        coord_sf()
    graph$labels$fill <- legend_title
    graph
}

# map <- addPie(map, test, "label_x", "label_y", c("name_len", "pop_rank"), "Name vs Pop", 1)
# print(map)

