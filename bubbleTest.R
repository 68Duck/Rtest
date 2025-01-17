library(ggplot2)
library(dplyr)
source("./Rtest\\cloropleth.R")

library(giscoR)
UK <- gisco_get_countries(country = "UK", resolution = 1)
library(maps)
data <-  world.cities %>% filter(country.etc == "UK") 

addPoints <- function(graph, points, x, y, measure, legend_title) {
graph <- graph + 
    geom_point(data = points, aes(x = x, y = y, size = measure, colour = measure)) +
    scale_size_continuous(range = c(1, 12)) +
    scale_color_viridis_c(trans = "log")
graph$labels$colour <- legend_title
graph$labels$size <- legend_title

return(graph)
}
print(addPoints(cloropleth(UK, UK$pop, "Population"), data, data$long, data$lat, data$pop, "Population"))


# test <- ne_countries(scale=110, type="countries", continent="africa")
# # # print(cloropleth(test, test$name_len, "Name Length"))
# c <- cloropleth(test, test$pop_rank, "Population rank")
# print(addPoints(c, test, test$label_x, test$label_y, test$name_len, "Name length"))