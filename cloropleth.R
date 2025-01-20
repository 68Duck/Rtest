library(ggplot2)
library(rnaturalearth)

cloropleth <- function(data, fill, legend_title) {
graph <- ggplot() + geom_sf(data=data, aes(fill = fill)) +
    theme_void() + scale_fill_viridis_c(option="default")
graph$labels$fill <- legend_title
return(graph)
}


test <- ne_countries(scale=110, type="countries", continent="africa")
# print(cloropleth(test, test$name_len, "Name Length"))
# print(test)
print(cloropleth(test, test$pop_rank, "Population rank"))