library(ggplot2)
library(rnaturalearth)
library(scales)

cloropleth <- function(data, fill, legend_title) {
graph <- ggplot() + geom_sf(data=data, aes(fill = fill)) +
    theme_void() + 
    scale_color_gradient2(low = "red", high = "blue", mid = "white", midpoint = 0) 
    # +
    # coord_sf(xlim=c(-6, 2), ylim = c(50, 56), expand=FALSE)
    # scale_fill_viridis_c(option="viridis")
graph$labels$fill <- legend_title
return(graph)
}


test <- ne_countries(scale=10, type="countries", continent = "africa")
print(cloropleth(test, test$pop_rank, "Population rank"))