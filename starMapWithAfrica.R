source("./Rtest\\find_closest_rect.R")
source("./Rtest\\cloropleth.R")
source("./Rtest\\add_stars.R")


data <- ne_countries(returnclass = "sf", scale = 110, continent = "europe")

width <- 8
height <- 6
small_country_area <- 10
star_size <- 2.5

data <- modify_label_positions(data, small_country_area, width, height)
map <- cloropleth(data, data$pop_rank, "Population rank")
map <- add_lines_to_labels(data, map)
map <- add_stars(map, data, width, height, star_size)
  
print(map)