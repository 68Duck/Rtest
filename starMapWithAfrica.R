source("./Rtest\\find_closest_rect.R")
source("./Rtest\\cloropleth.R")
source("./Rtest\\add_stars.R")


data <- ne_countries(returnclass = "sf", scale = 10, continent = "africa")

width <- 8
height <- 6
small_country_area <- 10

data <- modify_label_positions(data, small_country_area, width, height)
map <- cloropleth(data, data$pop_rank, "Population rank")
map <- add_lines_to_labels(data, map)
map <- add_stars(map, data, width, height)
# map <- addPie(map, data, "label_x", "label_y", c("name_len", "pop_rank"), "Name vs Pop", pie_scale)
  
print(map)