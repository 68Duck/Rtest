source("./Rtest\\find_closest_rect.R")
source("./Rtest\\addBarCharts.R")
source("./Rtest\\cloropleth.R")

data <- ne_countries(returnclass = "sf", scale = 10, continent = "africa")

width <- 6
height <- 6
small_country_area <- 15

data <- modify_label_positions(data, small_country_area, width, height)
map <- cloropleth(data, data$pop_rank, "Population rank")
map <- add_lines_to_labels(data, map)
map <- add_bar_charts(map, data, width, height)
  
print(map)