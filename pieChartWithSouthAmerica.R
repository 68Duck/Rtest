source("./Rtest\\find_closest_rect.R")
source("./Rtest\\addBarCharts.R")
source("./Rtest\\cloropleth.R")
source("./Rtest\\addPieChart.R")


data <- ne_countries(returnclass = "sf", scale = 10, continent = "south america")

width <- 6
height <- 6
small_country_area <- 15
pie_scale <- 1.5

data <- modify_label_positions(data, small_country_area, width, height)
map <- cloropleth(data, data$pop_rank, "Population rank")
map <- add_lines_to_labels(data, map)
map <- addPie(map, data, "label_x", "label_y", c("name_len", "pop_rank"), "Name vs Pop", pie_scale)
  
print(map)