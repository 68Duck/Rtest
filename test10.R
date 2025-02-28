source("./Rtest\\find_closest_rect.R")
source("./Rtest\\cloropleth.R")
source("./Rtest\\addBarCharts.R")


data <- ne_coastline(returnclass = "sf")

# width <- 5
# height <- 5
# small_country_area <- 10

# data <- modify_label_positions(data, small_country_area, width, height)
print(data)
# map <- cloropleth(data, data$pop_rank, "Population rank")
# # map <- add_lines_to_labels(data, map)
# map <- add_bar_charts(map, data, width, height)
  
# print(map)