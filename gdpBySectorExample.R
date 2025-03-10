library(rnaturalearth)
library(rvest)

source("./Rtest\\find_closest_rect.R")
source("./Rtest\\addBarCharts.R")
source("./Rtest\\cloropleth.R")
source("./Rtest\\addPieChart.R")
source("./Rtest\\utils.R")

url <- "https://en.wikipedia.org/wiki/List_of_countries_by_GDP_sector_composition"
webpage <- read_html(url)
table_nodes <- html_nodes(webpage, "table.wikitable")
gdp_data <- html_table(table_nodes[[2]], fill = TRUE)

country_data <- ne_countries(scale=10, type="countries", continent = "africa", returnclass = "sf")

# data <- merge_data(country_data, gdp_data, "name", "Country/Economy")
data <- merge_data_with_country_matching(country_data, gdp_data, "name", "Country/Economy")
data <- convert_columns_to_number(data, c("Agricultural (%)", "Industrial (%)", "Service (%)"), c("%"))

width <- 6
height <- 6
small_country_area <- 15
pie_scale <- 1.2

data <- modify_label_positions(data, small_country_area, width, height)
map <- cloropleth(data, data$pop_rank, "Population rank")
map <- add_lines_to_labels(data, map)
map <- addPie(map, data, "label_x", "label_y",
 c("Agricultural (%)", "Industrial (%)", "Service (%)"),
 "Gdp by sector", pie_scale)
  
print(map)
