library(rnaturalearth)
library(rvest)

source("./Rtest\\find_closest_rect.R")
source("./Rtest\\addBarCharts.R")
source("./Rtest\\cloropleth.R")
source("./Rtest\\addPieChart.R")

url <- "https://en.wikipedia.org/wiki/List_of_countries_by_GDP_sector_composition"

webpage <- read_html(url)

table_nodes <- html_nodes(webpage, "table.wikitable")
gdp_data <- html_table(table_nodes[[2]], fill = TRUE)


country_data <- ne_countries(scale=10, type="countries", continent = "africa", returnclass = "sf")

data <- merge(gdp_data, country_data, by.x = "Country/Economy", by.y="name", all = FALSE)
# print(data$"Country/Economy")
colnames(data)[colnames(data) == "Country/Economy"] <- "name"
data <- st_as_sf(data)
# print(data)
print(colnames(data))
# print(data$pop_rank)

# data$"Agricultural (%)" <- as.numeric(gsub(",", "", data$"Agricultural (%)"))
# data$"Industrial (%)" <- as.numeric(gsub(",", "", data$"Industrial (%)"))
# data$"Service (%)" <- as.numeric(gsub(",", "", data$"Service (%)"))

data$`Agricultural (%)` <- as.numeric(gsub(",", "", gsub("%", "", data$`Agricultural (%)`)))
data$`Industrial (%)` <- as.numeric(gsub(",", "", gsub("%", "", data$`Industrial (%)`)))
data$`Service (%)` <- as.numeric(gsub(",", "", gsub("%", "", data$`Service (%)`)))
print(data)
print(data$"Agricultural (%)")
# map <- ggplot(data) + geom_sf()
# print(map)

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
