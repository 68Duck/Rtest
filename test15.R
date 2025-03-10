# Load necessary libraries
library(rnaturalearth)
library(rvest)
library(dplyr)
library(ggplot2)
library(sf)  # For spatial data handling

# Load external R scripts
source("./Rtest\\find_closest_rect.R")
source("./Rtest\\addBarCharts.R")
source("./Rtest\\cloropleth.R")
source("./Rtest\\addPieChart.R")

# URL to scrape
url <- "https://en.wikipedia.org/wiki/List_of_countries_by_GDP_sector_composition"
webpage <- read_html(url)

# Extract the GDP data from the second table
table_nodes <- html_nodes(webpage, "table.wikitable")
gdp_data <- html_table(table_nodes[[2]], fill = TRUE)

# Get the country shapefile data from rnaturalearth
country_data <- ne_countries(scale = 10, type = "countries", continent = "africa", returnclass = "sf")

# Merge GDP data with country data
# Make sure that the country names in gdp_data match the "name" column in the spatial data (country_data)
data <- merge(country_data, gdp_data, by.x = "name", by.y = "Country/Economy", all.x = TRUE)

# Rename columns for easier handling
colnames(data)[colnames(data) == "Country/Economy"] <- "name"

# Plot the merged data
map <- ggplot(data) +
  geom_sf(aes(fill = data$"GDP in Services")) +  # Example of filling based on GDP in Services
  theme_minimal() +
  labs(title = "GDP Sector Composition by Country")


print(map)