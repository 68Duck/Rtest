library(ggplot2)
library(rnaturalearth)
library(sf)
library(ggnewscale)
source("./Rtest\\cloropleth.R")

df <- ne_countries(scale = 110, type = "countries", continent = "africa")
map <- cloropleth(df, df$pop_rank, "Population rank")

add_bar_charts <- function(map, df, width, height) {
  for (i in 1:nrow(df)) {
    map <- build_layer(map, df[i, , drop = FALSE], width, height)
  }
  return(map)
}

build_layer <- function(map, df, width, height) {
  data <- data.frame(
    Category = c("name len", "long len", "abbrev len"),
    Value = c(df$name_len, df$long_len, df$abbrev_len)
  )
  
  bar_chart <- ggplot(data, aes(x = Category, y = Value)) +
    geom_bar(stat = "identity", fill = "lightblue") +
    theme_minimal() +
    theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
  
  bar_grob <- ggplotGrob(bar_chart)
  
  map <- map + annotation_custom(grob = bar_grob, xmin = df$label_x - width / 2, xmax = df$label_x + width / 2, ymin = df$label_y - height / 2, ymax = df$label_y + height / 2)
  
  return(map)
}

map <- add_bar_charts(map, df, 5, 5)
print(map)
