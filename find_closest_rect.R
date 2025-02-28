library(rnaturalearth)
library(rnaturalearthhires)
library(sf)
library(dplyr)
library(ggplot2)
source("./Rtest\\cloropleth.R")

# Load African countries data
world <- ne_countries(returnclass = "sf", scale = 10, continent = "africa")


find_closest_rects <- function(data, small_area, width, height) {

  country_bboxes <- data %>%
    mutate(
      bbox = purrr::map(geometry, st_bbox), 
      xmin = purrr::map_dbl(bbox, 1),  
      ymin = purrr::map_dbl(bbox, 2),  
      xmax = purrr::map_dbl(bbox, 3),  
      ymax = purrr::map_dbl(bbox, 4)   
    ) %>%
    select(name, xmin, ymin, xmax, ymax)


  country_bboxes$area <- (country_bboxes$xmax - country_bboxes$xmin) * (country_bboxes$ymax - country_bboxes$ymin) 


  bounding_box <- st_bbox(data)
  bbox_polygon <- st_as_sfc(bounding_box)
  world_valid <- st_make_valid(data)
  world_polygon <- st_union(world_valid)
  non_intersecting_geometry <- st_difference(bbox_polygon, world_polygon)
  non_intersecting_bbox <- st_bbox(non_intersecting_geometry)

  xmin_values <- seq(non_intersecting_bbox["xmin"], non_intersecting_bbox["xmax"] - width, by = width)
  ymin_values <- seq(non_intersecting_bbox["ymin"], non_intersecting_bbox["ymax"] - height, by = height)

  rectangles <- expand.grid(xmin = xmin_values, ymin = ymin_values)
  rectangles <- rectangles %>%
    mutate(xmax = xmin + width, ymax = ymin + height)

  rectangles_sf <- st_sfc(lapply(1:nrow(rectangles), function(i) {
    st_polygon(list(matrix(c(
      rectangles$xmin[i], rectangles$ymin[i],
      rectangles$xmin[i] + width, rectangles$ymin[i],
      rectangles$xmin[i] + width, rectangles$ymin[i] + height,
      rectangles$xmin[i], rectangles$ymin[i] + height,
      rectangles$xmin[i], rectangles$ymin[i]
    ), ncol = 2, byrow = TRUE)))
  }))

  rectangles_sf <- st_set_crs(rectangles_sf, st_crs(world))  # WGS 84 is the CRS of world
  rectangles_intersects_world <- st_intersects(rectangles_sf, world_polygon, sparse = FALSE)
  valid_rectangles_sf <- rectangles_sf[!apply(rectangles_intersects_world, 1, any)]
  rectangles_centroids <- st_centroid(valid_rectangles_sf)

  euclidean_distance <- function(point1, point2) {
    lon_diff <- point1[1] - point2[1]
    lat_diff <- point1[2] - point2[2]
    sqrt(lon_diff^2 + lat_diff^2)
  }

  small_countries <- country_bboxes[country_bboxes$area < small_area, ]

  closest_rectangles_df <- data.frame(
    country_name = character(),
    closest_rectangle_sf = I(list()),
    country_point = I(list()),
    rectangle_point = I(list()),
    stringsAsFactors = FALSE
  )

  for (i in 1:nrow(small_countries)) {
      selected_country <- small_countries[i, ]
      selected_country_centroid <- st_centroid(selected_country)
      rectangles_centroids <- st_set_crs(rectangles_centroids, st_crs(world))  # Set CRS to match world
      selected_country_centroid <- st_set_crs(selected_country_centroid, st_crs(world))  # Set CRS to match world
      distances_df <- data.frame()
      country_point <- st_coordinates(selected_country_centroid)

      for (i in 1:length(rectangles_centroids)) {
        rect_point <- st_coordinates(rectangles_centroids[i])
        distance <- euclidean_distance(rect_point, country_point)
        distances_df <- rbind(distances_df, data.frame(rectangle_id = i, distance = distance))
      }

      closest_rectangle_index <- which.min(distances_df$distance)
      closest_rectangle <- valid_rectangles_sf[closest_rectangle_index]
      closest_rectangle_centroid <- st_centroid(closest_rectangle)
      closest_rectangle_point <- st_coordinates(closest_rectangle_centroid)

      closest_rectangles_df <- rbind(closest_rectangles_df, data.frame(
          country_name = selected_country$name,
          country_point = country_point,
          closest_rectangle_sf = closest_rectangle,
          rectangle_point = closest_rectangle_point
      ))

      valid_rectangles_sf <- valid_rectangles_sf[-closest_rectangle_index]
      rectangles_centroids <- rectangles_centroids[-closest_rectangle_index]
  }

  # m3 <- ggplot() +
  #     geom_sf(data = world_valid, fill = "white", color = "black") +  # Plot the world map
  #     geom_sf(data = valid_rectangles_sf, fill = NA, color = "red", size = 1) +  # Plot the rectangles
  #     geom_sf(data = small_countries, fill = "yellow", color = "black", size = 2) +  # Plot Kenya
  #     geom_sf(data = closest_rectangles_df$geometry, fill = "green", color = "black", size = 2) +  # Plot the closest rectangle
  #     geom_segment(data = closest_rectangles_df, aes(x = country_point.X, y = country_point.Y, xend = rectangle_point.X, yend = rectangle_point.Y), 
  #               color = "blue", size = 1) +
  #     coord_sf() + 
  #     theme_minimal() 
  # print(m3)

  return (closest_rectangles_df)
} 

modify_label_positions <- function(data, area, width, height) {
  df <- find_closest_rects(data, area, width, height)
  print(colnames(data))
  
  new_data <- data %>%
    left_join(df, by = c("name" = "country_name")) %>%
    mutate(
      label_x = ifelse(!is.na(rectangle_point.X), rectangle_point.X, label_x),
      label_y = ifelse(!is.na(rectangle_point.Y), rectangle_point.Y, label_y)
    ) 
  return(new_data)
}


add_bar_charts <- function(map, df, width, height) {
  for (i in 1:nrow(df)) {
    map <- build_layer(map, df[i, , drop = FALSE], width, height)
  }
  return(map)
}

build_layer <- function(map, df, width, height) {
  data <- data.frame(
    Category = c("A", "B", "C", "D"),
    Value = c(3, 7, 2, 5)
  )
  
  bar_chart <- ggplot(data, aes(x = Category, y = Value)) +
    geom_bar(stat = "identity", fill = "lightblue") +
    theme_minimal() +
    theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
  
  bar_grob <- ggplotGrob(bar_chart)
  
  
  map <- map + annotation_custom(grob = bar_grob, xmin = df$label_x - width / 2, xmax = df$label_x + width / 2, ymin = df$label_y - height / 2, ymax = df$label_y + height / 2)
  
  return(map)
}

width <- 7
height <- 7

data <- modify_label_positions(world, 20, width, height)
print(data)



map <- ggplot() +
    geom_sf(data = data, fill = "white", color = "black") +  # Plot the world map
    geom_point(data=data, aes(x = label_x, y = label_y, size = 3)) + 
    geom_segment(data = data, aes(x = country_point.X, y = country_point.Y, xend = rectangle_point.X, yend = rectangle_point.Y), 
              color = "blue", size = 1) +
    coord_sf() + 
    theme_minimal() 

map <- add_bar_charts(map, data, width, height)
  
print(map)
  # print(m3)