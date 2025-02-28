library(rnaturalearth)
library(rnaturalearthhires)
library(sf)
library(dplyr)
library(ggplot2)

# Load African countries data
world <- ne_countries(returnclass = "sf", scale = 10, continent = "africa")

country_bboxes <- world %>%
  mutate(
    bbox = purrr::map(geometry, st_bbox), 
    xmin = purrr::map_dbl(bbox, 1),  
    ymin = purrr::map_dbl(bbox, 2),  
    xmax = purrr::map_dbl(bbox, 3),  
    ymax = purrr::map_dbl(bbox, 4)   
  ) %>%
  select(name, xmin, ymin, xmax, ymax)


country_bboxes$area <- (country_bboxes$xmax - country_bboxes$xmin) * (country_bboxes$ymax - country_bboxes$ymin) 


# Get the bounding box for the entire world (Africa)
bounding_box <- st_bbox(world)

# Convert bounding box to sf polygon for further spatial operations
bbox_polygon <- st_as_sfc(bounding_box)

# Make the geometries of African countries valid (in case of invalid geometries)
world_valid <- st_make_valid(world)

# Create the union of all African countries (single geometry for the entire continent)
world_polygon <- st_union(world_valid)

# Find the non-intersecting area (bounding box - African countries)
non_intersecting_geometry <- st_difference(bbox_polygon, world_polygon)

# Get the bounding box of the non-intersecting geometry to define the search area
non_intersecting_bbox <- st_bbox(non_intersecting_geometry)

# Define the size of the rectangle you want to find (5x5 units)
width <- 5
height <- 5
reps <- 1 # Number of repetitions for finer resolution

# Generate sequences of xmin and ymin every 5 units
xmin_values <- seq(non_intersecting_bbox["xmin"], non_intersecting_bbox["xmax"] - width, by = width)
ymin_values <- seq(non_intersecting_bbox["ymin"], non_intersecting_bbox["ymax"] - height, by = height)

# Create a grid of possible rectangle positions (xmin, ymin)
rectangles <- expand.grid(xmin = xmin_values, ymin = ymin_values)

# Create a data frame of the rectangles with the proper xmax and ymax values
rectangles <- rectangles %>%
  mutate(xmax = xmin + width, ymax = ymin + height)

# Convert the rectangles to sf objects (polygons)
rectangles_sf <- st_sfc(lapply(1:nrow(rectangles), function(i) {
  st_polygon(list(matrix(c(
    rectangles$xmin[i], rectangles$ymin[i],
    rectangles$xmin[i] + width, rectangles$ymin[i],
    rectangles$xmin[i] + width, rectangles$ymin[i] + height,
    rectangles$xmin[i], rectangles$ymin[i] + height,
    rectangles$xmin[i], rectangles$ymin[i]
  ), ncol = 2, byrow = TRUE)))
}))

# Ensure that rectangles_sf has a CRS defined (set the CRS to WGS 84)
rectangles_sf <- st_set_crs(rectangles_sf, st_crs(world))  # WGS 84 is the CRS of world

# Check if the rectangles intersect with the world (Africa)
rectangles_intersects_world <- st_intersects(rectangles_sf, world_polygon, sparse = FALSE)

# Filter out rectangles that intersect with the world
valid_rectangles_sf <- rectangles_sf[!apply(rectangles_intersects_world, 1, any)]

# Calculate the center of each rectangle (midpoint of the bounding box)
rectangles_centroids <- st_centroid(valid_rectangles_sf)

euclidean_distance <- function(point1, point2) {
  lon_diff <- point1[1] - point2[1]
  lat_diff <- point1[2] - point2[2]
  sqrt(lon_diff^2 + lat_diff^2)
}

small_countries <- country_bboxes[country_bboxes$area < 5, ]

closest_rectangles_df <- data.frame(
  country_name = character(),
  closest_rectangle_id = integer(),
  closest_rectangle_sf = I(list()),
  country_point = I(list()),
  rectangle_point = I(list()),
  stringsAsFactors = FALSE
)

for (i in 1:nrow(small_countries)) {
     selected_country <- small_countries[i, ]
    # Get the centroid of the selected country (Kenya)
    selected_country_centroid <- st_centroid(selected_country)
    #  print(selected_country_centroid)

    # Ensure both centroids have the same CRS
    rectangles_centroids <- st_set_crs(rectangles_centroids, st_crs(world))  # Set CRS to match world
    selected_country_centroid <- st_set_crs(selected_country_centroid, st_crs(world))  # Set CRS to match world

    # Calculate the Euclidean distance from each rectangle center to Kenya's centroid
    distances_df <- data.frame()

    # Get the coordinates of the country centroid (Kenya)
    country_point <- st_coordinates(selected_country_centroid)
    # Calculate distance to each rectangle's centroid
    for (i in 1:length(rectangles_centroids)) {
    rect_point <- st_coordinates(rectangles_centroids[i])
    distance <- euclidean_distance(rect_point, country_point)
    
    # Store the result in the data frame
    distances_df <- rbind(distances_df, data.frame(rectangle_id = i, distance = distance))
    }

    # Find the closest rectangle to the selected country (Kenya)
    closest_rectangle_index <- which.min(distances_df$distance)

    # Extract the closest rectangle
    closest_rectangle <- valid_rectangles_sf[closest_rectangle_index]
    closest_rectangle_centroid <- st_centroid(closest_rectangle)
    closest_rectangle_point <- st_coordinates(closest_rectangle_centroid)
    # print(closest_rectangle)
    # print(selected_country)

    closest_rectangles_df <- rbind(closest_rectangles_df, data.frame(
        country_name = selected_country$name,
        country_point = country_point,
        closest_rectangle_id = closest_rectangle_index,
        closest_rectangle_sf = closest_rectangle,
        rectangle_point = closest_rectangle_point
    ))

    valid_rectangles_sf <- valid_rectangles_sf[-closest_rectangle_index]
    rectangles_centroids <- rectangles_centroids[-closest_rectangle_index]
}

print(closest_rectangles_df)


m3 <- ggplot() +
    geom_sf(data = world_valid, fill = "white", color = "black") +  # Plot the world map
    geom_sf(data = valid_rectangles_sf, fill = NA, color = "red", size = 1) +  # Plot the rectangles
    geom_sf(data = country_bboxes[country_bboxes$area < 5, ], fill = "yellow", color = "black", size = 2) +  # Plot Kenya
    geom_sf(data = closest_rectangles_df$geometry, fill = "green", color = "black", size = 2) + 
    geom_segment(data = closest_rectangles_df, aes(x = country_point.X, y = country_point.Y, xend = rectangle_point.X, yend = rectangle_point.Y), 
               color = "blue", size = 1) +
    coord_sf() +  # Use the correct coordinate system for the map
    theme_minimal() 
print(m3)


