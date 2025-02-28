library(rnaturalearth)
library(rnaturalearthhires)
library(sf)
library(dplyr)
library(ggplot2)

# Load African countries data
world <- ne_countries(returnclass = "sf", scale = 10, continent = "africa")

# Get the bounding box for Africa (the entire continent)
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
reps <- 1  # Number of repetitions for finer resolution

# Generate sequences of xmin and ymin every 5 units
xmin_values <- seq(non_intersecting_bbox["xmin"], non_intersecting_bbox["xmax"] - width, by = width / reps)
ymin_values <- seq(non_intersecting_bbox["ymin"], non_intersecting_bbox["ymax"] - height, by = height / reps)

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

# Ensure the rectangles have the same CRS as world_polygon and non_intersecting_geometry
rectangles_sf <- st_set_crs(rectangles_sf, st_crs(non_intersecting_geometry))

# Now you can safely transform the CRS if needed
rectangles_sf <- st_transform(rectangles_sf, crs = st_crs(non_intersecting_geometry))

# Check if the rectangles intersect with the world (Africa)
rectangles_intersects_world <- st_intersects(rectangles_sf, world_polygon, sparse = FALSE)

# Filter out rectangles that intersect with the world
valid_rectangles_sf <- rectangles_sf[!apply(rectangles_intersects_world, 1, any)]

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

# Example point for which we want to find the closest rectangle (e.g., centroid of Africa)
point <- st_sfc(st_point(c(0, 0)))  # Longitude 20, Latitude 0 (this is roughly central in Africa)

# Ensure the point has the correct CRS
point <- st_set_crs(point, st_crs(non_intersecting_geometry))

# Calculate distances from the point to each rectangle
distances <- st_distance(point, valid_rectangles_sf)
print(distances)

# Find the index of the closest rectangle
closest_rectangle_index <- which.min(distances)

closest_rectangle <- rectangles_sf[closest_rectangle_index]


# Create the plot with the non-intersecting area and the valid rectangles
m3 <- ggplot() +
  geom_sf(data = non_intersecting_geometry, fill = "green", color = "black") +  # Plot the non-intersecting area
  geom_sf(data = valid_rectangles_sf, fill = NA, color = "red", size = 1) +  # Plot the valid rectangles
  geom_sf(data = closest_rectangle, fill = "red", color = "black", size = 1) + 
  geom_sf(data = point, color = "blue", size = 3) +
  geom_sf(data = country_bboxes[country_bboxes$area < 5, ], fill = NA, color = "blue", size = 1) +
  coord_sf() +  # Use the correct coordinate system for the map
  theme_minimal()

print(m3)