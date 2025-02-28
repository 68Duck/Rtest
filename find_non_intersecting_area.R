library(rnaturalearth)
library(rnaturalearthhires)
library(sf)
library(dplyr)

world <- ne_countries(returnclass = "sf", scale=10, continent="africa")
small_area <- 10000
bounding_box <- st_bbox(world)

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

library(ggplot2)

map <- ggplot() +
  geom_sf(data = country_bboxes, fill = "lightblue", color = "black") +
  geom_rect(data = country_bboxes[country_bboxes$area < small_area, ], aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax),
            color = "red", fill = NA, linewidth = 1) + # Bounding boxes in red
  theme_minimal()

world_valid <- st_make_valid(world)
world_polygon <- st_union(world_valid)
bbox_polygon <- st_as_sfc(bounding_box)
non_intersecting <- st_difference(bbox_polygon, world_polygon)
print(non_intersecting)

m2 <- ggplot() +
  geom_sf(data = non_intersecting, fill = "green", color = "black") +
  theme_minimal()

print(m2)

