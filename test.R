

library(ggplot2)
library(sf)

eng_reg_map <- 
  st_read("./Rtest\\shape_files\\London_Assembly_Constituencies_Dec_2018_FCB_EN.shp")

test <- eng_reg_map |>
  ggplot() +
  geom_sf(fill   = "white",
          colour = "black") +
  theme_void()


print(test)
# plot(1, 2)