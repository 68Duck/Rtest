library(ggplot2)
library(rnaturalearth)

test <- ne_countries(scale=110, type="countries", continent="africa")
print(test[1:97])
# print(test$geometry)

# test2 <- ggplot(test$geometry)
# print(test2)

test3 <- ggplot(test, aes(fill = name_len)) + geom_sf() +
     theme_void()
print(test3)
print(test$geometry[1])