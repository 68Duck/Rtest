library(rnaturalearth)
library(ggplot2)


africa <- ne_countries(continent = "africa")
# print(africa)
dframe <- data.frame(africa)
print(dframe)
map <- ggplot(africa) + geom_map(map = africa, colour = "black")
print(map)


# plot(africa)
# plot(ne_countries(type = "countries", scale = "small"))
