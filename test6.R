# source("./Rtest\\cloropleth.R")

countries <- ne_countries(scale=110, type="countries", continent="south america")
# print(cloropleth(test, test$name_len, "Name Length"))


print(countries)
graph <- ggplot() +
    
    layer(data = countries, mapping = aes(fill = countries$pop_rank, geometry = countries$geometry),
    geom = "sf", stat = "identity", position = "identity") + coord_sf()
    # + coord_cartesian()
    # geom_sf(data = countries, aes(fill = countries$pop_rank))
print(graph)