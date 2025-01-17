library(ggplot2)
library(rnaturalearth)
source("./Rtest\\cloropleth.R")
source("./Rtest\\nameMatching.R")

test <- ne_countries(scale=110, type="countries", continent="south america")

indexes <- getCountryIndexesFromDataframe(c("uruguay", "argentin"), test$name, 2)
test <- test[unlist(indexes), ]
print(cloropleth(test, test$pop_rank, "Population rank"))