source("./Rtest\\cloropleth.R")

test <- ne_countries(scale=110, type="countries", continent="south america")
print(cloropleth(test, test$name_len, "Name Length"))