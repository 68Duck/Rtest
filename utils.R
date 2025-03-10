source("./Rtest\\nameMatching.R")
library(dplyr)

merge_data <- function(country_data, auxillary_data, country_name, auxillary_country_name) {
    data <- merge(country_data, auxillary_data, by.x = country_name, by.y = auxillary_country_name)
    data <- st_as_sf(data)
    data
}
    
merge_data_with_country_matching <- function(country_data, auxillary_data, country_name, auxillary_country_name) {
    print(country_data$name)
    country_data$country_number <- apply(country_data, 1, function(row) getCountryNumberWithLevenshteinDistance(row[country_name], 3))
    auxillary_data$country_number <- apply(auxillary_data, 1, function(row) getCountryNumberWithLevenshteinDistance(row[auxillary_country_name], 3))
    # country_data$country_number <- apply(country_data, 1, function(row) getCountryNumber(row[country_name]))
    # auxillary_data$country_number <- apply(auxillary_data, 1, function(row) getCountryNumber(row[auxillary_country_name]))
    print(country_data$country_number)
    print(auxillary_data$country_number)
    # auxillary_data <- auxillary_data %>% mutate(country_number = sapply(auxillary_country_name, getCountryNumber))
    # print(country_data$country_number)
    data <- merge(country_data, auxillary_data, by = "country_number")
    data <- st_as_sf(data)
    data
}

convert_columns_to_number <- function (data, columns, characters_to_remove) {
    for (column in columns) {
        data[[column]] <- gsub(",", "", data[[column]])
        for (character in characters_to_remove) {
            data[[column]] <- gsub(character, "", data[[column]])
        }
        data[[column]] <- as.numeric(data[[column]])
    }
    data 
}