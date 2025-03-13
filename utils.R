source("./Rtest\\nameMatching.R")
library(dplyr)

merge_data <- function(country_data, auxillary_data, country_name, auxillary_country_name) {
    data <- merge(country_data, auxillary_data, by.x = country_name, by.y = auxillary_country_name)
    data <- st_as_sf(data)
    data
}
    
merge_data_with_country_matching <- function(country_data, auxillary_data, country_name, auxillary_country_name) {
    country_data$country_number <- apply(country_data, 1, function(row) getCountryNumber3(row[country_name], 3))
    auxillary_data$country_number <- apply(auxillary_data, 1, function(row) getCountryNumber3(row[auxillary_country_name], 3))
    # country_data$country_number <- apply(country_data, 1, function(row) getCountryNumberWithLevenshteinDistance2(row[country_name], 3))
    # auxillary_data$country_number <- apply(auxillary_data, 1, function(row) getCountryNumberWithLevenshteinDistance2(row[auxillary_country_name], 3))

    data <- merge(country_data, auxillary_data, by = "country_number", all.x = TRUE)
    # print(data)
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