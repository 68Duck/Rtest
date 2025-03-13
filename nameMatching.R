library(stringdist)
library(purrr)

csv_data <<- read.csv("countryNames.csv", header = FALSE)

create_country_map <- function() {
    print("Creating country map")
    # data <- read.csv("countryNames.csv", header = FALSE)
    data <- csv_data
    country_map <- list()

    for (i in 1:nrow(data)) {
        for (j in 1:ncol(data)) {
        country_name <- tolower(data[i, j]) 
        country_map[[country_name]] <- i 
        }
    }

    return(country_map)  # Return the created hashmap
}

getCountryNumber <- function(country) {
    data <- read.csv("countryNames.csv", header = FALSE)
    for (i in 1:nrow(data)) {
        row <- data[i, ]
        number <- match(tolower(country), row)
        if (!is.na(number)) {
            print(i)
            return(i)
        }
    }
    return(-1)
}

getCountryNumber2 <- function(country) {
    if (!exists("country_map", envir = .GlobalEnv)) {
        country_map <<- create_country_map()
    }
    # print(country_map)

    country <- tolower(country)
    # print(country_map[[country]])
    # print(country_map)
    if (country %in% names(country_map)) {
        return (country_map[[country]])
    }

    return (-1)
}

getCountryNumberWithLevenshteinDistance <- function(country, x) {
    data <- csv_data
    for (i in 1:nrow(data)) {
        row <- data[i, ]
        for (j in 1:length(row)) {
            value <- row[j]
            if (length(value) == 0) {
                break
            }
            if (levenshteinDistanceLesserThan(toString(value),
                                              tolower(country), x)) {
                print(i)
                return(i)
            }
        }
    }
    return(-1)
}

getCountryNumber3 <- function(country, x) {
    if (!exists("country_map", envir = .GlobalEnv)) {
        country_map <<- create_country_map()
    }

    country <- tolower(country)
    if (country %in% names(country_map)) {
        # print(country_map[[country]])
        return (country_map[[country]])
    }

    country <- gsub("\\b(and|of|the)\\b", "", country, ignore.case = TRUE)

    if (country %in% names(country_map)) {
        # print(country_map[[country]])
        return (country_map[[country]])
    }

    data <- csv_data

    for (i in 1:nrow(data)) {
        row <- data[i, ]
        for (j in 1:length(row)) {
            value <- row[j]
            if (length(value) == 0) {
                break
            }
            value <- toString(value)
            value <- gsub("\\b(and|of|the)\\b", "", value, ignore.case = TRUE)
            if (levenshteinDistanceLesserThan(value,
                                              country, x)) {
                # print(i)
                return(i)
            }

            if (abbreviated(country, value)) {
                # print(i)
                # print("abbreviated")
                return (i)
            }
        }

    }
    print(country)

    return(-1)
}

getCountryNumberWithLevenshteinDistance2 <- function (country, x) {
    number <- getCountryNumber2(country)
    # print(number)
    if (number == -1) {
        # print(country)
        number <- getCountryNumberWithLevenshteinDistance(country, x)
        # print(country)
        # print(number)
        if (number == -1) {
            print(country)
        }
    }
    number
}

levenshteinDistanceLesserThan <- function(str1, str2, x) {
    dist <- stringdist(str1, str2, method = "lv")
    return(dist < x)
}

abbreviated <- function(str1, str2) {
    while (nchar(str1) > 0 && nchar(str2) > 0) {
        if (substring(str1, 0, 1) == ".") {
            str1 <- substring(str1, 3)
            str2 <- sub("^\\S+\\s*", "", str2)
        }
        else if (substring(str1, 0, 1) == substring(str2, 0, 1)) {
            str1 <- substring(str1, 2)
            str2 <- substring(str2, 2)
        } else {
            return (FALSE)
        }

    }
    return (str1 == str2)
}

compareNearestCountry <- function(country1, country2, maxDistance) {
    number1 <- getCountryNumberWithLevenshteinDistance(country1, maxDistance)
    number2 <- getCountryNumberWithLevenshteinDistance(country2, maxDistance)
    return (number1 == number2)
}

getCountryIndexesFromDataframe <- function(countries, dataFrameCountries, maxDistance) {
    numbers <- map(countries, \(x) getCountryNumberWithLevenshteinDistance(x, maxDistance))
    # numbers <- map(countries, \(x) getCountryNumber(x))
    indexes <- list()
    # countryNumbers <- map(dataFrameCountries, \(x) getCountryNumberWithLevenshteinDistance(x, maxDistance))
    countryNumbers <- map(dataFrameCountries, \(x) getCountryNumber(x))
    for (number in numbers) {
        if (number %in% countryNumbers) {
            for (i in 1:length(countryNumbers)) {
                if (countryNumbers[i] == number) {
                    indexes <- append(indexes, i)
                    # print(i)
                }
            }
        }
    }
    return (indexes)
}

# print(getCountryNumber("Chad"))
# print(getCountryNumber("United Kingdom"))
# print(getCountryNumber("UNITED KINGDOM"))
# print(getCountryNumber("NOT A COUNTRY"))
# print(getCountryNumberWithLevenshteinDistance("Chad", 2))
# print(getCountryNumberWithLevenshteinDistance("United kingdom", 2))
# print(getCountryNumberWithLevenshteinDistance("United kingdon", 2))
# print(getCountryNumberWithLevenshteinDistance("NOT A COUNTRY", 2))

# print(abbreviated("Dem. Rep. Congo", "Democratic Republic Congo"))
# print(abbreviated("Dem. Rep. Congo", "Democratic Republic of Congo"))
# print(abbreviated("western sahara", "w. sahara"))
# print(abbreviated("w. sahara", "western sahara"))