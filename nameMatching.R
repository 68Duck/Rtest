data <- read.csv("countryNames.csv", header = FALSE)
library(stringdist)
library(purrr)

getCountryNumber <- function(country) {
    for (i in 1:nrow(data)) {
        row <- data[i, ]
        number <- match(tolower(country), row)
        if (!is.na(number)) {
            return(i)
        }
    }
    return(-1)
}

getCountryNumberWithLevenshteinDistance <- function(country, x) {
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

levenshteinDistanceLesserThan <- function(str1, str2, x) {
    dist <- stringdist(str1, str2, method = "lv")
    return(dist < x)
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

