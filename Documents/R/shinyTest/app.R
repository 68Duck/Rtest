library(shiny) 
library(bslib)
library(ggplot2)
library(rnaturalearth)
source(".././Rtest\\cloropleth.R")

# Define UI for app that draws a histogram ----
ui <- page_sidebar(
  # App title ----
  title = "Hello Shiny!",
  # Sidebar panel for inputs ----
  sidebar = sidebar(

    selectInput(
    "continent",
    "Select continent",
    choices = list("Africa" = "Africa", "Europe" = "Europe", "South America" = "South America"),
    selected = "Africa"
    ),
    selectInput(
    "type",
    "Select type for the cloropleth",
    choices = list("Population Rank" = "Population Rank", "Name Length" = "Name Length"),
    selected = "Population Rank"
    ),
    textInput(
    "country",
    "Enter a country"
    ),
    selectInput(
    "type2",
    "Select type for bubble plot",
    choices = list("Population Rank" = "Population Rank", "Name Length" = "Name Length"),
    selected = "Population Rank"
    )
  ),
  # Output: Histogram ----
  plotOutput(outputId = "map")
)

server <- function(input, output) {

  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$map <- renderPlot({
    

    data <- ne_countries(scale=110, type="countries", continent=input$continent)
    if (nchar(input$country) > 1) {
        indexes <- getCountryIndexesFromDataframe(c(input$country), data$name, 2)
        data <- data[unlist(indexes),]
    }
    if (input$type == "Population Rank") {
        map <- cloropleth(data, data$pop_rank, "Population Rank")
    } else {
        map <- cloropleth(data, data$name_len, "Name Length")
    }
    if (input$type2 == "Name Length") {
        map <- addPoints(map, data, data$label_x, data$label_y, data$name_len, input$type2)
    } else if (input$type2 == "Population Rank") {
        map <- addPoints(map, data, data$label_x, data$label_y, data$pop_rank, input$type2)
    }
    map
    })

}

shinyApp(ui = ui, server = server)