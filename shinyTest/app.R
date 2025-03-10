library(shiny)
library(ggplot2)
library(dplyr)
source("C:\\Users\\Joshu\\Documents\\R\\Rtest\\find_closest_rect.R")
source("C:\\Users\\Joshu\\Documents\\R\\Rtest\\addBarCharts.R")
source("C:\\Users\\Joshu\\Documents\\R\\Rtest\\cloropleth.R")
source("C:\\Users\\Joshu\\Documents\\R\\Rtest\\addPieChart.R")
source("C:\\Users\\Joshu\\Documents\\R\\Rtest\\add_stars.R")



ui <- fluidPage(
  titlePanel("Test"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("chart_type", "Choose chart type:", 
                  choices = c("Bar Chart", "Pie Chart", "Star Plot")),
      numericInput("small_country_size_input", 
                   label = "Size of a country to draw a line towards", 
                   value = 1, 
                   min = 1, 
                   max = 25, 
                   step = 1)  
    ),
    mainPanel(
      plotOutput("ggplot")
    )
  )
)

# # Define server logic
server <- function(input, output) {
  
  width <- 1
  height <- 1
  data <- ne_countries(returnclass = "sf", scale = 110, continent = "europe")
  output$ggplot <- renderPlot({
    small_country_area <- input$small_country_size_input
    data <- modify_label_positions(data, small_country_area, width, height)
    base_plot <- cloropleth(data, data$pop_rank, "Population rank")
    map <- add_lines_to_labels(data, base_plot)

    if (input$chart_type == "Bar Chart") {
      add_bar_charts(map, data, width, height)

    } else if (input$chart_type == "Pie Chart") {
      pie_scale <- 1.5
      addPie(map, data, "label_x", "label_y", c("name_len", "pop_rank"), "Name vs Pop", pie_scale)
      
    } else if (input$chart_type == "Star Plot") {
      width <- 8
      height <- 6
      star_size <- 1
      add_stars(map, data, width, height, star_size)
    }
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
