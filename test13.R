# Load necessary libraries
library(shiny)
library(ggplot2)

# Define UI
ui <- fluidPage(
  titlePanel("Simple Shiny Plot Example"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("num", 
                  label = "Number of Points", 
                  min = 10, 
                  max = 100, 
                  value = 30)
    ),
    mainPanel(
      plotOutput("scatterPlot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$scatterPlot <- renderPlot({
    # Generate random data based on user input
    n <- input$num
    data <- data.frame(
      x = rnorm(n),
      y = rnorm(n)
    )
    
    # Create a scatter plot using ggplot2
    ggplot(data, aes(x = x, y = y)) +
      geom_point() +
      theme_minimal() +
      ggtitle(paste("Scatter Plot with", n, "points"))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
