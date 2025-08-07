#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30),
            sliderInput("obs",
                        "number of observatrions",
                        min = 1,
                        max = 10e6,
                        value = 1e3),
            numericInput("mean",
                        "mean",
                        value = 0),
            numericInput("sd",
                         "Standard deviation: ",
                         value = 1),
            fluidRow(
              column(6,
                     numericInput('x_min', 'Left end:', value = -10)),
              column(6,
                     numericInput('x_max', 'Right end:', value = 10))
            )
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
          fluidRow(
            splitLayout(cellWidths = c("50%", "50%"), plotOutput("plotgraph1"), plotOutput("plotgraph2")),
            splitLayout(cellWidths = c("50%", "50%"), plotOutput("plotgraph3"), plotOutput("plotgraph4"))
          )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  df_data <- reactive({
    data.frame(value = rnorm(n = input$obs, mean = input$mean, sd = input$sd))
  })
  
  output$plotgraph1 <- renderPlot({
    ggplot(df_data(), aes(x = value)) + 
      geom_histogram(bins = input$bins) #+
      #xlim(input$x_min, input$x_max)
  })
  
  output$plotgraph2 <- renderPlot({
    ggplot(df_data(), aes(x = value)) + 
      geom_boxplot() #+
      #xlim(input$x_min, input$x_max)
  })
  output$plotgraph3 <- renderPlot({
    ggplot(df_data(), aes(x = value)) + 
      geom_histogram(bins = input$bins) #+
    #xlim(input$x_min, input$x_max)
  })
  
  output$plotgraph4 <- renderPlot({
    ggplot(df_data(), aes(x = value)) + 
      geom_boxplot() #+
    #xlim(input$x_min, input$x_max)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
