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
            # sliderInput("max_y",
            #             "max_y",
            #             min = 1,
            #             max = 50,
            #             value = 50),
            # sliderInput("min_y",
            #             "min_y",
            #             min = 0,
            #             max = 50,
            #             value = 0),
            sliderInput("ylimit",
                        "ylimit",
                        min = 0,
                        max = 50,
                        value = c(0,50))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)





# Define server logic required to draw a histogram
server <- function(input, output) {
  output$distPlot <- renderPlot({(ggplot(data = tibble(faithful), aes(x=waiting)) + geom_histogram(bins=input$bins)) +
    ylim(input$ylimit[1], input$ylimit[2])
    #scale_y_continuous(limits=c(input$min_y,input$max_y))
      })
  
  # output$distPlot <- renderPlot({
  #      # generate bins based on input$bins from ui.R
  #      x    <- faithful[, 2]
  #      bins <- seq(min(x), max(x), length.out = input$bins + 1)
  # 
  #      # draw the histogram with the specified number of bins
  #      hist(x, breaks = bins, col = 'darkgray', border = 'white',
  #           xlab = 'Waiting time to next eruption (in mins)',
  #           main = 'Histogram of waiting times')
  #  })
}

# Run the application 
shinyApp(ui = ui, server = server)
