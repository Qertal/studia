library(shiny)
library(tidyverse)

rm(list = ls())
parameters <- c("PM2.5", "PM10", "SO2", "NO2", "CO", "O3", "TEMP", "PRES", "DEWP", "RAIN", "WSPM")


china_air <- read.table("../Zajęcia II/china_air.csv", sep = ",", header = TRUE) %>%
  mutate(date = as.Date(paste0(year, "-", month, "-", day))) %>%
  group_by(station, date) %>%
  mutate(across(parameters, ~mean(., na.rm = TRUE))) %>%
  ungroup()

min_max_date <- c(
  china_air %>% filter(date == min(date, na.rm = TRUE)) %>% distinct(date) %>% pull(),
  china_air %>% filter(date == max(date, na.rm = TRUE)) %>% distinct(date) %>% pull())

unique_stations <- china_air %>%
  distinct(station) %>%
  arrange(station) %>%
  pull()

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Air quality in China"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput("station", 
                      "Select station", 
                      unique_stations,
                      selected = unique_stations[1]),
          selectInput("parameter",
                      "Select parameter",
                      parameters,
                      selected = parameters[1]),
          dateRangeInput("date",
                    "Select date range",
                    start = min_max_date[1],
                    end = min_max_date[2],
                    min = min_max_date[1],
                    max = min_max_date[2]),
          checkboxInput("display_critical",
                        "Display critical value line"),
          conditionalPanel(
            condition = "input.display_critical == 1"),
            numericInput("critical",
                         "Enter the critical value",
                         10)
        ),
        

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("scatterPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  filtered_df <- reactive({
    china_air %>%
      rename(parameter = !!sym(input$parameter)) %>%
      filter(station == input$station) %>%
      filter(!is.na(parameter) & between(date, input$date[1], input$date[2])) %>%
      mutate(
        is_critical = case_when(
          parameter >= input$critical ~ "Yes",
          parameter < input$critical ~ "No"))
  })
  
    output$scatterPlot <- renderPlot({
      min_date <- filtered_df() %>%
        filter(date == min(date, na.rm = TRUE)) %>%
        distinct(date) %>%
        pull()
      max_date <- filtered_df() %>%
        filter(date == max(date, na.rm = TRUE)) %>%
        distinct(date) %>%
        pull()
      
      plot <- ggplot(filtered_df(), aes(x = date, y = parameter)) +
        theme(
          title = element_text(size = 20),
          legend.position = "none"
        ) +
        labs(
          title = paste0("Scatter plot of ", "daily", " mean ", input$parameter, " on station ", input$station, " from ", min_date, " to ", max_date, "."),
          y = input$parameter,
          x = "Date"
        )
      if(input$display_critical) {
        plot <- plot + 
          geom_point(aes(color = is_critical)) +
          geom_hline(yintercept = input$critical, color = "red") +
          scale_color_manual(values = c("No" = "green", "Yes" = "red"))
          
      } else {
        plot <- plot + geom_point()
      }
      plot
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
