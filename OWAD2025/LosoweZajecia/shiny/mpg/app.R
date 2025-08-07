library(shiny)
library(ggplot2)

mpg_data <- mpg
manufacturers <- c("All", sort(unique(mpg_data$manufacturer)))

ui <- fluidPage(
  tabsetPanel(id = "switch",
              tabPanel(
                "Brush",
                fluidRow(
                  column(12,
                         actionButton("transpose", "Transponuj wykres"),
                         numericInput("click_thresh", "Próg kliknięcia (threshold)", value = 25, min = 1, max = 5000),
                         br(),
                         plotOutput("scatter1", brush = brushOpts(id = "scatter1_brush", resetOnNew = TRUE),
                                    click = "scatter1_click"),
                         br(),
                         h4("Zaznaczone dane (Brush):"),
                         tableOutput("table_brush"),
                         br(),
                         h4("Najbliższy punkt (Click):"),
                         tableOutput("table_click")
                  )
                )
              ),
              tabPanel(
                "Tabela",
                fluidRow(
                  column(12, 
                         h4("Zaznaczone dane z wykresu:"),
                         tableOutput("table_tabela"))
                )
              ),
              tabPanel(
                "Wybor producenta",
                fluidRow(
                  column(
                    6,
                    plotOutput("scatter_manufacturer")
                  ),
                  column(
                    6, 
                    fluidRow(
                      selectInput(
                        "manufs",
                        "Wybierz producenta",
                        choices = manufacturers,
                        selected = "All"
                      ),
                      conditionalPanel(
                        condition = "input.manufs == 'All'",
                        checkboxInput("show_legend", "Pokaż legendę", value = TRUE)
                      ),
                      uiOutput("model_ui")
                    )
                  )
                )
              )
              
  )
)

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    data = NULL,
    transpose_axes = FALSE,
    click_data = NULL
  )
  
  observeEvent(input$transpose, {
    rv$transpose_axes <- !rv$transpose_axes
  })
  
  observeEvent(input$scatter1_brush, {
    rv$data <- brushedPoints(mpg_data, input$scatter1_brush)
  })
  
  observeEvent(input$scatter1_click, {
    req(input$click_thresh)
    rv$click_data <- nearPoints(mpg_data, input$scatter1_click,
                                threshold = input$click_thresh)
  })
  
  observeEvent(input$switch, {
    if (input$switch != "Brush") {
      rv$data <- NULL
      rv$click_data <- NULL
      session$resetBrush("scatter1_brush")
    }
  })
  
  output$scatter1 <- renderPlot({
    xvar <- if (rv$transpose_axes) "hwy" else "displ"
    yvar <- if (rv$transpose_axes) "displ" else "hwy"
    
    ggplot(mpg_data, aes_string(x = xvar, y = yvar)) +
      geom_point() +
      theme_minimal() +
      labs(x = xvar, y = yvar)
  })
  
  output$table_brush <- renderTable({
    if (is.null(rv$data) || nrow(rv$data) == 0) {
      return(data.frame("Informacja" = "Brak zaznaczonych danych"))
    }
    rv$data[, c("manufacturer", "model", "displ", "hwy")]
  })
  
  output$table_tabela <- renderTable({
    if (is.null(rv$data) || nrow(rv$data) == 0) {
      return(data.frame("Informacja" = "Brak zaznaczonych danych"))
    }
    rv$data[, c("manufacturer", "model", "displ", "hwy")]
  })
  
  output$table_click <- renderTable({
    if (is.null(rv$click_data) || nrow(rv$click_data) == 0) {
      return(data.frame("Informacja" = "Brak punktu w pobliżu kliknięcia"))
    }
    rv$click_data[, c("manufacturer", "model", "displ", "hwy")]
  })
  
  output$model_ui <- renderUI({
    if (input$manufs == "All") return(NULL)
    models <- unique(mpg_data$model[mpg_data$manufacturer == input$manufs])
    checkboxGroupInput("models", "Wybierz model", choices = models, selected = models)
  })
  
  output$scatter_manufacturer <- renderPlot({
    data_to_plot <- mpg_data
    
    if (input$manufs != "All") {
      data_to_plot <- data_to_plot[data_to_plot$manufacturer == input$manufs, ]
      
      if (!is.null(input$models) && length(input$models) > 0) {
        data_to_plot <- data_to_plot[data_to_plot$model %in% input$models, ]
      } else {
        return(NULL)
      }
    }
    
    if (nrow(data_to_plot) == 0) return(NULL)
    
    p <- ggplot(data_to_plot, aes(x = displ, y = hwy, color = model)) +
      geom_point(size = 3) +
      theme_minimal() +
      labs(title = if (input$manufs == "All") "Wszystkie dane" else paste("Dane producenta:", input$manufs),
           x = "displ", y = "hwy")
    
    if (input$manufs == "All" && !input$show_legend) {
      p <- p + theme(legend.position = "none")
    }
    
    p
  })
  
}

shinyApp(ui = ui, server = server)
