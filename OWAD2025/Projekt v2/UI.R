library(shiny)
library(tidyverse)
library(shinyjs)
library(plotly)
options(scipen = 999)

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("      
      .active-btn {
        background-color: #4ca2af !important;
        color: white !important;
      }
      
      #comparison_group + .selectize-control.multi .selectize-input {
      max-height: 65px;
      overflow-y: auto;
      }
      
      .nav-tabs > li > a:empty {
      display: none !important;
      }
    
    "))
  ),
  titlePanel("Covid-19 Dashboard"),
  tabsetPanel(id = "tabs",
              tabPanel("Global Trends",
                       actionButton("select_all", "All continents"),
                       actionButton("select_africa", "Africa"),
                       actionButton("select_asia", "Asia"),
                       actionButton("select_australia", "Australia/Oceania"),
                       actionButton("select_europe", "Europe"),
                       actionButton("select_north_america", "North America"),
                       actionButton("select_south_america", "South America"),
                       actionButton("clear_all", "Clear all"),
                       br(), br(),
                       sidebarLayout(
                         sidebarPanel(
                           selectizeInput(
                             inputId = "grouped_options",
                             label = "Select countries:",
                             choices = list(
                               "Africa" = as.list(unique_africa),
                               "Asia" = as.list(unique_asia),
                               "Australia/Oceania" = as.list(unique_australia_oceania),
                               "Europe" = as.list(unique_europe),
                               "North America" = as.list(unique_north_america),
                               "South America" = as.list(unique_south_america)
                             ),
                             selected = "Poland",
                             multiple = TRUE,
                             options = list(
                               maxItems = NULL,
                               plugins = list('remove_button'),
                               dropdownParent = 'body'
                             )
                           ),
                           tags$style(HTML("
                              .selectize-control.multi .selectize-input {
                                max-height: 100px;
                                overflow-y: auto;
                              }
                            ")),
                           dateRangeInput("daterange", "Date range:",
                                          start = min_date, end = max_date,
                                          min = min_date, max = max_date),
                           br(),
                           h4("Summary Statistics"),
                           uiOutput("global_stats"),
                           br(),
                           h5("Case definitions:"),
                           tags$ul(
                             tags$li("Confirmed: Total number of detected infections."),
                             tags$li("Recovered: Officially reported recoveries."),
                             tags$li("Deaths: Confirmed COVID-19 fatalities."),
                             tags$li("Active: Currently infected (Confirmed − Recovered − Deaths).")
                           )
                           
                         ),
                         mainPanel(
                           conditionalPanel(
                             condition = "input.grouped_options.length === 0",
                             tags$div(
                               style = "margin-top: 100px; text-align: center; font-size: 18px; color: #888;",
                               "Choose country(ies) to display"
                             )
                           ),
                           conditionalPanel(
                             condition = "input.grouped_options.length > 0",
                             tagList(
                               plotOutput("timeseries_plot", height = "325px"),
                               fluidRow(
                                 column(6, plotOutput("histogram", height = "275px")),
                                 column(6, plotOutput("global_pie", height = "275px"))
                               )
                             )
                           )
                         )
                       )
              ),
              tabPanel("Country Comparison",
                       actionButton("compare_select_all", "All continents"),
                       actionButton("compare_select_africa", "Africa"),
                       actionButton("compare_select_asia", "Asia"),
                       actionButton("compare_select_australia", "Australia/Oceania"),
                       actionButton("compare_select_europe", "Europe"),
                       actionButton("compare_select_north_america", "North America"),
                       actionButton("compare_select_south_america", "South America"),
                       actionButton("compare_clear_all", "Clear all"),
                       br(), br(),
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("compare_country", "Reference country:", choices = unique_all, selected = "Poland"),
                           selectizeInput(
                             inputId = "comparison_group",
                             label = "Comparison group (avg):",
                             choices = list(
                               "Africa" = as.list(unique_africa),
                               "Asia" = as.list(unique_asia),
                               "Australia/Oceania" = as.list(unique_australia_oceania),
                               "Europe" = as.list(unique_europe),
                               "North America" = as.list(unique_north_america),
                               "South America" = as.list(unique_south_america)
                             ),
                             multiple = TRUE,
                             options = list(
                               maxItems = NULL,
                               plugins = list("remove_button"),
                               dropdownParent = 'body'
                             )
                           ),
                           dateRangeInput("compare_daterange", "Date range:",
                                          start = min_date, end = max_date,
                                          min = min_date, max = max_date
                           ),
                           br(),
                           h5("Reference Country Stats"),
                           uiOutput("compare_ref_stats"),
                           br(),
                           h5("Comparison Group Stats"),
                           uiOutput("compare_group_stats")
                         ),
                         mainPanel(
                           conditionalPanel(
                             condition = "input.comparison_group.length === 0",
                             tags$div(
                               style = "margin-top: 100px; text-align: center; font-size: 18px; color: #888;",
                               "Choose country(ies) to compare"
                             )
                           ),
                           conditionalPanel(
                             condition = "input.comparison_group.length > 0",
                             tagList(
                               plotOutput("comparison_plot", height = "340px"),
                               br(),
                               fluidRow(
                                 column(6, h5("Reference Pie Chart"), plotOutput("compare_ref_pie", height = "240px")),
                                 column(6, h5("Comparison Group Pie Chart"), plotOutput("compare_group_pie", height = "240px"))
                               )
                             )
                           )
                         )
                       )
              ),
              tabPanel("Country Ranking",
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("ranking_case", "Case type:",
                                       choices = c("Confirmed", "Deaths", "Recovered", "Active")),
                           selectInput("ranking_reference_country", "Reference country:",
                                       choices = c("-- None --", unique_all),
                                       selected = "-- None --"),
                           dateInput("ranking_date", "Reference date:",
                                     value = max_date,
                                     min = min_date, max = max_date),
                           numericInput("top_n", "Number of countries:", value = 10, min = 1, max = 50),
                           conditionalPanel(
                             condition = "input.ranking_reference_country == '-- None --'",
                             radioButtons("ranking_order", "Sort order:",
                                          choices = c("Descending" = "desc", "Ascending" = "asc"),
                                          selected = "desc"),
                             uiOutput("ranking_stats")
                           )
                           ,
                           checkboxInput("ranking_hide_nulls", "Hide countries with 0 or missing value", value = TRUE)
                         ),
                         mainPanel(
                           plotlyOutput("ranking_plot",height='600px'),
                           hr(),
                           uiOutput("country_details_panel")
                         )
                       )
              ),
              tabPanel(title = NULL, value = "Country Details View",
                       fluidPage(
                         actionButton("back_to_ranking", "← Back to ranking"),
                         br(),
                         
                         # Nagłówek przesunięty na środek
                         h3(textOutput("detail_title"), style = "text-align: center;"),
                         br(),
                         
                         uiOutput("global_rank_info"),
                         br(),
                         
                         fluidRow(
                           column(
                             width = 6,
                             plotlyOutput("detail_timeseries", height = "250px"),
                             br(),
                             plotOutput("detail_histogram", height = "250px")
                           ),
                           column(
                             width = 6,
                             uiOutput("country_stats"),
                             br(),
                             tags$div(
                               style = "font-size: 12px; margin-top: 10px;",
                               DT::dataTableOutput("detail_table")
                             ),
                             br(),
                             downloadButton("download_data", "Download data")
                           )
                         )
                       )
              )

  )
)
