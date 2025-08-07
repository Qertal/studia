library(shiny)
library(tidyverse)
library(shinyjs)
options(scipen = 999)
Sys.setlocale("LC_TIME", "C")
server <- function(input, output, session) {
  
  delayed_top_n <- debounce(reactive({
    val <- input$top_n
    if (is.null(val)) return(10)
    val <- max(1, min(val, 50))  
    val
  }), millis = 300)
  
  filter_country <- reactive({
    base_df %>%
      filter(Country %in% input$grouped_options)
  })
  
  filter_date <- reactive({
    filter_country() %>%
      filter(!is.na(Date)) %>%
      filter(between(Date, input$daterange[1], input$daterange[2]))
  })
  
  
  # Final data po to, żeby na gotowo wpisywać do plotów. W razie czego mamy przygotowane na kolejne operacji.
  final_data <- reactive({
    filter_date() %>%
      filter(!is.na(Population) & Population > 0) %>%
      group_by(Case, Date) %>%
      mutate(Value = sum(Value, na.rm = TRUE)) %>%
      ungroup()
  })
  
  # To odpowiada za przyciski i zmiane kolorow przyciskow
  toggleSelection <- function(button_id, options_to_toggle) {
    observeEvent(input[[button_id]], {
      selected_now <- input$grouped_options
      if (all(options_to_toggle %in% selected_now)) {
        # Jeśli wszystkie opcje są wybrane -> usuń je
        new_selected <- setdiff(selected_now, options_to_toggle)
      } else {
        # W przeciwnym wypadku -> dodaj opcje
        new_selected <- union(selected_now, options_to_toggle)
      }
      updateSelectizeInput(session, "grouped_options", selected = new_selected)
      
      # Reset kolorów wszystkich przycisków
      lapply(c("select_all", "select_africa", "select_asia", "select_australia",
               "select_europe", "select_north_america", "select_south_america", "clear_all"), function(btn) {
                 removeClass(selector = paste0("#", btn), class = "active-btn")
               })
      
      # Ustaw kolor tylko klikniętemu przyciskowi
      addClass(selector = paste0("#", button_id), class = "active-btn")
    })
  }
  
  # Wywołania funkcji dla wszystkich przycisków
  toggleSelection("select_all", unique_all)
  toggleSelection("select_africa", unique_africa)
  toggleSelection("select_asia", unique_asia)
  toggleSelection("select_australia", unique_australia_oceania)
  toggleSelection("select_europe", unique_europe)
  toggleSelection("select_north_america", unique_north_america)
  toggleSelection("select_south_america", unique_south_america)
  
  # Wyczyść wszystko
  observeEvent(input$clear_all, {
    updateSelectizeInput(session, "grouped_options", selected = character(0))
    
    # Reset kolorów wszystkich przycisków
    lapply(c("select_all", "select_africa", "select_asia", "select_australia",
             "select_europe", "select_north_america", "select_south_america", "clear_all"), function(btn) {
               removeClass(selector = paste0("#", btn), class = "active-btn")
             })
    
    addClass(selector = "#clear_all", class = "active-btn")
  })
  
  
  output$selection <- renderPrint({
    input$grouped_options
  })
  
  output$timeseries_plot <- renderPlot({
    ggplot(final_data(), aes(x = Date, y = Value / 1e3, color = Case)) +
      geom_line() + 
      scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
      scale_y_continuous(labels = function(x) paste0(x, " tys.")) +
      labs(y = "Value (tys.)") +
      theme(
        legend.position = "top",
        axis.text = element_text(
          color = "black",
          size = "12"
        ),
        text = element_text(family = "serif"),
        panel.background = element_rect(fill = "white", color = NA),  # białe tło, brak obramowania
        panel.grid.major = element_line(color = "gray90", size = 0.5),  # główne linie jak w minimal
        panel.grid.minor = element_line(color = "gray95", size = 0.25),  # drobniejsze linie
        axis.line = element_blank(),  # brak linii osi
        axis.ticks = element_blank(),  # brak kresek na osiach
      )
    
  })
  output$histogram <- renderPlot({
    df <- final_data() %>%
      filter(Date == max(Date)) %>%             # ← tylko ostatni dzień
      filter(!is.na(Population), Population > 0) %>%
      group_by(Case) %>%
      summarise(Value = sum(Value, na.rm = TRUE)) %>%
      arrange(desc(Value))
    
    ggplot(df, aes(x = reorder(Case, -Value), y = Value / 1e6, fill = Case)) +
      geom_col() +
      scale_y_continuous(labels = function(x) paste0(x, " mln")) +
      labs(
        title = "Current case distribution",
        y = "Value (mln)",
        x = NULL
      ) +
      theme(
        axis.text = element_text(color = "black", size = 12),
        legend.position = "none",
        text = element_text(family = "serif"),
        panel.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "gray90", size = 0.5),
        panel.grid.minor = element_line(color = "gray95", size = 0.25),
        axis.line = element_blank(),
        axis.ticks = element_blank()
      )
  })
  
  
  
  comparison_data <- reactive({
    req(input$compare_country, input$comparison_group)
    
    ref_country <- base_df %>%
      filter(Country == input$compare_country,
             between(Date, input$compare_daterange[1], input$compare_daterange[2]),
             !is.na(Population), Population > 0)
    
    group_countries <- base_df %>%
      filter(Country %in% input$comparison_group,
             between(Date, input$compare_daterange[1], input$compare_daterange[2])) %>%
      group_by(Date, Case) %>%
      summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
      mutate(Country = "Avg of group")
    
    ref_country %>%
      bind_rows(group_countries)
  })
  
  output$comparison_plot <- renderPlot({
    req(nrow(comparison_data()) > 0)
    
    ggplot(comparison_data(), aes(x = Date, y = Value / 1e3, color = Country)) +
      geom_line() +
      scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
      scale_y_continuous(labels = function(x) paste0(x, " tys.")) +
      labs(y = "Value (tys.)", x = "Date") +
      facet_wrap(~Case, scales = "free_y") +
      theme_minimal() +
      theme(
        legend.position = "top",
        axis.text = element_text(size = 12),
        text = element_text(family = "serif")
      )
  })
  
  output$compare_ref_stats <- renderUI({
    df <- comparison_data() %>% filter(Country == input$compare_country)
    
    if (nrow(df) == 0) return(HTML("<em>No data for selected country.</em>"))
    
    latest <- df %>%
      filter(Date == max(Date)) %>%
      pivot_wider(names_from = Case, values_from = Value)
    
    pop <- df %>% distinct(Population) %>% pull()
    
    HTML(glue::glue("
    <strong>Population:</strong> {format(round(pop), big.mark = ',')}<br>
    <strong>Confirmed:</strong> {format(round(latest$Confirmed), big.mark = ',')}<br>
    <strong>Recovered:</strong> {format(round(latest$Recovered), big.mark = ',')}<br>
    <strong>Deaths:</strong> {format(round(latest$Deaths), big.mark = ',')}<br>
    <strong>Active:</strong> {format(round(latest$Active), big.mark = ',')}
  "))
  })
  
  output$compare_group_stats <- renderUI({
    df <- comparison_data() %>% filter(Country != input$compare_country)
    
    if (nrow(df) == 0) return(HTML("<em>No data for comparison group.</em>"))
    
    latest <- df %>%
      filter(Date == max(Date)) %>%
      group_by(Case) %>%
      summarise(Value = sum(Value, na.rm = TRUE)) %>%
      pivot_wider(names_from = Case, values_from = Value)
    
    pop <- base_df %>%
      filter(Country %in% input$comparison_group) %>%
      distinct(Country, Population) %>%
      summarise(Population = sum(Population, na.rm = TRUE)) %>%
      pull()
    
    
    HTML(glue::glue("
    <strong>Population:</strong> {format(round(pop), big.mark = ',')}<br>
    <strong>Confirmed:</strong> {format(round(latest$Confirmed), big.mark = ',')}<br>
    <strong>Recovered:</strong> {format(round(latest$Recovered), big.mark = ',')}<br>
    <strong>Deaths:</strong> {format(round(latest$Deaths), big.mark = ',')}<br>
    <strong>Active:</strong> {format(round(latest$Active), big.mark = ',')}
  "))
  })
  
  output$compare_ref_pie <- renderPlot({
    df <- comparison_data() %>%
      filter(Country == input$compare_country, Date == max(Date)) %>%
      pivot_wider(names_from = Case, values_from = Value)
    
    if (nrow(df) == 0) return(NULL)
    
    pie_df <- df %>%
      select(Recovered, Deaths, Active) %>%
      pivot_longer(cols = everything(), names_to = "Category", values_to = "Value")
    
    ggplot(pie_df, aes(x = "", y = Value, fill = Category)) +
      geom_col(width = 1) +
      coord_polar(theta = "y") +
      theme_void() +
      scale_fill_manual(values = c(
        "Recovered" = "#D685F0",
        "Deaths" = "#00C0E3",
        "Active" = "#F8766D"
      ))
  })
  
  
  output$compare_group_pie <- renderPlot({
    df <- comparison_data() %>%
      filter(Country != input$compare_country, Date == max(Date)) %>%
      group_by(Case) %>%
      summarise(Value = sum(Value, na.rm = TRUE)) %>%
      filter(Case %in% c("Recovered", "Deaths", "Active"))
    
    ggplot(df, aes(x = "", y = Value, fill = Case)) +
      geom_col(width = 1) +
      coord_polar(theta = "y") +
      theme_void() +
      scale_fill_manual(values = c(
        "Recovered" = "#D685F0",
        "Deaths" = "#00C0E3",
        "Active" = "#F8766D"
      ))
  })
  
  
  outputOptions(output, "compare_ref_stats", suspendWhenHidden = FALSE)
  outputOptions(output, "compare_group_stats", suspendWhenHidden = FALSE)
  
  
  
  ranking_data <- reactive({
    req(input$ranking_case, input$ranking_date, input$top_n, input$ranking_reference_country)
    
    df <- base_df %>%
      filter(Date == input$ranking_date,
             !is.na(Population), Population > 0,
             Case == input$ranking_case) %>%
      group_by(Country) %>%
      summarise(
        Value = sum(Value, na.rm = TRUE),
        Population = max(Population),
        .groups = "drop"
      ) %>%
      mutate(per_1M = round(1e6 * Value / Population, 2))
    
    if (isTRUE(input$ranking_hide_nulls)) {
      df <- df %>% filter(!is.na(per_1M), per_1M > 0)
    }
    
    if (input$ranking_reference_country == "-- None --") {
      # Tryb klasyczny
      if (input$ranking_order == "desc") {
        return(df %>% arrange(desc(per_1M)) %>% slice_head(n = input$top_n))
      } else {
        return(df %>% arrange(per_1M) %>% slice_head(n = input$top_n))
      }
    }
    
    # Tryb z krajem referencyjnym
    ref_country <- input$ranking_reference_country
    ref_row <- df %>% filter(Country == ref_country)
    if (nrow(ref_row) == 0) return(data.frame())
    
    ref_value <- ref_row$per_1M
    
    df <- df %>%
      filter(Country != ref_country) %>%
      arrange(per_1M) %>%
      mutate(Diff = per_1M - ref_value)
    
    # Podział na mniejsze i większe
    lower <- df %>% filter(per_1M < ref_value)
    higher <- df %>% filter(per_1M > ref_value)
    
    # Budowanie naprzemiennej listy
    n_needed <- input$top_n
    selected <- data.frame()
    
    i <- 1
    while (nrow(selected) < n_needed && (i <= nrow(lower) || i <= nrow(higher))) {
      if (i <= nrow(higher)) {
        selected <- bind_rows(selected, higher[i, ])
      }
      if (i <= nrow(lower) && nrow(selected) < n_needed) {
        selected <- bind_rows(selected, lower[nrow(lower) - i + 1, ])
      }
      i <- i + 1
    }
    
    bind_rows(selected, ref_row) %>% arrange(per_1M)
  })
  
  
  
  
  observe({
    if (input$top_n > 25) {
      updateNumericInput(session, "top_n", value = 25)
    } else if (input$top_n < 1) {
      updateNumericInput(session, "top_n", value = 1)
    } else if (input$top_n == '-') {
      updateNumericInput(session, "top_n", value = 1)
    } 
  })
  
  output$ranking_plot <- renderPlotly({
    df <- ranking_data()
    if (nrow(df) == 0) {
      return(NULL)
    }
    
    top_n_val <- delayed_top_n()
    
    if (input$ranking_reference_country == "-- None --") {
      df <- df %>%
        arrange(if (input$ranking_order == "desc") desc(per_1M) else per_1M) %>%
        slice_head(n = top_n_val)
      
      df$Country <- factor(
        df$Country,
        levels = if (input$ranking_order == "desc") rev(df$Country) else df$Country
      )
      
      p <- ggplot(df, aes(x = Country, y = per_1M, text = Country, key = Country)) +
        geom_col(fill = "#F8766D") +
        coord_flip() +
        labs(
          title = paste("Top", top_n_val, "countries by", input$ranking_case),
          y = paste(input$ranking_case, "cases per 1 mln"),
          x = NULL
        ) +
        theme_minimal() +
        theme(
          legend.position = "none",
          axis.text = element_text(size = 12),
          text = element_text(family = "serif")
        )
    } else {
      ref_country <- input$ranking_reference_country
      
      df <- df %>%
        mutate(Direction = ifelse(Country == ref_country, "Reference", "Other")) %>%
        arrange(per_1M) %>%
        mutate(Country = factor(Country, levels = Country))
      
      p <- ggplot(df, aes(x = Country, y = per_1M, fill = Direction, text = Country, key = Country)) +
        geom_col() +
        scale_fill_manual(values = c("Other" = "#F8766D", "Reference" = "#00BFC4")) +
        coord_flip() +
        labs(
          title = paste("Countries closest to", ref_country, "in", input$ranking_case),
          y = paste(input$ranking_case, "cases per 1 mln"),
          x = NULL
        ) +
        theme_minimal() +
        theme(
          legend.position = "top",
          axis.text = element_text(size = 12),
          text = element_text(family = "serif")
        )
    }
    
    ggplotly(p, tooltip = "text", source = "ranking_click")
  })
  
  
  
  
  output$ranking_stats <- renderUI({
    df <- ranking_data()
    if (nrow(df) == 0) return(NULL)
    
    avg <- round(mean(df$per_1M, na.rm = TRUE), 0)
    med <- round(median(df$per_1M, na.rm = TRUE), 0)
    maxv <- round(max(df$per_1M, na.rm = TRUE), 0)
    minv <- round(min(df$per_1M, na.rm = TRUE), 0)
    
    HTML(glue::glue("
    <hr>
    <h5>Ranking Summary</h5>
    <strong>Average:</strong> {avg} / 1 mln<br>
    <strong>Median:</strong> {med} / 1 mln<br>
    <strong>Max:</strong> {maxv} / 1 mln<br>
    <strong>Min:</strong> {minv} / 1 mln<br>
  "))
  })

  selected_detail_country <- reactiveVal(NULL)
  selected_detail_case <- reactiveVal("Confirmed")
  
  observeEvent(plotly::event_data("plotly_click", source = "ranking_click"), {
    clicked <- plotly::event_data("plotly_click", source = "ranking_click")
    if (!is.null(clicked)) {
      if (!is.null(clicked$key)) {
        country_name <- clicked$key
        selected_detail_country(country_name)
        selected_detail_case(input$ranking_case)
        updateTabsetPanel(session, "tabs", selected = "Country Details View")
      } else {
        message("Klikniety punkt nie zawiera pola 'key'.")
      }
    } else {
      message("Brak danych klikniecia plotly_click.")
    }
  })
  
  
  observeEvent(input$back_to_ranking, {
    updateTabsetPanel(session, "tabs", selected = "Country Ranking")
  })
  
  output$detail_title <- renderText({
    req(selected_detail_country())
    paste("Detailed view for", selected_detail_country())
  })
  
  detail_data <- reactive({
    req(selected_detail_country())
    base_df %>%
      filter(Country == selected_detail_country(),
             between(Date, min_date, max_date),
             !is.na(Population), Population > 0)
  })
  
  output$detail_timeseries <- renderPlotly({
    p <- ggplot(detail_data(), aes(x = Date, y = Value, color = Case)) +
      geom_line(size = 1) +
      scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
      labs(
        y = "Cases",
        x = "Date"
      ) +
      theme_minimal() +
      theme(
        legend.position = "top",
        axis.text = element_text(size = 12),
        text = element_text(family = "serif")
      )
    
    ggplotly(p)
  })
  
  
  output$detail_histogram <- renderPlot({
    latest <- detail_data() %>%
      filter(Date == max(Date)) %>%
      pivot_wider(names_from = Case, values_from = Value) %>%
      mutate(across(c(Confirmed, Deaths, Recovered, Active),
                    ~ round(1e6 * .x / Population, 2),
                    .names = "{.col}_per_1M")) %>%
      pivot_longer(cols = ends_with("_per_1M"),
                   names_to = "Case", values_to = "Value") %>%
      mutate(Case = gsub("_per_1M", "", Case)) %>%
      mutate(Case = fct_reorder(Case, Value, .desc = TRUE))
    
    ggplot(latest, aes(x = Case, y = Value, fill = Case)) +
      geom_col() +
      scale_fill_manual(values = c(
        "Confirmed" = "#7CAE00",
        "Recovered" = "#CC79A7",
        "Deaths"    = "#00BFC4",
        "Active"    = "#F8766D"
      )) +
      labs(y = "per 1 mln", x = NULL) +
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.text = element_text(size = 12),
        text = element_text(family = "serif")
      )
  })
  
  
  
  output$detail_table <- DT::renderDataTable({
    detail_data() %>%
      select(Date, Case, Value) %>%
      pivot_wider(names_from = Case, values_from = Value) %>%
      arrange(Date)
  }, options = list(pageLength = 6, lengthChange = FALSE))
  
  output$detail_summary <- renderUI({
    latest <- detail_data() %>%
      filter(Date == max(Date)) %>%
      pivot_wider(names_from = Case, values_from = Value)
    
    pop <- detail_data() %>% distinct(Population) %>% pull()
    
    HTML(glue::glue("
    <div style='display: flex; justify-content: space-between; margin-bottom: 15px;'>
      <div><strong>Population:</strong><br>{format(round(pop), big.mark=',')}</div>
      <div><strong>Confirmed:</strong><br>{format(round(latest$Confirmed), big.mark=',')}</div>
      <div><strong>Recovered:</strong><br>{format(round(latest$Recovered), big.mark=',')}</div>
      <div><strong>Deaths:</strong><br>{format(round(latest$Deaths), big.mark=',')}</div>
      <div><strong>Active:</strong><br>{format(round(latest$Active), big.mark=',')}</div>
    </div>
  "))
  })
  
  output$detail_rank_info <- renderText({
    req(selected_detail_country())
    rank_df <- base_df %>%
      filter(Date == max(Date), Case == "Confirmed", !is.na(Population)) %>%
      group_by(Country) %>%
      summarise(Value = sum(Value), Population = max(Population)) %>%
      mutate(per_1M = 1e6 * Value / Population) %>%
      arrange(desc(per_1M)) %>%
      mutate(Rank = row_number())
    
    r <- rank_df %>% filter(Country == selected_detail_country()) %>% pull(Rank)
    
    paste("Global rank in Confirmed cases per 1 mln:", r)
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0(selected_detail_country(), "_covid_data.csv")
    },
    content = function(file) {
      write.csv(detail_data(), file, row.names = FALSE)
    }
  )
  
  output$global_rank_info <- renderUI({
    req(selected_detail_country(), selected_detail_case())
    
    current_case <- selected_detail_case()
    
    df <- base_df %>%
      filter(Date == max_date,
             Case == current_case,
             !is.na(Population),
             Population > 0) %>%
      group_by(Country) %>%
      summarise(per_1M = sum(Value) * 1e6 / max(Population)) %>%
      arrange(desc(per_1M)) %>%
      mutate(rank = row_number())
    
    country <- selected_detail_country()
    country_rank <- df %>% filter(Country == country)
    
    if (nrow(country_rank) == 0) return(NULL)
    
    # Dopasuj kolor do kategorii
    case_color <- case_when(
      current_case == "Confirmed" ~ "#7CAE00",
      current_case == "Recovered" ~ "#CC79A7",
      current_case == "Deaths" ~ "#00BFC4",
      current_case == "Active" ~ "#F8766D",
      TRUE ~ "#000000"
    )
    
    HTML(glue::glue(
      "<div style='text-align:center;'>
       <span style='font-size:22px;'>
         Global rank in <span style='color:{case_color};'><strong>{current_case}</strong></span> cases per 1 mln:
         <strong style='color:{case_color}; font-size:26px;'>{country_rank$rank}</strong>
       </span>
     </div>"
    ))
  })
  
  
  
  output$country_stats <- renderUI({
    df <- detail_data()
    latest <- df %>% filter(Date == max(Date)) %>% pivot_wider(names_from = Case, values_from = Value)
    pop <- df %>% distinct(Population) %>% pull()
    
    HTML(glue::glue(
      "<strong>Population:</strong> {format(round(pop), big.mark = ',')}<br>
     <strong>Confirmed:</strong> {format(round(latest$Confirmed), big.mark = ',')}<br>
     <strong>Recovered:</strong> {format(round(latest$Recovered), big.mark = ',')}<br>
     <strong>Deaths:</strong> {format(round(latest$Deaths), big.mark = ',')}<br>
     <strong>Active:</strong> {format(round(latest$Active), big.mark = ',')}"
    ))
  })
  
  
  output$global_stats <- renderUI({
    df <- final_data()
    
    if (nrow(df) == 0) {
      stats <- data.frame(
        Label = c("Total population", "Total confirmed cases", "Total recovered", "Total deaths", "Total active"),
        Value = c(0, 0, 0, 0, 0)
      )
    } else {
      df_wide <- df %>%
        pivot_wider(names_from = Case, values_from = Value, values_fill = 0) %>%
        group_by(Date) %>%
        summarise(across(Confirmed:Active, sum, na.rm = TRUE), .groups = "drop") %>%
        slice_tail(n = 1)
      
      pop_sum <- filter_date() %>%
        distinct(Country, Population) %>%
        summarise(Population = sum(Population, na.rm = TRUE)) %>%
        pull()
      
      stats <- data.frame(
        Label = c("Total population", "Total confirmed cases", "Total recovered", "Total deaths", "Total active"),
        Value = c(pop_sum, df_wide$Confirmed, df_wide$Recovered, df_wide$Deaths, df_wide$Active)
      )
    }
    
    html <- paste0(
      "<table style='width:100%; font-size: 14px; line-height: 1.6;'>",
      paste0(
        apply(stats, 1, function(row) {
          sprintf("<tr><td><strong>%s:</strong></td><td style='text-align:right;'><strong>%s</strong></td></tr>",
                  row[["Label"]], format(as.numeric(row[["Value"]]), big.mark = ","))
        }),
        collapse = ""
      ),
      "</table>"
    )
    
    HTML(html)
  })
  
  
  
  
  output$global_pie <- renderPlot({
    df <- final_data() %>%
      filter(Date == max(Date)) %>%
      pivot_wider(names_from = Case, values_from = Value, values_fill = 0)
    
    if (nrow(df) == 0) return(NULL)
    
    df_sum <- df %>%
      summarise(across(c(Recovered, Deaths, Active), sum)) %>%
      pivot_longer(cols = everything(),
                   names_to = "Category", values_to = "Value")
    
    color_map <- c(
      "Recovered" = "#D685F0",
      "Deaths" = "#00C0E3",     
      "Active" = "#F8766D"      
    )
    
    ggplot(df_sum, aes(x = "", y = Value, fill = Category)) +
      geom_col(width = 1) +
      coord_polar(theta = "y") +
      theme_void() +
      labs(title = "Distribution of current cases") +
      scale_fill_manual(values = color_map)
  })
  
  
  # Obsługa przycisków dla zakładki "Country Comparison"
  observeEvent(input$compare_select_all, {
    updateSelectizeInput(session, "comparison_group", selected = unique_all)
  })
  
  observeEvent(input$compare_select_africa, {
    updateSelectizeInput(session, "comparison_group", selected = unique_africa)
  })
  
  observeEvent(input$compare_select_asia, {
    updateSelectizeInput(session, "comparison_group", selected = unique_asia)
  })
  
  observeEvent(input$compare_select_australia, {
    updateSelectizeInput(session, "comparison_group", selected = unique_australia_oceania)
  })
  
  observeEvent(input$compare_select_europe, {
    updateSelectizeInput(session, "comparison_group", selected = unique_europe)
  })
  
  observeEvent(input$compare_select_north_america, {
    updateSelectizeInput(session, "comparison_group", selected = unique_north_america)
  })
  
  observeEvent(input$compare_select_south_america, {
    updateSelectizeInput(session, "comparison_group", selected = unique_south_america)
  })
  
  observeEvent(input$compare_clear_all, {
    updateSelectizeInput(session, "comparison_group", selected = character(0))
  })
  
  
}