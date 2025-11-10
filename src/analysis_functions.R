
library(dplyr)
library(ggplot2)
library(plotly)

# diabetes trends plot


plot_diabetes_trends <- function(data, countries, sex_filter = "Men") {

  
  plot_data <- data %>%
    filter(country %in% countries, sex == sex_filter)
  
  p <- ggplot(plot_data, aes(x = year, y = diabetes_prev, color = country)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    labs(
      title = "Diabetes Prevalence Trends",
      subtitle = paste("Sex:", sex_filter),
      x = "Year",
      y = "Diabetes Prevalence (%)",
      color = "Country"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "bottom"
    )
  
  return(ggplotly(p))
}


# obesity trends plot


plot_obesity_trends <- function(data, countries, sex_filter = "Men") {

  plot_data <- data %>%
    filter(country %in% countries, sex == sex_filter)
  
  p <- ggplot(plot_data, aes(x = year, y = obesity_prev, color = country)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    labs(
      title = "Obesity Prevalence Trends",
      subtitle = paste("Sex:", sex_filter),
      x = "Year",
      y = "Obesity Prevalence (%)",
      color = "Country"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "bottom"
    )
  
  return(ggplotly(p))
}


# obesity-diabetes correlation 


plot_obesity_diabetes_correlation <- function(data, year_filter = NULL, sex_filter = "Men") {
  
  plot_data <- data %>%
    filter(sex == sex_filter)
  
  if (!is.null(year_filter)) {
    plot_data <- plot_data %>% filter(year == year_filter)
  }
  
  plot_data <- plot_data %>%
    filter(!is.na(obesity_prev), !is.na(diabetes_prev))
  
  cor_value <- cor(plot_data$obesity_prev, plot_data$diabetes_prev)
  
  p <- ggplot(plot_data, aes(x = obesity_prev, y = diabetes_prev)) +
    geom_point(alpha = 0.6, color = "steelblue", size = 3) +
    geom_smooth(method = "lm", color = "red", se = TRUE, linewidth = 1.2) +
    labs(
      title = "Relationship between Obesity and Diabetes",
      subtitle = paste(ifelse(!is.null(year_filter), 
                              paste("Year:", year_filter), 
                              "All years"), "-", sex_filter,
                       "| Correlation:", round(cor_value, 3)),
      x = "Obesity Prevalence (%)",
      y = "Diabetes Prevalence (%)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11)
    )
  
  return(p) 
}


# gender differences plot


plot_sex_differences <- function(data, country_name) {
  
  plot_data <- data %>%
    filter(country == country_name, sex %in% c("Men", "Women"))
  
  p <- ggplot(plot_data, aes(x = year, y = diabetes_prev, color = sex)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    labs(
      title = paste("Sex Differences in Diabetes Prevalence:", country_name),
      x = "Year",
      y = "Diabetes Prevalence (%)",
      color = "Sex"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 12),
      legend.position = "bottom"
    )
  
  return(ggplotly(p))
}


# top countries bar chart


plot_top_countries <- function(data, year_filter, n = 10, variable = "diabetes", sex_filter = "Men") {
  
  var_col <- ifelse(variable == "diabetes", "diabetes_prev", "obesity_prev")
  var_label <- ifelse(variable == "diabetes", "Diabetes", "Obesity")
  
  plot_data <- data %>%
    filter(year == year_filter, sex == sex_filter) %>%
    arrange(desc(.data[[var_col]])) %>%
    head(n)
  
  p <- ggplot(plot_data, aes(x = reorder(country, .data[[var_col]]), 
                             y = .data[[var_col]])) +
    geom_col(fill = "steelblue", alpha = 0.8) +
    geom_text(aes(label = round(.data[[var_col]], 1)), 
              hjust = -0.2, size = 3.5) +
    coord_flip() +
    labs(
      title = paste("Top", n, "Countries by", var_label, "Prevalence"),
      subtitle = paste("Year:", year_filter, "-", sex_filter),
      x = NULL,
      y = paste(var_label, "Prevalence (%)")
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11),
      axis.text.y = element_text(size = 10)
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
  
  return(p)  
}


# world map - Diabetes


plot_diabetes_world_map <- function(data, year_filter, sex_filter = "Men") {
 
  map_data <- data %>%
    filter(year == year_filter, sex == sex_filter) %>%
    select(country, iso, diabetes_prev, treated_prop) %>%
    mutate(
      hover_text = paste0(
        "<b>", country, "</b><br>",
        "Diabetes Prevalence: ", round(diabetes_prev, 2), "%<br>",
        "Treatment Rate: ", ifelse(!is.na(treated_prop), 
                                   paste0(round(treated_prop, 2), "%"), 
                                   "N/A")
      )
    )
  
  fig <- plot_ly(
    data = map_data,
    type = 'choropleth',
    locations = ~iso,
    z = ~diabetes_prev,
    text = ~hover_text,
    hoverinfo = 'text',
    colorscale = 'Reds',
    marker = list(line = list(color = 'rgb(255,255,255)', width = 0.5)),
    colorbar = list(title = "Diabetes<br>Prevalence (%)")
  ) %>%
    layout(
      title = list(
        text = paste0("Global Diabetes Prevalence (", year_filter, ")<br>",
                      "<sub>", sex_filter, "</sub>"),
        x = 0.5
      ),
      geo = list(
        showframe = FALSE,
        showcoastlines = TRUE,
        projection = list(type = 'natural earth')
      )
    )
  
  return(fig)
}


# map - Treatment Rates


plot_treatment_world_map <- function(data, year_filter, sex_filter = "Men") {

  map_data <- data %>%
    filter(year == year_filter, sex == sex_filter) %>%
    filter(!is.na(treated_prop)) %>%
    select(country, iso, diabetes_prev, treated_prop) %>%
    mutate(
      hover_text = paste0(
        "<b>", country, "</b><br>",
        "Treatment Rate: ", round(treated_prop, 2), "%<br>",
        "Diabetes Prevalence: ", round(diabetes_prev, 2), "%"
      )
    )
  
  fig <- plot_ly(
    data = map_data,
    type = 'choropleth',
    locations = ~iso,
    z = ~treated_prop,
    text = ~hover_text,
    hoverinfo = 'text',
    colorscale = 'Blues',
    marker = list(line = list(color = 'rgb(255,255,255)', width = 0.5)),
    colorbar = list(title = "Treatment<br>Rate (%)")
  ) %>%
    layout(
      title = list(
        text = paste0("Diabetes Treatment Rates (", year_filter, ")<br>",
                      "<sub>", sex_filter, "</sub>"),
        x = 0.5
      ),
      geo = list(
        showframe = FALSE,
        showcoastlines = TRUE,
        projection = list(type = 'natural earth')
      )
    )
  
  return(fig)
}


# map - Obesity


plot_obesity_world_map <- function(data, year_filter, sex_filter = "Men") {

  map_data <- data %>%
    filter(year == year_filter, sex == sex_filter) %>%
    select(country, iso, obesity_prev) %>%
    mutate(
      hover_text = paste0(
        "<b>", country, "</b><br>",
        "Obesity Prevalence: ", round(obesity_prev, 2), "%"
      )
    )
  
  fig <- plot_ly(
    data = map_data,
    type = 'choropleth',
    locations = ~iso,
    z = ~obesity_prev,
    text = ~hover_text,
    hoverinfo = 'text',
    colorscale = 'Oranges',
    marker = list(line = list(color = 'rgb(255,255,255)', width = 0.5)),
    colorbar = list(title = "Obesity<br>Prevalence (%)")
  ) %>%
    layout(
      title = list(
        text = paste0("Global Obesity Prevalence (", year_filter, ")<br>",
                      "<sub>", sex_filter, "</sub>"),
        x = 0.5
      ),
      geo = list(
        showframe = FALSE,
        showcoastlines = TRUE,
        projection = list(type = 'natural earth')
      )
    )
  
  return(fig)
}

# treatment vs prevalence scat

plot_treatment_vs_prevalence <- function(data, year_filter, sex_filter = "Men") {

  plot_data <- data %>%
    filter(year == year_filter, sex == sex_filter) %>%
    filter(!is.na(treated_prop), !is.na(diabetes_prev))
  
  fig <- plot_ly(
    data = plot_data,
    x = ~diabetes_prev,
    y = ~treated_prop,
    text = ~country,
    type = 'scatter',
    mode = 'markers',
    marker = list(
      size = 10,
      color = ~diabetes_prev,
      colorscale = 'Viridis',
      showscale = TRUE,
      colorbar = list(title = "Diabetes<br>Prevalence")
    ),
    hovertemplate = paste(
      '<b>%{text}</b><br>',
      'Diabetes Prevalence: %{x:.2f}%<br>',
      'Treatment Rate: %{y:.2f}%<br>',
      '<extra></extra>'
    )
  ) %>%
    layout(
      title = paste0("Treatment Rate vs Diabetes Prevalence (", year_filter, ")"),
      xaxis = list(title = "Diabetes Prevalence (%)"),
      yaxis = list(title = "Treatment Rate (%)"),
      hovermode = 'closest'
    )
  
  return(fig)
}


# correlation


calculate_correlation <- function(data, year_filter = NULL, sex_filter = "Men") {
 
  analysis_data <- data %>%
    filter(sex == sex_filter)
  
  if (!is.null(year_filter)) {
    analysis_data <- analysis_data %>% filter(year == year_filter)
  }
  
  analysis_data <- analysis_data %>%
    filter(!is.na(obesity_prev), !is.na(diabetes_prev))
  
  cor_test <- cor.test(analysis_data$obesity_prev, analysis_data$diabetes_prev)
  
  return(list(
    correlation = cor_test$estimate,
    p_value = cor_test$p.value,
    n_observations = nrow(analysis_data)
  ))
}


# Summary country stats


summarize_country <- function(data, country_name, sex_filter = "Men") {
 
  summary_stats <- data %>%
    filter(country == country_name, sex == sex_filter) %>%
    summarise(
      country = first(country),
      years_available = n_distinct(year),
      year_range = paste(min(year), "-", max(year)),
      mean_diabetes = round(mean(diabetes_prev, na.rm = TRUE), 2),
      latest_diabetes = round(last(diabetes_prev[order(year)]), 2),
      mean_obesity = round(mean(obesity_prev, na.rm = TRUE), 2),
      latest_obesity = round(last(obesity_prev[order(year)]), 2),
      diabetes_change = round(last(diabetes_prev[order(year)]) - 
                                first(diabetes_prev[order(year)]), 2)
    )
  
  return(summary_stats)
}