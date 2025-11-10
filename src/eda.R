library(ProjectTemplate)
library(dplyr)
library(ggplot2)
library(plotly)

load.project()

str(diabetes_clean)
str(bmi_clean)
str(combined_data)

cat("Diabetes dataset dimensions:", dim(diabetes_clean), "\n")
cat("BMI dataset dimensions:", dim(bmi_clean), "\n")
cat("Combined dataset dimensions:", dim(combined_data), "\n")

summary(diabetes_clean)
summary(bmi_clean)

cat("Unique countries:", n_distinct(diabetes_clean$country), "\n")
cat("Year range:", range(diabetes_clean$year, na.rm = TRUE), "\n")
cat("Sex categories:", unique(diabetes_clean$sex), "\n")

country_counts <- diabetes_clean %>%
  group_by(country) %>%
  summarise(n_observations = n(),
            years_available = n_distinct(year)) %>%
  arrange(desc(n_observations))

head(country_counts, 20)

countries_of_interest <- c("United Kingdom", "United States of America", 
                           "Germany", "Japan", "India", "China")

diabetes_subset <- diabetes_clean %>%
  filter(country %in% countries_of_interest, sex == "Men")

ggplot(diabetes_subset, aes(x = year, y = diabetes_prev, color = country)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(title = "Diabetes Prevalence Trends - Men",
       x = "Year", 
       y = "Diabetes Prevalence (%)") +
  theme_minimal()

latest_diabetes <- diabetes_clean %>%
  filter(year == max(year), sex == "Men") %>%
  arrange(desc(diabetes_prev)) %>%
  select(country, year, diabetes_prev) %>%
  head(10)

print(latest_diabetes)

obesity_subset <- bmi_clean %>%
  filter(country %in% countries_of_interest, sex == "Men")

ggplot(obesity_subset, aes(x = year, y = obesity_prev, color = country)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(title = "Obesity Prevalence Trends - Men",
       x = "Year", 
       y = "Obesity Prevalence (%)") +
  theme_minimal()

latest_obesity <- bmi_clean %>%
  filter(year == max(year), sex == "Men") %>%
  arrange(desc(obesity_prev)) %>%
  select(country, year, obesity_prev) %>%
  head(10)

print(latest_obesity)

uk_sex_comparison <- diabetes_clean %>%
  filter(country == "United Kingdom", sex %in% c("Men", "Women"))

ggplot(uk_sex_comparison, aes(x = year, y = diabetes_prev, color = sex)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(title = "Sex Differences in Diabetes - United Kingdom",
       x = "Year", 
       y = "Diabetes Prevalence (%)") +
  theme_minimal()

sex_diff_summary <- diabetes_clean %>%
  filter(sex %in% c("Men", "Women")) %>%
  group_by(country, sex) %>%
  summarise(mean_diabetes = mean(diabetes_prev, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = sex, values_from = mean_diabetes) %>%
  mutate(difference = Men - Women) %>%
  arrange(desc(difference))

head(sex_diff_summary, 10)

latest_year <- max(combined_data$year, na.rm = TRUE)

correlation_data <- combined_data %>%
  filter(year == latest_year, sex == "Men") %>%
  filter(!is.na(obesity_prev), !is.na(diabetes_prev))

ggplot(correlation_data, aes(x = obesity_prev, y = diabetes_prev)) +
  geom_point(alpha = 0.6, color = "steelblue", size = 3) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = paste("Obesity vs Diabetes Correlation -", latest_year),
       x = "Obesity Prevalence (%)",
       y = "Diabetes Prevalence (%)") +
  theme_minimal()

cor_result <- cor.test(correlation_data$obesity_prev, 
                       correlation_data$diabetes_prev)

cat("\nCorrelation coefficient:", round(cor_result$estimate, 3), "\n")
cat("P-value:", format(cor_result$p.value, scientific = TRUE), "\n")
cat("Observations:", nrow(correlation_data), "\n")

treatment_data <- diabetes_clean %>%
  filter(!is.na(treated_prop), year == latest_year, sex == "Men")

cat("\nTreatment data available for", nrow(treatment_data), "countries\n")

top_treatment <- treatment_data %>%
  arrange(desc(treated_prop)) %>%
  select(country, diabetes_prev, treated_prop) %>%
  head(10)

print(top_treatment)

ggplot(treatment_data, aes(x = diabetes_prev, y = treated_prop)) +
  geom_point(alpha = 0.6, color = "darkgreen", size = 3) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Treatment Rate vs Diabetes Prevalence",
       x = "Diabetes Prevalence (%)",
       y = "Treatment Rate (%)") +
  theme_minimal()

top_both <- combined_data %>%
  filter(year == latest_year, sex == "Men") %>%
  arrange(desc(diabetes_prev + obesity_prev)) %>%
  select(country, diabetes_prev, obesity_prev) %>%
  head(10)

print(top_both)

time_trends <- combined_data %>%
  filter(country %in% countries_of_interest, sex == "Men") %>%
  group_by(country) %>%
  arrange(year) %>%
  summarise(
    diabetes_first = first(diabetes_prev),
    diabetes_last = last(diabetes_prev),
    diabetes_change = diabetes_last - diabetes_first,
    obesity_first = first(obesity_prev),
    obesity_last = last(obesity_prev),
    obesity_change = obesity_last - obesity_first,
    .groups = "drop"
  )

print(time_trends)