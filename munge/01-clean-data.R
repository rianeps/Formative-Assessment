library(dplyr)

diabetes_clean <- Diabetes %>%

  rename(
    country = `Country.Region.World`,
    iso = ISO,
    sex = Sex,
    year = Year,
    age = Age,
    diabetes_prev = `Prevalence.of.diabetes..18..years.`,
    diabetes_prev_lower = `Prevalence.of.diabetes..18..years..lower.95..uncertainty.interval`,
    diabetes_prev_upper = `Prevalence.of.diabetes..18..years..upper.95..uncertainty.interval`,
    treated_prop = `Proportion.of.people.with.diabetes.who.were.treated..30..years.`,
    treated_prop_lower = `Proportion.of.people.with.diabetes.who.were.treated..30..years..lower.95..uncertainty.interval`,
    treated_prop_upper = `Proportion.of.people.with.diabetes.who.were.treated..30..years..upper.95..uncertainty.interval`
  ) %>%

  filter(!is.na(diabetes_prev) | !is.na(treated_prop)) %>%
  
  mutate(year = as.numeric(year))

bmi_clean <- BMI %>%
  rename(
    year = Year,
    sex = Sex,
    country = `Country.Region.World`,
    iso = ISO,
    underweight_prev = `Prevalence.of.BMI.18.5.kg.m...underweight.`,
    underweight_prev_lower = `Prevalence.of.BMI.18.5.kg.m...underweight..lower.95..uncertainty.interval`,
    underweight_prev_upper = `Prevalence.of.BMI.18.5.kg.m...underweight..upper.95..uncertainty.interval`,
    obesity_prev = `Prevalence.of.BMI..30.kg.m...obesity.`,
    obesity_prev_lower = `Prevalence.of.BMI..30.kg.m...obesity..lower.95..uncertainty.interval`,
    obesity_prev_upper = `Prevalence.of.BMI..30.kg.m...obesity..upper.95..uncertainty.interval`,
    double_burden_prev = `Combined.prevalence.of.BMI.18.5.kg.m..and.BMI..30.kg.m...double.burden.of.underweight.and.obesity.`,
    double_burden_prev_lower = `Combined.prevalence.of.BMI.18.5.kg.m..and.BMI..30.kg.m...double.burden.of.underweight.and.obesity..lower.95..uncertainty.interval`,
    double_burden_prev_upper = `Combined.prevalence.of.BMI.18.5.kg.m..and.BMI..30.kg.m...double.burden.of.underweight.and.obesity..upper.95..uncertainty.interval`,
    obesity_proportion = `Proportion.of.double.burden.from.obesity`,
    obesity_proportion_lower = `Proportion.of.double.burden.from.obesity.lower.95..uncertainty.interval`,
    obesity_proportion_upper = `Proportion.of.double.burden.from.obesity.upper.95..uncertainty.interval`,
    bmi_18_5_to_20_prev = `Prevalence.of.BMI.18.5.kg.m..to..20.kg.m.`,
    bmi_20_to_25_prev = `Prevalence.of.BMI.20.kg.m..to..25.kg.m.`,
    bmi_25_to_30_prev = `Prevalence.of.BMI.25.kg.m..to..30.kg.m.`,
    bmi_30_to_35_prev = `Prevalence.of.BMI.30.kg.m..to..35.kg.m.`,
    bmi_35_to_40_prev = `Prevalence.of.BMI.35.kg.m..to..40.kg.m.`,
    morbid_obesity_prev = `Prevalence.of.BMI...40.kg.m...morbid.obesity.`
  ) %>%
  
  filter(!is.na(obesity_prev) | !is.na(underweight_prev)) %>%
 
  mutate(year = as.numeric(year))

combined_data <- diabetes_clean %>%
  inner_join(
    bmi_clean,
    by = c("country", "iso", "sex", "year")
  ) %>%
  # Keep only relevant columns for analysis
  select(
    country, iso, sex, year, age,
    diabetes_prev, diabetes_prev_lower, diabetes_prev_upper,
    obesity_prev, obesity_prev_lower, obesity_prev_upper,
    treated_prop, treated_prop_lower, treated_prop_upper
  )

both_sexes_data <- combined_data %>%
  # Filter for *only* Men and Women, just in case
  filter(sex %in% c("Men", "Women")) %>%
  group_by(country, iso, year, age) %>%
  summarise(
    # Calculate the mean for all numeric columns
    diabetes_prev = mean(diabetes_prev, na.rm = TRUE),
    diabetes_prev_lower = mean(diabetes_prev_lower, na.rm = TRUE),
    diabetes_prev_upper = mean(diabetes_prev_upper, na.rm = TRUE),
    obesity_prev = mean(obesity_prev, na.rm = TRUE),
    obesity_prev_lower = mean(obesity_prev_lower, na.rm = TRUE),
    obesity_prev_upper = mean(obesity_prev_upper, na.rm = TRUE),
    treated_prop = mean(treated_prop, na.rm = TRUE),
    treated_prop_lower = mean(treated_prop_lower, na.rm = TRUE),
    treated_prop_upper = mean(treated_prop_upper, na.rm = TRUE),
    .groups = "drop"  
  ) %>%
  mutate(sex = "Both sexes combined")  


combined_data <- bind_rows(combined_data, both_sexes_data)

country_summary <- combined_data %>%
  group_by(country) %>%
  summarise(
    mean_diabetes = mean(diabetes_prev, na.rm = TRUE),
    mean_obesity = mean(obesity_prev, na.rm = TRUE),
    years_available = n_distinct(year),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_diabetes))

cat("\n=== Data Cleaning Summary ===\n")
cat("Diabetes dataset rows:", nrow(diabetes_clean), "\n")
cat("BMI dataset rows:", nrow(bmi_clean), "\n")
cat("Combined dataset rows:", nrow(combined_data), "\n")
cat("Countries in combined data:", n_distinct(combined_data$country), "\n")
cat("Year range in diabetes:", range(diabetes_clean$year, na.rm = TRUE), "\n")
cat("Year range in BMI:", range(bmi_clean$year, na.rm = TRUE), "\n")
cat("Year range in combined:", range(combined_data$year, na.rm = TRUE), "\n")
cat("\nSex categories in diabetes:", unique(diabetes_clean$sex), "\n")
cat("Sex categories in BMI:", unique(bmi_clean$sex), "\n")