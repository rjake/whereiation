library(tidyverse)

change_case <- function(x) {
  str_replace_all(
    x,
    pattern = "([a-z])([A-Z])",
    replacement = "\\1_\\2"
  ) %>%
    tolower()
}


raw_data <-
  read_csv("https://raw.githubusercontent.com/IBM/employee-attrition-aif360/master/data/emp_attrition.csv") %>%
  rename_all(change_case)


employee_attrition <-
  raw_data %>%
  select(
    -c(employee_count, over18, standard_hours, employee_number),
    -matches("(dai|month)ly_rate")
  ) %>%
  mutate(
    attrition = as.integer(attrition == "Yes"),
    marital_status = recode(marital_status, "Divorced" = "Single"),
    education = recode(
      education,
      "1" = "Below Associate",
      "2" = "Associate",
      "3" = "Bachelor",
      "4" = "Master",
      "5" = "Doctor"
    ),
    job_level = recode(
      job_level,
      "1" = "Intern",
      "2" = "Junior",
      "3" = "Mid-level",
      "4" = "Senior",
      "5" = "Director"
    ),
    job_involvement = recode(
      job_involvement,
      "1" = "Low",
      "2" = "Medium",
      "3" = "High",
      "4" = "Very High"
    ),
    environment_satisfaction = recode(
      environment_satisfaction,
      "1" = "Low",
      "2" = "Medium",
      "3" = "High",
      "4" = "Very High"
    ),
    job_satisfaction = recode(
      job_satisfaction,
      "1" = "Low",
      "2" = "Medium",
      "3" = "High",
      "4" = "Very High"
    ),
    performance_rating = recode(
      performance_rating,
      "1" = "Low",
      "2" = "Good",
      "3" = "Excellent",
      "4" = "Outstanding"
    ),
    relationship_satisfaction = recode(
      relationship_satisfaction,
      "1" = "Low",
      "2" = "Medium",
      "3" = "High",
      "4" = "Very High"
    ),
    work_life_balance = recode(
      work_life_balance,
      "1" = "Bad",
      "2" = "Good",
      "3" = "Better",
      "4" = "Best"
    )
  ) %>%
  select(
    attrition, gender,
    job_level, department, job_role,
    education, education_field,
    over_time, hourly_rate, monthly_income,
    age,
    performance_rating,
    work_life_balance,
    everything()
  ) %>%
  print()


# employee_attrition %>% plot_deltas(dep_var = "attrition")

usethis::use_data(employee_attrition, overwrite = TRUE)
