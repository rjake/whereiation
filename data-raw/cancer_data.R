library(tidyverse)
df <-
  read_tsv("https://raw.githubusercontent.com/cognoma/cancer-data/master/data/complete/samples.tsv") |>
  drop_na(dead)

attr(df, "spec") <- NULL
attr(df, "problems") <- NULL

cancer_data <-
  df |>
  select_if(~mean(is.na(.x)) < 0.4) |>
  drop_na(ajcc_stage, race) |>
  select(
    -contains("disease_specific"),
    -c(
      acronym,
      histological_type, event_status, vital_status,
      last_contact_days_to, event_days, birth_days_to,
      progression_free_interval_status, tumor_status
    )
  ) |>
  rename(
    death_ind = dead,
    year_diagnosed = initial_pathologic_dx_year,
    n_days_progression_free = progression_free_interval_days
  ) |>
  filter(
    across(where(is.character), ~(!str_detect(.x, "\\["))),
    !str_detect(sample_type, "Additional"),
    days_survived > 0
  ) |>
  group_by(patient_id) |>
  filter(n() == 1) |>
  ungroup() |>
  select(-patient_id) |>
  drop_na() |>
  relocate(sample_id, .after = everything())


plot_spread(cancer_data, death_ind)
plot_spread(cancer_data, age_diagnosed, )

plot_deltas(cancer_data, death_ind)
plot_group_split(cancer_data, split_on = death_ind, type =  "percent_factor")

usethis::use_data(cancer_data, overwrite = TRUE)
