# About -------------------------------------------------------------------

# This script performs basic data manipulation on raw data
# Raw data is taken from PHS Open Data website

# Load libraries ----------------------------------------------------------
library(janitor)
library(tidyverse)


# Read stroke data --------------------------------------------------------

# Two sets of data:
#  1. Hospital activity related to strokes
#  2. Stroke mortality data
# Both datasets are produced at Health Board (hb) and Council Area (ca) level
# Columns ending 'qf' are codes relating to data quality and can be removed

activity_hb_raw <- read_csv("data/raw/stroke_activitybyhb.csv") %>%
  clean_names() %>%
  select(-ends_with("qf"))

activity_ca_raw <- read_csv("data/raw/stroke_activitybyca.csv") %>%
  clean_names() %>%
  select(-ends_with("qf"))

mortality_hb_raw <- read_csv("data/raw/stroke_mortalitybyhbr.csv") %>%
  clean_names() %>%
  select(-ends_with("qf"))

mortalilty_ca_raw <- read_csv("data/raw/stroke_mortalitybyca.csv") %>%
  clean_names() %>%
  select(-ends_with("qf"))


# Read population estimates -----------------------------------------------
population_hb_raw <- read_csv("data/raw/population_est_hb.csv") %>%
  clean_names() %>%
  select(-ends_with("qf"))

population_ca_raw <- read_csv("data/raw/population_est_ca.csv") %>%
  clean_names() %>%
  select(-ends_with("qf"))


# Read population projections ---------------------------------------------
population_proj_hb_raw <- read_csv("data/raw/population_proj_hb.csv") %>%
  clean_names() %>%
  select(-ends_with("qf"))

population_proj_ca_raw <- read_csv("data/raw/population_proj_ca.csv") %>%
  clean_names() %>%
  select(-ends_with("qf"))


# Read Health Board/Council id lookups ------------------------------------
health_board_id <- read_csv("data/raw/healthboard_id.csv") %>%
  clean_names() %>%
  select(hb, hb_name)

council_id <- read_csv("data/raw/council_id.csv") %>%
  clean_names() %>%
  select(ca, ca_name, hb_name)


# Wrangle activity data ---------------------------------------------------
activity_hb <- activity_hb_raw %>%
  left_join(health_board_id, by = c("hbr" = "hb")) %>%
  # get Health Board name
  mutate(
    year = str_sub(financial_year, 1, 4),
    hb_name = if_else(hbr == "S92000003", "Scotland", hb_name),
    number_of_discharges = coalesce(number_of_discharges, 0),
    crude_rate = coalesce(crude_rate, 0)
  ) %>%
  select(-financial_year) %>%
  relocate(year)

activity_ca <- activity_ca_raw %>%
  left_join(council_id, by = "ca") %>%
  # get Council name
  mutate(
    year = str_sub(financial_year, 1, 4),
    ca_name = if_else(ca == "S92000003", "Scotland", ca_name),
    hb_name = if_else(ca == "S92000003", "Scotland", hb_name),
    number_of_discharges = coalesce(number_of_discharges, 0),
    crude_rate = coalesce(crude_rate, 0)
  ) %>%
  select(-financial_year) %>%
  relocate(year)


# Wrangle mortality data --------------------------------------------------
mortality_hb <- mortality_hb_raw %>%
  left_join(health_board_id, by = c("hbr" = "hb")) %>% # get Health Board name
  mutate(
    hb_name = if_else(hbr == "S92000003", "Scotland", hb_name),
    number_of_deaths = coalesce(number_of_deaths, 0),
    crude_rate = coalesce(crude_rate, 0)
  )

mortality_ca <- mortalilty_ca_raw %>%
  left_join(council_id, by = "ca") %>% # get Council name
  mutate(
    ca_name = if_else(ca == "S92000003", "Scotland", ca_name),
    hb_name = if_else(ca == "S92000003", "Scotland", hb_name),
    number_of_deaths = coalesce(number_of_deaths, 0),
    crude_rate = coalesce(crude_rate, 0)
  )


# Wrangle population estimates --------------------------------------------
population_hb <- population_hb_raw %>%
  pivot_longer(
    cols = c("all_ages":"age90plus"),
    names_to = "age",
    values_to = "count"
  ) %>%
  left_join(health_board_id, by = c("hb" = "hb")) %>%  # get Health Board name
  mutate(
    age = if_else(str_sub(age, 1, 3) == "age",
      str_sub(age, 4, -1),
      age
    ),
    hb_name = if_else(hb == "S92000003", "Scotland", hb_name),
  ) %>%
  select(-id)

population_ca <- population_ca_raw %>%
  pivot_longer(
    cols = c("all_ages":"age90plus"),
    names_to = "age",
    values_to = "count"
  ) %>%
  left_join(council_id, by = "ca") %>%  # get Council name
  mutate(
    age = if_else(str_sub(age, 1, 3) == "age",
      str_sub(age, 4, -1),
      age
    ),
    ca_name = if_else(ca == "S92000003", "Scotland", ca_name),
    hb_name = if_else(ca == "S92000003", "Scotland", hb_name)
  ) %>%
  select(-id)


# Wrangle population projections ------------------------------------------
population_proj_hb <- population_proj_hb_raw %>%
  pivot_longer(
    cols = c("all_ages":"age90plus"),
    names_to = "age",
    values_to = "count"
  ) %>%
  left_join(health_board_id, by = c("hb" = "hb")) %>%  # get Health Board name
  mutate(
    age = if_else(str_sub(age, 1, 3) == "age",
      str_sub(age, 4, -1),
      age
    ),
    hb_name = if_else(hb == "S92000003", "Scotland", hb_name)
  ) %>%
  select(-id)

population_proj_ca <- population_proj_ca_raw %>%
  pivot_longer(
    cols = c("all_ages":"age90plus"),
    names_to = "age",
    values_to = "count"
  ) %>%
  left_join(council_id, by = "ca") %>%  # get Council name
  mutate(
    age = if_else(str_sub(age, 1, 3) == "age",
      str_sub(age, 4, -1),
      age
    ),
    ca_name = if_else(ca == "S92000003", "Scotland", ca_name),
    hb_name = if_else(ca == "S92000003", "Scotland", hb_name)
  ) %>%
  select(-id)


# Bind population estimate and projection datasets ------------------------

# Population estimates run from 1981 to 2019
# Population projections run from 2018 to 2043
# Drop 2018/2019 from projections and bind to population estimates
# Add three age bands:
# 1. Banding for population pyramids
# 2. Banding based on stroke data
# 3. Banding over/under 75

population_proj_hb <- population_proj_hb %>%
  filter(year > 2019)

population_hb <- population_hb %>%
  bind_rows(population_proj_hb)

population_by_ages_hb <- population_hb %>%
  filter(age != "all_ages") %>%
  mutate(
    age = if_else(age == "90plus", "90", age),
    age = as.numeric(age),
    age_band1 = case_when(
      age <= 4 ~ "0 - 9",
      age <= 9 ~ "05 - 9",
      age <= 14 ~ "10 - 14",
      age <= 19 ~ "15 - 19",
      age <= 24 ~ "20 - 24",
      age <= 29 ~ "25 - 29",
      age <= 34 ~ "30 - 34",
      age <= 39 ~ "35 - 39",
      age <= 44 ~ "40 - 44",
      age <= 49 ~ "45 - 49",
      age <= 54 ~ "50 - 54",
      age <= 59 ~ "55 - 59",
      age <= 64 ~ "60 - 64",
      age <= 69 ~ "65 - 69",
      age <= 74 ~ "70 - 74",
      age <= 79 ~ "75 - 79",
      age <= 84 ~ "80 - 84",
      age <= 89 ~ "85 - 89",
      TRUE ~ "90+"
    ),
    age_band2 = case_when(
      age < 45 ~ "0-44 years",
      age < 65 ~ "45-64 years",
      age < 75 ~ "65-74 years",
      TRUE ~ "75plus years"
    ),
    age_band3 = case_when(
      age < 75 ~ "under75 years",
      TRUE ~ "75plus years"
    )
  )

population_all_ages_hb <- population_hb %>%
  filter(age == "all_ages")


population_proj_ca <- population_proj_ca %>%
  filter(year > 2019)

population_ca <- population_ca %>%
  bind_rows(population_proj_ca)

population_by_ages_ca <- population_ca %>%
  filter(age != "all_ages") %>%
  mutate(
    age = if_else(age == "90plus", "90", age),
    age = as.numeric(age),
    age_band1 = case_when(
      age <= 18 ~ "0 - 18",
      age <= 30 ~ "18 - 30",
      age <= 40 ~ "31 - 40",
      age <= 50 ~ "41 - 50",
      age <= 60 ~ "51 - 60",
      age <= 70 ~ "61 - 70",
      age <= 80 ~ "71 - 80",
      age < 90 ~ "81 - 90",
      TRUE ~ "90+"
    ),
    age_band2 = case_when(
      age < 45 ~ "0-44 years",
      age < 65 ~ "45-64 years",
      age < 75 ~ "65-74 years",
      TRUE ~ "75plus years"
    ),
    age_band3 = case_when(
      age < 75 ~ "under75 years",
      TRUE ~ "75plus years"
    )
  )

population_all_ages_ca <- population_ca %>%
  filter(age == "all_ages")


# Write .csv  -------------------------------------------------------------
write_csv(activity_hb, "data/clean/activity_hb.csv")
write_csv(activity_ca, "data/clean/activity_ca.csv")
write_csv(mortality_hb, "data/clean/mortality_hb.csv")
write_csv(mortality_ca, "data/clean/mortality_ca.csv")
write_csv(population_all_ages_hb, "data/clean/population_total_hb.csv")
write_csv(population_by_ages_hb, "data/clean/population_ages_hb.csv")
write_csv(population_all_ages_ca, "data/clean/population_total_ca.csv")
write_csv(population_by_ages_ca, "data/clean/population_ages_ca.csv")
