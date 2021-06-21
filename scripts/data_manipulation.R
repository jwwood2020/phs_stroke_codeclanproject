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
# Both datasets are produced at Health Board (hb) and Council Area (ca)
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

mortalilty_ca_raw <- read_csv("data/raw/stroke_mortalitybyca.csv")%>% 
  clean_names() %>% 
  select(-ends_with("qf"))


# Read population estimates -----------------------------------------------
population_hb_raw <- read_csv("data/raw/population_est_hb.csv") %>% 
  clean_names() %>% 
  select(-ends_with("qf"))

population_ca_raw <- read_csv("data/raw/population_est_ca.csv")%>% 
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
hb_id <- read_csv("data/clean/health_board_id.csv")
ca_id <- read_csv("data/clean/council_id.csv")
id_lookups <- read_csv("data/clean/id_lookups.csv")


# Get HB names for councils -----------------------------------------------
hb_ca_id <- id_lookups %>% 
  select("la_code", "hb_name") %>% 
  distinct(la_code, hb_name)


# Wrangle activity data ---------------------------------------------------
activity_hb <- activity_hb_raw %>% 
  left_join(hb_id, by = c("hbr" = "hb_code")) %>% # get Health Board name
  mutate(year = str_sub(financial_year, 1, 4),
         hb_name = if_else(hbr == "S92000003", "Scotland", hb_name) 
  ) %>% 
  select(-financial_year) %>% 
  relocate(year)

activity_ca <- activity_ca_raw %>% 
  left_join(ca_id, by = c("ca" = "la_code")) %>%  # get Council name
  left_join(hb_ca_id, by = c("ca" = "la_code")) %>% # get Health Board name
  mutate(year = str_sub(financial_year, 1, 4),
         la_name = if_else(ca == "S92000003", "Scotland", la_name),
         hb_name = if_else(ca == "S92000003", "Scotland", hb_name)
  ) %>% 
  select(-financial_year) %>% 
  relocate(year)


# Wrangle mortality data --------------------------------------------------
mortality_hb <- mortality_hb_raw %>% 
  left_join(hb_id, by = c("hbr" = "hb_code")) %>% # get Health Board name
  mutate(hb_name = if_else(hbr == "S92000003", "Scotland", hb_name))

mortality_ca <- mortalilty_ca_raw %>% 
  left_join(ca_id, by = c("ca" = "la_code")) %>%  # get Council name
  left_join(hb_ca_id, by = c("ca" = "la_code")) %>% # get Health Board name
  mutate(la_name = if_else(ca == "S92000003", "Scotland", la_name),
         hb_name = if_else(ca == "S92000003", "Scotland", hb_name)
  )


# Wrangle population estimates --------------------------------------------
population_hb <- population_hb_raw %>%
  pivot_longer(cols = c("age0":"age90plus"),
               names_to = "age",
               values_to = "count") %>% 
  left_join(hb_id, by = c("hb" = "hb_code")) %>% # get Health Board name
  mutate(age = str_sub(age, 4, -1),
         hb_name = if_else(hb == "S92000003", "Scotland", hb_name))

population_ca <- population_ca_raw %>%
  pivot_longer(cols = c("age0":"age90plus"),
               names_to = "age",
               values_to = "count") %>% 
  left_join(ca_id, by = c("ca" = "la_code")) %>%  # get Council name
  left_join(hb_ca_id, by = c("ca" = "la_code")) %>% # get Health Board name
  mutate(age = str_sub(age, 4, -1),
         la_name = if_else(ca == "S92000003", "Scotland", la_name),
         hb_name = if_else(ca == "S92000003", "Scotland", hb_name))


# Wrangle population projections ------------------------------------------
population_proj_hb <- population_proj_hb_raw %>%
  pivot_longer(cols = c("age0":"age90plus"),
               names_to = "age",
               values_to = "count") %>% 
  left_join(hb_id, by = c("hb" = "hb_code")) %>% # get Health Board name
  mutate(age = str_sub(age, 4, -1),
         hb_name = if_else(hb == "S92000003", "Scotland", hb_name))

population_proj_ca <- population_proj_ca_raw %>%
  pivot_longer(cols = c("age0":"age90plus"),
               names_to = "age",
               values_to = "count") %>% 
  left_join(ca_id, by = c("ca" = "la_code")) %>%  # get Council name
  left_join(hb_ca_id, by = c("ca" = "la_code")) %>% # get Health Board name
  mutate(age = str_sub(age, 4, -1),
         la_name = if_else(ca == "S92000003", "Scotland", la_name),
         hb_name = if_else(ca == "S92000003", "Scotland", hb_name))


# Write .csv  -------------------------------------------------------------
write_csv(activity_hb, "data/clean/activity_hb.csv")
write_csv(activity_ca, "data/clean/activity_ca.csv")
write_csv(mortality_hb, "data/clean/mortality_hb.csv")
write_csv(mortality_ca, "data/clean/mortality_ca.csv")
write_csv(population_hb, "data/clean/population_hb.csv")
write_csv(population_ca, "data/clean/population_ca.csv")
write_csv(population_proj_hb, "data/clean/population_proj_hb.csv")
write_csv(population_proj_ca, "data/clean/population_proj_ca.csv")