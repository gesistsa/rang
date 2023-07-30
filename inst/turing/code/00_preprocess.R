## This file is modified from
## https://github.com/allisonhorst/palmerpenguins/blob/main/data-raw/penguins.R
## License CC0: https://github.com/allisonhorst/palmerpenguins/blob/main/LICENSE.md

library(janitor)
library(here)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)

penguins_raw_df <- read_csv(here("data_raw", "penguins_raw.csv"))

penguins_df <- penguins_raw_df %>%
  clean_names() %>%
  mutate(species_short = word(species, 1)) %>%
  mutate(sex = tolower(sex)) %>%
  mutate(year = as.integer(lubridate::year(date_egg))) %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate(flipper_length_mm = as.integer(flipper_length_mm)) %>%
  mutate(body_mass_g = as.integer(body_mass_g)) %>%
  rename(bill_length_mm = culmen_length_mm,
         bill_depth_mm = culmen_depth_mm) %>%
  select(species_short,
         island,
         bill_length_mm,
         bill_depth_mm,
         flipper_length_mm,
         body_mass_g,
         sex,
         year) %>%
  rename(species = species_short) %>%
    as.data.frame()

write_csv(penguins_df, here("data_clean", "penguins.csv"))
