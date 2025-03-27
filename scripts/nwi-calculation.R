library(tidyverse)
library(dplyr)
library(lubridate)

current_water_year <- ifelse(month(Sys.Date()) >= 10, as.integer(year(Sys.Date()) + 1), as.integer(year(Sys.Date())))

model_weights <- read_csv(here::here("data/nwi_data/nwi_model_weights.csv"))
prism_data <- read_csv(here::here("data/prism_data/prism_ppt_summary.csv"), col_types = cols(date =col_date(format = "%m/%d/%Y")))
climate_indicies <- read_csv(here::here("data/climate_indicies_data/normalized_climate_indicies.csv"), col_types = cols(date=col_date(format = "%Y-%m")))
swe <- read_csv(here::here("data/swe_data/swe_summary.csv"), col_types = cols(date =col_date(format = "%m/%d/%Y")))
uklni_net_inflow <- read_csv(here::here("data/ukr_net_inflow/combined_net_inflow.csv"), , col_types = cols(date =col_date(format = "%m/%d/%Y")))

expanded_climate_indicies <- climate_indicies |>
  rowwise() %>%
  mutate(date = list(seq.Date(date, ceiling_date(date, "month") - days(1), by = "day"))) |>
  unnest(date)

full_data <-prism_data |>
  filter(water_year == current_water_year) |>
  left_join(swe, by = c("date", "water_year", "day_of_water_year")) |>
  mutate(month = as.integer(format(date, "%m")),
         day = as.integer(format(date, "%d")),
         year = as.integer(format(date, "%Y")),
         leap_year_index = if_else(leap_year(date), 1, 0),
         period = ceiling(day_of_water_year / 15)
         ) |>
  left_join(uklni_net_inflow, by = c("date")) |>
  left_join(expanded_climate_indicies, by=c("date", "year", "month")) |>View()
  dplyr::select(c(date, month, day, year, day_of_water_year, leap_year_index, period,
                  ))
