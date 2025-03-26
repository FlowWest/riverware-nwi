library(tidyverse)
library(dplyr)
library(lubridate)

model_weights <- read_csv(here::here("data/nwi_model_weights.csv"))
prism_data <- read_csv(here::here("data/prism_data/prism_ppt_summary.csv"), col_types = cols(date =col_date(format = "%m/%d/%Y")))
climate_indicies <- read_csv(here::here("data/normalized_climate_indicies.csv"))
swe <- read_csv(here::here("data/swe_summary.csv"), col_types = cols(date =col_date(format = "%m/%d/%Y")))
uklni_net_inflow <- read_csv(here::here("data/combined_net_inflow.csv"), col_types = cols(date =col_date(format = "%m/%d/%Y")))
uklni_net_inflow_smoothed <- read_csv(here::here("data/combined_net_inflow_smooth.csv"), , col_types = cols(date =col_date(format = "%m/%d/%Y")))

full_data <-prism_data |>
  left_join(swe, by = c("date", "water_year", "day_of_water_year")) |>
  mutate(month = as.integer(format(date, "%m")),
         day = as.integer(format(date, "%d")),
         year = as.integer(format(date, "%Y")),
         leap_year_index = if_else(leap_year(date), 1, 0),
         period = ceiling(day_of_water_year / 15)
         ) |>
  left_join(uklni_net_inflow, by = c("date")) |>
  left_join(uklni_net_inflow_smoothed, by = c("date")) |> View()

