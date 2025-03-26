library(tidyverse)
library(dplyr)
library(lubridate)

model_weights <- read_csv(here::here("data/nwi_model_weights.csv"))
prism_data <- read_csv(here::here("data/prism_data/prism_ppt_summary.csv"), col_types = cols(date =col_date(format = "%m/%d/%Y")))
climate_indicies <- read_csv(here::here("data/normalized_climate_indicies.csv"))
swe <- read_csv(here::here("data/swe_summary.csv"), col_types = cols(date =col_date(format = "%m/%d/%Y")))
uklni_net_inflow <- read_csv(here::here("data/combined_net_inflow.csv"))
uklni_net_inflow_smoothed <- read_csv(here::here("data/combined_net_inflow_smooth.csv"))

prism_data |>
  left_join(swe, by = c("date", "water_year", "day_of_water_year"))

