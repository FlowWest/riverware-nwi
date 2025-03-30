library(tidyverse)
library(dplyr)
library(lubridate)

normalize <- function(x, min_val, max_val) {
  norm_value <- (x - min_val) / (max_val - min_val)
  return(max(0, min(1, norm_value)))  # Ensure within [0,1] range
}

nwi_min_max <- read_csv(here::here("data/nwi_data/nwi_min_max.csv"))
model_weights <- read_csv(here::here("data/nwi_data/nwi_model_weights.csv")) |> dplyr::select(-c("date", "day_of_water_year"))
prism_data <- read_csv(here::here("data/prism_data/prism_ppt_summary_export.csv")) |> glimpse()
climate_indicies <- read_csv(here::here("data/climate_indicies_data/normalized_climate_indicies.csv"), col_types = cols(date=col_date(format = "%Y-%m")))
swe <- read_csv(here::here("data/swe_data/swe_summary.csv"))
uklni_net_inflow <- read_csv(here::here("data/ukr_net_inflow/net_inflow_export.csv"))

current_water_year <- ifelse(month(Sys.Date()) >= 10, as.integer(year(Sys.Date()) + 1), as.integer(year(Sys.Date())))
start_year <- current_water_year - 1
start_date <- as.Date(paste0(year(Sys.Date()) - 1, "-09-14"))
expanded_climate_indicies <- climate_indicies |>
  rowwise() %>%
  mutate(date = list(seq.Date(date, ceiling_date(date, "month") - days(1), by = "day"))) |>
  unnest(date)

nwi <-prism_data |>
  filter(water_year >= start_year & date > start_date) |>
  left_join(swe, by = c("date", "water_year", "day_of_water_year")) |>
  mutate(month = as.integer(format(date, "%m")),
         day = as.integer(format(date, "%d")),
         year = as.integer(format(date, "%Y")),
         leap_year_index = if_else(leap_year(date), 1, 0),
         period = as.integer(ceiling(day_of_water_year / 15))
         ) |>
  left_join(uklni_net_inflow, by = c("date", "day_of_water_year", "water_year")) |>
  left_join(expanded_climate_indicies, by=c("date", "month", "year")) |>
  mutate(day_of_water_year = as.integer(day_of_water_year)) |>
  left_join(model_weights, by = c("period")) |>
  mutate(weight_for_30_d_trailing_sum_UKLNI = round(day_of_water_year*wi_slope + wi_intercept, 3),
         weight_for_30_d_trailing_sum_precip = round(day_of_water_year*wp_30_slope + wp_30_intercept, 3),
         weight_for_tot_31_1095_d_trailing_sum_precip = round(day_of_water_year*wp_1095_slope + wp_1095_intercept, 3),
         weight_for_normalized_weighted_avg_swe = round(day_of_water_year*ws_slope + ws_intercept, 3),
         weight_for_climate_index = round(day_of_water_year*wc_slope + wc_intercept, 3),
         climate_index_lag_0 = round(ifelse(climate_index_used == "3 mta PDO_CN34", three_mta_normalized_PDO_CN34_lag_0, three_mta_normalized_PDO_lag_0), 3),
         wetness_index_v20 =round(
           normalized_30_d_trailing_sum_UKLNI*weight_for_30_d_trailing_sum_UKLNI +
           normalized_30_trailing_sum_precipitation*weight_for_30_d_trailing_sum_precip +
           normalized_trailing_sum_31_1095_d_precip_in*weight_for_tot_31_1095_d_trailing_sum_precip +
           normalized_average_swe*weight_for_normalized_weighted_avg_swe +
           climate_index_lag_0*weight_for_climate_index, 3)
         ) |>
  left_join(nwi_min_max, by=c("day_of_water_year")) |>
  rowwise() |>
  mutate(normalized_wetness_index_v20 = round(normalize(wetness_index_v20, min_nwi_v20, max_nwi_v20), 3)) |>
  ungroup() |>
  mutate(trailing_2_week_mean_nwi_v20 = round(zoo::rollmean(normalized_wetness_index_v20, k=14, fill=NA, align = "right"), 3))
nwi_cleaned <- nwi[,c("date", "month", "day", "year", "day_of_water_year", "leap_year_index",
       "period",
       "normalized_30_d_trailing_sum_UKLNI",
       "weight_for_30_d_trailing_sum_UKLNI",
       "normalized_30_trailing_sum_precipitation",
       "weight_for_30_d_trailing_sum_precip",
       "normalized_trailing_sum_31_1095_d_precip_in",
       "weight_for_tot_31_1095_d_trailing_sum_precip",
       "normalized_average_swe",
       "weight_for_normalized_weighted_avg_swe",
       "three_mta_normalized_PDO_lag_0",
       "three_mta_normalized_PDO_CN34_lag_0",
       "climate_index_used",
       "climate_index_lag_0",
       "weight_for_climate_index",
       "wetness_index_v20",
       "normalized_wetness_index_v20",
       "trailing_2_week_mean_nwi_v20",
       "ukl_net_inflow_smoothed")]
write_csv(nwi_cleaned, "data/nwi_data/nwi_cleaned.csv")
