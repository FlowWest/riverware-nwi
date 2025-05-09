5_Generate_NWI
================
Inigo Peng
2025-04-01

- [Expand Climate Indicies](#expand-climate-indicies)
- [Calculate NWI, normalized NWI and 2 week lag
  NWI](#calculate-nwi-normalized-nwi-and-2-week-lag-nwi)

This R Markdown script is used to generate NWI using outputs from
previous Rmd. Inputs:

- NWI min max for normalization (data/nwi_data/nwi_min_max.csv)
- Model weights (data/nwi_data/nwi_model_weights.csv)
- Prism data (data/prism_data/prism_ppt_summary_export.csv)
- Climate indicies
  (data/climate_indicies_data/normalized_climate_indicies.csv)
- SWE (data/swe_data/swe_summary.csv)
- UKLNI net inflow (data/ukr_net_inflow/net_inflow_export.csv)
- Leap year period and non leap year period lookup (data/nwi_data)

``` r
normalize <- function(x, min_val, max_val) {
  if (isTRUE(all(c(x, min_val, max_val) == 0, na.rm = TRUE))) {
    return(0)
  }
  norm_value <- (x - min_val) / (max_val - min_val)
  return(max(0, min(1, norm_value)))  # Ensure within [0,1] range
}
nwi_min_max <- read_csv(here::here("data/nwi_data/nwi_min_max.csv")) |> glimpse()
```

    ## Rows: 365 Columns: 3
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (3): day_of_water_year, max_nwi_v20, min_nwi_v20
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## Rows: 365
    ## Columns: 3
    ## $ day_of_water_year <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 1…
    ## $ max_nwi_v20       <dbl> 1.250, 1.259, 1.269, 1.277, 1.285, 1.296, 1.306, 1.3…
    ## $ min_nwi_v20       <dbl> 0.248, 0.253, 0.261, 0.267, 0.272, 0.278, 0.283, 0.2…

``` r
model_weights <- read_csv(here::here("data/nwi_data/nwi_model_weights.csv")) |> 
  dplyr::select(-c("date", "day_of_water_year")) |> glimpse()
```

    ## Rows: 24 Columns: 18
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (3): climate_index_used, nwi_version, date
    ## dbl (15): period, day_of_water_year, MAE, MSE, MAPE, wi_slope, wi_intercept,...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## Rows: 24
    ## Columns: 16
    ## $ climate_index_used <chr> "3 mta PDO_CN34", "3 mta PDO_CN34", "3 mta PDO_CN34…
    ## $ nwi_version        <chr> "20h", "20h", "20h", "20h", "20d", "20d", "20d", "2…
    ## $ period             <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, …
    ## $ MAE                <dbl> 0.9169209, 1.2669894, 1.4983433, 1.5780127, 1.64945…
    ## $ MSE                <dbl> 1.2609235, 3.1942620, 4.7570526, 5.1220528, 5.85229…
    ## $ MAPE               <dbl> 0.05637465, 0.07183913, 0.07958913, 0.08124958, 0.0…
    ## $ wi_slope           <dbl> 0.0028910930, -0.0058781120, 0.0228556290, 0.038754…
    ## $ wi_intercept       <dbl> 0.05656151, 0.18809958, -0.73138013, -1.46272777, 1…
    ## $ wp_30_slope        <dbl> 0.000000e+00, 0.000000e+00, 3.571492e-02, 3.731122e…
    ## $ wp_30_intercept    <dbl> 0.0000000, 0.0000000, -1.1428775, 0.3283773, -0.326…
    ## $ wp_1095_slope      <dbl> -7.113837e-03, -2.928044e-03, 1.356220e-02, -5.2505…
    ## $ wp_1095_intercept  <dbl> 0.86662238, 0.80383549, 0.27614768, 3.31525585, -2.…
    ## $ ws_slope           <dbl> 0.000000000, 0.012931555, 0.052867266, -0.052491337…
    ## $ ws_intercept       <dbl> 0.00000000, -0.19397332, -1.47191610, 3.37457968, -…
    ## $ wc_slope           <dbl> 0.019278989, -0.033529301, 0.009283112, 0.007498260…
    ## $ wc_intercept       <dbl> 0.490821384, 1.282945733, -0.087051502, -0.00494829…

``` r
prism_data <- read_csv(here::here("data/prism_data/prism_ppt_summary_export.csv")) |> glimpse()
```

    ## Rows: 1642 Columns: 11
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl  (10): water_year, day_of_water_year, mean_total_daily_precip_ukl, mean_...
    ## date  (1): date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## Rows: 1,642
    ## Columns: 11
    ## $ date                                                     <date> 2020-10-01, …
    ## $ water_year                                               <dbl> 2021, 2021, 2…
    ## $ day_of_water_year                                        <dbl> 1, 2, 3, 4, 5…
    ## $ mean_total_daily_precip_ukl                              <dbl> 0.000000000, …
    ## $ mean_total_daily_precip_williamson                       <dbl> 0.000000e+00,…
    ## $ mean_total_daily_precip_sprague                          <dbl> 0.0000000000,…
    ## $ weighted_mean_total_daily_precipitation_ukl_catchment_in <dbl> 0.000, 0.000,…
    ## $ thirty_d_trailing_sum_precip                             <dbl> NA, NA, NA, N…
    ## $ normalized_30_trailing_sum_precipitation                 <dbl> NA, NA, NA, N…
    ## $ trailing_sum_31_1095_d_precip_in                         <dbl> 0, 0, 0, 0, 0…
    ## $ normalized_trailing_sum_31_1095_d_precip_in              <dbl> 0, 0, 0, 0, 0…

``` r
climate_indicies <- read_csv(here::here("data/climate_indicies_data/normalized_climate_indicies.csv")) |> glimpse()
```

    ## Rows: 555 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl  (12): year, month, PDO, normalized_pdo, N34, normalized_CN34, combined_...
    ## date  (1): date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## Rows: 555
    ## Columns: 13
    ## $ date                                <date> 1979-01-01, 1979-02-01, 1979-03-0…
    ## $ year                                <dbl> 1979, 1979, 1979, 1979, 1979, 1979…
    ## $ month                               <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11,…
    ## $ PDO                                 <dbl> -0.38, -1.29, -0.68, -0.12, 0.57, …
    ## $ normalized_pdo                      <dbl> 0.517, 0.181, 0.381, 0.531, 0.649,…
    ## $ N34                                 <dbl> -0.13, -0.22, -0.02, 0.02, -0.25, …
    ## $ normalized_CN34                     <dbl> 0.622, 0.631, 0.580, 0.511, 0.589,…
    ## $ combined_pdo_cn34                   <dbl> 1.139, 0.812, 0.961, 1.042, 1.238,…
    ## $ normalized_pdo_cn34                 <dbl> 0.492, 0.198, 0.428, 0.436, 0.641,…
    ## $ three_mta_normalized_PDO            <dbl> NA, NA, 0.360, 0.364, 0.520, 0.568…
    ## $ three_mta_normalized_PDO_CN34       <dbl> NA, NA, 0.373, 0.354, 0.502, 0.618…
    ## $ three_mta_normalized_PDO_lag_0      <dbl> NA, NA, NA, 0.360, 0.364, 0.520, 0…
    ## $ three_mta_normalized_PDO_CN34_lag_0 <dbl> NA, NA, NA, 0.373, 0.354, 0.502, 0…

``` r
swe <- read_csv(here::here("data/swe_data/swe_summary.csv")) |> glimpse()
```

    ## Rows: 200 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl  (10): water_year, day_of_water_year, ukl_huc8_mean_swe, williamson_huc8...
    ## lgl   (2): day_after_may_15_desc, may_16_jun_30_linear_weight
    ## date  (1): date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## Rows: 200
    ## Columns: 13
    ## $ date                        <date> 2024-09-14, 2024-09-15, 2024-09-16, 2024-…
    ## $ water_year                  <dbl> 2024, 2024, 2024, 2024, 2024, 2024, 2024, …
    ## $ day_of_water_year           <dbl> 350, 351, 352, 353, 354, 355, 356, 357, 35…
    ## $ ukl_huc8_mean_swe           <dbl> 0.00, 0.18, 0.16, 0.14, 0.16, 0.16, 0.16, …
    ## $ williamson_huc8_mean_swe    <dbl> 0.000, 0.000, 0.000, 0.000, 0.000, 0.025, …
    ## $ sprague_huc8_mean_swe       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ weighted_average_swe        <dbl> 0.00000000, 0.02487903, 0.02211470, 0.0193…
    ## $ normalized_average_swe      <dbl> 0.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, …
    ## $ day_after_oct_15            <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ oct_16_nov_30_linear_weight <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ day_after_may_15_desc       <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ may_16_jun_30_linear_weight <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ weighted_normalized_avg_swe <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…

``` r
uklni_net_inflow <- read_csv(here::here("data/ukr_net_inflow/net_inflow_export.csv")) |> glimpse()
```

    ## Rows: 16346 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl  (5): water_year, day_of_water_year, ukl_net_inflow_smoothed, ukl_net_in...
    ## date (1): date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## Rows: 16,346
    ## Columns: 6
    ## $ date                                 <date> 1980-06-30, 1980-07-01, 1980-07-…
    ## $ water_year                           <dbl> 1980, 1980, 1980, 1980, 1980, 198…
    ## $ day_of_water_year                    <dbl> 274, 275, 276, 277, 278, 279, 280…
    ## $ ukl_net_inflow_smoothed              <dbl> NA, 1.3168759, 1.1601920, 1.24303…
    ## $ ukl_net_inflow_30_d_trailing_sum_taf <dbl> NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ normalized_30_d_trailing_sum_UKLNI   <dbl> NA, NA, NA, NA, NA, NA, NA, NA, N…

``` r
leap_year_period <- read_csv(here::here("data/nwi_data/leap_period_lookup.csv")) |> glimpse()
```

    ## Rows: 366 Columns: 4
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): lookup_date
    ## dbl (3): day_of_water_year, period, lookup_month
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## Rows: 366
    ## Columns: 4
    ## $ day_of_water_year <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 1…
    ## $ period            <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2…
    ## $ lookup_date       <chr> "1-Oct", "2-Oct", "3-Oct", "4-Oct", "5-Oct", "6-Oct"…
    ## $ lookup_month      <dbl> 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, …

``` r
non_leap_year_period <- read_csv(here::here("data/nwi_data/non_leap_period_lookup.csv")) |> glimpse()
```

    ## Rows: 365 Columns: 3
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): lookup_date
    ## dbl (2): day_of_water_year, period
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## Rows: 365
    ## Columns: 3
    ## $ day_of_water_year <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 1…
    ## $ period            <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2…
    ## $ lookup_date       <chr> "1-Oct", "2-Oct", "3-Oct", "4-Oct", "5-Oct", "6-Oct"…

Outputs:

- NWI (data/nwi_data/nwi_cleaned.csv)

TODOs: Default NWI start date is september 15 of previous water year.

``` r
current_water_year <- ifelse(month(Sys.Date()) >= 10, as.integer(year(Sys.Date()) + 1), as.integer(year(Sys.Date())))
start_year <- current_water_year - 1
start_date <- as.Date(paste0(year(Sys.Date()) - 1, "-09-14"))
```

### Expand Climate Indicies

``` r
expanded_climate_indicies <- climate_indicies |>
  rowwise() %>%
  mutate(date = list(seq.Date(date, ceiling_date(date, "month") - days(1), by = "day"))) |>
  unnest(date) |> 
  mutate(year = as.integer(format(date, "%Y")),
         month = as.integer(format(date, "%m")))
```

### Calculate NWI, normalized NWI and 2 week lag NWI

$$
WI_d = q_d Q_{d-1} + s_d S_{d-1} + pn_d PN_{d-1} + pl_d PL_{d-1} + c_d C_{m-1}
$$

``` r
prism_data_edit <-prism_data |>
  filter(water_year >= start_year & date > start_date) |> 
  mutate(month = as.integer(format(date, "%m")),
         day = as.integer(format(date, "%d")),
         year = as.integer(format(date, "%Y")),
         leap_year_index = if_else(leap_year(date), 1, 0)
         )
prism_leap <- prism_data_edit |> filter(leap_year_index == 1) |> 
  left_join(leap_year_period, by = c("day_of_water_year")) |> 
  dplyr::select(-c("lookup_date", "lookup_month")) |> glimpse()
```

    ## Rows: 108
    ## Columns: 16
    ## $ date                                                     <date> 2024-09-15, …
    ## $ water_year                                               <dbl> 2024, 2024, 2…
    ## $ day_of_water_year                                        <dbl> 351, 352, 353…
    ## $ mean_total_daily_precip_ukl                              <dbl> 0.13176574, 0…
    ## $ mean_total_daily_precip_williamson                       <dbl> 0.04834527, 0…
    ## $ mean_total_daily_precip_sprague                          <dbl> 0.167011415, …
    ## $ weighted_mean_total_daily_precipitation_ukl_catchment_in <dbl> 0.000, 0.115,…
    ## $ thirty_d_trailing_sum_precip                             <dbl> 0.729, 0.842,…
    ## $ normalized_30_trailing_sum_precipitation                 <dbl> 0.504, 0.500,…
    ## $ trailing_sum_31_1095_d_precip_in                         <dbl> 69.493, 69.63…
    ## $ normalized_trailing_sum_31_1095_d_precip_in              <dbl> 0.273, 0.280,…
    ## $ month                                                    <int> 9, 9, 9, 9, 9…
    ## $ day                                                      <int> 15, 16, 17, 1…
    ## $ year                                                     <int> 2024, 2024, 2…
    ## $ leap_year_index                                          <dbl> 1, 1, 1, 1, 1…
    ## $ period                                                   <dbl> 24, 24, 24, 2…

``` r
prism_non_leap <- prism_data_edit |> filter(leap_year_index == 0) |> 
  left_join(non_leap_year_period, by = c("day_of_water_year")) |> 
  dplyr::select(-c("lookup_date")) |> glimpse()
```

    ## Rows: 89
    ## Columns: 16
    ## $ date                                                     <date> 2025-01-01, …
    ## $ water_year                                               <dbl> 2025, 2025, 2…
    ## $ day_of_water_year                                        <dbl> 93, 94, 95, 9…
    ## $ mean_total_daily_precip_ukl                              <dbl> 0.7009869908,…
    ## $ mean_total_daily_precip_williamson                       <dbl> 0.453922825, …
    ## $ mean_total_daily_precip_sprague                          <dbl> 0.3196992048,…
    ## $ weighted_mean_total_daily_precipitation_ukl_catchment_in <dbl> 0.278, 0.444,…
    ## $ thirty_d_trailing_sum_precip                             <dbl> 7.166, 7.608,…
    ## $ normalized_30_trailing_sum_precipitation                 <dbl> 0.610, 0.601,…
    ## $ trailing_sum_31_1095_d_precip_in                         <dbl> 67.442, 67.42…
    ## $ normalized_trailing_sum_31_1095_d_precip_in              <dbl> 0.205, 0.207,…
    ## $ month                                                    <int> 1, 1, 1, 1, 1…
    ## $ day                                                      <int> 1, 2, 3, 4, 5…
    ## $ year                                                     <int> 2025, 2025, 2…
    ## $ leap_year_index                                          <dbl> 0, 0, 0, 0, 0…
    ## $ period                                                   <dbl> 7, 7, 7, 7, 7…

``` r
prism_full <- bind_rows(prism_leap, prism_non_leap) |> arrange(date)
nwi <- prism_full |> 
  left_join(swe, by = c("date", "water_year", "day_of_water_year")) |>
  left_join(uklni_net_inflow, by = c("date", "day_of_water_year", "water_year")) |>
  left_join(expanded_climate_indicies, by=c("date", "month", "year")) |>
  mutate(day_of_water_year = as.integer(day_of_water_year),
         normalized_average_swe = if_else(day_of_water_year < 16 | (month > 6 & month <=9), 0, normalized_average_swe)) |>
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
  mutate(
    normalized_wetness_index_v20 = if_else(
      day_of_water_year == 366, lag(normalized_wetness_index_v20),
      normalized_wetness_index_v20
    )
  ) |> 
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
write_csv(nwi_cleaned, here::here("data/nwi_data/nwi_export.csv"))
```
