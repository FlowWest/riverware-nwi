Generate SWE
================
Inigo Peng
2025-03-20

- [Calculate Basin Averages](#calculate-basin-averages)

\##Download SWE Data

``` r
# Function to get timeseries data
# Default element_cd is WTEQ (Snow Water Equivalent)
# Default start date is Oct 1 of the current water year
# Default end date is current date
# Default data temporal resolution is daily
get_station_data <- function(station_triplet){
  station_data <- get_timeseries_data(station_triplet = station_triplet)
  station_data_df <- as.data.frame(station_data[,2][[1]])$values[[1]]
  col_name <- station_triplets_lookup[station_triplet]
  colnames(station_data_df) <- c("Date", col_name)
  return(station_data_df)
}
get_timeseries_data <- function(station_triplet,
                                element_cd = "WTEQ",
                                temp_duration = "Daily",
                                start_date = paste0(year(today() - months(9)), "-10-01"),
                                end_date = today()) {
  url <- paste0(nrcs_api_url, "/services/v1/data")
  params <- list(
    stationTriplets = station_triplet,
    elements = element_cd,
    duration = temp_duration,
    beginDate = start_date,
    endDate = end_date,
    periodRef = "START"
  )
  response <- GET(url, query = params)
  if (status_code(response) == 200) {
    data <- content(response, "text", encoding = "UTF-8")
    return(fromJSON(data))
  } else {
    print(paste("Error:", status_code(response)))
    return(NULL)
  }
}
```

``` r
station_triplets_lookup <- readRDS(here::here("data/station_triplets"))
nrcs_api_url <- "https://wcc.sc.egov.usda.gov/awdbRestApi"
data_list <- list()
for (station in names(station_triplets_lookup)) {
  station_df <- get_station_data(station)
  data_list[[station]] <- station_df
}
full_swe_df <- Reduce(function(x, y) full_join(x, y, by = "Date"), data_list)
```

## Calculate Basin Averages

``` r
normalize <- function(x, min_val, max_val) {
  norm_value <- (x - min_val) / (max_val - min_val)
  return(max(0, min(1, norm_value)))  # Ensure within [0,1] range
}

snotel_area <- read_csv(here::here("data/snotel-area.csv"))
```

    ## Rows: 3 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): HUC8
    ## dbl (4): Area (km2) >1500 m, Proportion of area >1500 m, Area (km2) total, P...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
min_max_weight_swe <- read_csv(here::here("data/weighted_avg_swe.csv")) 
```

    ## Rows: 365 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (5): day_of_water_year, max_weighted_average_swe_in, min_weighted_averag...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
swe_summary <- full_swe_df |> 
  mutate(
    water_year = ifelse(month(Date) >= 10, year(Date) + 1, year(Date))) |> 
  group_by(water_year) %>% 
  mutate(
    day_of_water_year = as.integer(difftime(Date, ymd(paste0(water_year - 1 ,'-09-30')), units = "days"))) |> 
  ungroup() |> 
  mutate(
    ukl_huc8_mean_swe = lag(rowMeans(pick(
      `Billie Creek Divide SWE (in)`,
      `Cold Springs Camp SWE (in)`,
      `Fish Lake SWE (in)`,
      `Fourmile Lake SWE (in)`,
      `Sevenmile Marsh SWE (in)`
      )
  ), n=1),
  williamson_huc8_mean_swe = lag(rowMeans(pick(
    `Diamond Lake SWE (in)`,
    `Chemult Alternate SWE (in)`,
    `Silver Creek SWE (in)`, 
    `Taylor Butte SWE (in)`
    )
  ), n=1),
  sprague_huc8_mean_swe = lag(rowMeans(pick(
    `Silver Creek SWE (in)`,
    `Taylor Butte SWE (in)`,
    `Summer Rim SWE (in)`,
    `Quartz Mountain SWE (in)`,
    `Strawberry SWE (in)`
    )
  ), n=1)) |> 
  mutate_all(funs(replace_na(.,0))) |> 
  mutate(weighted_average_swe = 
           (ukl_huc8_mean_swe*snotel_area$`Proportion of area >1500 m`[1]) + 
           (williamson_huc8_mean_swe*snotel_area$`Proportion of area >1500 m`[2]) + 
           (sprague_huc8_mean_swe*snotel_area$`Proportion of area >1500 m`[3])) |> 
  left_join(min_max_weight_swe, by = "day_of_water_year") |>
  rowwise()|>
  mutate(normalized_average_swe = round(normalize(weighted_average_swe, 
                                                  min_weighted_average_swe_in, 
                                                  max_weighted_average_swe_in), 2))|>
  ungroup() |> 
  mutate(
    day_after_oct_15 = as.integer(difftime(Date, ymd(paste0(year(Date), "-10-15")), units = "days")),
    day_after_oct_15 = ifelse(Date > ymd(paste0(year(Date), "-11-30")) | day_after_oct_15 < 1, NA, day_after_oct_15),
    oct_16_nov_30_linear_weight = round(day_after_oct_15 * (1/46), 4),
    day_after_may_15_desc = 47 - as.integer(difftime(Date, ymd(paste0(year(Date), "-5-15")), units = "days")),
    day_after_may_15_desc = ifelse(Date > ymd(paste0(year(Date), "-6-30")) | Date < ymd(paste0(year(Date), "-5-16")), NA, day_after_may_15_desc),
    may_16_jun_30_linear_weight = round(day_after_may_15_desc * (1/46), 4),
    weighted_normalized_avg_swe = case_when(!is.na(day_after_oct_15) ~ normalized_average_swe * oct_16_nov_30_linear_weight,
                                            !is.na(day_after_may_15_desc) ~ normalized_average_swe * may_16_jun_30_linear_weight,
                                            .default = NA)) |> 
  select(-c(min_weighted_average_swe_in, 
            max_weighted_average_swe_in, 
            max_weighted_normalized_average_swe, 
            min_weighted_normalized_average_swe)) |> 
  relocate(water_year, .after = Date) |> 
  relocate(day_of_water_year, .after = water_year)
```

    ## Warning: `funs()` was deprecated in dplyr 0.8.0.
    ## ℹ Please use a list of either functions or lambdas:
    ## 
    ## # Simple named list: list(mean = mean, median = median)
    ## 
    ## # Auto named with `tibble::lst()`: tibble::lst(mean, median)
    ## 
    ## # Using lambdas list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

``` r
write.csv(swe_summary, here::here("data/swe_summary.csv"), row.names = FALSE)
```
