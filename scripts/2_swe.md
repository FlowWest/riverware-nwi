Generate SWE
================
Inigo Peng
2025-03-29

- [Download SWE Data](#download-swe-data)
- [Calculate basin averages, weighted averaged, and normalized
  values](#calculate-basin-averages-weighted-averaged-and-normalized-values)

This R Markdown script is used to generate SWE inputs for NWI. Inputs:

- HUC 8 Station Codes (data/swe_data/station_triplets)
- HUC 8 Area Proportions (data/swe_data/snotel-area.csv)
- SWE weighted averages (data/swe_data/weighted_avg_swe.csv)

``` r
station_triplets_lookup <- readRDS(here::here("data/swe_data/station_triplets")) |> glimpse()
```

    ##  Named chr [1:12] "Billie Creek Divide SWE (in)" ...
    ##  - attr(*, "names")= chr [1:12] "344:OR:SNTL" "395:OR:SNTL" "406:OR:SNTL" "442:OR:SNTL" ...

``` r
snotel_area <- read_csv(here::here("data/swe_data/snotel-area.csv")) |> glimpse()
```

    ## Rows: 3 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): HUC8
    ## dbl (4): Area (km2) >1500 m, Proportion of area >1500 m, Area (km2) total, P...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## Rows: 3
    ## Columns: 5
    ## $ HUC8                         <chr> "UKL", "Williamson", "Sprague"
    ## $ `Area (km2) >1500 m`         <dbl> 705.262, 1709.322, 2687.992
    ## $ `Proportion of area >1500 m` <dbl> 0.1382169, 0.3349920, 0.5267912
    ## $ `Area (km2) total`           <dbl> 1875.17, 3725.91, 4170.92
    ## $ `Proportion of total area`   <dbl> 0.1918921, 0.3812843, 0.4268236

``` r
min_max_weight_swe <- read_csv(here::here("data/swe_data/weighted_avg_swe.csv")) |> glimpse() 
```

    ## Rows: 365 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (5): day_of_water_year, max_weighted_average_swe_in, min_weighted_averag...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## Rows: 365
    ## Columns: 5
    ## $ day_of_water_year                   <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11,…
    ## $ max_weighted_average_swe_in         <dbl> 0.043, 0.024, 0.038, 0.048, 0.113,…
    ## $ min_weighted_average_swe_in         <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
    ## $ max_weighted_normalized_average_swe <dbl> 0.000, 0.000, 0.000, 0.000, 0.000,…
    ## $ min_weighted_normalized_average_swe <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…

Outputs:

- SWE Download (data/swe_data/swe_download.csv)
- SWE indices with values for NWI caluclation
  (data/swe_data/swe_summary.csv)

TODOs: Decide on start date and end date for SWE time series data.
Default start date is Oct 1 of the current water year. Default end date
is current date.

``` r
timeseries_start_date = paste0(year(today() - months(9)), "-10-01")
timeseries_end_date = today()
```

## Download SWE Data

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
                                start_date = timeseries_start_date,
                                end_date = timeseries_end_date) {
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
nrcs_api_url <- "https://wcc.sc.egov.usda.gov/awdbRestApi"
data_list <- list()
for (station in names(station_triplets_lookup)) {
  station_df <- get_station_data(station)
  data_list[[station]] <- station_df
}
full_swe_df <- Reduce(function(x, y) full_join(x, y, by = "Date"), data_list) |> glimpse()
```

    ## Rows: 180
    ## Columns: 13
    ## $ Date                           <chr> "2024-10-01", "2024-10-02", "2024-10-03…
    ## $ `Billie Creek Divide SWE (in)` <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ `Chemult Alternate SWE (in)`   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ `Cold Springs Camp SWE (in)`   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ `Diamond Lake SWE (in)`        <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ `Fish Lake SWE (in)`           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ `Fourmile Lake SWE (in)`       <dbl> 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,…
    ## $ `Quartz Mountain SWE (in)`     <dbl> 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,…
    ## $ `Sevenmile Marsh SWE (in)`     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ `Silver Creek SWE (in)`        <dbl> 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,…
    ## $ `Strawberry SWE (in)`          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ `Summer Rim SWE (in)`          <dbl> 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,…
    ## $ `Taylor Butte SWE (in)`        <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …

``` r
write_csv(full_swe_df, here::here("data/swe_data/swe_download.csv"))
```

## Calculate basin averages, weighted averaged, and normalized values

``` r
normalize <- function(x, min_val, max_val) {
  norm_value <- (x - min_val) / (max_val - min_val)
  return(max(0, min(1, norm_value)))  # Ensure within [0,1] range
}

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
  dplyr::select(-c(min_weighted_average_swe_in, 
            max_weighted_average_swe_in, 
            max_weighted_normalized_average_swe, 
            min_weighted_normalized_average_swe)) |> 
  relocate(water_year, .after = Date) |> 
  relocate(day_of_water_year, .after = water_year) |> 
  rename(date = Date) |> 
  mutate(date = as.Date(date, format= "%Y-%m-%d")) |> 
  dplyr::select(-c(
    `Billie Creek Divide SWE (in)`,
    `Cold Springs Camp SWE (in)`,
    `Fish Lake SWE (in)`,
    `Fourmile Lake SWE (in)`,
    `Sevenmile Marsh SWE (in)`,
    `Diamond Lake SWE (in)`,
    `Chemult Alternate SWE (in)`,
    `Silver Creek SWE (in)`, 
    `Taylor Butte SWE (in)`,
    `Summer Rim SWE (in)`,
    `Quartz Mountain SWE (in)`,
    `Strawberry SWE (in)`
      )) |> glimpse()
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

    ## Rows: 180
    ## Columns: 13
    ## $ date                        <date> 2024-10-01, 2024-10-02, 2024-10-03, 2024-…
    ## $ water_year                  <dbl> 2025, 2025, 2025, 2025, 2025, 2025, 2025, …
    ## $ day_of_water_year           <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13,…
    ## $ ukl_huc8_mean_swe           <dbl> 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, …
    ## $ williamson_huc8_mean_swe    <dbl> 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, …
    ## $ sprague_huc8_mean_swe       <dbl> 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, …
    ## $ weighted_average_swe        <dbl> 0.00000000, 0.00000000, 0.00000000, 0.0000…
    ## $ normalized_average_swe      <dbl> 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, …
    ## $ day_after_oct_15            <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ oct_16_nov_30_linear_weight <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ day_after_may_15_desc       <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ may_16_jun_30_linear_weight <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ weighted_normalized_avg_swe <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…

``` r
write.csv(swe_summary, here::here("data/swe_data/swe_summary.csv"), row.names = FALSE)
```
