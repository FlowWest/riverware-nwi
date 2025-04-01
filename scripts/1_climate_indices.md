Generate Climate Indices
================
Inigo Peng
2025-04-01

- [Download N34](#download-n34)
- [Read in and clean N34](#read-in-and-clean-n34)
- [Download PDO](#download-pdo)
- [Read in and clean PDO](#read-in-and-clean-pdo)
- [Normalize N34, PDO, generate three day moving
  average](#normalize-n34-pdo-generate-three-day-moving-average)

This R Markdown script is used to generate climate index inputs for NWI.
Inputs:

- PDO N34 min max (data/climate_indicies_data/min_max_pdo_n34.csv)

``` r
normalizing_values <- read_csv(here::here( "data/climate_indicies_data/min_max_pdo_n34.csv"))
```

    ## Rows: 12 Columns: 7
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (7): Month, Maximum_of_PDO, Minimum_of_PDO, Maximum_of_N34, Minimum_of_N...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

Outputs:

- data-raw/n34.txt
- data-raw/pdo.txt
- data/climate_indicies_data/normalized_climate_indicies.csv

TODOs: None

### Download N34

``` r
nino_url <- "https://psl.noaa.gov/data/correlation/nina34.anom.data"
workspace <- here::here("data-raw")
nino_file <- paste0(workspace,"/n34.txt")
download.file(nino_url, destfile = nino_file, mode = "wb")
```

### Read in and clean N34

``` r
nino_raw <- read_lines(nino_file)
#Remove header
n34 <- nino_raw[-1]
# Convert to dataframe
n34_df <- read.table(text = n34, header = FALSE, fill = TRUE)
colnames(n34_df) <- c("Year", "Jan", "Feb", "Mar", "Apr", "May", "Jun",
                      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
n34_df_filtered <- n34_df|>
  filter(Year <= year(Sys.Date()))|>
  filter(Year > 1978)|>
  mutate(across(-Year, as.numeric))
clean_n34_df <- n34_df_filtered |> 
  pivot_longer(cols = -Year, names_to = "month", values_to = "N34")|>
  mutate(N34 = ifelse(N34 == -99.99, NA, N34))|>
  filter(!is.na(N34)) |> 
  mutate(
    month = match(tolower(month), tolower(month.abb)), 
    month = sprintf("%02d", month),  # Ensure two-digit format
    index = paste(Year, month, sep = "-"),
    month = as.numeric(month),
    Year = as.numeric(Year))|>
  relocate(index, .before = Year)
```

### Download PDO

``` r
pdo_url <- "https://www.ncei.noaa.gov/pub/data/cmb/ersst/v5/index/ersst.v5.pdo.dat"
workspace <- here::here("data-raw")
pdo_file <- paste0(workspace, "/pdo.txt")
download.file(pdo_url, destfile = pdo_file, mode = "wb")
```

### Read in and clean PDO

``` r
pdo_raw <- read_lines(pdo_file)
#Remove header
pdo<- pdo_raw[-1]
# Convert to dataframe
pdo_df <- read.table(text = pdo, header = FALSE, fill = TRUE)
colnames(pdo_df) <- c("Year", "Jan", "Feb", "Mar", "Apr", "May", "Jun",
                      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
pdo_df_filtered <- pdo_df|>
  filter(Year <= year(Sys.Date()))|>
  filter(Year > 1978)|>
  mutate(across(-Year, as.numeric))

clean_pdo_df <- pdo_df_filtered |> 
  pivot_longer(cols = -Year, names_to = "month", values_to = "PDO")|>
  mutate(PDO = ifelse(PDO == 99.99, NA, PDO))|>
  filter(!is.na(PDO)) |> 
  mutate(
    month = match(tolower(month), tolower(month.abb)), 
    month = sprintf("%02d", month),  # Ensure two-digit format
    index = paste(Year, month, sep = "-"),
    month = as.numeric(month),
    Year = as.numeric(Year))|>
  relocate(index, .before = Year)
```

### Normalize N34, PDO, generate three day moving average

``` r
normalize <- function(x, min_val, max_val) {
  norm_value <- (x - min_val) / (max_val - min_val)
  return(max(0, min(1, norm_value)))  # Ensure within [0,1] range
}

colnames(normalizing_values) <- tolower(colnames(normalizing_values))

full_normalized_df <- clean_n34_df|>
  left_join(normalizing_values, by = "month")|>
  left_join(clean_pdo_df, by = c("month", "index", "Year")) |>
  rowwise()|>
  mutate(normalized_CN34 = round(1 - normalize(N34, minimum_of_n34, maximum_of_n34), 3),
         normalized_pdo = round(normalize(PDO, minimum_of_pdo, maximum_of_pdo), 3),
         combined_pdo_cn34 = normalized_CN34 + normalized_pdo,
         normalized_pdo_cn34 = round(normalize(combined_pdo_cn34, minimum_of_pdo_cn34, maximum_of_pdo_cn34), 3)
         ) |> 
  ungroup() |> 
  mutate(
    three_mta_normalized_PDO = round(rollmean(normalized_pdo, k = 3, fill=NA, align = "right"), 3),
    three_mta_normalized_PDO_CN34 = round(rollmean(normalized_pdo_cn34, k = 3, fill = NA, align = "right"), 3),
    three_mta_normalized_PDO_lag_0 = lag(three_mta_normalized_PDO, n=1),
    three_mta_normalized_PDO_CN34_lag_0 = lag(three_mta_normalized_PDO_CN34, n=1)
  ) |> 
  dplyr::select(index, Year, month, N34, normalized_CN34, PDO, normalized_pdo, combined_pdo_cn34, normalized_pdo_cn34, three_mta_normalized_PDO, three_mta_normalized_PDO_CN34,three_mta_normalized_PDO_lag_0, three_mta_normalized_PDO_CN34_lag_0) |> 
  rename(date=index,
         year=Year) 
  # mutate(date = as.Date(date)) |> glimpse()

write.csv(full_normalized_df, here::here("data/climate_indicies_data/normalized_climate_indicies.csv"), row.names = FALSE)
```
