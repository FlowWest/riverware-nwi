library(httr)
library(readr)
library(ncdf4)
library(stringr)
library(tidyverse)
library(raster)
library(zoo)

normalize <- function(x, min_val, max_val) {
  norm_value <- (x - min_val) / (max_val - min_val)
  return(max(0, min(1, norm_value)))  # Ensure within [0,1] range
}

# element_list <- c("ppt", "tmean", "tmax")
element_list <- c("ppt")
prism_base_url <- "http://services.nacse.org/prism/data/public/4km"
start_date <- as.Date("2020-10-01")
stop_date <- as.Date("2024-9-30")
prism_locations <- readRDS("data/prism_data/prism_locations")
prism_coordinates <- apply(prism_locations[,2:3], 2, as.numeric)

prism_ppt_data <- as.data.frame(prism_locations)
prism_tmean_data <- as.data.frame(prism_locations)
prism_tmax_data <- as.data.frame(prism_locations)
local_dir <- "Z:/Shared/Active_Projects/083-02_SDM_Support_Base_Year/Modeling/NWI"

snotel_area <- read_csv(here::here("data/swe_data/snotel-area.csv"))
min_max_ppt <- read_csv("data/prism_data/min_max_ppt.csv")
for (element in element_list){
  for (current_date in seq(start_date, stop_date, by = "day")) {
    day <- format(as.Date(current_date), "%Y%m%d")
    download_url <- paste0(prism_base_url, "/", element, "/", day, "?format=nc")
    download.file(download_url, destfile = paste0(local_dir,"/Prism_Files/",element,"/",day,"_",element, ".zip"), mode = "wb")
    Sys.sleep(2)
  }
  nc_files <- list.files(path = paste0(local_dir,"/Prism_Files/", element), pattern = "*.zip", full.names = TRUE)
  for (file in nc_files){
    print(file)
    date_str <- str_extract(file, "[0-9]{8}")
    unzip(file, exdir = paste0(local_dir, "/Prism_Files/", element, "/unzipped_files"))
    base_path <- paste0(local_dir,"/Prism_Files/", element, "/unzipped_files/PRISM_", element, "_")
    possible_files <- c(
      paste0(base_path, "provisional_4kmD2_", date_str, "_nc.nc"),
      paste0(base_path, "stable_4kmD2_", date_str, "_nc.nc"),
      paste0(base_path, "early_4kmD2_", date_str, "_nc.nc")
    )
    nc_file <- possible_files[file.exists(possible_files)][1]
    print(nc_file)
    ncin <- nc_open(nc_file)
    lon <- ncvar_get(ncin,"lon")
    lat <- ncvar_get(ncin, "lat")
    element_val <-ncvar_get(ncin, "Band1")
    fillvalue <- ncatt_get(ncin, "Band1", "_FillValue")
    nc_close(ncin)
    element_val[element_val == fillvalue$value] <- NA
    r <- raster::raster(t(element_val), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
    r <- flip(r, direction='y')
    date_obj <- as.Date(date_str, format="%Y%m%d")
    raster_data <- raster::extract(r, cbind(prism_coordinates[,"longitude"],prism_coordinates[,"latitude"]), method='simple')
    if (element == "ppt"){
      ppt_inch <- raster_data/25.4
      ppt_inch <- as.data.frame(ppt_inch)
      colnames(ppt_inch) <- date_obj
      prism_ppt_data <- cbind(prism_ppt_data, ppt_inch)
    }else if(element == "tmean") {
      tmean_f <- (raster_data*9/5) + 32
      tmean_f <- as.data.frame(tmean_f)
      colnames(tmean_f) <- date_obj
      prism_tmean_data <- cbind(prism_tmean_data, tmean_f)
    }else{
      tmax_f <- (raster_data*9/5) + 32
      tmax_f <- as.data.frame(tmax_f)
      colnames(tmax_f) <- date_obj
      prism_tmax_data <- cbind(prism_tmax_data, tmax_f)
    }
  }
}

# date_col <- names(prism_ppt_data[-c(1:3)])
ppt_long_data <- prism_ppt_data |>
  dplyr::select(-c(longitude, latitude)) |>
  pivot_longer(!name, names_to = "date", values_to = "ppt")
write_csv(ppt_long_data, "data/prism_data/prism_ppt_download.csv")

tmean_long_data <- prism_tmean_data |>
  dplyr::select(-c(longitude, latitude)) |>
  pivot_longer(!name, names_to = "date", values_to = "tmean")
tmax_long_data <- prism_tmax_data |>
  dplyr::select(-c(longitude, latitude)) |>
  pivot_longer(!name, names_to = "date", values_to = "tmax")
full_download_data <- ppt_long_data |>
  full_join(tmean_long_data, by = c("name", "date")) |>
  full_join(tmax_long_data, by = c("name", "date")) |>
  mutate(across(where(is.numeric), ~ round(.x, 3)))
write_csv(full_download_data, "data/prism_data/prism_download.csv")

ppt_data_summary <- ppt_long_data |>
  mutate(group = case_when(
    startsWith(name, "UKL") ~ "mean_total_daily_precip_ukl",
    startsWith(name, "WIL") ~ "mean_total_daily_precip_williamson",
    startsWith(name, "SPR") ~ "mean_total_daily_precip_sprague"
  )) |>
  group_by(date, group) |>
  summarise(mean = mean(ppt)) |>
  pivot_wider(names_from = "group", values_from ="mean") |>
  mutate(water_year = ifelse(month(date) >= 10, year(date) + 1, year(date))) |>
  group_by(water_year) %>%
  mutate(
    day_of_water_year = as.integer(difftime(date, ymd(paste0(water_year - 1 ,'-09-30')), units = "days"))) |>
  ungroup() |>
  arrange(date) |>
  mutate(
    mean_total_daily_precip_ukl = lead(mean_total_daily_precip_ukl, 1),
    mean_total_daily_precip_williamson = lead(mean_total_daily_precip_williamson, 1),
    mean_total_daily_precip_sprague = lead(mean_total_daily_precip_sprague, 1)
  ) |>
  relocate("mean_total_daily_precip_sprague", .after = "mean_total_daily_precip_williamson") |>
  relocate(c("water_year", "day_of_water_year"), .after = "date") |>
  mutate(weighted_mean_total_daily_precipitation_ukl_catchment_in = lag(round(
         (mean_total_daily_precip_ukl*snotel_area$`Proportion of total area`[1] +
         mean_total_daily_precip_williamson*snotel_area$`Proportion of total area`[2]+
         mean_total_daily_precip_sprague*snotel_area$`Proportion of total area`[3]), 3), 1, default = 0),
         thirty_d_trailing_sum_precip = round(rollsum(weighted_mean_total_daily_precipitation_ukl_catchment_in, k = 30, fill=NA, align = "right"), 3)
         ) |>
  left_join(min_max_ppt, by = "day_of_water_year") |>
  rowwise()|>
  mutate(normalized_30_trailing_sum_precipitation = round(normalize(thirty_d_trailing_sum_precip,
                                                                    min_30_trailing_sum_ppt,
                                                                    max_30_trailing_sum_ppt), 2))|>
  ungroup() |>
  # dplyr::select(-c(min_30_trailing_sum_ppt, max_30_trailing_sum_ppt, max_of_31_1095_d_trailing_sum_ppt, min_31_1095_d_trailing_sum_ppt)) |>
  mutate(date = as.Date(date, format= "%Y-%m-%d"))

ppt_data_export <- ppt_data_summary |>
  arrange(date) |>
  rowwise() |>
  mutate(
    start_date = date %m-% years(3),
    end_date = date %m-% months(1) - days(1),
    trailing_sum_31_1095_d_precip_in
    = sum(ppt_data_summary$weighted_mean_total_daily_precipitation_ukl_catchment_in[ppt_data_summary$date >= start_date & ppt_data_summary$date <= end_date], na.rm = TRUE),
    normalized_trailing_sum_31_1095_d_precip_in = round(normalize(trailing_sum_31_1095_d_precip_in,
                                                            min_31_1095_d_trailing_sum_ppt,
                                                            max_of_31_1095_d_trailing_sum_ppt), 2)
  ) |>
  ungroup() |>
  dplyr::select(-c(min_30_trailing_sum_ppt, max_30_trailing_sum_ppt, max_of_31_1095_d_trailing_sum_ppt, min_31_1095_d_trailing_sum_ppt))

write_csv(ppt_data_export, "data/prism_data/prism_ppt_summary_export.csv")



  tmean_data_summary <- tmean_long_data |>
  mutate(group = case_when(
    startsWith(name, "UKL") ~ "UKL_tmean",
    startsWith(name, "WIL") ~ "Williamson_tmean",
    startsWith(name, "SPR") ~ "Sprague_tmean"
  )) |>
  group_by(date, group) |>
  summarise(mean = mean(tmean)) |>
  pivot_wider(names_from = "group", values_from ="mean")
tmax_data_summary <- tmax_long_data |>
  mutate(group = case_when(
    startsWith(name, "UKL") ~ "UKL_tmax",
    startsWith(name, "WIL") ~ "Williamson_tmax",
    startsWith(name, "SPR") ~ "Sprague_tmax"
  )) |>
  group_by(date, group) |>
  summarise(mean = mean(tmax)) |>
  pivot_wider(names_from = "group", values_from ="mean")

summary_data <- ppt_data_summary |>
  full_join(tmean_data_summary, by = c("date")) |>
  full_join(tmax_data_summary, by = c("date")) |>
  mutate(across(where(is.numeric), ~ round(.x, 3)))
write_csv(summary_data, "data/prism_data/prism_summary.csv")

# nc_close(ncin)
# precip[precip == fillvalue$value] <- NA
# r <- raster(t(precip), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
# r <- flip(r, direction='y')
# par(mar=c(1,1,1,1))
# plot(r)i in 1:nrow(mat)
# date_obj <- as.Date(date_str, format="%Y%m%d")

