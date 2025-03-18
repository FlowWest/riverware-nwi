library(httr)
library(readr)
library(ncdf4)
library(stringr)
library(tidyverse)
element_list <- c("ppt", "tmean", "tmax")
prism_base_url <- "http://services.nacse.org/prism/data/public/4km"
# element <- "tmean"
start_date <- as.Date("2025-02-01")
stop_date <- as.Date("2025-02-03")
prism_locations <- readRDS("data/prism_data/prism_locations")
prism_coordinates <- apply(prism_locations[,2:3], 2, as.numeric)

prism_ppt_data <- as.data.frame(prism_locations)
prism_tmean_data <- as.data.frame(prism_locations)
prism_tmax_data <- as.data.frame(prism_locations)

for (element in element_list){
  for (current_date in seq(start_date, stop_date, by = "day")) {
    day <- format(as.Date(current_date), "%Y%m%d")
    download_url <- paste0(prism_base_url, "/", element, "/", day, "?format=nc")
    download.file(download_url, destfile = paste0("data-raw/Prism_Files/",day,"_",element, ".zip"), mode = "wb")
    Sys.sleep(2)
  }
  nc_files <- list.files(path = "data-raw/Prism_Files", pattern = "*.zip", full.names = TRUE)

  for (file in nc_files){
    date_str <- str_extract(file, "[0-9]{8}")
    unzip(file, exdir = "data-raw/Prism_Files/unzipped/")
    ncin <- nc_open(paste0("data-raw/Prism_Files/unzipped/PRISM_", element,"_provisional_4kmD2_", date_str, "_nc.nc"))
    lon <- ncvar_get(ncin,"lon")
    lat <- ncvar_get(ncin, "lat")
    element_val <-ncvar_get(ncin, "Band1")
    fillvalue <- ncatt_get(ncin, "Band1", "_FillValue")
    nc_close(ncin)
    element_val[element_val == fillvalue$value] <- NA
    r <- raster(t(element_val), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
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

date_col <- names(prism_ppt_data[-c(1:3)])
ppt_long_data <- prism_ppt_data |>
  dplyr::select(-c(longitude, latitude)) |>
  pivot_longer(!name, names_to = "date", values_to = "ppt")
tmean_long_data <- tmean_prism_data |>
  dplyr::select(-c(longitude, latitude)) |>
  pivot_longer(!name, names_to = "date", values_to = "tmean")
tmax_long_data <- tmax_prism_data |>
  dplyr::select(-c(longitude, latitude)) |>
  pivot_longer(!name, names_to = "date", values_to = "tmax")
full_download_data <- ppt_long_data |>
  full_join(tmean_long_data, by = c("name", "date")) |>
  full_join(tmax_long_data, by = c("name", "date"))
write_csv(full_download_data, "data/prism_data/prism_download.csv")

# wide_data <-  ppt_long_data |>
#   pivot_wider(names_from = "name", values_from = "ppt")

ppt_data_summary <- ppt_long_data |>
  mutate(group = case_when(
    startsWith(name, "UKL") ~ "UKL_ppt",
    startsWith(name, "WIL") ~ "Williamson_ppt",
    startsWith(name, "SPR") ~ "Sprague_ppt "
  )) |>
  group_by(date, group) |>
  summarise(mean = mean(ppt)) |>
  pivot_wider(names_from = "group", values_from ="mean")
tmean_data_summary <- tmean_long_data |>
  mutate(group = case_when(
    startsWith(name, "UKL") ~ "UKL_tmean",
    startsWith(name, "WIL") ~ "Williamson_tmean",
    startsWith(name, "SPR") ~ "Sprague_tmean "
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
summary_data <- rbind(ppt_data_summary, tmean_data_summary, tmax_data_summary)
write_csv(summary_data, "data/prism_data/prisma_summary.csv")

# file <- "data-raw/20250201.zip"
# unzip(file, exdir = "data-raw/Prism_Files")
# ncin <- nc_open("data-raw/Prism_Files/PRISM_tmean_provisional_4kmD2_20250201_nc.nc")
# lon <- ncvar_get(ncin,"lon")
# lat <- ncvar_get(ncin, "lat")
# precip <-ncvar_get(ncin, "Band1")
# fillvalue <- ncatt_get(ncin, "Band1", "_FillValue")
# nc_close(ncin)
# precip[precip == fillvalue$value] <- NA
# r <- raster(t(precip), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
# r <- flip(r, direction='y')
# par(mar=c(1,1,1,1))
# plot(r)i in 1:nrow(mat)
# date_obj <- as.Date(date_str, format="%Y%m%d")
# ppt_mm <- raster::extract(r, cbind(prism_coordinates[,"longitude"],prism_coordinates[,"latitude"]), method='simple')
# ppt_inch <- ppt_mm/25.4
# ppt_inch <- as.data.frame(as.numeric(ppt_inch))
# colnames(ppt_inch) <- date_obj
# prism_data <- as.data.frame(prism_locations)
# prism_data <- cbind(prism_data, ppt_inch)

