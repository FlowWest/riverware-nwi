library(httr)
library(readr)
library(ncdf4)
library(stringr)

prism_base_url <- "http://services.nacse.org/prism/data/public/4km"
element <- "ppt"
start_date <- as.Date("2025-02-01")
stop_date <- as.Date("2025-02-03")

for (current_date in seq(start_date, stop_date, by = "day")) {
  # Format date as YYYYMMDD
  day <- format(as.Date(current_date), "%Y%m%d")

  # Construct download URL
  download_url <- paste0(prism_base_url, "/", element, "/", day, "?format=nc")

  # Use download.file() to fetch the file
  download.file(download_url, destfile = paste0("data-raw/",day, ".zip"), mode = "wb")

  # Be nice to the server (wait 2 seconds)
  Sys.sleep(2)
}
prism_locations <- readRDS("data/prism_locations")
prism_coordinates <- apply(prism_locations[,2:3], 2, as.numeric)
prism_data <- as.data.frame(prism_locations)
nc_files <- list.files(path = "data-raw", pattern = "*.zip", full.names = TRUE)
for (file in nc_files){
  date_str <- str_extract(file, "[0-9]{8}")
  unzip(file, exdir = "data-raw/Prism_Files")
  ncin <- nc_open(paste0("data-raw/Prism_Files/PRISM_ppt_provisional_4kmD2_", date_str, "_nc.nc"))
  lon <- ncvar_get(ncin,"lon")
  lat <- ncvar_get(ncin, "lat")
  precip <-ncvar_get(ncin, "Band1")
  fillvalue <- ncatt_get(ncin, "Band1", "_FillValue")
  nc_close(ncin)
  precip[precip == fillvalue$value] <- NA
  r <- raster(t(precip), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  r <- flip(r, direction='y')
  date_obj <- as.Date(date_str, format="%Y%m%d")
  ppt_mm <- raster::extract(r, cbind(prism_coordinates[,"longitude"],prism_coordinates[,"latitude"]), method='simple')
  ppt_inch <- ppt_mm/25.4
  ppt_inch <- as.data.frame(ppt_inch)
  colnames(ppt_inch) <- date_obj
  prism_data <- cbind(prism_data, ppt_inch)
}
file <- "data-raw/20250201.zip"
unzip(file, exdir = "data-raw/Prism_Files")
ncin <- nc_open("data-raw/Prism_Files/PRISM_ppt_provisional_4kmD2_20250201_nc.nc")
lon <- ncvar_get(ncin,"lon")
lat <- ncvar_get(ncin, "lat")
precip <-ncvar_get(ncin, "Band1")
fillvalue <- ncatt_get(ncin, "Band1", "_FillValue")
nc_close(ncin)
precip[precip == fillvalue$value] <- NA
r <- raster(t(precip), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
r <- flip(r, direction='y')
# par(mar=c(1,1,1,1))
# plot(r)i in 1:nrow(mat)
date_obj <- as.Date(date_str, format="%Y%m%d")
ppt_mm <- raster::extract(r, cbind(prism_coordinates[,"longitude"],prism_coordinates[,"latitude"]), method='simple')
ppt_inch <- ppt_mm/25.4
ppt_inch <- as.data.frame(ppt_inch)
colnames(ppt_inch) <- date_obj
prism_data <- as.data.frame(prism_locations)
prism_data <- cbind(prism_data, ppt_inch)
# prism_data[, date_obj]
