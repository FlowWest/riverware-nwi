library(httr)
library(readr)
library(ncdf4)

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

file <- "data-raw/20250201.zip"
unzip(file, exdir = "data-raw/Prism_Files")
ncin <- nc_open("data-raw/Prism_Files/PRISM_ppt_provisional_4kmD2_20250201_nc.nc")
lon <- ncvar_get(ncin,"lon")
lat <- ncvar_get(ncin, "lat")
precip <-ncvar_get(ncin, "Band1")
nc_close(ncin)
df <- expand.grid(lon = lon, lat = lat) %>%
  mutate(precip = as.vector(precip)) %>%
  filter(precip != -9999)  # Remove missing values

# View data
head(df)

# Plot precipitation
ggplot(df, aes(x = lon, y = lat, fill = precip)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(title = "PRISM Precipitation Data", x = "Longitude", y = "Latitude") +
  theme_minimal()
