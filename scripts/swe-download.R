library(httr)
library(jsonlite)
library(lubridate)

nrcs_api_url <- "https://wcc.sc.egov.usda.gov/awdbRestApi"

get_station_data <- function(station_triplet){
  station_data <- get_timeseries_data(station_triplet = station_triplet)
  station_data_df <- as.data.frame(station_data[,2][[1]])$values[[1]]
  col_name <- station_triplets_lookup[station_triplet]
  colnames(station_data_df) <- c("Date", col_name)
  return(station_data_df)
}
# Function to get timeseries data
# Default element_cd is WTEQ (Snow Water Equivalent)
# Default start date is Oct 1 of the current water year
# Default end date is current date
# Default data temporal resolution is daily
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

station_triplets_lookup <- c(
  "344:OR:SNTL" = "Billie Creek Divide SWE (in)",
  "395:OR:SNTL" = "Chemult Alternate SWE (in)",
  "406:OR:SNTL" = "Cold Springs Camp SWE (in)",
  "442:OR:SNTL" = "Diamond Lake SWE (in)",
  "478:WA:SNTL" = "Fish Lake SWE (in)",
  "483:OR:SNTL" = "Fourmile Lake SWE (in)",
  "706:OR:SNTL" = "Quartz Mountain SWE (in)",
  "745:OR:SNTL" = "Sevenmile Marsh SWE (in)",
  "756:OR:SNTL" = "Silver Creek SWE (in)",
  "794:OR:SNTL" = "Strawberry SWE (in)",
  "800:OR:SNTL" = "Summer Rim SWE (in)",
  "810:OR:SNTL" = "Taylor Butte SWE (in)")

data_list <- list()
for (station in names(station_triplets_lookup)) {
  station_df <- get_station_data(station)
  data_list[[station]] <- station_df
}
full_swe_df <- Reduce(function(x, y) full_join(x, y, by = "Date"), data_list)
write.csv(full_swe_df, "data/swe_data/snotel_swe_data.csv", row.names=FALSE)
