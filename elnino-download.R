# Load required libraries
library(httr)
library(readr)

# Define the PDO data URL
nino_url <- "https://psl.noaa.gov/data/correlation/nina34.anom.data"
nino_file <- "data-raw/n34.txt"
download.file(nino_url, destfile = nino_file, mode = "wb")
nino_raw <- read_lines(nino_file)
#Remove header
n34 <- nino_raw[-1]
# Convert to dataframe
n34_df <- read.table(text = n34, header = FALSE, fill = TRUE)
colnames(n34_df) <- c("Year", "Jan", "Feb", "Mar", "Apr", "May", "Jun",
                      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
write.csv(n34_df, "data/n34_index_data.csv", row.names = FALSE)


