library(httr)
library(readr)

# Define the PDO data URL
pdo_url <- "https://www.ncei.noaa.gov/pub/data/cmb/ersst/v5/index/ersst.v5.pdo.dat"
pdo_file <- "data-raw/pdo_index_data.txt"
download.file(pdo_url, destfile = pdo_file, mode = "wb")
pdo_raw <- read_lines(pdo_file)
#Remove header
pdo <- pdo_raw[-1]
# Convert to dataframe
pdo_df <- read.table(text = pdo, header = TRUE, fill = TRUE)
write.csv(pdo_df, "data/pdo_index_data.csv", row.names = FALSE)
