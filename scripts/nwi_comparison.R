library(dplyr)
library(ggplot2)
nwi_comp <- read_csv("data/nwi_data/nwi_comparison.csv")
nwi_comp_long <- nwi_comp |>
  mutate(across(-date, ~as.numeric(.x))) |>
  pivot_longer(!date, names_to = "index") |>
  mutate(date = as.Date(date, format="%m/%d/%Y")) |>
  glimpse()

nwi_wetness_index <- nwi_comp_long |> filter(index %in% c("wetness_index_v20_ip", "wetness_index_v20_vik"))

ggplot(nwi_wetness_index, aes(x = date, y = value, color = index)) +
    geom_line() +
    theme_minimal()
