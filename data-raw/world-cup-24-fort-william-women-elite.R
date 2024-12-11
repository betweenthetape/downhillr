# ---- Libraries ----
library(tidyverse)
library(tabulapdf)
library(janitor)
pkgload::load_all()

# ---- Final ----
# Set area grid with: locate_areas(file = "inst/extdata/world-cup-24-fort-william-women-elite-final.pdf")
finals_raw <- extract_tables(
  file = "inst/extdata/world-cup-24-fort-william-women-elite-final.pdf",
  area = list(c(133.32832, 20.50118, 321.96753, 575.93883)),
  guess = FALSE,
  method = "stream",
  output = "tibble"
)

finals <- finals_raw |>
  bind_tables() |>
  clean_results_table() |>
  mutate(
    event_name = "Fort William",
    event_type = "World Cup",
    event_year = "2024",
    round_type = "Final",
    round_category = "Women Elite",
    metadata_weather = "Cloudy",
    metadata_temp_deg_c = 19,
    metadata_distance_km = 3,
    metadata_average_speed_kmh = 38.376
  )

# ---- Semi-Final ----
# Set area grids with: locate_areas(file = "inst/extdata/world-cup-24-fort-william-women-elite-semi-final.pdf")
semi_finals_raw <- extract_tables(
  file = "inst/extdata/world-cup-24-fort-william-women-elite-semi-final.pdf",
  area = list(c(134.49276, 20.50118, 415.12269, 575.93883)),
  guess = FALSE,
  method = "stream",
  output = "tibble"
)

semi_finals <- semi_finals_raw |>
  bind_tables() |>
  clean_results_table() |>
  mutate(
    event_name = "Fort William",
    event_type = "World Cup",
    event_year = "2024",
    round_type = "Semi-Final",
    round_category = "Women Elite",
    metadata_weather = "Cloudy",
    metadata_temp_deg_c = 19,
    metadata_distance_km = 3,
    metadata_average_speed_kmh = 37.846
  )

# ---- Qualification ----
# Set area grids with: locate_areas(file = "inst/extdata/world-cup-24-fort-william-women-elite-qualifying.pdf", pages = 1:4)
qualifying_raw <- extract_tables(
  file = "inst/extdata/world-cup-24-fort-william-women-elite-qualifying.pdf",
  pages = 1:4,
  area = list(
    c(),
    c(),
    c(),
    c()
  ),
  guess = FALSE,
  method = "stream",
  output = "tibble"
)

qualifying <- qualifying_raw |>
  bind_tables() |>
  clean_results_table() |>
  mutate(
    event_name = "Fort William",
    event_type = "World Cup",
    event_year = "2024",
    round_type = "Qualifying",
    round_category = "Men Elite",
    metadata_weather = "Cloudy",
    metadata_temp_deg_c = 21,
    metadata_distance_km = 3,
    metadata_average_speed_kmh = 43.490
  )

# ---- Time training ----
# Set area grids with: locate_areas(file = "inst/extdata/world-cup-24-fort-william-women-elite-timed-training.pdf", pages = 1:7)
timed_training_raw <- extract_tables(
  file = "inst/extdata/world-cup-24-fort-william-women-elite-timed-training.pdf",
  pages = 1:7,
  area = list(
    c(),
    c(),
    c()
  ),
  guess = FALSE,
  method = "stream",
  output = "tibble"
)

timed_training <- timed_training_raw |>
  bind_tables() |>
  clean_timed_training_table() |>
  mutate(
    event_name = "Fort William",
    event_type = "World Cup",
    event_year = "2024",
    round_type = "Timed Training",
    round_category = "Men Elite"
  )

# ---- Save data ----
world_cup_24_fort_william_men_elite_results <-
  bind_rows(finals, semi_finals, qualifying)

usethis::use_data(
  world_cup_24_fort_william_men_elite_results,
  overwrite = TRUE
)

world_cup_24_fort_william_men_elite_timed_training <- timed_training

usethis::use_data(
  world_cup_24_fort_william_men_elite_timed_training,
  overwrite = TRUE
)
