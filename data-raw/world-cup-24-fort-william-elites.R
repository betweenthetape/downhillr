# ---- Libraries ----
library(tidyverse)
library(tabulapdf)
library(janitor)
pkgload::load_all()

# ------------------------------------------------------------------------------
# ELITE MENS
# ------------------------------------------------------------------------------

# ---- Final ----
# Set area grid with: locate_areas(file = "inst/extdata/world-cup-24-fort-william-men-elite-final.pdf")
mens_finals_raw <- extract_tables(
  file = "inst/extdata/world-cup-24-fort-william-men-elite-final.pdf",
  area = list(c(135.16215, 20.5751, 634.58829, 575.49312)),
  guess = FALSE,
  method = "stream",
  output = "tibble"
)

mens_finals <- mens_finals_raw |>
  bind_tables() |>
  clean_results_table() |>
  mutate(
    event_name = "Fort William",
    event_type = "World Cup",
    event_year = "2024",
    round_type = "Final",
    round_category = "Men Elite",
    metadata_weather = "Light rain",
    metadata_temp_deg_c = 19,
    metadata_distance_km = 3,
    metadata_average_speed_kmh = 44.214
  )

# ---- Semi-Final ----
# Set area grids with: locate_areas(file = "inst/extdata/world-cup-24-fort-william-men-elite-semi-final.pdf", pages = 1:2)
mens_semi_finals_raw <- extract_tables(
  file = "inst/extdata/world-cup-24-fort-william-men-elite-semi-final.pdf",
  pages = 1:2,
  area = list(
    c(134.36941, 20.57519, 743.19365, 575.49312),
    c(134.36941, 20.57519, 539.45950, 576.28586)
  ),
  guess = FALSE,
  method = "stream",
  output = "tibble"
)

mens_semi_finals <- mens_semi_finals_raw |>
  bind_tables() |>
  clean_results_table() |>
  mutate(
    event_name = "Fort William",
    event_type = "World Cup",
    event_year = "2024",
    round_type = "Semi-Final",
    round_category = "Men Elite",
    metadata_weather = "Mostly sunny",
    metadata_temp_deg_c = 21,
    metadata_distance_km = 3,
    metadata_average_speed_kmh = 42.368
  )

# ---- Qualification ----
# Set area grids with: locate_areas(file = "inst/extdata/world-cup-24-fort-william-men-elite-qualifying.pdf", pages = 1:4)
mens_qualifying_raw <- extract_tables(
  file = "inst/extdata/world-cup-24-fort-william-men-elite-qualifying.pdf",
  pages = 1:4,
  area = list(
    c(133.57667, 20.57519, 743.19365, 575.49312),
    c(133.57667, 20.57519, 743.19365, 575.49312),
    c(133.57667, 19.78245, 743.19365, 575.49312),
    c(133.57667, 20.57519, 603.67143, 576.28586)
  ),
  guess = FALSE,
  method = "stream",
  output = "tibble"
)

mens_qualifying <- mens_qualifying_raw |>
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
# Set area grids with: locate_areas(file = "inst/extdata/world-cup-24-fort-william-men-elite-timed-training.pdf", pages = 1:7)
mens_timed_training_raw <- extract_tables(
  file = "inst/extdata/world-cup-24-fort-william-men-elite-timed-training.pdf",
  pages = 1:7,
  area = list(
    c(140.71133, 19.78245, 736.85173, 576.28586),
    c(140.71133, 19.78245, 737.64447, 576.28586),
    c(140.71133, 20.57519, 737.64447, 575.49312),
    c(140.71133, 19.78245, 737.64447, 576.28586),
    c(140.71133, 19.78245, 738.43721, 576.28586),
    c(139.91859, 19.78245, 737.64447, 576.28586),
    c(140.71133, 19.78245, 364.26398, 576.28586)
  ),
  guess = FALSE,
  method = "stream",
  output = "tibble"
)

mens_timed_training <- mens_timed_training_raw |>
  bind_tables() |>
  clean_timed_training_table() |>
  mutate(
    event_name = "Fort William",
    event_type = "World Cup",
    event_year = "2024",
    round_type = "Timed Training",
    round_category = "Men Elite"
  )

# ------------------------------------------------------------------------------
# ELITE WOMENS
# ------------------------------------------------------------------------------

# ---- Final ----
# Set area grid with: locate_areas(file = "inst/extdata/world-cup-24-fort-william-women-elite-final.pdf")
womens_finals_raw <- extract_tables(
  file = "inst/extdata/world-cup-24-fort-william-women-elite-final.pdf",
  area = list(c(133.32832, 20.50118, 321.96753, 575.93883)),
  guess = FALSE,
  method = "stream",
  output = "tibble"
)

womens_finals <- womens_finals_raw |>
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
womens_semi_finals_raw <- extract_tables(
  file = "inst/extdata/world-cup-24-fort-william-women-elite-semi-final.pdf",
  area = list(c(134.49276, 20.50118, 415.12269, 575.93883)),
  guess = FALSE,
  method = "stream",
  output = "tibble"
)

womens_semi_finals <- womens_semi_finals_raw |>
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
# Set area grids with: locate_areas(file = "inst/extdata/world-cup-24-fort-william-women-elite-qualifying.pdf")
womens_qualifying_raw <- extract_tables(
  file = "inst/extdata/world-cup-24-fort-william-women-elite-qualifying.pdf",
  area = list(c(133.57667, 20.17882, 695.62926, 575.09675)),
  guess = FALSE,
  method = "stream",
  output = "tibble"
)

womens_qualifying <- womens_qualifying_raw |>
  bind_tables() |>
  clean_results_table() |>
  mutate(
    event_name = "Fort William",
    event_type = "World Cup",
    event_year = "2024",
    round_type = "Qualifying",
    round_category = "Women Elite",
    metadata_weather = "Cloudy",
    metadata_temp_deg_c = 21,
    metadata_distance_km = 3,
    metadata_average_speed_kmh = 37.494
  )

# ---- Time training ----
# Set area grids with: locate_areas(file = "inst/extdata/world-cup-24-fort-william-women-elite-timed-training.pdf", pages = 1:2)
womens_timed_training_raw <- extract_tables(
  file = "inst/extdata/world-cup-24-fort-william-women-elite-timed-training.pdf",
  pages = 1:2,
  area = list(
    c(140.71133, 20.17882, 738.43721, 575.88949),
    c(140.71133, 20.17882, 338.10357, 575.88949)
  ),
  guess = FALSE,
  method = "stream",
  output = "tibble"
)

womens_timed_training <- womens_timed_training_raw |>
  bind_tables() |>
  clean_timed_training_table() |>
  mutate(
    event_name = "Fort William",
    event_type = "World Cup",
    event_year = "2024",
    round_type = "Timed Training",
    round_category = "Women Elite"
  )

# ------------------------------------------------------------------------------
# COMBINE + SAVE
# ------------------------------------------------------------------------------

world_cup_24_fort_william_elites_results <- bind_rows(
  mens_finals, mens_semi_finals, mens_qualifying, womens_finals,
  womens_semi_finals, womens_qualifying
)

usethis::use_data(
  world_cup_24_fort_william_elites_results,
  overwrite = TRUE
)

world_cup_24_fort_william_elites_timed_training <- bind_rows(
  mens_timed_training, womens_timed_training
)

usethis::use_data(
  world_cup_24_fort_william_elites_timed_training,
  overwrite = TRUE
)
