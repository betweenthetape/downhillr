# ------------------------------------------------------------------------------
# README:
# To run this script, you first need to manually download the results PDF's
# from https://www.uci.org/competition-hub/2024-uci-mountain-bike-world-cup/6BCZY2ZIDMZdCWN7Ee5WD3
# matching the file names to those below
# ------------------------------------------------------------------------------

# ---- Load ----
pkgload::load_all()
library(tidyverse)
library(tabulapdf)

# ---- Create spec ----
# Run `pdf_areas(<INSERT PATH>)` on each path to determine PDF table areas.
spec <- tribble(
  ~path,
  ~areas,
  ~metadata,
  "inst/extdata/20250517-biel_dhi_me_results_f.pdf",
  list(c(132.1639, 19.919, 617.7352, 575.3566)),
  list(
    event_name = "Bielsko-Biala",
    event_type = "World Cup",
    event_year = "2025",
    round_type = "Final",
    round_category = "Men Elite",
    metadata_weather = "Rain",
    metadata_temp_deg_c = 4,
    metadata_distance_km = 2.000,
    metadata_average_speed_kmh = 38.947
  )
)

# ---- Extract PDF's ----
results <- pmap(
  .l = spec,
  .f = \(path, areas, metadata) {
    extract_tables(
      file = path,
      area = areas,
      guess = FALSE,
      method = "stream",
      output = "tibble"
    ) |>
      bind_tables() |>
      clean_results() |>
      mutate(!!!metadata)
  }
)

world_cup_25_elite_men_results <- list_rbind(results)

usethis::use_data(world_cup_25_elite_men_results, overwrite = TRUE)
