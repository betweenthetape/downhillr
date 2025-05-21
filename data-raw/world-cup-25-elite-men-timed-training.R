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
  ~pages,
  ~areas,
  ~metadata,
  "inst/extdata/20250517-biel_dhi_me_results_tt.pdf",
  1:5,
  list(
    c(140.7004, 19.7062, 738.9664, 575.5693),
    c(140.7004, 19.7062, 737.4245, 575.5693),
    c(139.9295, 18.9353, 738.9664, 577.1113),
    c(139.9295, 19.7062, 737.4245, 577.1113),
    c(140.7004, 19.7062, 390.4919, 574.7984)
  ),
  list(
    event_name = "Bielsko-Biala",
    event_type = "World Cup",
    event_year = "2025",
    round_type = "Timed Training",
    round_category = "Men Elite"
  )
)

# ---- Extract PDF's ----
timed_training <- pmap(
  .l = spec,
  .f = \(path, pages, areas, metadata) {
    extract_tables(
      file = path,
      pages = pages,
      area = areas,
      guess = FALSE,
      method = "stream",
      output = "tibble"
    ) |>
      bind_tables() |>
      clean_timed_training_25() |>
      mutate(!!!metadata)
  }
)

world_cup_25_elite_men_timed_training <- list_rbind(timed_training)

usethis::use_data(world_cup_25_elite_men_timed_training, overwrite = TRUE)
