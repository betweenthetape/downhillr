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
  "inst/extdata/20250605-leog_dhi_we_results_tt.pdf",
  1:2,
  list(
    c(140.7113, 19.7825, 736.8517, 575.4931),
    c(140.7113, 19.7825, 469.6984, 575.4931)
  ),
  list(
    event_name = "Leogang",
    event_type = "World Cup",
    event_year = "2025",
    round_type = "Timed Training",
    round_category = "Women Elite"
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

world_cup_25_elite_women_timed_training <- list_rbind(timed_training)

usethis::use_data(world_cup_25_elite_women_timed_training, overwrite = TRUE)
