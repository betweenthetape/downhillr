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
  ),
  "inst/extdata/20250619-vdso_dhi_we_results_tt.pdf",
  1:2,
  list(
    c(140.7113, 20.1788, 737.6445, 576.6822),
    c(140.7113, 19.3861, 472.8693, 576.6822)
  ),
  list(
    event_name = "Val di Sole",
    event_type = "World Cup",
    event_year = "2025",
    round_type = "Timed Training",
    round_category = "Women Elite"
  ),
  "inst/extdata/20250704-thui_dhi_we_results_tt.pdf",
  1:2,
  list(
    c(139.9186, 18.9897, 737.6445, 576.2859),
    c(140.7113, 19.7825, 311.9431, 576.2859)
  ),
  list(
    event_name = "La Thuile",
    event_type = "World Cup",
    event_year = "2025",
    round_type = "Timed Training",
    round_category = "Women Elite"
  ),
  "inst/extdata/20250918-lenz_dhi_we_results_tt.pdf",
  1:2,
  list(
    c(140.5866, 18.9095, 738.7929, 576.3661),
    c(139.7716, 18.0945, 392.4201, 577.1811)
  ),
  list(
    event_name = "Lenzerheide",
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
