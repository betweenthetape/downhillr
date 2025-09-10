# ------------------------------------------------------------------------------
# README:
# To run this script, you first need to manually download the results PDF's
# from https://www.valais2025.ch/results#result matching the file names to those
# below.
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
  # ---- Elite men ----
  "inst/extdata/20250906-world-champs-elite-men-f.pdf",
  NULL,
  list(
    c(175.5919, 19.7825, 707.5204, 575.4931),
    c(174.7991, 18.9897, 706.7276, 576.2859),
    c(175.5919, 18.9897, 581.4747, 577.0786)
  ),
  list(
    event_name = "Champéry",
    event_type = "World Championships",
    event_year = "2025",
    round_type = "Final",
    round_category = "Men Elite",
    metadata_weather = "Sunny",
    metadata_temp_deg_c = 20,
    metadata_distance_km = 1.720,
    metadata_average_speed_kmh = 35.555
  ),
  # ---- Elite women ----
  "inst/extdata/20250906-world-champs-elite-women-f.pdf",
  NULL,
  list(
    c(175.5919, 19.7825, 707.5204, 577.0786),
    c(175.5919, 19.7825, 388.0462, 576.2859)
  ),
  list(
    event_name = "Champéry",
    event_type = "World Championships",
    event_year = "2025",
    round_type = "Final",
    round_category = "Women Elite",
    metadata_weather = "Sunny",
    metadata_temp_deg_c = 20,
    metadata_distance_km = 1.720,
    metadata_average_speed_kmh = 29.893
  ),
  # ---- Junior Men ----
  "inst/extdata/20250906-world-champs-junior-men-f.pdf",
  NULL,
  list(
    c(174.7991, 19.7825, 706.7276, 576.2859),
    c(174.7991, 19.7825, 709.1058, 577.8713),
    c(175.5919, 19.7825, 224.7418, 577.0786)
  ),
  list(
    event_name = "Champéry",
    event_type = "World Championships",
    event_year = "2025",
    round_type = "Final",
    round_category = "Junior Men",
    metadata_weather = "Sunny",
    metadata_temp_deg_c = 12,
    metadata_distance_km = 1.720,
    metadata_average_speed_kmh = 34.233
  ),
  # ---- Junior Women ----
  "inst/extdata/20250906-world-champs-junior-women-f.pdf",
  NULL,
  list(c(175.5919, 19.7825, 545.0087, 577.0786)),
  list(
    event_name = "Champéry",
    event_type = "World Championships",
    event_year = "2025",
    round_type = "Final",
    round_category = "Women Elite",
    metadata_weather = "Sunny",
    metadata_temp_deg_c = 12,
    metadata_distance_km = 1.720,
    metadata_average_speed_kmh = 28.672
  )
)

# ---- Extract PDF's ----
results <- pmap(
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
      clean_world_champs_25() |>
      mutate(!!!metadata)
  }
)

world_championships_25 <- list_rbind(results)

usethis::use_data(world_championships_25, overwrite = TRUE)
