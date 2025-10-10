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
  # ---- Leogang ----
  "inst/extdata/20250605-leog_dhi_we_results_f.pdf",
  1,
  list(c(133.5767, 19.7825, 380.9115, 575.4931)),
  list(
    event_name = "Leogang",
    event_type = "World Cup",
    event_year = "2025",
    round_type = "Final",
    round_category = "Women Elite",
    metadata_weather = "Cloudy",
    metadata_temp_deg_c = 20,
    metadata_distance_km = 2.115,
    metadata_average_speed_kmh = 37.700
  ),
  "inst/extdata/20250605-leog_dhi_we_results_q1.pdf",
  1,
  list(c(133.5767, 19.7825, 711.4841, 575.4931)),
  list(
    event_name = "Leogang",
    event_type = "World Cup",
    event_year = "2025",
    round_type = "Qualifying Round 1",
    round_category = "Women Elite",
    metadata_weather = "Sunny",
    metadata_temp_deg_c = 24,
    metadata_distance_km = 2.115,
    metadata_average_speed_kmh = 36.536
  ),
  "inst/extdata/20250605-leog_dhi_we_results_q2.pdf",
  1,
  list(c(133.5767, 19.7825, 522.812, 576.2859)),
  list(
    event_name = "Leogang",
    event_type = "World Cup",
    event_year = "2025",
    round_type = "Qualifying Round 2",
    round_category = "Women Elite",
    metadata_weather = "Mostly sunny",
    metadata_temp_deg_c = 25,
    metadata_distance_km = 2.115,
    metadata_average_speed_kmh = 36.414
  ),
  # ---- Val di Sole ----
  "inst/extdata/20250619-vdso_dhi_we_results_f.pdf",
  1,
  list(c(133.5767, 19.3861, 384.0825, 575.8895)),
  list(
    event_name = "Val di Sole",
    event_type = "World Cup",
    event_year = "2025",
    round_type = "Final",
    round_category = "Women Elite",
    metadata_weather = "Mostly Sunny",
    metadata_temp_deg_c = 21,
    metadata_distance_km = 2.240,
    metadata_average_speed_kmh = 32.216
  ),
  "inst/extdata/20250619-vdso_dhi_we_results_q1.pdf",
  1,
  list(c(132.7839, 19.3861, 700.3857, 576.6822)),
  list(
    event_name = "Val di Sole",
    event_type = "World Cup",
    event_year = "2025",
    round_type = "Qualifying Round 1",
    round_category = "Women Elite",
    metadata_weather = "Sunny",
    metadata_temp_deg_c = 24,
    metadata_distance_km = 2.240,
    metadata_average_speed_kmh = 31.607
  ),
  "inst/extdata/20250619-vdso_dhi_we_results_q2.pdf",
  1,
  list(c(132.7839, 20.1788, 522.0192, 575.8895)),
  list(
    event_name = "Val di Sole",
    event_type = "World Cup",
    event_year = "2025",
    round_type = "Qualifying Round 2",
    round_category = "Women Elite",
    metadata_weather = "Sunny",
    metadata_temp_deg_c = 24,
    metadata_distance_km = 2.240,
    metadata_average_speed_kmh = 30.777
  ),
  # ---- La Thiule ----
  "inst/extdata/20250704-thui_dhi_we_results_f.pdf",
  1,
  list(c(132.7839, 19.7825, 382.497, 576.2859)),
  list(
    event_name = "La Thuile",
    event_type = "World Cup",
    event_year = "2025",
    round_type = "Final",
    round_category = "Women Elite",
    metadata_weather = "Mostly Cloudy",
    metadata_temp_deg_c = 20,
    metadata_distance_km = 2.300,
    metadata_average_speed_kmh = 34.800
  ),
  "inst/extdata/20250704-thui_dhi_we_results_q1.pdf",
  1,
  list(c(133.5767, 19.7825, 649.6503, 577.0786)),
  list(
    event_name = "La Thuile",
    event_type = "World Cup",
    event_year = "2025",
    round_type = "Qualifying Round 1",
    round_category = "Women Elite",
    metadata_weather = "Sunny",
    metadata_temp_deg_c = 22,
    metadata_distance_km = 2.300,
    metadata_average_speed_kmh = 33.805
  ),
  "inst/extdata/20250704-thui_dhi_we_results_q2.pdf",
  1,
  list(c(132.7839, 18.9897, 349.9947, 576.2859)),
  list(
    event_name = "La Thuile",
    event_type = "World Cup",
    event_year = "2025",
    round_type = "Qualifying Round 2",
    round_category = "Women Elite",
    metadata_weather = "Sunny",
    metadata_temp_deg_c = 22,
    metadata_distance_km = 2.300,
    metadata_average_speed_kmh = 32.524
  ),
  # ---- Lenzerheide ----
  "inst/extdata/20250918-lenz_dhi_we_results_f.pdf",
  1,
  list(c(132.4367, 18.0945, 385.0851, 577.1811)),
  list(
    event_name = "Lenzerheide",
    event_type = "World Cup",
    event_year = "2025",
    round_type = "Final",
    round_category = "Women Elite",
    metadata_weather = "Sunny",
    metadata_temp_deg_c = 25,
    metadata_distance_km = 1.840,
    metadata_average_speed_kmh = 34.576
  ),
  "inst/extdata/20250918-lenz_dhi_we_results_q1.pdf",
  1,
  list(c(133.2517, 18.9095, 680.9283, 576.3661)),
  list(
    event_name = "Lenzerheide",
    event_type = "World Cup",
    event_year = "2025",
    round_type = "Qualifying Round 1",
    round_category = "Women Elite",
    metadata_weather = "Sunny",
    metadata_temp_deg_c = 20,
    metadata_distance_km = 1.840,
    metadata_average_speed_kmh = 33.691
  ),
  "inst/extdata/20250918-lenz_dhi_we_results_q2.pdf",
  1,
  list(c(133.2517, 19.7245, 478.8095, 576.3661)),
  list(
    event_name = "Lenzerheide",
    event_type = "World Cup",
    event_year = "2025",
    round_type = "Qualifying Round 2",
    round_category = "Women Elite",
    metadata_weather = "Sunny",
    metadata_temp_deg_c = 22,
    metadata_distance_km = 1.840,
    metadata_average_speed_kmh = 33.058
  ),
  # ---- Mount-Sainte-Anne ----
  NULL,
  NULL,
  list(),
  list(
    event_name = "Mount-Sainte-Anne",
    event_type = "World Cup",
    event_year = "2025",
    round_type = "Final",
    round_category = "Women Elite",
    metadata_weather = NULL,
    metadata_temp_deg_c = NULL,
    metadata_distance_km = NULL,
    metadata_average_speed_kmh = NULL
  ),
  NULL,
  NULL,
  list(),
  list(
    event_name = "Mont-Sainte-Anne",
    event_type = "World Cup",
    event_year = "2025",
    round_type = "Qualifying Round 1",
    round_category = "Women Elite",
    metadata_weather = NULL,
    metadata_temp_deg_c = NULL,
    metadata_distance_km = NULL,
    metadata_average_speed_kmh = NULL
  ),
  NULL,
  NULL,
  list(),
  list(
    event_name = "Mont-Sainte-Anne",
    event_type = "World Cup",
    event_year = "2025",
    round_type = "Qualifying Round 2",
    round_category = "Women Elite",
    metadata_weather = NULL,
    metadata_temp_deg_c = NULL,
    metadata_distance_km = NULL,
    metadata_average_speed_kmh = NULL
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
      clean_results_25() |>
      mutate(!!!metadata)
  }
)

world_cup_25_elite_women_results <- list_rbind(results)

usethis::use_data(world_cup_25_elite_women_results, overwrite = TRUE)
