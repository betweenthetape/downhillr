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
  "inst/extdata/20250517-biel_dhi_me_results_f.pdf",
  NULL,
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
  ),
  "inst/extdata/20250517-biel_dhi_me_results_q1.pdf",
  NULL,
  list(
    c(133.3696, 20.125, 743.4741, 578.3282),
    c(133.3696, 20.125, 742.4149, 574.0914),
    c(133.3696, 20.125, 570.823, 576.2098)
  ),
  list(
    event_name = "Bielsko-Biala",
    event_type = "World Cup",
    event_year = "2025",
    round_type = "Qualifying Round 1",
    round_category = "Men Elite",
    metadata_weather = "Cloudy",
    metadata_temp_deg_c = 15,
    metadata_distance_km = 2.000,
    metadata_average_speed_kmh = 38.166
  ),
  "inst/extdata/20250517-biel_dhi_me_results_q2.pdf",
  1:2,
  list(
    c(133.3696, 20.125, 743.4741, 576.2098),
    c(132.3104, 20.125, 728.6452, 575.1506)
  ),
  list(
    event_name = "Bielsko-Biala",
    event_type = "World Cup",
    event_year = "2025",
    round_type = "Qualifying Round 2",
    round_category = "Men Elite",
    metadata_weather = "Rainy",
    metadata_temp_deg_c = 9,
    metadata_distance_km = 2.000,
    metadata_average_speed_kmh = 37.586
  ),
  # ---- Loudenvielle ----
  "inst/extdata/20250530-loud_dhi_me_results_f.pdf",
  NULL,
  list(c(132.7839, 18.9897, 618.7335, 576.2859)),
  list(
    event_name = "Loudenvielle",
    event_type = "World Cup",
    event_year = "2025",
    round_type = "Final",
    round_category = "Men Elite",
    metadata_weather = "Cloudy",
    metadata_temp_deg_c = 22,
    metadata_distance_km = 2.530,
    metadata_average_speed_kmh = 47.145
  ),
  "inst/extdata/20250530-loud_dhi_me_results_q1.pdf",
  NULL,
  list(
    c(133.5767, 19.7825, 741.6082, 576.2859),
    c(132.7839, 18.9897, 740.8154, 575.4931),
    c(132.7839, 19.7825, 428.4759, 576.2859)
  ),
  list(
    event_name = "Loudenvielle",
    event_type = "World Cup",
    event_year = "2025",
    round_type = "Qualifying Round 1",
    round_category = "Men Elite",
    metadata_weather = "Mostly Sunny",
    metadata_temp_deg_c = 22,
    metadata_distance_km = 2.530,
    metadata_average_speed_kmh = 46.200
  ),
  "inst/extdata/20250530-loud_dhi_me_results_q2.pdf",
  1:2,
  list(
    c(132.7839, 19.7825, 742.4009, 576.2859),
    c(132.7839, 19.7825, 630.6246, 575.4931)
  ),
  list(
    event_name = "Loudenvielle",
    event_type = "World Cup",
    event_year = "2025",
    round_type = "Qualifying Round 2",
    round_category = "Men Elite",
    metadata_weather = "Mostly Sunny",
    metadata_temp_deg_c = 22,
    metadata_distance_km = 2.530,
    metadata_average_speed_kmh = 46.160
  ),
  # ---- Val di Sole ----
  "inst/extdata/20250619-vdso_dhi_me_results_f.pdf",
  NULL,
  list(c(132.7839, 19.3861, 618.7335, 576.6822)),
  list(
    event_name = "Val di Sole",
    event_type = "World Cup",
    event_year = "2025",
    round_type = "Final",
    round_category = "Men Elite",
    metadata_weather = "Mostly Sunny",
    metadata_temp_deg_c = 23,
    metadata_distance_km = 2.240,
    metadata_average_speed_kmh = 36.845
  ),
  "inst/extdata/20250619-vdso_dhi_me_results_q1.pdf",
  NULL,
  list(
    c(132.7839, 19.3861, 742.4009, 576.6822),
    c(132.7839, 19.3861, 742.4009, 575.0968),
    c(132.7839, 19.3861, 492.6878, 575.8895)
  ),
  list(
    event_name = "Val di Sole",
    event_type = "World Cup",
    event_year = "2025",
    round_type = "Qualifying Round 1",
    round_category = "Men Elite",
    metadata_weather = "Cloudy",
    metadata_temp_deg_c = 15,
    metadata_distance_km = 2.240,
    metadata_average_speed_kmh = 36.288
  ),
  "inst/extdata/20250619-vdso_dhi_me_results_q2.pdf",
  1:2,
  list(
    c(132.7839, 19.3861, 743.1937, 575.8895),
    c(131.9912, 19.3861, 680.5672, 575.8895)
  ),
  list(
    event_name = "Val di Sole",
    event_type = "World Cup",
    event_year = "2025",
    round_type = "Qualifying Round 2",
    round_category = "Men Elite",
    metadata_weather = "Sunny",
    metadata_temp_deg_c = 24,
    metadata_distance_km = 2.240,
    metadata_average_speed_kmh = 36.162
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

world_cup_25_elite_men_results <- list_rbind(results)

usethis::use_data(world_cup_25_elite_men_results, overwrite = TRUE)
