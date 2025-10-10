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
  # ---- Leogang ----
  "inst/extdata/20250605-leog_dhi_me_results_f.pdf",
  NULL,
  list(c(133.0079, 19.4016, 618.0472, 576.7559)),
  list(
    event_name = "Leogang",
    event_type = "World Cup",
    event_year = "2025",
    round_type = "Final",
    round_category = "Men Elite",
    metadata_weather = "Cloudy",
    metadata_temp_deg_c = 20,
    metadata_distance_km = 2.115,
    metadata_average_speed_kmh = 42.961
  ),
  "inst/extdata/20250605-leog_dhi_me_results_q1.pdf",
  1:3,
  list(
    c(132.126, 19.4016, 744.1575, 575.874),
    c(133.0079, 19.4016, 743.2756, 577.6378),
    c(133.0079, 19.4016, 680.6614, 577.6378)
  ),
  list(
    event_name = "Leogang",
    event_type = "World Cup",
    event_year = "2025",
    round_type = "Qualifying Round 1",
    round_category = "Men Elite",
    metadata_weather = "Cloudy",
    metadata_temp_deg_c = 15,
    metadata_distance_km = 2.115,
    metadata_average_speed_kmh = 41.725
  ),
  "inst/extdata/20250605-leog_dhi_me_results_q2.pdf",
  1:3,
  list(
    c(133.0079, 19.4016, 743.2756, 576.7559),
    c(133.0079, 19.4016, 743.2756, 575.874),
    c(133.0079, 19.4016, 272.3465, 576.7559)
  ),
  list(
    event_name = "Leogang",
    event_type = "World Cup",
    event_year = "2025",
    round_type = "Qualifying Round 2",
    round_category = "Men Elite",
    metadata_weather = "Mostly Sunny",
    metadata_temp_deg_c = 25,
    metadata_distance_km = 2.115,
    metadata_average_speed_kmh = 42.023
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
  ),
  # ---- La Thiule ----
  "inst/extdata/20250704-thui_dhi_me_results_f.pdf",
  NULL,
  list(c(133.5767, 20.5752, 617.9407, 574.7004)),
  list(
    event_name = "La Thuile",
    event_type = "World Cup",
    event_year = "2025",
    round_type = "Final",
    round_category = "Men Elite",
    metadata_weather = "Light Rain",
    metadata_temp_deg_c = 20,
    metadata_distance_km = 2.300,
    metadata_average_speed_kmh = 39.974
  ),
  "inst/extdata/20250704-thui_dhi_me_results_q1.pdf",
  1:4,
  list(
    c(133.5767, 19.7825, 743.1937, 575.4931),
    c(132.7839, 18.9897, 742.4009, 574.7004),
    c(132.7839, 18.9897, 742.4009, 575.4931),
    c(132.7839, 18.9897, 165.2863, 576.2859)
  ),
  list(
    event_name = "La Thuile",
    event_type = "World Cup",
    event_year = "2025",
    round_type = "Qualifying Round 1",
    round_category = "Men Elite",
    metadata_weather = "Sunny",
    metadata_temp_deg_c = 22,
    metadata_distance_km = 2.300,
    metadata_average_speed_kmh = 38.743
  ),
  "inst/extdata/20250704-thui_dhi_me_results_q2.pdf",
  1:3,
  list(
    c(132.7839, 19.7825, 743.1937, 576.2859),
    c(132.7839, 18.9897, 743.9864, 577.0786),
    c(132.7839, 18.9897, 258.0368, 575.4931)
  ),
  list(
    event_name = "La Thuile",
    event_type = "World Cup",
    event_year = "2025",
    round_type = "Qualifying Round 2",
    round_category = "Men Elite",
    metadata_weather = "Sunny",
    metadata_temp_deg_c = 22,
    metadata_distance_km = 2.300,
    metadata_average_speed_kmh = 38.385
  ),
  # ---- Pal Arinsal ----
  "inst/extdata/20250711-valn_dhi_me_results_f.pdf",
  NULL,
  list(c(132.7839, 18.9897, 617.148, 575.4931)),
  list(
    event_name = "Pal Arinsal",
    event_type = "World Cup",
    event_year = "2025",
    round_type = "Final",
    round_category = "Men Elite",
    metadata_weather = "Cloudy",
    metadata_temp_deg_c = 17,
    metadata_distance_km = 1.900,
    metadata_average_speed_kmh = 44.310
  ),
  "inst/extdata/20250711-valn_dhi_me_results_q1.pdf",
  1:3,
  list(
    c(132.7839, 19.7825, 743.9864, 576.2859),
    c(132.7839, 18.9897, 744.7791, 578.6641),
    c(132.7839, 18.9897, 461.771, 577.8713)
  ),
  list(
    event_name = "Pal Arinsal",
    event_type = "World Cup",
    event_year = "2025",
    round_type = "Qualifying Round 1",
    round_category = "Men Elite",
    metadata_weather = "Cloudy",
    metadata_temp_deg_c = 16,
    metadata_distance_km = 1.900,
    metadata_average_speed_kmh = 42.677
  ),
  "inst/extdata/20250711-valn_dhi_me_results_q2.pdf",
  1:2,
  list(
    c(132.7839, 19.7825, 743.1937, 577.0786),
    c(132.7839, 18.9897, 635.381, 575.4931)
  ),
  list(
    event_name = "Pal Arinsal",
    event_type = "World Cup",
    event_year = "2025",
    round_type = "Qualifying Round 2",
    round_category = "Men Elite",
    metadata_weather = "Sunny",
    metadata_temp_deg_c = 22,
    metadata_distance_km = 1.900,
    metadata_average_speed_kmh = 42.513
  ),
  # ---- Les Gets ----
  "inst/extdata/20250829-gets_dhi_me_results_f.pdf",
  NULL,
  list(c(132.6648, 19.4335, 618.5427, 578.0811)),
  list(
    event_name = "Les Gets",
    event_type = "World Cup",
    event_year = "2025",
    round_type = "Final",
    round_category = "Men Elite",
    metadata_weather = "Cloudy",
    metadata_temp_deg_c = 13,
    metadata_distance_km = 2.380,
    metadata_average_speed_kmh = 36.215
  ),
  "inst/extdata/20250829-gets_dhi_me_results_q1.pdf",
  1:4,
  list(
    c(132.6648, 19.4335, 743.9305, 578.0811),
    c(132.6648, 19.4335, 743.9305, 576.9616),
    c(132.6648, 18.314, 745.0501, 578.0811),
    c(132.6648, 18.314, 164.0118, 575.8421)
  ),
  list(
    event_name = "Les Gets",
    event_type = "World Cup",
    event_year = "2025",
    round_type = "Qualifying Round 1",
    round_category = "Men Elite",
    metadata_weather = "Mostly Sunny",
    metadata_temp_deg_c = 18,
    metadata_distance_km = 2.380,
    metadata_average_speed_kmh = 35.975
  ),
  "inst/extdata/20250829-gets_dhi_me_results_q2.pdf",
  1:3,
  list(
    c(132.6648, 19.4335, 743.9305, 575.8421),
    c(132.6648, 19.4335, 745.0501, 578.0811),
    c(131.5453, 19.4335, 385.6796, 576.9616)
  ),
  list(
    event_name = "Les Gets",
    event_type = "World Cup",
    event_year = "2025",
    round_type = "Qualifying Round 2",
    round_category = "Men Elite",
    metadata_weather = "Cloudy",
    metadata_temp_deg_c = 18,
    metadata_distance_km = 2.380,
    metadata_average_speed_kmh = 37.350
  ),
  # ---- Lenzerheide ----
  "inst/extdata/20250918-lenz_dhi_me_results_f.pdf",
  NULL,
  list(c(132.4367, 18.0945, 618.9887, 575.5511)),
  list(
    event_name = "Lenzerheide",
    event_type = "World Cup",
    event_year = "2025",
    round_type = "Final",
    round_category = "Men Elite",
    metadata_weather = "Sunny",
    metadata_temp_deg_c = 25,
    metadata_distance_km = 1.840,
    metadata_average_speed_kmh = 40.219
  ),
  "inst/extdata/20250918-lenz_dhi_me_results_q1.pdf",
  1:3,
  list(
    c(133.2517, 19.7245, 743.6829, 574.7361),
    c(131.6217, 18.0945, 741.2379, 576.3661),
    c(132.4367, 18.9095, 416.8699, 576.3661)
  ),
  list(
    event_name = "Lenzerheide",
    event_type = "World Cup",
    event_year = "2025",
    round_type = "Qualifying Round 1",
    round_category = "Men Elite",
    metadata_weather = "Sunny",
    metadata_temp_deg_c = 22,
    metadata_distance_km = 1.840,
    metadata_average_speed_kmh = 39.510
  ),
  "inst/extdata/20250918-lenz_dhi_me_results_q2.pdf",
  1:2,
  list(
    c(132.4367, 19.7245, 744.4979, 575.5511),
    c(132.4367, 18.9095, 574.1639, 576.3661)
  ),
  list(
    event_name = "Lenzerheide",
    event_type = "World Cup",
    event_year = "2025",
    round_type = "Qualifying Round 2",
    round_category = "Men Elite",
    metadata_weather = "Sunny",
    metadata_temp_deg_c = 18,
    metadata_distance_km = 1.840,
    metadata_average_speed_kmh = 39.429
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
    round_category = "Men Elite",
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
    round_category = "Men Elite",
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
    round_category = "Men Elite",
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

world_cup_25_elite_men_results <- list_rbind(results)

usethis::use_data(world_cup_25_elite_men_results, overwrite = TRUE)
