# ------------------------------------------------------------------------------
# README:
# To run this script, you first need to run `inst/scripts/scrape-chronorace.R`
# to download all the results PDF's locally.
# ------------------------------------------------------------------------------

# ---- Load ----
pkgload::load_all()
library(tidyverse)
library(tabulapdf)

# ---- Create spec ----
# Run `pdf_areas(<INSERT PATH>)` on each path to determine PDF table areas.
spec <- tribble(
  ~path, ~areas, ~metadata,
  "inst/extdata/20240503-fwil_dhi_me_results_f.pdf",
  list(
    c(133.5767, 20.1788, 634.5883, 575.0968)
  ),
  list(
    event_name = "Fort William",
    event_type = "World Cup",
    event_year = "2024",
    round_type = "Final",
    round_category = "Men Elite",
    metadata_weather = "Light rain",
    metadata_temp_deg_c = 19,
    metadata_distance_km = 3,
    metadata_average_speed_kmh = 44.214
  ),
  "inst/extdata/20240503-fwil_dhi_me_results_qr.pdf",
  list(
    c(133.57667, 20.57519, 743.19365, 575.49312),
    c(133.57667, 20.57519, 743.19365, 575.49312),
    c(133.57667, 19.78245, 743.19365, 575.49312),
    c(133.57667, 20.57519, 603.67143, 576.28586)
  ),
  list(
    event_name = "Fort William",
    event_type = "World Cup",
    event_year = "2024",
    round_type = "Qualifying",
    round_category = "Men Elite",
    metadata_weather = "Cloudy",
    metadata_temp_deg_c = 21,
    metadata_distance_km = 3,
    metadata_average_speed_kmh = 43.490
  ),
  "inst/extdata/20240503-fwil_dhi_me_results_semi.pdf",
  list(
    c(133.5767, 20.1788, 742.4009, 575.0968),
    c(133.5767, 20.9716, 541.045, 575.8895)
  ),
  list(
    event_name = "Fort William",
    event_type = "World Cup",
    event_year = "2024",
    round_type = "Semi-Final",
    round_category = "Men Elite",
    metadata_weather = "Mostly sunny",
    metadata_temp_deg_c = 21,
    metadata_distance_km = 3,
    metadata_average_speed_kmh = 42.368
  ),
  "inst/extdata/20240518-biel_dhi_me_results_f.pdf",
  list(
    c(133.5767, 19.7825, 679.7745, 575.4931)
  ),
  list(
    event_name = "Bielsko-Biala",
    event_type = "World Cup",
    event_year = "2024",
    round_type = "Final",
    round_category = "Men Elite",
    metadata_weather = "Light Rain",
    metadata_temp_deg_c = 19,
    metadata_distance_km = 2.010,
    metadata_average_speed_kmh = 41.168
  ),
  "inst/extdata/20240518-biel_dhi_me_results_qr.pdf",
  list(
    c(133.5767, 19.7825, 741.6082, 575.4931),
    c(133.5767, 19.7825, 741.6082, 575.4931),
    c(133.5767, 19.7825, 743.9864, 577.0786),
    c(133.5767, 19.7825, 743.1937, 575.4931),
    c(133.5767, 19.7825, 319.8705, 575.4931)
  ),
  list(
    event_name = "Bielsko-Biala",
    event_type = "World Cup",
    event_year = "2024",
    round_type = "Qualifying",
    round_category = "Men Elite",
    metadata_weather = "Mostly Sunny",
    metadata_temp_deg_c = 21,
    metadata_distance_km = 2.0190,
    metadata_average_speed_kmh = 39.913
  ),
  "inst/extdata/20240518-biel_dhi_me_results_semi.pdf",
  list(
    c(133.5767, 19.7825, 743.9864, 575.4931),
    c(132.7839, 19.7825, 492.6878, 575.4931)
  ),
  list(
    event_name = "Bielsko-Biala",
    event_type = "World Cup",
    event_year = "2024",
    round_type = "Semi-Final",
    round_category = "Men Elite",
    metadata_weather = "Mostly Sunny",
    metadata_temp_deg_c = 21,
    metadata_distance_km = 2.010,
    metadata_average_speed_kmh = 40.230
  ),
  "inst/extdata/20240607-leog_dhi_me_results_f.pdf",
  list(
    c(133.5767, 19.7825, 648.8576, 576.2859)
  ),
  list(
    event_name = "Leogang",
    event_type = "World Cup",
    event_year = "2024",
    round_type = "Final",
    round_category = "Men Elite",
    metadata_weather = "Light rain",
    metadata_temp_deg_c = 19,
    metadata_distance_km = 2.115,
    metadata_average_speed_kmh = 40.041
  ),
  "inst/extdata/20240607-leog_dhi_me_results_qr.pdf",
  list(
    c(133.5767, 19.7825, 743.1937, 575.4931),
    c(133.5767, 20.5752, 743.1937, 576.2859),
    c(133.5767, 20.5752, 743.1937, 575.4931),
    c(133.5767, 18.9897, 743.1937, 576.2859),
    c(133.5767, 19.7825, 680.5672, 576.2859),
    c(132.7839, 18.9897, 167.6645, 581.0423)
  ),
  list(
    event_name = "Leogang",
    event_type = "World Cup",
    event_year = "2024",
    round_type = "Qualifying",
    round_category = "Men Elite",
    metadata_weather = "Mostly Sunny",
    metadata_temp_deg_c = 27,
    metadata_distance_km = 2.115,
    metadata_average_speed_kmh = 40.214
  ),
  "inst/extdata/20240607-leog_dhi_me_results_semi.pdf",
  list(
    c(132.7839, 19.7825, 743.1937, 576.2859),
    c(133.5767, 19.7825, 539.4595, 576.2859)
  ),
  list(
    event_name = "Leogang",
    event_type = "World Cup",
    event_year = "2024",
    round_type = "Semi-Final",
    round_category = "Men Elite",
    metadata_weather = "Mostly Sunny",
    metadata_temp_deg_c = 26,
    metadata_distance_km = 2.115,
    metadata_average_speed_kmh = 41.124
  ),
  "inst/extdata/20240614-vdso_dhi_me_results_f.pdf",
  list(
    c(133.5767, 19.7825, 648.8576, 574.7004)
  ),
  list(
    event_name = "Val di Sole",
    event_type = "World Cup",
    event_year = "2024",
    round_type = "Final",
    round_category = "Men Elite",
    metadata_weather = "Cloudy",
    metadata_temp_deg_c = 13,
    metadata_distance_km = 2.261,
    metadata_average_speed_kmh = 37.166
  ),
  "inst/extdata/20240614-vdso_dhi_me_results_qr.pdf",
  list(
    c(133.5767, 19.7825, 743.1937, 574.7004),
    c(132.7839, 19.7825, 743.1937, 576.2859),
    c(133.5767, 19.7825, 742.4009, 574.7004),
    c(133.5767, 19.7825, 743.1937, 576.2859),
    c(132.7839, 19.7825, 414.2066, 577.0786)
  ),
  list(
    event_name = "Val di Sole",
    event_type = "World Cup",
    event_year = "2024",
    round_type = "Qualifying",
    round_category = "Men Elite",
    metadata_weather = "Cloudy",
    metadata_temp_deg_c = 20,
    metadata_distance_km = 2.261,
    metadata_average_speed_kmh = 35.479
  ),
  "inst/extdata/20240614-vdso_dhi_me_results_semi.pdf",
  list(
    c(133.5767, 19.7825, 743.1937, 577.0786),
    c(133.5767, 19.7825, 491.8951, 576.2859)
  ),
  list(
    event_name = "Val di Sole",
    event_type = "World Cup",
    event_year = "2024",
    round_type = "Semi-Final",
    round_category = "Men Elite",
    metadata_weather = "Mostly Sunny",
    metadata_temp_deg_c = 26,
    metadata_distance_km = 2.261,
    metadata_average_speed_kmh = 36.287
  ),
  "inst/extdata/20240704-gets_dhi_me_results_f.pdf",
  list(
    c(133.5767, 20.5752, 634.5883, 576.2859)
  ),
  list(
    event_name = "Les Gets",
    event_type = "World Cup",
    event_year = "2024",
    round_type = "Final",
    round_category = "Men Elite",
    metadata_weather = "Rain",
    metadata_temp_deg_c = 15,
    metadata_distance_km = 2.380,
    metadata_average_speed_kmh = 38.254
  ),
  "inst/extdata/20240704-gets_dhi_me_results_qr.pdf",
  list(
    c(133.5767, 19.7825, 743.1937, 575.4931),
    c(133.5767, 20.5752, 741.6082, 575.4931),
    c(132.7839, 19.7825, 743.1937, 577.8713),
    c(133.5767, 20.5752, 743.1937, 577.0786),
    c(133.5767, 19.7825, 476.833, 575.4931)
  ),
  list(
    event_name = "Les Gets",
    event_type = "World Cup",
    event_year = "2024",
    round_type = "Qualifying",
    round_category = "Men Elite",
    metadata_weather = "Sunny",
    metadata_temp_deg_c = 22,
    metadata_distance_km = 2.380,
    metadata_average_speed_kmh = 41.347
  ),
  "inst/extdata/20240704-gets_dhi_me_results_semi.pdf",
  list(
    c(133.5767, 20.5752, 741.6082, 574.7004),
    c(133.5767, 19.7825, 509.3354, 576.2859)
  ),
  list(
    event_name = "Les Gets",
    event_type = "World Cup",
    event_year = "2024",
    round_type = "Semi-Final",
    round_category = "Men Elite",
    metadata_weather = "Sunny",
    metadata_temp_deg_c = 20,
    metadata_distance_km = 2.380,
    metadata_average_speed_kmh = 41.934
  ),
  "inst/extdata/20240906-loud_dhi_me_results_f.pdf",
  list(
    c(133.5767, 19.7825, 619.5262, 575.4931)
  ),
  list(
    event_name = "Loudenvielle",
    event_type = "World Cup",
    event_year = "2024",
    round_type = "Final",
    round_category = "Men Elite",
    metadata_weather = "Rain",
    metadata_temp_deg_c = 14,
    metadata_distance_km = 2.490,
    metadata_average_speed_kmh = 40.657
  ),
  "inst/extdata/20240906-loud_dhi_me_results_qr.pdf",
  list(
    c(133.5767, 19.7825, 743.9864, 575.4931),
    c(133.5767, 19.7825, 743.9864, 575.4931),
    c(133.5767, 19.7825, 743.1937, 576.2859),
    c(133.5767, 18.9897, 602.086, 577.0786)
  ),
  list(
    event_name = "Loudenvielle",
    event_type = "World Cup",
    event_year = "2024",
    round_type = "Qualifying",
    round_category = "Men Elite",
    metadata_weather = "Sunny",
    metadata_temp_deg_c = 20,
    metadata_distance_km = 2.490,
    metadata_average_speed_kmh = 41.660
  ),
  "inst/extdata/20241004-mtsa_dhi_me_results_f.pdf",
  list(
    c(133.5767, 19.7825, 617.9407, 575.4931)
  ),
  list(
    event_name = "Mont-Sainte-Anne",
    event_type = "World Cup",
    event_year = "2024",
    round_type = "Final",
    round_category = "Men Elite",
    metadata_weather = "Light Rain",
    metadata_temp_deg_c = 16,
    metadata_distance_km = 2.500,
    metadata_average_speed_kmh = 38.461
  ),
  "inst/extdata/20241004-mtsa_dhi_me_results_qr.pdf",
  list(
    c(133.5767, 19.7825, 743.9864, 576.2859),
    c(133.5767, 19.7825, 743.1937, 575.4931),
    c(132.7839, 19.7825, 743.9864, 575.4931),
    c(133.5767, 19.7825, 198.5813, 575.4931)
  ),
  list(
    event_name = "Mont-Sainte-Anne",
    event_type = "World Cup",
    event_year = "2024",
    round_type = "Qualifying",
    round_category = "Men Elite",
    metadata_weather = "Sunny",
    metadata_temp_deg_c = 20,
    metadata_distance_km = 2.500,
    metadata_average_speed_kmh = 40.211
  ),
  "inst/extdata/20241004-mtsa_dhi_me_results_semi.pdf",
  list(
    c(133.5767, 19.7825, 743.1937, 576.2859),
    c(133.5767, 19.7825, 602.086, 576.2859)
  ),
  list(
    event_name = "Mont-Sainte-Anne",
    event_type = "World Cup",
    event_year = "2024",
    round_type = "Semi-Final",
    round_category = "Men Elite",
    metadata_weather = "Sunny",
    metadata_temp_deg_c = 20,
    metadata_distance_km = 2.500,
    metadata_average_speed_kmh = 40.505
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

world_cup_24_elite_men_results <- list_rbind(results)

usethis::use_data(world_cup_24_elite_men_results, overwrite = TRUE)
