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
  ~path,
  ~pages,
  ~areas,
  ~metadata,
  "inst/extdata/20240503-fwil_dhi_me_results_tt.pdf",
  1:7,
  list(
    c(140.7113, 20.1788, 738.4372, 576.6822),
    c(140.7113, 19.3861, 736.8517, 575.8895),
    c(140.7113, 20.1788, 738.4372, 575.8895),
    c(140.7113, 20.1788, 736.8517, 576.6822),
    c(140.7113, 20.1788, 737.6445, 576.6822),
    c(140.7113, 20.1788, 739.23, 575.8895),
    c(140.7113, 19.3861, 364.264, 575.8895)
  ),
  list(
    event_name = "Fort William",
    event_type = "World Cup",
    event_year = "2024",
    round_type = "Timed Training",
    round_category = "Men Elite"
  ),
  "inst/extdata/20240518-biel_dhi_me_results_tt.pdf",
  1:7,
  list(
    c(139.9186, 20.5752, 739.23, 575.4931),
    c(140.7113, 19.7825, 738.4372, 576.2859),
    c(140.7113, 19.7825, 738.4372, 576.2859),
    c(140.7113, 19.7825, 737.6445, 576.2859),
    c(140.7113, 19.7825, 738.4372, 576.2859),
    c(140.7113, 19.7825, 739.23, 575.4931),
    c(140.7113, 19.7825, 362.6785, 575.4931)
  ),
  list(
    event_name = "Bielsko-Biala",
    event_type = "World Cup",
    event_year = "2024",
    round_type = "Timed Training",
    round_category = "Men Elite"
  ),
  "inst/extdata/20240607-leog_dhi_me_results_tt.pdf",
  1:9,
  list(
    c(141.5041, 19.7825, 739.23, 577.0786),
    c(140.7113, 19.7825, 738.4372, 575.4931),
    c(140.7113, 19.7825, 737.6445, 577.0786),
    c(140.7113, 20.5752, 739.23, 577.8713),
    c(139.9186, 20.5752, 738.4372, 576.2859),
    c(140.7113, 19.7825, 739.23, 576.2859),
    c(140.7113, 19.7825, 738.4372, 575.4931),
    c(139.9186, 19.7825, 738.4372, 576.2859),
    c(140.7113, 19.7825, 203.3378, 576.2859)
  ),
  list(
    event_name = "Leogang",
    event_type = "World Cup",
    event_year = "2024",
    round_type = "Timed Training",
    round_category = "Men Elite"
  ),
  "inst/extdata/20240614-vdso_dhi_me_results_tt.pdf",
  1:8,
  list(
    c(140.7113, 19.7825, 737.6445, 575.4931),
    c(140.7113, 19.7825, 738.4372, 576.2859),
    c(140.7113, 19.7825, 737.6445, 575.4931),
    c(140.7113, 19.7825, 738.4372, 577.0786),
    c(140.7113, 19.7825, 736.059, 576.2859),
    c(139.9186, 19.7825, 738.4372, 574.7004),
    c(140.7113, 19.7825, 737.6445, 575.4931),
    c(140.7113, 19.7825, 232.6692, 576.2859)
  ),
  list(
    event_name = "Val di Sole",
    event_type = "World Cup",
    event_year = "2024",
    round_type = "Timed Training",
    round_category = "Men Elite"
  ),
  "inst/extdata/20240704-gets_dhi_me_results_tt.pdf",
  1:8,
  list(
    c(140.7113, 19.7825, 736.8517, 576.2859),
    c(140.7113, 19.7825, 738.4372, 575.4931),
    c(139.9186, 18.9897, 738.4372, 576.2859),
    c(139.9186, 19.7825, 737.6445, 576.2859),
    c(140.7113, 19.7825, 738.4372, 576.2859),
    c(139.9186, 19.7825, 737.6445, 577.0786),
    c(140.7113, 19.7825, 736.8517, 577.0786),
    c(139.9186, 19.7825, 337.3108, 575.4931)
  ),
  list(
    event_name = "Les Gets",
    event_type = "World Cup",
    event_year = "2024",
    round_type = "Timed Training",
    round_category = "Men Elite"
  ),
  "inst/extdata/20241004-mtsa_dhi_me_results_tt.pdf",
  1:5,
  list(
    c(140.7113, 19.7825, 739.23, 577.0786),
    c(140.7113, 19.7825, 737.6445, 576.2859),
    c(140.7113, 19.7825, 737.6445, 577.0786),
    c(140.7113, 19.7825, 738.4372, 576.2859),
    c(140.7113, 19.7825, 738.4372, 577.0786)
  ),
  list(
    event_name = "Mont-Sainte-Anne",
    event_type = "World Cup",
    event_year = "2024",
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
      clean_timed_training_24() |>
      mutate(!!!metadata)
  }
)


world_cup_24_elite_men_timed_training <- list_rbind(timed_training)

usethis::use_data(world_cup_24_elite_men_timed_training, overwrite = TRUE)
