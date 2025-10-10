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
  ),
  "inst/extdata/20250530-loud_dhi_me_results_tt.pdf",
  1:5,
  list(
    c(140.7113, 19.7825, 737.6445, 576.2859),
    c(139.9186, 18.9897, 738.4372, 576.2859),
    c(139.9186, 19.7825, 737.6445, 575.4931),
    c(140.7113, 19.7825, 736.8517, 576.2859),
    c(140.7113, 19.7825, 284.1973, 577.0786)
  ),
  list(
    event_name = "Loudenvielle",
    event_type = "World Cup",
    event_year = "2025",
    round_type = "Timed Training",
    round_category = "Men Elite"
  ),
  "inst/extdata/20250605-leog_dhi_me_results_tt.pdf",
  1:5,
  list(
    c(140.063, 19.4016, 737.9842, 576.7559),
    c(140.063, 19.4016, 739.748, 576.7559),
    c(140.063, 19.4016, 737.9842, 576.7559),
    c(140.063, 19.4016, 738.8661, 577.6378),
    c(140.063, 19.4016, 657.7323, 574.9921)
  ),
  list(
    event_name = "Leogang",
    event_type = "World Cup",
    event_year = "2025",
    round_type = "Timed Training",
    round_category = "Men Elite"
  ),
  "inst/extdata/20250619-vdso_dhi_me_results_tt.pdf",
  1:5,
  list(
    c(140.7113, 20.1788, 738.4372, 575.8895),
    c(139.9186, 19.3861, 738.4372, 574.304),
    c(140.7113, 19.3861, 740.0227, 576.6822),
    c(139.9186, 19.3861, 738.4372, 575.8895),
    c(139.9186, 19.3861, 284.99, 575.0968)
  ),
  list(
    event_name = "Val di Sole",
    event_type = "World Cup",
    event_year = "2025",
    round_type = "Timed Training",
    round_category = "Men Elite"
  ),
  "inst/extdata/20250704-thui_dhi_me_results_tt.pdf",
  1:5,
  list(
    c(140.7113, 19.7825, 737.6445, 576.2859),
    c(139.9186, 19.7825, 736.8517, 574.7004),
    c(140.7113, 19.7825, 738.4372, 576.2859),
    c(139.9186, 18.9897, 737.6445, 575.4931),
    c(140.7113, 19.7825, 738.4372, 576.2859)
  ),
  list(
    event_name = "La Thuile",
    event_type = "World Cup",
    event_year = "2025",
    round_type = "Timed Training",
    round_category = "Men Elite"
  ),
  "inst/extdata/20250711-valn_dhi_me_results_tt.pdf",
  1:5,
  list(
    c(139.9186, 18.9897, 737.6445, 576.2859),
    c(139.9186, 18.9897, 738.4372, 577.8713),
    c(139.9186, 18.9897, 739.23, 577.0786),
    c(139.9186, 18.9897, 739.23, 576.2859),
    c(139.9186, 18.9897, 232.6692, 578.6641)
  ),
  list(
    event_name = "Pal Arinsal",
    event_type = "World Cup",
    event_year = "2025",
    round_type = "Timed Training",
    round_category = "Men Elite"
  ),
  "inst/extdata/20250829-gets_dhi_me_results_tt.pdf",
  1:5,
  list(
    c(140.315, 19.919, 740.0013, 576.5211),
    c(139.1505, 18.7545, 740.0013, 576.5211),
    c(139.1505, 18.7545, 741.1658, 577.6855),
    c(140.315, 19.919, 740.0013, 577.6855),
    c(139.1505, 18.7545, 632.8729, 576.5211)
  ),
  list(
    event_name = "Les Gets",
    event_type = "World Cup",
    event_year = "2025",
    round_type = "Timed Training",
    round_category = "Men Elite"
  ),
  "inst/extdata/20250918-lenz_dhi_me_results_tt.pdf",
  1:5,
  list(
    c(140.5866, 18.9095, 737.9779, 575.5511),
    c(139.7716, 18.9095, 738.7929, 577.1811),
    c(139.7716, 18.9095, 739.6079, 575.5511),
    c(138.9566, 18.0945, 739.6079, 576.3661),
    c(139.7716, 18.9095, 180.5214, 575.5511)
  ),
  list(
    event_name = "Lenzerheide",
    event_type = "World Cup",
    event_year = "2025",
    round_type = "Timed Training",
    round_category = "Men Elite"
  ),
  NULL,
  NULL,
  list(),
  list(
    event_name = "Mont-Sainte-Anne",
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
