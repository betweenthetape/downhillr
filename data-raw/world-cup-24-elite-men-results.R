# ------------------------------------------------------------------------------
# README:
# To run this script, you first need to run `inst/scripts/scrape-chronorace.R`
# to download all the results PDF's locally.
# ------------------------------------------------------------------------------

# ---- Load ----
library(tidyverse)
library(tabulapdf)
pkgload::load_all()

# ---- Set PDF Areas ----
# Run `pdf_areas(<INSERT PATH>)` on each path to determine PDF table areas.
# Run `files <- fs::dir_ls("inst/extdata")[str_detect(fs::dir_ls("inst/extdata"), "_me_")]`
# Then `pdf_areas(files[1])` and iterate over the numbers to save copy and pasting
areas <- tribble(
  ~path, ~areas,
  "inst/extdata/20240503-fwil_dhi_me_results_f.pdf", list(
    c(133.5767, 20.1788, 634.5883, 575.0968)
  ),
  "inst/extdata/20240503-fwil_dhi_me_results_qr.pdf", list(
    c(134.3694, 19.3861, 742.4009, 575.0968),
    c(133.5767, 20.1788, 42.4009, 575.8895),
    c(133.5767, 19.3861, 743.1937, 575.8895),
    c(133.5767, 20.1788, 602.086, 574.304)
  )
  # "inst/extdata/20240503-fwil_dhi_me_results_semi.pdf",
  # "inst/extdata/20240503-fwil_dhi_me_results_tt.pdf",
  # "inst/extdata/20240607-leog_dhi_me_results_f.pdf",
  # "inst/extdata/20240607-leog_dhi_me_results_qr.pdf",
  # "inst/extdata/20240607-leog_dhi_me_results_semi.pdf",
  # "inst/extdata/20240607-leog_dhi_me_results_tt.pdf",
  # "inst/extdata/20240614-vdso_dhi_me_results_f.pdf",
  # "inst/extdata/20240614-vdso_dhi_me_results_qr.pdf",
  # "inst/extdata/20240614-vdso_dhi_me_results_semi.pdf",
  # "inst/extdata/20240614-vdso_dhi_me_results_tt.pdf",
  # "inst/extdata/20240704-gets_dhi_me_results_f.pdf",
  # "inst/extdata/20240704-gets_dhi_me_results_qr.pdf",
  # "inst/extdata/20240704-gets_dhi_me_results_semi.pdf",
  # "inst/extdata/20240704-gets_dhi_me_results_tt.pdf",
  # "inst/extdata/20240906-loud_dhi_me_results_f.pdf",
  # "inst/extdata/20240906-loud_dhi_me_results_qr.pdf",
  # "inst/extdata/20241004-mtsa_dhi_me_results_f.pdf",
  # "inst/extdata/20241004-mtsa_dhi_me_results_qr.pdf",
  # "inst/extdata/20241004-mtsa_dhi_me_results_semi.pdf",
  # "inst/extdata/20241004-mtsa_dhi_me_results_tt.pdf"
)

# ------------------------------------------------------------------------------
# TODO:
# - Iterate over paths and areas to extract, clean, combine, and save results
# - Create one data set per category (e.g., elite men, elite women, etc.), and
#   split the data sets by "timed training" and "results". All races for the
#   whole season for one category should exist in a single dataframe.
# - Delete old "world-cup-24-fort-william-elites.R" file and .rda files.
# - Updae data.R with new changes and data sets.
# ------------------------------------------------------------------------------
