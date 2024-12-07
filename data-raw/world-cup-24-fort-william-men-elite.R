# ---- TODO ----
# - Document in data.R
# - Add weather, temperature, and distance metrics to each table
# - Add event name (e.g., fort william), event type (e.g., world cup),
#   event year (e.g., 2024), round type (e.g., finals), category (e.g., men elite)
#   to each table.
# - Should the meta data above live in separate tables joined by a primary key?

# ---- Source ----
# https://prod.chronorace.be/angular/results.html#/uci/event/20240503_mtb/DHI/CG1

# ---- Libraries ----
library(tidyverse)
library(tabulapdf)
library(janitor)
pkgload::load_all()

# ---- Final ----
# Set area grid with: locate_areas(file = "inst/extdata/world-cup-24-fort-william-men-elite-final.pdf")
finals_raw <- extract_tables(
  file = "inst/extdata/world-cup-24-fort-william-men-elite-final.pdf",
  area = list(c(135.16215, 20.5751, 634.58829, 575.49312)),
  guess = FALSE,
  method = "stream",
  output = "tibble"
)

finals <- finals_raw |>
  bind_tables() |>
  clean_results_table()

# ---- Semi-Final ----
# Set area grids with: locate_areas(file = "inst/extdata/world-cup-24-fort-william-men-elite-semi-final.pdf", pages = 1:2)
semi_finals_raw <- extract_tables(
  file = "inst/extdata/world-cup-24-fort-william-men-elite-semi-final.pdf",
  pages = 1:2,
  area = list(
    c(134.36941, 20.57519, 743.19365, 575.49312),
    c(134.36941, 20.57519, 539.45950, 576.28586)
  ),
  guess = FALSE,
  method = "stream",
  output = "tibble"
)

semi_finals <- semi_finals_raw |>
  bind_tables() |>
  clean_results_table()

# ---- Qualification ----
# Set area grids with: locate_areas(file = "inst/extdata/world-cup-24-fort-william-men-elite-qualifying.pdf", pages = 1:4)
qualifying_raw <- extract_tables(
  file = "inst/extdata/world-cup-24-fort-william-men-elite-qualifying.pdf",
  pages = 1:4,
  area = list(
    c(133.57667, 20.57519, 743.19365, 575.49312),
    c(133.57667, 20.57519, 743.19365, 575.49312),
    c(133.57667, 19.78245, 743.19365, 575.49312),
    c(133.57667, 20.57519, 603.67143, 576.28586)
  ),
  guess = FALSE,
  method = "stream",
  output = "tibble"
)

qualifying <- qualifying_raw |>
  bind_tables() |>
  clean_results_table()

# ---- Time training ----
# Set area grids with: locate_areas(file = "inst/extdata/world-cup-24-fort-william-men-elite-timed-training.pdf", pages = 1:7)
timed_training_raw <- extract_tables(
  file = "inst/extdata/world-cup-24-fort-william-men-elite-timed-training.pdf",
  pages = 1:7,
  area = list(
    c(140.71133, 19.78245, 736.85173, 576.28586),
    c(140.71133, 19.78245, 737.64447, 576.28586),
    c(140.71133, 20.57519, 737.64447, 575.49312),
    c(140.71133, 19.78245, 737.64447, 576.28586),
    c(140.71133, 19.78245, 738.43721, 576.28586),
    c(139.91859, 19.78245, 737.64447, 576.28586),
    c(140.71133, 19.78245, 364.26398, 576.28586)
  ),
  guess = FALSE,
  method = "stream",
  output = "tibble"
)

time_training <- timed_training_raw |>
  bind_tables() |>
  clean_timed_training_table()
