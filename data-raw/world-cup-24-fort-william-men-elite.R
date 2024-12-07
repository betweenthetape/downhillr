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

finals <- clean_results_table(finals_raw[[1]])

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

semi_finals_tables_combined <- semi_finals_raw |>
  pluck(1) |>
  mutate(Points = as.character(Points)) |>
  bind_rows(semi_finals_raw[[2]])

semi_finals <- clean_results_table(semi_finals_tables_combined)

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

qualifying <- list_rbind(qualifying_raw) |>
  clean_results_table()
