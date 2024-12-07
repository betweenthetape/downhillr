# ---- TODO ----
# - Document in data.R

# ---- Source ----
# https://prod.chronorace.be/angular/results.html#/uci/event/20240503_mtb/DHI/CG1

# ---- Libraries ----
library(tidyverse)
library(tabulapdf)
library(janitor)
pkgload::load_all()

# ---- Finals ----
# Set area grid with: locate_areas(file = "inst/extdata/world-cup-24-fort-william-men-elite-final.pdf")
finals_raw <- extract_tables(
  file = "inst/extdata/world-cup-24-fort-william-men-elite-final.pdf",
  area = list(c(135.16215, 20.5751, 634.58829, 575.49312)),
  guess = FALSE,
  method = "stream",
  output = "tibble"
)

finals_all_rows <-
  finals_raw |>
  pluck(1) |>
  select(-`...2`) |>
  fill(`UCI ID`)

finals_odd_rows <- finals_all_rows |>
  filter(row_number() %% 2 == 1)

finals_even_rows <- finals_all_rows |>
  filter(row_number() %% 2 == 0)

finals_odd_rows_cleaned <- finals_odd_rows |>
  separate_wider_regex(
    cols = `Nr Name / UCI MTB Team`,
    patterns = c(Nr = "^\\d+", `Name / UCI MTB Team` = "\\D+")
  ) |>
  clean_names() |>
  rename(
    name = name_uci_mtb_team,
    split_1 = i1_i2,
    split_3 = i3_i4
  ) |>
  mutate(name = str_remove(name, "\\*$")) |>
  mutate(name = str_squish(name))

finals_even_rows_cleaned <- finals_even_rows |>
  select(
    uci_team = `Nr Name / UCI MTB Team`,
    uci_id = `UCI ID`,
    split_2 = `I1 / I2`,
    split_4 = `I3 / I4`,
    time_from_leader = Time
  )

world_cup_24_fort_william_men_elite_final <- finals_odd_rows_cleaned |>
  left_join(finals_even_rows_cleaned) |>
  mutate(across(everything(), ~ if_else(.x == "-", NA, .x))) |>
  mutate(across(starts_with("sp"), ~ str_remove(.x, "\\s*\\(\\d+\\)$"))) |>
  mutate(time_from_leader = str_remove(time_from_leader, "^\\+")) |>
  mutate(
    across(
      c(starts_with("split_"), starts_with("time")),
      ~ convert_to_seconds(.x)
    )
  ) |>
  mutate(speed = as.numeric(speed)) |>
  select(
    rank, points, nr, name, nat, yob, uci_team, uci_id, speed, split_1, split_2,
    split_3, split_4, time, time_from_leader
  )

usethis::use_data(world_cup_24_fort_william_men_elite_final, overwrite = TRUE)
