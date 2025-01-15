# ------------------------------------------------------------------------------
# Load
# ------------------------------------------------------------------------------
pkgload::load_all()
library(tidyverse)
library(fs)
library(ggbump)
library(ggimage)
library(ggtext)
library(gganimate)
library(glue)

# ------------------------------------------------------------------------------
# Prepare data sets
# ------------------------------------------------------------------------------
world_cup_24_elite_men_results <- world_cup_24_elite_men_results |>
  mutate(name = str_to_title(name)) |>
  mutate(
    name = map_chr(str_split(name, " "), ~ str_c(rev(.x), collapse = " "))
  )

image_data <- tibble(path = dir_ls("inst/rider-images")) |>
  mutate(name = str_remove(path, "^inst/rider-images/")) |>
  mutate(name = str_remove(name, ".png$")) |>
  mutate(name = str_replace(name, "(?<=[a-z])(?=[A-Z])", " "))

# ------------------------------------------------------------------------------
# Calculate actual vs Possible times
# ------------------------------------------------------------------------------
fastest_acutal_times <- world_cup_24_elite_men_results |>
  summarise(
    fastest_actual_time = min(time, na.rm = TRUE),
    .by = c(name, event_name)
  )

fastest_possible_sections <- world_cup_24_elite_men_results |>
  select(name, starts_with("split"), time, event_name, round_type) |>
  mutate(
    section_1 = split_1,
    section_2 = split_2 - split_1,
    section_3 = split_3 - split_2,
    section_4 = split_4 - split_3,
    section_5 = time - split_4
  ) |>
  summarise(
    across(starts_with("section_"), ~ min(.x, na.rm = TRUE)),
    .by = c(name, event_name)
  )

fastest_possible_times <- fastest_possible_sections |>
  rowwise(name, event_name) |>
  summarise(
    fastest_possible_time = sum(
      c_across(starts_with("section_")),
      na.rm = TRUE
    ),
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# Simulate season
# ------------------------------------------------------------------------------
simulated_top_30_each_race <- fastest_possible_times |>
  group_by(event_name) |>
  arrange(fastest_possible_time, .by_group = TRUE) |>
  slice_head(n = 30) |>
  mutate(position = row_number()) |>
  ungroup()

# Points are only given to top 30 for each race run
finals_points <- world_cup_24_elite_men_points_scale |>
  filter(round_type == "Final") |>
  select(-round_type)

simulated_season <- simulated_top_30_each_race |>
  left_join(finals_points) |>
  rename(simulated_rank = position, simulated_points = points)

simulated_overall <- simulated_season |>
  summarise(simulated_points = sum(simulated_points), .by = name) |>
  arrange(desc(simulated_points)) |>
  mutate(simulated_rank = row_number())

actual_season <- world_cup_24_elite_men_results |>
  filter(round_type == "Final") |>
  select(name, event_name, time, actual_rank = rank, actual_points = points)

actual_overall <- world_cup_24_elite_men_results |>
  summarise(actual_points = sum(points, na.rm = TRUE), .by = name) |>
  arrange(desc(actual_points)) |>
  mutate(actual_rank = row_number())

# ------------------------------------------------------------------------------
# Bump plot
# ------------------------------------------------------------------------------
# Show bump plot here to start the narrative about the overall season

# ------------------------------------------------------------------------------
# Delta scores per season and overall
# ------------------------------------------------------------------------------
# E.g., a delta of -6 means they ranked 6 places below their potential. A delta
# of +2 means they ranked 2 places higher than their raw speed alone would
# dictate.
delta_overall <-
  simulated_overall |>
  left_join(actual_overall) |>
  mutate(delta = simulated_rank - actual_rank)

delta_season <-
  simulated_season |>
  left_join(actual_season) |>
  mutate(delta = simulated_rank - actual_rank)

delta_overall_subset <- delta_overall |>
  mutate(event_name = "Overall") |>
  select(name, event_name, simulated_rank, actual_rank, delta)

delta_season_subset <- delta_season |>
  select(name, event_name, simulated_rank, actual_rank, delta)

delta_all_wide <- bind_rows(delta_overall_subset, delta_season_subset) |>
  select(-ends_with("_rank")) |>
  pivot_wider(names_from = event_name, values_from = delta) |>
  relocate(Overall, .after = "Val di Sole")

# TODO:
# - Add rider images
# - Add colour palette
# - Replace NA with "-" or Blank values or note
# - Add title and subtitle/description describing what it does/how to read
# - Remove "name" column header, it isn't needed
# - Add formatting/styling
# - Should this whole table just be a big bump chart?

# - Insights:
# - Dak and Finn had bad several bad performances.
# - This table can be confusing, because it shows deltas, NOT ranks. It also has
#   lots of missing data. Perhaps we need to just cherry pick several races and
#   make several simple tables/bump charts to tell a story?
delta_all_wide |>
  gt::gt()

# ------------------------------------------------------------------------------
# Heat maps
# ------------------------------------------------------------------------------
# Next, draw heat maps using red/green across split times to show how races
# would have unfolded in the simulated season. Draw attention to where this is
# significantly different from the actual season.

# ------------------------------------------------------------------------------
# gganimate races
# ------------------------------------------------------------------------------
# Supplement heat maps with gganimate races to create drama of the simulated
# season.
