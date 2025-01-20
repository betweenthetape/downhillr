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
library(gt)

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
  pivot_wider(
    names_from = event_name, values_from = c(simulated_rank, actual_rank, delta)
  ) |>
  left_join(image_data)

# TODO:
# - Add colour palette
# - Add title and subtitle/description describing what it does/how to read
# - Remove "name" column header, it isn't needed
# - Add formatting/styling
# - Rather than display just numbers and a difference column, can we compute
#   many tiny bump charts, per rider and per race, an embed these in with
#   `ggplot_image` or `gtExtras::gt_plt_sparkline()`. These could be annotated
#   to show results, and we could bold/colour the plots we want to draw the eye
#   too differently.

# - Insights:
# - Dak and Finn had bad several bad performances.
# - Fort William would have made virtually no difference to any rider. This is
#   quite interesting, and is perhaps the race that best reflects pure speed.
# - This table can be confusing, because it shows deltas, NOT ranks. It also has
#   lots of missing data. Perhaps we need to just cherry pick several races and
#   make several simple tables/bump charts to tell a story?
delta_all_wide |>
  dplyr::select(
    path, name,
    "simulated_rank_Overall", "actual_rank_Overall", "delta_Overall",
    "simulated_rank_Fort William", "actual_rank_Fort William", "delta_Fort William",
    "simulated_rank_Bielsko-Biala", "actual_rank_Bielsko-Biala", "delta_Bielsko-Biala",
    "simulated_rank_Leogang", "actual_rank_Leogang", "delta_Leogang",
    "simulated_rank_Val di Sole", "actual_rank_Val di Sole", "delta_Val di Sole",
    "simulated_rank_Les Gets", "actual_rank_Les Gets", "delta_Les Gets",
    "simulated_rank_Loudenvielle", "actual_rank_Loudenvielle", "delta_Loudenvielle",
    "simulated_rank_Mont-Sainte-Anne", "actual_rank_Mont-Sainte-Anne", "delta_Mont-Sainte-Anne"
  ) |>
  filter(simulated_rank_Overall <= 10) |>
  gt() |>
  cols_label("path" = "", "name" = "") |>
  tab_spanner(
    "Overall",
    c("actual_rank_Overall", "simulated_rank_Overall", "delta_Overall")
  ) |>
  cols_label(
    "actual_rank_Overall" = "Actual",
    "simulated_rank_Overall" = "Simulated",
    "delta_Overall" = "Difference"
  ) |>
  tab_spanner(
    "Fort William",
    c("actual_rank_Fort William", "simulated_rank_Fort William", "delta_Fort William")
  ) |>
  cols_label(
    "actual_rank_Fort William" = "Actual",
    "simulated_rank_Fort William" = "Simulated",
    "delta_Fort William" = "Difference"
  ) |>
  tab_spanner(
    "Bielsko-Biala",
    c("actual_rank_Bielsko-Biala", "simulated_rank_Bielsko-Biala", "delta_Bielsko-Biala")
  ) |>
  cols_label(
    "actual_rank_Bielsko-Biala" = "Actual",
    "simulated_rank_Bielsko-Biala" = "Simulated",
    "delta_Bielsko-Biala" = "Difference"
  ) |>
  tab_spanner(
    "Leogang",
    c("actual_rank_Leogang", "simulated_rank_Leogang", "delta_Leogang")
  ) |>
  cols_label(
    "actual_rank_Leogang" = "Actual",
    "simulated_rank_Leogang" = "Simulated",
    "delta_Leogang" = "Difference"
  ) |>
  tab_spanner(
    "Val di Sole",
    c("actual_rank_Val di Sole", "simulated_rank_Val di Sole", "delta_Val di Sole")
  ) |>
  cols_label(
    "actual_rank_Val di Sole" = "Actual",
    "simulated_rank_Val di Sole" = "Simulated",
    "delta_Val di Sole" = "Difference"
  ) |>
  tab_spanner(
    "Les Gets",
    c("actual_rank_Les Gets", "simulated_rank_Les Gets", "delta_Les Gets")
  ) |>
  cols_label(
    "actual_rank_Les Gets" = "Actual",
    "simulated_rank_Les Gets" = "Simulated",
    "delta_Les Gets" = "Difference"
  ) |>
  tab_spanner(
    "Loudenvielle",
    c("actual_rank_Loudenvielle", "simulated_rank_Loudenvielle", "delta_Loudenvielle")
  ) |>
  cols_label(
    "actual_rank_Loudenvielle" = "Actual",
    "simulated_rank_Loudenvielle" = "Simulated",
    "delta_Loudenvielle" = "Difference"
  ) |>
  tab_spanner(
    "Mont-Sainte-Anne",
    c("actual_rank_Mont-Sainte-Anne", "simulated_rank_Mont-Sainte-Anne", "delta_Mont-Sainte-Anne")
  ) |>
  cols_label(
    "actual_rank_Mont-Sainte-Anne" = "Actual",
    "simulated_rank_Mont-Sainte-Anne" = "Simulated",
    "delta_Mont-Sainte-Anne" = "Difference"
  ) |>
  fmt_missing(
    columns = everything(),
    missing_text = "-"
  ) |>
  text_transform(
    locations = cells_body(columns = path),
    fn = function(path) {
      local_image(
        filename = path,
        height = 75
      )
    }
  ) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_spanners()
  ) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = "name")
  ) |>
  opt_row_striping()


# Spark lines don't work:
tribble(
  ~name, ~overall, ~bielko_biala, ~leogang,
  "Loic", list(1, 1), list(2, 2), list(1, 1),
  "Amaury", list(3, 2), list(5, 6), list(13, 6),
  "Dakotah", list(9, 3), list(31, 17), list(14, 14)
) |>
  gt() |>
  gtExtras::gt_plt_sparkline(overall, type = "default") |>
  gtExtras::gt_plt_sparkline(bielko_biala, type = "points") |>
  gtExtras::gt_plt_sparkline(leogang)

# Instead we could try:
# - For the overall plot distrubtions of results per rider for both simulated
#   and actual, with a final score
# - For each rider per race, plot a bar chart showing position (e.g., out of 20)
#   This might have the down side of smaller bars looking like worse results
# - Alternatively, just keep numbers and use colour shading for differences.
#   Perhaps the difference col can be dropped and we just colour the simulated
#   result col by the difference (but then 2x info in one cell might be
#   confusing). Remember these tables should be fool proof.
# - Rename "Difference" to a more obvious name? Position change?

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
