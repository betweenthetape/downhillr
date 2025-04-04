# ------------------------------------------------------------------------------
# README:
# Across each event, we can take the fastest section times available for each
# rider and combine them together to simulate what could have been their
# fastest race run.
# ------------------------------------------------------------------------------
library(tidyverse)
pkgload::load_all()

# ------------------------------------------------------------------------------
# Prepare data sets
# ------------------------------------------------------------------------------
world_cup_24_elite_men_results <- world_cup_24_elite_men_results |>
  mutate(name = str_to_title(name)) |>
  mutate(
    name = map_chr(str_split(name, " "), ~ str_c(rev(.x), collapse = " "))
  ) |>
  mutate(across(everything(), ~ if_else(is.infinite(.x), NA, .x)))

timed_training <- world_cup_24_elite_men_timed_training |>
  mutate(name = str_to_title(name)) |>
  mutate(
    name = map_chr(str_split(name, " "), ~ str_c(rev(.x), collapse = " "))
  ) |>
  mutate(across(everything(), ~ if_else(is.infinite(.x), NA, .x))) |>
  select(
    name,
    contains("_split_"),
    matches("\\d+_time$"),
    event_name,
    round_type
  ) |>
  pivot_longer(
    cols = matches("^(run_\\d+_split_\\d+|run_\\d+_time)$"),
    names_to = c("run_number", "split_type"),
    names_pattern = "run_(\\d+)_(.+)",
    values_to = "value"
  ) |>
  mutate(round_type = paste(round_type, run_number)) |>
  select(-run_number) |>
  pivot_wider(
    names_from = split_type,
    values_from = value
  ) |>
  relocate(event_name, round_type, .after = time)

# Correct data entry errors for Kimi Viardot & Valentin Rohrmoser (in both
# cases, hours are mistaken as minutes).
timed_training <- timed_training |>
  mutate(
    across(
      c(split_3, split_4, time),
      ~ if_else(
        name == "Kimi Viardot" &
          event_name == "Bielsko-Biala" &
          round_type == "Timed Training 1",
        NA,
        .x
      )
    ),
    across(
      c(split_3, split_4, time),
      ~ if_else(
        name == "Valentin Rohrmoser" &
          event_name == "Val di Sole" &
          round_type == "Timed Training 1",
        NA,
        .x
      )
    )
  )

# Check that later splits are always larger than earlier splits to catch any
# other data entry errors
timed_training <- timed_training |>
  mutate(
    split_2 = if_else(split_2 <= split_1, NA, split_2),
    split_3 = if_else(split_3 <= split_2, NA, split_3),
    split_4 = if_else(split_4 <= split_3, NA, split_4),
    time = if_else(time <= split_4, NA, time)
  )

# ------------------------------------------------------------------------------
# Fastest Possible times
# ------------------------------------------------------------------------------
fastest_possible_sections <- world_cup_24_elite_men_results |>
  select(name, starts_with("split"), time, event_name, round_type) |>
  bind_rows(timed_training) |>
  mutate(
    section_1 = split_1,
    section_2 = split_2 - split_1,
    section_3 = split_3 - split_2,
    section_4 = split_4 - split_3,
    section_5 = time - split_4
  ) |>
  summarise(
    across(
      starts_with("section_"),
      list(
        time = ~ min(.x, na.rm = TRUE),
        round = ~ round_type[which.min(.x)]
      )
    ),
    .by = c(name, event_name)
  ) |>
  filter(if_all(starts_with("section_"), ~ !is.infinite(.x)))

fastest_times_possible <- fastest_possible_sections |>
  rowwise(name, event_name) |>
  summarise(
    fastest_time_possible = sum(
      c_across(ends_with("_time")),
      na.rm = TRUE
    ),
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# Simulate season
# ------------------------------------------------------------------------------
simulated_top_30_each_race <- fastest_times_possible |>
  group_by(event_name) |>
  arrange(fastest_time_possible, .by_group = TRUE) |>
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
