# TODO: ADD TIMED TRAINING RESULTS INTO FASTEST TIMES COMPUTATIONS AND RERUN TO
# SEE WHAT CHANGES!

# ------------------------------------------------------------------------------
# Load
# ------------------------------------------------------------------------------
pkgload::load_all()
library(tidyverse)
library(fs)
library(ggbump)
library(ggimage)
library(ggtext)
library(glue)
library(gt)

# ------------------------------------------------------------------------------
# Prepare data sets
# ------------------------------------------------------------------------------
world_cup_25_elite_men_results <- world_cup_25_elite_men_results |>
  mutate(name = str_to_title(name)) |>
  mutate(
    name = map_chr(str_split(name, " "), ~ str_c(rev(.x), collapse = " "))
  ) |>
  mutate(across(everything(), ~ if_else(is.infinite(.x), NA, .x))) |>
  filter(event_name == "Loudenvielle")

timed_training <- world_cup_25_elite_men_timed_training |>
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

# Check that later splits are always larger than earlier splits to catch any
# other data entry errors
timed_training <- timed_training |>
  mutate(
    split_2 = if_else(split_2 <= split_1, NA, split_2),
    split_3 = if_else(split_3 <= split_2, NA, split_3),
    split_4 = if_else(split_4 <= split_3, NA, split_4),
    time = if_else(time <= split_4, NA, time)
  )

image_data <- tibble(path = dir_ls("inst/rider-images")) |>
  mutate(name = str_remove(path, "^inst/rider-images/")) |>
  mutate(name = str_remove(name, ".png$")) |>
  mutate(name = str_replace(name, "(?<=[a-z])(?=[A-Z])", " ")) |>
  mutate(
    name = case_when(
      name == "Angel Suarez" ~ "Angel Alonso Suarez",
      name == "Lachlan StevensMcNab" ~ "Lachlan Stevens-Mcnab",
      name == "Oisin OCallaghan" ~ "Oisin Callaghan O",
      name == "Remi Thirion" ~ "Rémi Thirion",
      name == "Ethan Craik" ~ "Ethan George Craik",
      .default = name
    )
  )

weather <- world_cup_25_elite_men_results |>
  distinct(event_name, round_type, metadata_weather, metadata_temp_deg_c) |>
  mutate(
    weather = paste(
      metadata_weather,
      paste0("(", metadata_temp_deg_c, "°C)"),
      sep = " "
    )
  ) |>
  select(event_name, round_type, weather) |>
  pivot_wider(
    names_from = round_type,
    values_from = weather,
    names_prefix = "weather_"
  )

weather_emoji <- world_cup_25_elite_men_results |>
  distinct(event_name, round_type, metadata_weather) |>
  mutate(
    metadata_weather = case_when(
      metadata_weather == "Mostly sunny" ~ "Mostly Sunny",
      metadata_weather == "Light rain" ~ "Light Rain",
      .default = metadata_weather
    )
  ) |>
  mutate(
    metadata_weather = case_when(
      metadata_weather == "Cloudy" ~ "☁️",
      metadata_weather == "Light Rain" ~ "🌦️",
      metadata_weather == "Rain" ~ "🌧️",
      metadata_weather == "Rainy" ~ "🌧️",
      metadata_weather == "Mostly Sunny" ~ "⛅️",
      metadata_weather == "Sunny" ~ "☀️"
    )
  ) |>
  pivot_wider(
    names_from = round_type,
    values_from = metadata_weather,
    names_prefix = "weather_"
  )

# ------------------------------------------------------------------------------
# Fastest actual times
# ------------------------------------------------------------------------------
fastest_times_weekend <- world_cup_25_elite_men_results |>
  summarise(
    fastest_time_weekend = min(time, na.rm = TRUE),
    .by = c(name, event_name)
  ) |>
  filter(!is.infinite(fastest_time_weekend))

fastest_times_final <- world_cup_25_elite_men_results |>
  filter(round_type == "Final") |>
  summarise(
    fastest_time_finals = min(time, na.rm = TRUE),
    .by = c(name, event_name)
  ) |>
  filter(!is.infinite(fastest_time_finals))

# ------------------------------------------------------------------------------
# Fastest Possible times
# ------------------------------------------------------------------------------
fastest_possible_sections <- world_cup_25_elite_men_results |>
  select(name, starts_with("split"), time, event_name, round_type) |>
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

# There is a data entry with Christian Hauser's Q2 section 4 time, where it is a
# repeat of his section 3 time. Assign him the mean of his two nearest
# competitors for Q2, Troy Brosnan and Jakob Jewett.
q2_section_4_times <- world_cup_25_elite_men_results |>
  select(name, starts_with("split"), time, event_name, round_type) |>
  mutate(
    section_1 = split_1,
    section_2 = split_2 - split_1,
    section_3 = split_3 - split_2,
    section_4 = split_4 - split_3,
    section_5 = time - split_4
  )

troy_brosnan_q2_section_4_time <- q2_section_4_times |>
  filter(
    name == "Troy Brosnan",
    event_name == "Loudenvielle",
    round_type == "Qualifying Round 2"
  ) |>
  pull(section_4)

jakob_jewett_q2_section_4_time <- q2_section_4_times |>
  filter(
    name == "Jakob Jewett",
    event_name == "Loudenvielle",
    round_type == "Qualifying Round 2"
  ) |>
  pull(section_4)

fastest_possible_sections <- fastest_possible_sections |>
  mutate(
    section_4_time = if_else(
      name == "Christian Hauser",
      mean(c(troy_brosnan_q2_section_4_time, jakob_jewett_q2_section_4_time)),
      section_4_time
    )
  )

fastest_possible_times <- fastest_possible_sections |>
  rowwise(name, event_name) |>
  summarise(
    fastest_time_possible = sum(
      c_across(ends_with("_time")),
      na.rm = TRUE
    ),
    .groups = "drop"
  )

fastest_possible_run <- fastest_possible_sections |>
  summarise(
    across(
      ends_with("_time"),
      ~ min(.x, na.rm = TRUE)
    )
  )

# ------------------------------------------------------------------------------
# Bump plot
# ------------------------------------------------------------------------------
actual <- fastest_times_final |>
  arrange(fastest_time_finals) |>
  mutate(rank_actual = row_number()) |>
  select(name, rank_actual)

simulated <- fastest_possible_times |>
  arrange(fastest_time_possible) |>
  mutate(rank_simulated = row_number()) |>
  select(name, rank_simulated)

actual |>
  left_join(simulated) |>
  View()

# ------------------------------------------------------------------------------
# Fastest run comparison
# ------------------------------------------------------------------------------
fastest_possible_run |>
  rowwise() |>
  mutate(total_time = sum(c_across(everything()))) |>
  ungroup()

# What is the fastest possible run total time?
# How far off were each rider from this hypotehtical fastest run?
# Per section, what is the variance in times?
# Which section was the most important in the race (seems like Amaury smoked
# everyone on the lower section)
