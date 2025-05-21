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
  mutate(across(everything(), ~ if_else(is.infinite(.x), NA, .x)))

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
  mutate(rank_actual = row_number()) |>
  select(name, rank_actual)

simulated <- fastest_possible_times |>
  mutate(rank_simulated = row_number()) |>
  select(name, rank_simulated)

actual |>
  left_join(simulated) |>
  View()

# Insight: there is no difference between actual and simulated performance.
# All riders achieved all their fastest splits in finals (check this is true).
# Contrast this to last seasons overall bump chart, where we can see this did
# not hold true. Is this a signal Q1/Q2 works? That the reduced field is making
# for better racing?
# Fort william has almost identical simulated and actual overall results in
# 24 season, but then variance increased as season progressed. Is there
# something special about the openeing round? Perhaps it takes riders a while to
# get up to speed, where as in subsequent rounds they know the level better
# straight off the bat?

# ------------------------------------------------------------------------------
# Fastest run comparison
# ------------------------------------------------------------------------------
fastest_possible_run

# What is the fastest possible run total time?
# How far off were each rider from this hypotehtical fastest run?
# Per section, what is the variance in times?
# Which section was the most important in the race (seems like Amaury smoked
# everyone on the lower section)
