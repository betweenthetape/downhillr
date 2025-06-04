# ------------------------------------------------------------------------------
# Analysis questions/ideas
# 1. Create "fastest possible run" by combining times for the fastest splits
# 2. Print tables of riders fastest splits, showing difference from fastest
#    possible splits and where they could have gained time
# 3. Plot variance in split times (for Q1/Q2/finals)
# 4. Which split was most important for the race? Show which rider gained the
#    most in a single split (normalise split times to make them comparable?)
# 5. Show simulated heat map
# 6. Show what could have been bump chart
# 7. Comment on weather
# 8. What's to come in the next round (reference 2024 simulated results to say
#    who could have gone fastest last year)
# ------------------------------------------------------------------------------

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
  filter(event_name == "Loudenvielle") |>
  mutate(name = if_else(name == "Oisin Callaghan O", "Oisin O'Callaghan", name))

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
  relocate(event_name, round_type, .after = time) |>
  filter(event_name == "Loudenvielle") |>
  mutate(name = if_else(name == "Oisin Callaghan O", "Oisin O'Callaghan", name))

# There are data entry errors where split_4 times are impossibly close to
# split_3 times for riders: Alexandre Fayolle, Jon Pardos Pardo, Rémi Thirion,
# Toby Meek, Oising O'Callaghan and Simon Chapelet. In each instance the time
# difference between split_4 and split_3 is 0.00100. This must have been an
# error with the recording equipment. We need to remove these entries.
timed_training <- timed_training |>
  mutate(
    split_4 = if_else(
      name %in%
        c(
          "Alexandre Fayolle",
          "Jon Pardos Pardo",
          "Rémi Thirion",
          "Toby Meek",
          "Oisin O'Callaghan",
          "Simon Chapelet"
        ),
      NA,
      split_4
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

image_data <- tibble(path = dir_ls("inst/rider-images")) |>
  mutate(name = str_remove(path, "^inst/rider-images/")) |>
  mutate(name = str_remove(name, ".png$")) |>
  mutate(name = str_replace(name, "(?<=[a-z])(?=[A-Z])", " ")) |>
  mutate(
    name = case_when(
      name == "Angel Suarez" ~ "Angel Alonso Suarez",
      name == "Lachlan StevensMcNab" ~ "Lachlan Stevens-Mcnab",
      name == "Oisin OCallaghan" ~ "Oisin O'Callaghan",
      name == "Remi Thirion" ~ "Rémi Thirion",
      name == "Ethan Craik" ~ "Ethan George Craik",
      name == "Tuhoto ArikiPene" ~ "Tuhoto-Ariki Pene",
      name == "Luke MeierSmith" ~ "Luke Meier-Smith",
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

# There is a data entry with Christian Hauser's Q2 section 4 time, where it is a
# repeat of his section 3 time. Assign him the mean of his two nearest
# competitors for Q2, Troy Brosnan and Jakob Jewett.
all_section_times <- world_cup_25_elite_men_results |>
  select(name, starts_with("split"), time, event_name, round_type) |>
  mutate(
    section_1 = split_1,
    section_2 = split_2 - split_1,
    section_3 = split_3 - split_2,
    section_4 = split_4 - split_3,
    section_5 = time - split_4
  )

troy_brosnan_q2_section_4_time <- all_section_times |>
  filter(
    name == "Troy Brosnan",
    event_name == "Loudenvielle",
    round_type == "Qualifying Round 2"
  ) |>
  pull(section_4)

jakob_jewett_q2_section_4_time <- all_section_times |>
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
# Loris Vergier table showing origin of fastest splits
# ------------------------------------------------------------------------------
table_loris_vergier <- world_cup_25_elite_men_results |>
  select(name, starts_with("split"), time, event_name, round_type) |>
  bind_rows(timed_training) |>
  filter(name == "Loris Vergier") |>
  mutate(
    `1` = split_1,
    `2` = split_2 - split_1,
    `3` = split_3 - split_2,
    `4` = split_4 - split_3,
    `5` = time - split_4
  ) |>
  select(-name, -starts_with("split_"), -event_name) |>
  mutate(
    round_type = factor(
      round_type,
      levels = c(
        "Timed Training 1",
        "Timed Training 2",
        "Timed Training 3",
        "Timed Training 4",
        "Timed Training 5",
        "Qualifying Round 1",
        "Final"
      )
    )
  ) |>
  relocate(time, .after = `5`) |>
  arrange(round_type) |>
  gt(rowname_col = "round_type") |>
  sub_missing() |>
  opt_row_striping() |>
  cols_label("round_type" = "") |>
  fmt_number(!round_type, decimals = 2) |>
  tab_spanner(label = "Section", columns = !round_type) |>
  tab_style(
    style = cell_text(align = "left"),
    locations = cells_stub()
  ) |>
  tab_header(
    md(
      "**Loris Vergier's fastest sections were spread across the event**"
    ),
    md("Fastest sections (in seconds) are highlighted in green")
  ) |>
  tab_source_note(
    md("_Timed Training runs 4 and 5 were not performed_")
  ) |>
  tab_style(
    style = cell_text(align = "left"),
    locations = cells_source_notes()
  ) |>
  data_color(
    columns = `1`,
    rows = round_type == "Timed Training 2",
    palette = "#4daf4a"
  ) |>
  data_color(
    columns = `2`,
    rows = round_type == "Final",
    palette = "#4daf4a"
  ) |>
  data_color(
    columns = `3`,
    rows = round_type == "Timed Training 3",
    palette = "#4daf4a"
  ) |>
  data_color(
    columns = `4`,
    rows = round_type == "Final",
    palette = "#4daf4a"
  ) |>
  data_color(
    columns = `5`,
    rows = round_type == "Qualifying Round 1",
    palette = "#4daf4a"
  )

# gtsave(
#   table_loris_vergier,
#   "inst/scripts/analysis-loudenvielle-25/table_loris_vergier.png",
#   zoom = 10
# )

# ------------------------------------------------------------------------------
# Table showing % ridets with fastest simulated results
# ------------------------------------------------------------------------------
fastest_times_all <- fastest_times_weekend |>
  left_join(fastest_times_final) |>
  left_join(fastest_possible_times) |>
  mutate(
    possible_faster_than_weekend = if_else(
      fastest_time_possible < fastest_time_weekend,
      TRUE,
      FALSE
    )
  ) |>
  mutate(
    possible_faster_than_finals = if_else(
      fastest_time_possible < fastest_time_finals,
      TRUE,
      FALSE
    )
  )

table_possible_faster <- fastest_times_all |>
  filter(!is.na(possible_faster_than_weekend)) |>
  summarise(
    possible_faster_than_weekend = sum(
      possible_faster_than_weekend,
      na.rm = TRUE
    ),
    total_riders = n()
  ) |>
  mutate(percentage_riders = possible_faster_than_weekend / total_riders) |>
  gt() |>
  opt_row_striping() |>
  cols_label(
    total_riders = "Riders (count)",
    possible_faster_than_weekend = "Faster simulation (count)",
    percentage_riders = "Faster simulation (%)"
  ) |>
  fmt_percent(columns = percentage_riders, decimals = 1) |>
  tab_header(
    title = md(
      "**The count & percentage of riders with faster simulation times than actual times**"
    ),
    subtitle = md(
      "Actual times determined as each riders fastest time from each event"
    )
  )

# gtsave(
#   table_possible_faster,
#   "inst/scripts/analysis-loudenvielle-25/table_possible_faster.png",
#   zoom = 10
# )

# ------------------------------------------------------------------------------
# Who left the most time on the track
# ------------------------------------------------------------------------------
table_time_left <- fastest_times_all |>
  mutate(diff = fastest_time_finals - fastest_time_possible) |>
  arrange(desc(diff)) |>
  slice(1:10) |>
  select(name, fastest_time_finals, fastest_time_possible, time_left = diff) |>
  gt() |>
  opt_row_striping() |>
  fmt_number(
    columns = c(fastest_time_finals, fastest_time_possible, time_left),
    decimals = 2
  ) |>
  cols_label(
    name = "Rider",
    fastest_time_finals = "Finals Time",
    fastest_time_possible = "Simulated Fastest Time",
    time_left = "Time Left on Track"
  ) |>
  tab_header(
    title = md(
      "**The top 10 riders with the most time left on the track**"
    ),
    subtitle = md(
      "The time left on the track was calculated as the difference between their Finals time and their simulated fastest time"
    )
  )

# gtsave(
#   table_time_left,
#   "inst/scripts/analysis-loudenvielle-25/table_time_left.png",
#   zoom = 10
# )

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

bump_data <- actual |>
  left_join(simulated) |>
  pivot_longer(
    starts_with("rank_"),
    names_to = "type",
    values_to = "rank"
  ) |>
  mutate(
    type = if_else(
      type == "rank_actual",
      "Actual \noverall rank",
      "Simulated \noverall rank"
    )
  ) |>
  mutate(
    color = case_when(
      name == "Jackson Goldstone" ~ "#57106e",
      name == "Tuhoto-Ariki Pene" ~ "#f98e09",
      TRUE ~ "#E7E7E7"
    )
  ) |>
  left_join(image_data) |>
  filter(rank <= 28)

plot_bump <- ggplot() +
  geom_bump(
    aes(type, rank, group = name, color = I(color)),
    data = bump_data,
    linewidth = 1.5
  ) +
  geom_image(
    data = bump_data,
    aes(type, rank, image = path)
  ) +
  scale_y_reverse() +
  theme_minimal() +
  theme(
    text = element_text(family = "sans"),
    plot.title = element_textbox_simple(
      halign = 0.5,
      margin = margin(b = 10, t = 15),
      size = 16
    ),
    plot.subtitle = element_textbox_simple(
      halign = 0,
      hjust = 0.5,
      margin = margin(b = 10),
      width = grid::unit(6, "in"),
      size = 11,
      color = "#424242"
    ),
    axis.text.x = element_text(size = 10, vjust = 2),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank()
  ) +
  geom_richtext(
    data = filter(bump_data, type == "Actual \noverall rank"),
    hjust = 1,
    nudge_x = -0.1,
    mapping = aes(
      x = type,
      y = rank,
      label.size = NA,
      family = "sans",
      label = glue(
        "<span style='font-size:14px;'>{name}<span style='color:white;'>...</span><span style='font-size:16px;'>**{rank}**</span></span>"
      )
    )
  ) +
  geom_richtext(
    data = filter(bump_data, type == "Simulated \noverall rank"),
    nudge_x = 0.1,
    hjust = 0,
    family = "sans",
    mapping = aes(
      x = type,
      y = rank,
      label.size = NA,
      label = glue(
        "<span style='font-size:14px;'><span style='font-size:16px;'>**{rank}**</span><span style='color:white;'>...</span>{name}</span>"
      )
    )
  ) +
  labs(
    title = "<span>**What Could Have Been**</span>",
    subtitle = "<span> Each riders fastest splits from across the Loudenvielle
    race weekend were combined to simulate their fastest hypothetical runs.
    These runs were then ranked to create a new simulated leaderboard. Even in
    this simulated world, <span style='color:#57106e;'>**Jackson Goldstone**</span>
    reigns king with unmatched speed. Other riders like <span
    style='color:#f98e09;'>**Tuhoto-Ariki Pene**</span> climb a whopping 17
    places, showing they still left time left on the hill. Could these riders be
    a good bet for the next race?</span>"
  )

# ggsave(
#   "inst/scripts/analysis-loudenvielle-25/plot_bump.png",
#   plot = plot_bump,
#   width = 2300,
#   height = 3200,
#   units = "px",
#   bg = "white",
#   limitsize = FALSE,
#   dpi = 330
# )

# ------------------------------------------------------------------------------
# Simulated heat map
# ------------------------------------------------------------------------------
fastest_possible_splits_ranked <-
  fastest_possible_sections |>
  select(-ends_with("_round")) |>
  rename_with(\(x) str_remove_all(x, "_time"), starts_with("section_")) |>
  rowwise() |>
  mutate(
    section_2 = section_1 + section_2,
    section_3 = section_2 + section_3,
    section_4 = section_3 + section_4,
    section_5 = section_4 + section_5
  ) |>
  ungroup() |>
  mutate(
    across(
      starts_with("section_"),
      ~ rank(.x, ties.method = "min"),
      .names = "{.col}_rank"
    )
  ) |>
  mutate(
    across(
      starts_with("section_") & !ends_with("_rank"),
      ~ .x - min(.x),
      .names = "{.col}_gap"
    )
  )

merge_section_columns <- function(gt_tbl) {
  reduce(
    1:5,
    \(gt_tbl, x) {
      cols_merge(
        gt_tbl,
        columns = c(
          paste0("section_", x, "_gap"),
          paste0("section_", x, "_rank")
        ),
        pattern = "{1} ({2})"
      )
    },
    .init = gt_tbl
  )
}

table_heat_map <- fastest_possible_splits_ranked |>
  filter(section_5_rank <= 10) |>
  left_join(image_data) |>
  select(path, name, ends_with("_gap"), ends_with("_rank")) |>
  arrange(section_5_gap) |>
  gt() |>
  text_transform(
    locations = cells_body(columns = path),
    fn = function(path) {
      local_image(
        filename = path,
        height = 60
      )
    }
  ) |>
  cols_label(
    path = "",
    name = "",
    section_1_gap = "Split 1",
    section_2_gap = "Split 2",
    section_3_gap = "Split 3",
    section_4_gap = "Split 4",
    section_5_gap = "Finish"
  ) |>
  data_color(
    columns = ends_with("_gap"),
    rows = everything(),
    palette = c("#4daf4a", "#ffffbf", "#e41a1c")
  ) |>
  text_transform(
    fn = \(x) if_else(x == "0.000", paste0(x), paste("+", x)),
    locations = cells_body(columns = ends_with("_gap"))
  ) |>
  merge_section_columns() |>
  tab_style(
    style = cell_borders(sides = "all", style = "solid", color = "#e9e9e9"),
    locations = cells_body(
      columns = ends_with("_gap")
    )
  ) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) |>
  opt_row_striping() |>
  tab_options(
    table.font.size = px(12),
    column_labels.font.weight = "bold"
  ) |>
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(columns = !name)
  ) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = "name")
  ) |>
  tab_header(
    title = md("## Simulated Race Split Times and Rankings"),
    subtitle = md(
      "### Each split is colored by split time from fastest (green) to slowest (red)"
    )
  )

gtsave(
  table_heat_map,
  "inst/scripts/analysis-loudenvielle-25/table_heat_map.png",
  zoom = 10
)

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
