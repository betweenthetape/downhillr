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

weather <- world_cup_24_elite_men_results |>
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

weather_emoji <- world_cup_24_elite_men_results |>
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
fastest_times_weekend <- world_cup_24_elite_men_results |>
  summarise(
    fastest_time_weekend = min(time, na.rm = TRUE),
    .by = c(name, event_name)
  ) |>
  filter(!is.infinite(fastest_time_weekend))

fastest_times_final <- world_cup_24_elite_men_results |>
  filter(round_type == "Final") |>
  summarise(
    fastest_time_finals = min(time, na.rm = TRUE),
    .by = c(name, event_name)
  ) |>
  filter(!is.infinite(fastest_time_finals))

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
# Validate fastest possible time as a metric
#
# Notes:
# - Next, summary stats showing which races and riders had the most time left on
#   the track when using simulated race runs.
# - Just use comparison with fastest weekend time as this takes into
#   consideration changing weather and track conditions.
# ------------------------------------------------------------------------------
# Use Luca's Leogang results to show how simulation works
luca_les_gets <- world_cup_24_elite_men_results |>
  select(name, starts_with("split"), time, event_name, round_type) |>
  bind_rows(timed_training) |>
  filter(name == "Luca Shaw") |>
  filter(event_name == "Les Gets") |>
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
        "Qualifying",
        "Semi-Final",
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
      "**Luca Shaw's fastest sections in Les Gets were spread across the event**"
    ),
    md("Fastest splits are highlighted in green")
  ) |>
  data_color(
    columns = `1`,
    rows = round_type == "Qualifying",
    palette = "#4daf4a"
  ) |>
  data_color(
    columns = `2`,
    rows = round_type == "Timed Training 1",
    palette = "#4daf4a"
  ) |>
  data_color(
    columns = `3`,
    rows = round_type == "Semi-Final",
    palette = "#4daf4a"
  ) |>
  data_color(
    columns = `4`,
    rows = round_type == "Semi-Final",
    palette = "#4daf4a"
  ) |>
  data_color(
    columns = `5`,
    rows = round_type == "Semi-Final",
    palette = "#4daf4a"
  )

gtsave(luca_les_gets, "inst/scripts/luca-les-gets.png", zoom = 10)

# Next, let's calculate the percentage of riders where their simulated result
# was faster than their fastest weekend time
fastest_times_all <- fastest_times_weekend |>
  left_join(fastest_times_final) |>
  left_join(fastest_times_possible) |>
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

gt_possible_faster_than_weekend <- fastest_times_all |>
  filter(!is.na(possible_faster_than_weekend)) |>
  summarise(
    possible_faster_than_weekend = sum(
      possible_faster_than_weekend,
      na.rm = TRUE
    ),
    total_riders = n(),
    .by = event_name
  ) |>
  mutate(percentage_riders = possible_faster_than_weekend / total_riders) |>
  relocate(total_riders, .after = event_name) |>
  gt() |>
  opt_row_striping() |>
  cols_label(
    event_name = "",
    total_riders = "Riders (count)",
    possible_faster_than_weekend = "Faster simulation (count)",
    percentage_riders = "Faster simulation (%)"
  ) |>
  fmt_percent(columns = percentage_riders, decimals = 1) |>
  tab_header(
    title = md(
      "The count & percentage of riders with faster simulation times than actual times."
    ),
    subtitle = md(
      "Actual times determined as each riders fastest time from each event."
    )
  )

gtsave(
  gt_possible_faster_than_weekend,
  "inst/scripts/validation.png",
  zoom = 10
)

# Which riders had the most time left?
riders_with_most_time_left <- fastest_times_all |>
  summarise(
    possible_faster_than_weekend = sum(
      possible_faster_than_weekend,
      na.rm = TRUE
    ),
    total_races_completed = n(),
    .by = name
  ) |>
  mutate(
    percentage_races_time_left = possible_faster_than_weekend /
      total_races_completed
  ) |>
  arrange(desc(percentage_races_time_left), desc(total_races_completed))

gt_riders_with_most_time_left <- riders_with_most_time_left |>
  relocate(total_races_completed, .after = name) |>
  slice_head(n = 10) |>
  gt() |>
  opt_row_striping() |>
  cols_label(
    name = "",
    total_races_completed = "Events done (count)",
    possible_faster_than_weekend = "Simulation faster (count)",
    percentage_races_time_left = "Simulation faster (%)"
  ) |>
  fmt_percent(percentage_races_time_left, decimals = 0) |>
  tab_header(
    title = md(
      "Each rider in this table left time on each track they completed."
    )
  )

gtsave(
  gt_riders_with_most_time_left,
  "inst/scripts/riders_time_left.png",
  zoom = 10
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

finals_plot <- finals_points |>
  ggplot(aes(x = position, y = points)) +
  geom_col() +
  coord_flip() +
  scale_x_reverse(breaks = c(1, seq(5, 30, 5))) +
  theme_minimal() +
  labs(
    title = "Simulated runs were scored using the same exponential points distribtuion \nused in Finals.",
    x = "Position",
    y = "Points"
  )

ggsave(
  "inst/scripts/finals_plot.png",
  plot = finals_plot,
  width = 2200,
  height = 1500,
  units = "px",
  bg = "white",
  limitsize = FALSE,
  dpi = 330
)

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
#
# Notes:
# - Start the narrative by showing how the top of table would have changed in a
#   simulated season
# ------------------------------------------------------------------------------
# Keep seasonal data incase useful down the line
bump_data_season <- simulated_season |>
  left_join(actual_season) |>
  select(name, event_name, ends_with("_rank")) |>
  filter(name %in% simulated_overall$name[1:10]) |>
  pivot_longer(ends_with("_rank"), names_to = "season", values_to = "rank") |>
  mutate(
    season = if_else(
      season == "actual_rank",
      "Actual \nrank",
      "Simulated \nrank"
    )
  )

bump_data_overall <- simulated_overall |>
  left_join(actual_overall) |>
  slice(1:10) |>
  select(-ends_with("_points")) |>
  pivot_longer(
    ends_with("_rank"),
    names_to = "season",
    values_to = "rank"
  ) |>
  mutate(
    season = if_else(
      season == "actual_rank",
      "Actual \noverall rank",
      "Simulated \noverall rank"
    )
  ) |>
  mutate(
    color = case_when(
      name == "Loic Bruni" ~ "#57106e",
      name == "Dakotah Norton" ~ "#f98e09",
      TRUE ~ "#E7E7E7"
    )
  ) |>
  left_join(image_data)

bump_plot <- ggplot() +
  geom_bump(
    aes(season, rank, group = name, color = I(color)),
    data = bump_data_overall,
    linewidth = 1.5
  ) +
  geom_image(
    data = bump_data_overall,
    aes(season, rank, image = path)
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
    data = filter(bump_data_overall, season == "Actual \noverall rank"),
    hjust = 1,
    nudge_x = -0.1,
    mapping = aes(
      x = season,
      y = rank,
      label.size = NA,
      family = "sans",
      label = glue(
        "<span style='font-size:14px;'>{name}<span style='color:white;'>...</span><span style='font-size:16px;'>**{rank}**</span></span>"
      )
    )
  ) +
  geom_richtext(
    data = filter(bump_data_overall, season == "Simulated \noverall rank"),
    nudge_x = 0.1,
    hjust = 0,
    family = "sans",
    mapping = aes(
      x = season,
      y = rank,
      label.size = NA,
      label = glue(
        "<span style='font-size:14px;'><span style='font-size:16px;'>**{rank}**</span><span style='color:white;'>...</span>{name}</span>"
      )
    )
  ) +
  labs(
    title = "<span>**WHAT COULD HAVE BEEN**</span>",
    subtitle = "<span> Each riders fastest splits from across each race weekend
    were combined to simulate their fastest hypothetical runs. These runs were
    then rescored to create a new overall simulated leaderboard. Even in this
    simulated world, <span style='color:#57106e;'>**Loic
    Bruni**</span> reigns king with unmatched speed. Other riders like <span
    style='color:#f98e09;'>**Dakotah Norton**</span> climb up to 4 places,
    showing they still left time left on the hill. Could these riders be a good
    bet next season?</span>"
  )

ggsave(
  "inst/scripts/bump_plot.png",
  plot = bump_plot,
  width = 2200,
  height = 1500,
  units = "px",
  bg = "white",
  limitsize = FALSE,
  dpi = 330
)

# ------------------------------------------------------------------------------
# Delta scores per season and overall
#
# Notes:
# - Then we can break down the season by race, highlighting which races were
#   pivotal to the story
# - Insights for title and story text (TODO: highlight these cells in the
#   table):
#   - Dak could have won Les Gets, we all knew this, but the data confirms it.
#   - Troy and Ronan show incredible consistency. They find their pace, and show
#     little variation. Impressive. Good bets for fantasy league for consistent
#     points.
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
    names_from = event_name,
    values_from = c(simulated_rank, actual_rank, delta)
  ) |>
  left_join(image_data)

# TODO:
# - Consider changing coloured boxes for different types of grey shaded boxes so
#   the red/green colours of the delta scores is clear.
# - In addition to the table, how about copying the distrubtion plots that are
#   used to show male/female age distrubtions back-to-back, but instead have
#   actual vs. simulated back-to-back? This could then be faceted by race. Would
#   this be a nice way to visualise a riders virtual vs actual season in a
#   single plot?
delta_table_gt <- delta_all_wide |>
  dplyr::select(
    path,
    name,
    "simulated_rank_Overall",
    "actual_rank_Overall",
    "delta_Overall",
    "simulated_rank_Fort William",
    "actual_rank_Fort William",
    "delta_Fort William",
    "simulated_rank_Bielsko-Biala",
    "actual_rank_Bielsko-Biala",
    "delta_Bielsko-Biala",
    "simulated_rank_Leogang",
    "actual_rank_Leogang",
    "delta_Leogang",
    "simulated_rank_Val di Sole",
    "actual_rank_Val di Sole",
    "delta_Val di Sole",
    "simulated_rank_Les Gets",
    "actual_rank_Les Gets",
    "delta_Les Gets",
    "simulated_rank_Loudenvielle",
    "actual_rank_Loudenvielle",
    "delta_Loudenvielle",
    "simulated_rank_Mont-Sainte-Anne",
    "actual_rank_Mont-Sainte-Anne",
    "delta_Mont-Sainte-Anne"
  ) |>
  filter(simulated_rank_Overall <= 10) |>
  arrange(actual_rank_Overall) |>
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
    c(
      "actual_rank_Fort William",
      "simulated_rank_Fort William",
      "delta_Fort William"
    )
  ) |>
  cols_label(
    "actual_rank_Fort William" = "Actual",
    "simulated_rank_Fort William" = "Simulated",
    "delta_Fort William" = "Difference"
  ) |>
  tab_spanner(
    "Bielsko-Biala",
    c(
      "actual_rank_Bielsko-Biala",
      "simulated_rank_Bielsko-Biala",
      "delta_Bielsko-Biala"
    )
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
    c(
      "actual_rank_Val di Sole",
      "simulated_rank_Val di Sole",
      "delta_Val di Sole"
    )
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
    c(
      "actual_rank_Loudenvielle",
      "simulated_rank_Loudenvielle",
      "delta_Loudenvielle"
    )
  ) |>
  cols_label(
    "actual_rank_Loudenvielle" = "Actual",
    "simulated_rank_Loudenvielle" = "Simulated",
    "delta_Loudenvielle" = "Difference"
  ) |>
  tab_spanner(
    "Mont-Sainte-Anne",
    c(
      "actual_rank_Mont-Sainte-Anne",
      "simulated_rank_Mont-Sainte-Anne",
      "delta_Mont-Sainte-Anne"
    )
  ) |>
  cols_label(
    "actual_rank_Mont-Sainte-Anne" = "Actual",
    "simulated_rank_Mont-Sainte-Anne" = "Simulated",
    "delta_Mont-Sainte-Anne" = "Difference"
  ) |>
  sub_missing(
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
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = starts_with("delta_"))
  ) |>
  tab_style(
    style = cell_borders(sides = "left", style = "dashed", color = "#d3d3d3"),
    locations = cells_body(columns = starts_with("actual_"))
  ) |>
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(columns = !name)
  ) |>
  opt_row_striping() |>
  data_color(
    columns = ends_with("_rank_Loudenvielle"),
    rows = name == "Loic Bruni",
    palette = "#7fb7db", # Jersey stripe colour
  ) |>
  tab_style(
    style = cell_borders(
      sides = c("left", "top", "bottom"),
      style = "solid",
      color = "#3c3737"
    ),
    locations = cells_body(
      columns = "actual_rank_Loudenvielle",
      rows = name == "Loic Bruni"
    )
  ) |>
  tab_style(
    style = cell_borders(
      sides = c("right", "top", "bottom"),
      style = "solid",
      color = "#3c3737"
    ),
    locations = cells_body(
      columns = "simulated_rank_Loudenvielle",
      rows = name == "Loic Bruni"
    )
  ) |>
  data_color(
    columns = ends_with("_rank_Les Gets"),
    rows = name == "Dakotah Norton",
    palette = "#f3d39d", # Hat colour
    alpha = .7
  ) |>
  tab_style(
    style = cell_borders(
      sides = c("left", "top", "bottom"),
      style = "solid",
      color = "#3c3737"
    ),
    locations = cells_body(
      columns = "actual_rank_Les Gets",
      rows = name == "Dakotah Norton"
    )
  ) |>
  tab_style(
    style = cell_borders(
      sides = c("right", "top", "bottom"),
      style = "solid",
      color = "#3c3737"
    ),
    locations = cells_body(
      columns = "simulated_rank_Les Gets",
      rows = name == "Dakotah Norton"
    )
  ) |>
  data_color(
    columns = ends_with("delta_Fort William"),
    palette = "#d9d9d9",
    alpha = .7
  ) |>
  tab_style(
    style = cell_borders(
      sides = c("left"),
      style = "solid",
      color = "#3c3737"
    ),
    locations = cells_body(
      columns = "delta_Fort William",
    )
  ) |>
  tab_style(
    style = cell_borders(
      sides = c("right"),
      style = "solid",
      color = "#3c3737"
    ),
    locations = cells_body(
      columns = "delta_Fort William"
    )
  ) |>
  tab_style(
    style = cell_borders(
      sides = c("top"),
      style = "solid",
      color = "#3c3737"
    ),
    locations = cells_body(
      columns = "delta_Fort William",
      rows = name == "Loic Bruni"
    )
  ) |>
  tab_style(
    style = cell_borders(
      sides = c("bottom"),
      style = "solid",
      color = "#3c3737"
    ),
    locations = cells_body(
      columns = "delta_Fort William",
      rows = name == "Loris Vergier"
    )
  ) |>
  data_color(
    columns = !c(path, name) & !ends_with("Loudenvielle"),
    rows = name == "Troy Brosnan",
    palette = "#1daa28", # Jersey colour
    alpha = .5
  ) |>
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"),
      style = "solid",
      color = "#3c3737"
    ),
    locations = cells_body(
      columns = !c(path, name),
      rows = name == "Troy Brosnan"
    )
  ) |>
  tab_style(
    style = cell_borders(sides = "left", style = "solid", color = "#3c3737"),
    locations = cells_body(
      columns = actual_rank_Overall,
      rows = name == "Troy Brosnan"
    )
  ) |>
  tab_style(
    style = cell_borders(sides = "right", style = "solid", color = "#3c3737"),
    locations = cells_body(
      columns = "delta_Mont-Sainte-Anne",
      rows = name == "Troy Brosnan"
    )
  ) |>
  data_color(
    columns = !c(path, name),
    rows = name == "Ronan Dunne",
    palette = "#00007f", # Jersey colour
    alpha = .3
  ) |>
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"),
      style = "solid",
      color = "#3c3737"
    ),
    locations = cells_body(
      columns = !c(path, name),
      rows = name == "Ronan Dunne"
    )
  ) |>
  tab_style(
    style = cell_borders(sides = "left", style = "solid", color = "#3c3737"),
    locations = cells_body(
      columns = actual_rank_Overall,
      rows = name == "Ronan Dunne"
    )
  ) |>
  tab_style(
    style = cell_borders(sides = "right", style = "solid", color = "#3c3737"),
    locations = cells_body(
      columns = "delta_Mont-Sainte-Anne",
      rows = name == "Ronan Dunne"
    )
  ) |>
  data_color(
    columns = starts_with("delta_"),
    alpha = 1,
    apply_to = "text",
    fn = \(x) {
      case_when(
        x >= 0 ~ "forestgreen",
        x < 0 ~ "red",
        is.na(x) ~ "black"
      )
    }
  ) |>
  tab_header(
    title = md("## Simulated Results Table"),
    subtitle = md(
      "<span style='font-size: 1.2em; font-weight: 500; line-height: 1.5; display: inline-block; width: 80%; text-align: justify;'>
      For each race of the season, we can we see the
      actual result, simulated result, and the difference between the two.
      A negative difference,
      <span style='color:red;'><b>shown in red</b></span>,
      means their actual result was slower than their simulated result. Some riders
      such as
      <span style='background:rgba(29, 170, 40, 0.5); border: 1px solid #3c3737; padding: 1px;'><b>Troy Brosnan</b></span>
      and
      <span style='background:rgba(0, 0, 127, .3); border: 1px solid #3c3737; padding: 1px;'><b>Ronan Dunne</b></span>
      demonstrate impressive consistency across the season. Might these be a good bet for this years Vital fantasy league?
      In only two races,
      <span style='background:rgba(127, 183, 219, .7); border: 1px solid #3c3737; padding: 1px;'><b>Loudenvielle</b></span>
      and
      <span style='background:rgba(243, 211, 157, .7); border: 1px solid #3c3737; padding: 1px;'><b>Les Gets</b></span>
      might we have seen a different winner.
      <span style='background:rgba(217, 217, 217, .7); border: 1px solid #3c3737; padding: 1px;'><b>Fort William</b></span>
      was the race which showed the smallest difference between the actual and simulated worlds.
      </span>
      "
    )
  )

gtsave(
  delta_table_gt,
  "inst/scripts/delta_table_gt.png",
  zoom = 10,
  expand = 20
)

# ------------------------------------------------------------------------------
# Simulated heat maps
# - Then, we can break down the races even further by showing how things would
#   have played out across the different splits. Note that there will be no
#   narrative here and readers are encouraged to find their own stories
# ------------------------------------------------------------------------------
# TODO:
# - Consider presenting this as a grid using patchwork
# - Highlight that these heatmaps show what the top 10 for each race could have
#   been (it is fine that this is inconsistent with the overall leaderboards
#   shown so far).
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
    ),
    .by = event_name
  ) |>
  mutate(
    across(
      starts_with("section_") & !ends_with("_rank"),
      ~ .x - min(.x),
      .names = "{.col}_gap"
    ),
    .by = "event_name"
  ) |>
  mutate(
    event_name = factor(
      event_name,
      levels = c(
        "Fort William",
        "Bielsko-Biala",
        "Leogang",
        "Val di Sole",
        "Les Gets",
        "Loudenvielle",
        "Mont-Sainte-Anne"
      )
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

apply_event_colors <- function(gt_tbl, event_names) {
  reduce(
    event_names,
    \(tbl, event) {
      data_color(
        tbl,
        columns = ends_with("_gap"),
        rows = event_name == event,
        palette = c("#4daf4a", "#ffffbf", "#e41a1c")
      )
    },
    .init = gt_tbl
  )
}

heat_map_gt <- fastest_possible_splits_ranked |>
  filter(section_5_rank <= 10) |> # Top 10 each race
  # filter(name %in% actual_overall$name[1:10]) |> # Top 10 from actual overall
  left_join(image_data) |>
  select(path, name, event_name, ends_with("_gap"), ends_with("_rank")) |>
  group_by(event_name) |>
  arrange(section_5_gap, .by_group = TRUE) |>
  ungroup() |>
  gt(groupname_col = "event_name") |>
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
  apply_event_colors(unique(fastest_possible_splits_ranked$event_name)) |>
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
      "### Each split in each race is colored by split time from fastest (green) to slowest (red)"
    )
  )

gtsave(heat_map_gt, "inst/scripts/heat_map_gt.png", zoom = 10)

# ------------------------------------------------------------------------------
# Split analysis
# NOTE:
# - All of these questions step out the realm of the simulated world, and into
#   the world of actual performance. Should these feature in the next or a
#   separate article? Can we end the simulated article by asking the questions
#   featured below as a cliff-hanger? It might be a nice way to transition to
#   the next article.
# ------------------------------------------------------------------------------
# ---- Question 1 ----
# In which part of the weekend did riders perform their fastest splits?
fastest_sections_by_round <- fastest_possible_sections |>
  select(name, event_name, ends_with("_round")) |>
  pivot_longer(
    !c(name, event_name),
    names_to = "section",
    values_to = "round_type",
    names_pattern = "section_(.*)_round"
  )

# We need percentagres here, as not every rider completes every round type
total_splits_ridden_by_round <- world_cup_24_elite_men_results |>
  select(name, starts_with("split"), time, event_name, round_type) |>
  bind_rows(timed_training) |>
  count(event_name, round_type) |>
  mutate(total_splits_ridden = n * 5) |>
  select(-n)

fastest_sections_by_round |>
  count(event_name, round_type, name = "fastest_splits") |>
  left_join(total_splits_ridden_by_round) |>
  summarise(
    "percentage_fastest_splits" = fastest_splits / total_splits_ridden,
    .by = c(event_name, round_type)
  )

# The percentages are not intuitive because we have to factor in that not every
# rider makes it into each round. Better, may be to just take the riders that
# made it into the final in each race and calculate the percentages from that.
finals_list <- world_cup_24_elite_men_results |>
  filter(round_type == "Final") |>
  select(name, event_name)

fastest_sections_locations_finals_riders <- fastest_sections_by_round |>
  semi_join(finals_list) |>
  count(event_name, round_type) |>
  mutate(percentage = n / sum(n), .by = event_name)

# Insights:
# - Timed training means nothing
# - Weather obviously impacted rider speed, but in some races such as Val di Sole
#   where weather had no effect, riders still were faster in Semi-Final. I think
#   semi's destory the excitement of the race as riders build and build over the
#   weekend. It's like they are peaking too early.
fastest_sections_locations_finals_gt <- fastest_sections_locations_finals_riders |>
  mutate(
    event_name = factor(
      event_name,
      levels = c(
        "Fort William",
        "Bielsko-Biala",
        "Leogang",
        "Val di Sole",
        "Les Gets",
        "Loudenvielle",
        "Mont-Sainte-Anne"
      )
    )
  ) |>
  select(-n) |>
  pivot_wider(names_from = round_type, values_from = percentage) |>
  relocate(Qualifying, `Semi-Final`, Final, .after = `Timed Training 3`) |>
  left_join(weather_emoji) |>
  gt(rowname_col = "event_name") |>
  fmt_percent(
    columns = c(
      `Timed Training 1`,
      `Timed Training 2`,
      `Timed Training 3`,
      Qualifying,
      `Semi-Final`,
      Final
    ),
    decimals = 1
  ) |>
  tab_spanner(label = "Timed Training", columns = starts_with("Timed")) |>
  sub_missing(missing_text = "0 %") |>
  cols_label(
    "Timed Training 1" = "1",
    "Timed Training 2" = "2",
    "Timed Training 3" = "3"
  ) |>
  cols_merge(
    columns = c(Qualifying, weather_Qualifying),
    pattern = "{2}<br>{1}"
  ) |>
  cols_merge(
    columns = c(`Semi-Final`, `weather_Semi-Final`),
    pattern = "{2}<br>{1}"
  ) |>
  cols_merge(
    columns = c(Final, weather_Final),
    pattern = "{2}<br>{1}"
  ) |>
  tab_options(
    table.font.size = px(16),
    column_labels.font.weight = "bold"
  ) |>
  tab_style(
    style = cell_text(align = "left"),
    locations = cells_stub(rows = TRUE)
  ) |>
  data_color(
    palette = c("#e41a1c", "#ffffbf", "#4daf4a"),
    na_color = "white",
    direction = "row",
    columns = c(
      `Timed Training 1`,
      `Timed Training 2`,
      `Timed Training 3`,
      Qualifying,
      `Semi-Final`,
      Final
    )
  ) |>
  tab_style(
    style = cell_borders(sides = "all", style = "solid", color = "#e9e9e9"),
    locations = cells_body(columns = everything())
  ) |>
  text_transform(
    fn = \(x) str_replace(x, ".*", "*"),
    locations = cells_body(
      columns = "Semi-Final",
      rows = event_name == "Loudenvielle"
    )
  ) |>
  tab_header(
    title = md(
      "## Most riders had their fastest splits in semi-finals on average"
    ),
    subtitle = md("#### Weather conditions partially explain this trend")
  ) |>
  tab_footnote("* Race cancelled", placement = "right")

gtsave(
  fastest_sections_locations_finals_gt,
  "inst/scripts/fastest-splits-locations.png",
  zoom = 10
)

# ---- Question 2 ----
# Is there a difference between the top 10 and the test of the finals riders?
# If no, does this hint that there isn't much in it between the top and bottom?

# ---- Question 3 ----
# Which riders speed up / slow down the most during a weekend?

# ------------------------------------------------------------------------------
# gganimate races
# We can simulate the pivotal races/splits using gganimate races.
# ------------------------------------------------------------------------------
# Supplement heat maps with gganimate races to create drama of the simulated
# season.

# ------------------------------------------------------------------------------
# Closing notes
# - We need to rerun this analysis with the elite women, juniors, and the rest
#   of the elite men. Best solution is to write a shiny dashboard in python, and
#   host this as a static .html file via shinylive.
# ------------------------------------------------------------------------------
