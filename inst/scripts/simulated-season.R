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
    name = map_chr(str_split(name, " "), ~str_c(rev(.x), collapse = " "))
  ) |>
  mutate(across(everything(), ~if_else(is.infinite(.x), NA, .x)))

timed_training <- world_cup_24_elite_men_timed_training |>
  mutate(name = str_to_title(name)) |>
  mutate(
    name = map_chr(str_split(name, " "), ~str_c(rev(.x), collapse = " "))
  ) |>
  mutate(across(everything(), ~if_else(is.infinite(.x), NA, .x))) |>
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
      ~if_else(
        name == "Kimi Viardot" &
          event_name == "Bielsko-Biala" &
          round_type == "Timed Training 1",
        NA,
        .x
      )
    ),
    across(
      c(split_3, split_4, time),
      ~if_else(
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
#
# Notes:
# - Start write-up introducing the idea of simulating a riders fastest race run
# - Probably best to not show times at this point, as they are meaningless
#   without a comparison (which comes later)
# - Introduce why we would want to do this: see which riders left time on the
#   track and has more potential to give. See who is consistent. See which
#   tracks and sections proved most troublesome in the series.
# - Consider using GT here to build a table of actual times and splits for a
#   single rider (Dak?) and show the differences between actual and simulated.
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
    across(starts_with("section_"), ~min(.x, na.rm = TRUE)),
    .by = c(name, event_name)
  ) |>
  filter(if_all(starts_with("section_"), ~!is.infinite(.x)))

fastest_times_possible <- fastest_possible_sections |>
  rowwise(name, event_name) |>
  summarise(
    fastest_time_possible = sum(
      c_across(starts_with("section_")),
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
#   consideration changing weather and track conditions. See notes in this
#   section for more details.
# ------------------------------------------------------------------------------
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

# Percentage riders where possible faster than weekend
fastest_times_all |>
  filter(!is.na(possible_faster_than_weekend)) |>
  summarise(
    possible_faster_than_weekend = sum(
      possible_faster_than_weekend,
      na.rm = TRUE
    ),
    total_riders = n(),
    .by = event_name
  ) |>
  mutate(percentage_riders = possible_faster_than_weekend / total_riders * 100)

# Percentage riders where possible faster than finals
fastest_times_all |>
  filter(!is.na(possible_faster_than_finals)) |>
  summarise(
    possible_faster_than_finals = sum(
      possible_faster_than_finals,
      na.rm = TRUE
    ),
    total_riders = n(),
    .by = event_name
  ) |>
  mutate(percentage_riders = possible_faster_than_finals / total_riders * 100)

# Insights:
# - Only includes riders with complete data (completed full runs)
# - Fastest time of weekend is preferred over finals, because it takes into
#   consideration track conditions and weather. For example, for
#   Mont-Sainte-Anne we can see that weather deterioated over the weekend, so it
#   is no surprised that finals were slower:

world_cup_24_elite_men_results |>
  distinct(
    event_name,
    round_type,
    round_category,
    metadata_weather,
    metadata_temp_deg_c
  ) |>
  tail(3)

# - For fastest time of the weekend, across all tracks, an average of 29.6% of
#   riders could have acheived a faster time than their fastest run of the
#   weekend. This inisght also hints in the other direction, that ~70% of riders
#   are able to piece together their fastest run in a single run. Who are they?

fastest_times_all |>
  filter(!is.na(possible_faster_than_weekend)) |>
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
      total_races_completed *
      100
  ) |>
  arrange(desc(percentage_races_time_left))

# Insight:
# - It is very surprising to see Bruni in position 11. The overall champ still
#   had time to play with.

fastest_times_all |>
  filter(!is.na(possible_faster_than_finals)) |>
  summarise(
    possible_faster_than_finals = sum(
      possible_faster_than_finals,
      na.rm = TRUE
    ),
    total_races_completed = n(),
    .by = name
  ) |>
  mutate(
    percentage_races_time_left = possible_faster_than_finals /
      total_races_completed *
      100
  ) |>
  arrange(desc(percentage_races_time_left))

# Insight:
# - Again, it is better to not use finals results for a comparison because it
#   doesn't take into account weather and track conditions. It is unsurprising
#   that the percentages are higher here. For the write-up, perhaps just present
#   fastest of weekend comparison with a note that about why finals isn't used.
# - We need to be careful in this section to not present simulated season
#   results, or take too much away from the next section. This section should be
#   about validating the measure in the simplest way possible.

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

ggplot() +
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
      size = 22
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
    style='color:#f98e09;'>**Dakotah Norton**</span> climb up to 6 places,
    showing they still left time left on the hill. Could these riders be a good
    bet next season?</span>"
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
delta_all_wide |>
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

# ------------------------------------------------------------------------------
# Simulated heat maps
# - Then, we can break down the pivotal races even further by showing which
#   sections of track proved troublesome.
# ------------------------------------------------------------------------------
# TODO:
# - Consider presenting this as a grid using patchwork
# - Should the heatmaps show the simulated top 10, or the actual top 10?
#   currently it shows the former, but it doesn't create consistency with the
#   rest of the narrative
fastest_possible_splits_ranked <- fastest_possible_sections |>
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
      ~rank(.x, ties.method = "min"),
      .names = "{.col}_rank"
    ),
    .by = event_name
  ) |>
  mutate(
    across(
      starts_with("section_") & !ends_with("_rank"),
      ~.x - min(.x),
      .names = "{.col}_gap"
    ),
    .by = "event_name"
  )

# Helper function to add data colors for all sections
add_section_colors <- function(gt_tbl) {
  reduce(
    1:5,
    \(gt_tbl, x) {
      data_color(
        gt_tbl,
        columns = paste0("section_", x, "_rank"),
        target_columns = paste0("section_", x, "_gap"),
        palette = c("#4daf4a", "#ffffbf", "#e41a1c")
      )
    },
    .init = gt_tbl
  )
}

# Helper function to merge gap and rank columns
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

# Use the helper functions in the pipeline
fastest_possible_splits_ranked |>
  filter(section_5_rank <= 10) |>
  left_join(image_data) |>
  select(path, name, event_name, ends_with("_gap"), ends_with("_rank")) |>
  group_by(event_name) |>
  arrange(section_5_gap, .by_group = TRUE) |>
  ungroup() |>
  mutate(
    across(
      ends_with("_gap"),
      ~if_else(.x == 0, sprintf("%.3f", .x), paste("+", round(.x, 3)))
    )
  ) |>
  gt(groupname_col = "event_name") |>
  cols_label("path" = "", "name" = "") |>
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
    section_1_gap = "Split 1",
    section_2_gap = "Split 2",
    section_3_gap = "Split 3",
    section_4_gap = "Split 4",
    section_5_gap = "Split 5"
  ) |>
  add_section_colors() |>
  merge_section_columns() |>
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
    title = md("## Simulated Race Split Rankings"),
    subtitle = md("### Time from leader (ranked)")
  )

# ------------------------------------------------------------------------------
# Split analysis
# - From the heat maps, perform an analysis to determine which splits from which
#   races were pivotal in the virtual season. Who smoked the rest of the field?
#   Is there somewhere riders should be looking for next season (e.g., where
#   top 10 were noticebly faster than the rest of the field?)
# ------------------------------------------------------------------------------
# Question: in which part of the weekend, did most riders peform their fastest
# splits? Was it timed training, finals, etc.? Perhaps have a table breaking
# down where riders performed best on average, then again for top 10, have a
# table for each "round_type" showing which riders left the most time where.
# We could then try to answer, who speeds up/down over a weekend? I think this
# is for the actual analysis article.

# ------------------------------------------------------------------------------
# gganimate races
# We can simulate the pivotal races/splits using gganimate races.
# ------------------------------------------------------------------------------
# Supplement heat maps with gganimate races to create drama of the simulated
# season.

# ------------------------------------------------------------------------------
# Closing notes
# - We need to rerun this analysis with the elite women.
# - There is too much data to also do juniors. We could recreate all data/plots
#   and host them as interactive graphics (using crosstalk) for people to
#   explore on betweenthetape.com. This would be a good use of the website.
# ------------------------------------------------------------------------------
