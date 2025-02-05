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

image_data <- tibble(path = dir_ls("inst/rider-images")) |>
  mutate(name = str_remove(path, "^inst/rider-images/")) |>
  mutate(name = str_remove(name, ".png$")) |>
  mutate(name = str_replace(name, "(?<=[a-z])(?=[A-Z])", " "))

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
  ) |>
  filter(if_all(starts_with("section_"), ~ !is.infinite(.x)))

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
      fastest_time_possible < fastest_time_weekend, TRUE, FALSE
    )
  ) |>
  mutate(
    possible_faster_than_finals = if_else(
      fastest_time_possible < fastest_time_finals, TRUE, FALSE
    )
  )

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
# - Remove riders with incomplete data (e.g., didn't complete a full run)
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
      total_races_completed * 100
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
      total_races_completed * 100
  ) |>
  arrange(desc(percentage_races_time_left))

# Insight:
# - Again, it is better to not use finals results for a comparison because it
#   doesn't take into account weather and track conditions. It is unsurprising
#   that the percentages are higher here. For the write-up, perhaps just present
#   fastet of weekend comparison with a note that about why finals isn't used.

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
    names_to = "season", values_to = "rank"
  ) |>
  mutate(
    season = if_else(
      season == "actual_rank",
      "Actual \noverall rank",
      "Simulated \noverall rank"
    )
  ) |>
  mutate(color = case_when(
    name == "Loic Bruni" ~ "#57106e",
    name == "Dakotah Norton" ~ "#f98e09",
    TRUE ~ "#E7E7E7"
  )) |>
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
      halign = 0.5, margin = margin(b = 10, t = 15), size = 22
    ),
    plot.subtitle = element_textbox_simple(
      halign = 0,
      hjust = 0.5,
      margin = margin(b = 10),
      width = grid::unit(6, "in"),
      size = 11, color = "#424242"
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
      label = glue("<span style='font-size:14px;'>{name}<span style='color:white;'>...</span><span style='font-size:16px;'>**{rank}**</span></span>")
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
      label = glue("<span style='font-size:14px;'><span style='font-size:16px;'>**{rank}**</span><span style='color:white;'>...</span>{name}</span>")
    )
  ) +
  labs(
    title = "<span>**WHAT COULD HAVE BEEN**</span>",
    subtitle = "<span> Each riders fastest splits from across each race weekend
    were combined to simulate their fastest hypothetical run. These runs were
    then rescored to created a new overall simulated leaderboard. Even in this
    simulated world, <span style='color:#57106e;background:red;'>**Loic
    Bruni**</span> reigns king with unmatched speed. Other riders like <span
    style='color:#f98e09;'>**Dakotah Norton**</span> fall up to 6 places
    behind their potential, leaving time on the track. Could these riders be a
    good bet next season?</span>"
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
    names_from = event_name, values_from = c(simulated_rank, actual_rank, delta)
  ) |>
  left_join(image_data)

# TODO:
# - Add title and subtitle/description describing what it does/how to read
# - Add formatting/styling:
#   - I think we should just use a binary colour scale of Red/Green if it is
#     worse/better, and then use cell shading to highlight partiular results.
#     This way we can use a title with red and green text to explain the two
#     simple colours, and use background shading in the title to explain that we
#     are drawing attention to certain results. This way we need no legend for
#     the colour scale
# - In addition to the table, how about copying the distrubtion plots that are
#   used to show male/female age distrubtions back-to-back, but instead have
#   actual vs. simulated back-to-back? This could then be faceted by race. Would
#   this be a nice way to visualise a riders virtual vs actual season in a
#   single plot?
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
  opt_row_striping()

# ------------------------------------------------------------------------------
# Heat maps
# Then, we can break down the pivotal races even further by showing which
# sections of track proved troublesome.
# ------------------------------------------------------------------------------
# Next, draw heat maps using red/green across split times to show how races
# would have unfolded in the simulated season. Draw attention to where this is
# significantly different from the actual season.

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
