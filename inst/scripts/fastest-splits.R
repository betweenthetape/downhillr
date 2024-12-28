# ------------------------------------------------------------------------------
# README: Create a narrative based on a new season with simulated results where
# no rider left any time on any track, and show how the season would have
# unfolded. Highlight tracks which were important in determining this simulated
# season. Highlight which sections of track showed the greatest impact. Use
# callout boxes that show insights relating to the Fantasy league throughout the
# article. Discuss who underperformed and who shows great race craft maintaing
# their consistent top speed. Make sure you include the "So whats" throughout
# the article (E.g., so what: consistency is king, it's better to not have the
# fastest pure speed, but instead be deadly consistent).
# As a general rule, each static visualisation needs a dynamic counterpart
# hosted externally for interactive exploration.
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

# ------------------------------------------------------------------------------
# TODO:
# - add timed training splits
# - get rid of Inf values
# - make sure ranks are sequential
# ------------------------------------------------------------------------------

# Should we pick the fastest result of the weekend, or their actual finals
# results, or both? The first option allows us to play out the scenario of
# what could have been on finals day. The other scenario allows us to better
# compare fastest actual lap completed when all together.
fastest_acutal_times <- world_cup_24_elite_men_results |>
  summarise(
    fastest_actual_time = min(time, na.rm = TRUE),
    .by = c(name, event_name)
  )

# Using finals only doesn't allow us to simulate all riders, as not all riders
# make it to finals. For this reason, we won't use it as it limits our analysis.
# Using fasest acutal times is also advantageous for getting a true assessment
# of how much time is left on the hill for each rider.
finals_results <- world_cup_24_elite_men_results |>
  filter(round_type == "Final") |>
  select(name, event_name, time)

fastest_possible_times <- world_cup_24_elite_men_results |>
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
  rowwise(name, event_name) |>
  summarise(
    fastest_possible_time = sum(
      c_across(starts_with("section_")),
      na.rm = TRUE
    ),
    .groups = "drop"
  )

time_left_on_tack <- fastest_acutal_times |>
  left_join(fastest_possible_times) |>
  mutate(
    time_left_on_tack = if_else(
      fastest_possible_time < fastest_actual_time, TRUE, FALSE
    )
  )

# This metric doesn't really tell us all too much other than who could have gone
# even faster. Because the winner could go even faster, it doesn't imply that
# standings would have changed. It is more of a "potential index". Still a fun
# story to tell. It could also be interesting for the riders to see. Find a nice
# way to present this in coloured gt table. You should also quantify the "time
# left on track" for each rider at each race by subtracting their fastest
# possible from their fastest actual. We could arrange this table by highest
# time left on track, per track to get a sense of ranking. This would mean
# multiple rows per rider. Perhaps we should link to externally hosted tables
# which contains all this additional data and only show key insights in the
# article. This externally hosted site is proving to be more and more important.
time_left_on_tack |>
  count(name, time_left_on_tack) |>
  mutate(percent = n / sum(n), .by = name) |>
  filter(time_left_on_tack == TRUE) |>
  select(
    name,
    percentage_races_time_left_on_track = percent,
    num_races_time_left_on_track = n
  ) |>
  print(n = Inf)

# ------------------------------------------------------------------------------
# Simulate results with fastest race runs:
# - Removes qualies/semi's and assumes riders perform perfectly
# ------------------------------------------------------------------------------
top_30_each_race <- fastest_possible_times |>
  group_by(event_name) |>
  arrange(fastest_possible_time, .by_group = TRUE) |>
  slice_head(n = 30) |>
  mutate(position = row_number()) |>
  ungroup()

# Points are only given to top 30 for each race run
finals_points <- world_cup_24_elite_men_points_scale |>
  filter(round_type == "Final") |>
  select(-round_type)

simulated_points <- top_30_each_race |>
  left_join(finals_points)

simulated_overall <- simulated_points |>
  summarise(simulated_points = sum(points), .by = name) |>
  arrange(desc(simulated_points)) |>
  mutate(simulated_rank = row_number())

actual_overall <- world_cup_24_elite_men_results |>
  summarise(actual_points = sum(points, na.rm = TRUE), .by = name) |>
  arrange(desc(actual_points)) |>
  mutate(actual_rank = row_number())

# ------------------------------------------------------------------------------
# Who left least and most time on track over the season?
# TODO:
# - Make GT table showing top/bottom 5 least/most changes
# ------------------------------------------------------------------------------
delta_scores <- simulated_overall |>
  left_join(actual_overall) |>
  mutate(delta = actual_rank - simulated_rank) |>
  select(name, delta)

# Number of ranks over where they could have placed
least_time_left <- delta_scores |>
  arrange(delta)

# Number of ranks under where they could have placed
most_time_left <- delta_scores |>
  arrange(desc(delta))

# ------------------------------------------------------------------------------
# Correlation plot
# ------------------------------------------------------------------------------
image_data <- tibble(path = dir_ls("inst/rider-images")) |>
  mutate(name = str_remove(path, "^inst/rider-images/")) |>
  mutate(name = str_remove(name, ".png$")) |>
  mutate(name = str_replace(name, "(?<=[a-z])(?=[A-Z])", " "))

# TODO:
# - Tell a story about how faces above the line could not have done any better,
#   riders under the line all left time on the track and could have faired
#   better. Think about links to fantasy league
simulated_overall |>
  left_join(actual_overall) |>
  mutate(name = str_to_title(name)) |>
  mutate(
    name = map_chr(str_split(name, " "), ~ str_c(rev(.x), collapse = " "))
  ) |>
  left_join(image_data) |>
  mutate(
    path = if_else(is.na(path), "inst/rider-images/MissingRider.png", path)
  ) |>
  ggplot(aes(x = actual_rank, y = simulated_rank)) +
  geom_point(size = 2, alpha = .7) +
  geom_image(aes(image = path), size = .09) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    linewidth = 1,
    alpha = .8,
  ) +
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
      size = 11,
      color = "#424242"
    ),
  ) +
  labs(
    title = "<span>**RIDING UP TO POTENTIAL**</span>",
    subtitle = "<span>This chart highlights which riders of the 2024 DH
      World Cup were riding up to their potential. For each track, each riders fastest splits
      from qualies, semi's, and finals were added together to calculate a hypothetical fastest
      run. Theses runs were then re-scored to create a new simulated overall leaderboard. Riders
      <span style='color:#57106e;background:red;'>**in the purple area**</span>
      are riding up to their potential (their actual ranks are better than simulated ranks).
      Riders <span style='color:#f98e09;'>**in the orange area**</span> are
      riding below their potential (their simulated ranks are better than their actual ranks).</span>",
    x = "Actual rank",
    y = "Simulated rank"
  ) +
  geom_richtext(
    data = data.frame(x = 10, y = 50),
    aes(x = x, y = y),
    label = "<span style='color:#57106e;background:red;'>**Above potential**</span>",
    label.size = NA,
    family = "sans"
  ) +
  geom_richtext(
    data = data.frame(x = 60, y = 10),
    aes(x = x, y = y),
    label = "<span style='color:#f98e09;background:red;'>**Below potential**</span>",
    label.size = NA,
    family = "sans"
  ) +
  geom_ribbon(
    stat = "function",
    fun = \(x) x,
    mapping = aes(ymin = after_stat(y), ymax = Inf),
    fill = "#57106e", alpha = 0.1
  ) +
  geom_ribbon(
    stat = "function",
    fun = \(x) x,
    mapping = aes(ymin = after_stat(y), ymax = -Inf),
    fill = "#f98e09", alpha = 0.1
  )

# ------------------------------------------------------------------------------
# Bump Chart
# Inspiration: https://tanyaviz.com/blog/bump-chart/
# ------------------------------------------------------------------------------
# TODO:
# - Recreate for other pats of the results table (e.g., top 20, bottom, etc.)
# - Recreate for single races, not just overall (and potentially facet multiple
#   races together in a single plot to tell a story).
# - One way we could recreate for every race is to have each race as a tick on
#   the x-axis, then we could start with fort william at position one, and then
#   each race is a new tick, untill we end up in overall ranks. We would have to
#   miss off actual overall ranks, or include them at the beggining or end, but
#   I think this mixture woudl be confusing. I think it's better to have two
#   separate plots, one for the whole simulated season, showing how it played
#   out, and one for the comparison between actual and simulated.
# - Think about how results link to Fantasy league. Should you pick riders
#   showing promising speed? Or is consistent and predictable better?

# Overall vs. Simulated
bump_data_overall <- simulated_overall |>
  left_join(actual_overall) |>
  mutate(name = str_to_title(name)) |>
  mutate(
    name = map_chr(str_split(name, " "), ~ str_c(rev(.x), collapse = " "))
  ) |>
  slice(1:10) |>
  select(-ends_with("_points")) |>
  pivot_longer(
    ends_with("_rank"),
    names_to = "season", values_to = "rank"
  ) |>
  mutate(season = str_remove_all(season, "_rank$")) |>
  mutate(
    season = if_else(
      season == "actual", "Actual \noverall rank", "Simulated \noverall rank"
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
    title = "<span>**TIME LEFT ON TRACK**</span>",
    subtitle = "<span>This chart highlights which top 10 riders of the 2024 DH
    World Cup left time on the track. For each track, each riders fastest splits
    from qualies, semi's, and finals were added together to calculate their fastest
    possible run. Theses runs were then re-scored and added together to create a
    new overall leaderboard. When comparing actual to simulated performance,
    <span style='color:#57106e;background:red;'>**Loic Bruni**</span>
    demonstrates not only raw speed, but unmatched consistency. Other riders like
    <span style='color:#f98e09;'>**Dakotah Norton**</span> fall up to 6 places
    behind their potential, leaving time on the track. Could these riders be a
    good bet for next season?</span>"
  )

# Just simulated season
bump_data_season <- top_30_each_race |>
  select(name, season = event_name, rank = position) |>
  filter(name %in% simulated_overall$name[1:10]) |>
  mutate(name = str_to_title(name)) |>
  mutate(
    name = map_chr(str_split(name, " "), ~ str_c(rev(.x), collapse = " "))
  ) |>
  mutate(
    season = factor(
      season,
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
  mutate(color = case_when(
    name == "Troy Brosnan" ~ "#57106e",
    name == "Dakotah Norton" ~ "#f98e09",
    TRUE ~ "#E7E7E7"
  )) |>
  left_join(image_data) |>
  mutate(
    path = if_else(is.na(path), "inst/rider-images/MissingRider.png", path)
  )

# TODO:
# - Select just a few riders to tell the story of, and only show their paths
# - Add rank numbers down the entire length of the y-axis to show positions.
# - Tweak y-axis so that it is logarithmic, or so that there is not too much
#   white space for the each dup in Loudenvielle for Dak.
ggplot() +
  geom_bump(
    aes(season, rank, group = name, color = I(color)),
    data = bump_data_season,
    linewidth = 1.5
  ) +
  geom_image(
    data = bump_data_season,
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
    data = filter(bump_data_season, season == "Fort William"),
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
    data = filter(bump_data_season, season == "Mont-Sainte-Anne"),
    nudge_x = 0.1,
    hjust = 0,
    family = "sans",
    mapping = aes(
      x = season,
      y = rank,
      label.size = NA,
      label = glue("<span style='font-size:14px;'><span style='font-size:16px;'>**{rank}**</span><span style='color:white;'>...</span>{name}</span>")
    )
  )

# ------------------------------------------------------------------------------
# gganimate races
# - Create .gif files simulating races for simulated points. These races can go
#   alongside bump charts to tell a story of the important races, and how they
#   decided how the virtual season would have unfolded.
# ------------------------------------------------------------------------------
