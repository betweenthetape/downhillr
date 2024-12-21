# ------------------------------------------------------------------------------
# TODO:
# - add timed training splits
# - get rid of Inf values
# - make sure ranks are sequential
# ------------------------------------------------------------------------------

pkgload::load_all()
library(tidyverse)
library(fs)
library(ggbump)
library(ggimage)
library(ggtext)
library(glue)

# Should we pick the fastest result of the weekend, or their actual finals
# results, or both? The first option allows us to play out the scenario of
# what could have been on finals day. The other scenario allows us to better
# compare fastest actual lap completed when all together.
fastest_acutal_times <- world_cup_24_elite_men_results |>
  summarise(
    fastest_actual_time = min(time, na.rm = TRUE),
    .by = c(name, event_name)
  )

finals_results <- world_cup_24_elite_men_results |>
  filter(round_type == "Final")

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
# possible from their fastest possible.
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

# Now let's rebuild the results table just using the fastest possible times for
# each rider over a weekend, and reattribute the points system (reverse
# engineer) it. Then calculate total points for the season. We can then compare
# that to the actual season rankings/total points. While the final
# ranking/points take into consideration points from qualies and semi's, this
# doesn't matter too much because we only want to know what could have happened
# in the best possible scenario for each rider (assuming they achieved perfect
# performance across each round_type). So let's just tally points for each rider
# assuming it was a final, and each riders fastest time was used to determine
# the final position. Let's also assume that the person who wins the final would
# would have won all the qualies/semis too. Then let's do a simple correlation
# plot and see how this virtual series compares with the actual results. Each
# point can be a riders face.
top_30_each_race <- fastest_possible_times |>
  group_by(event_name) |>
  arrange(fastest_possible_time, .by_group = TRUE) |>
  slice_head(n = 30) |>
  mutate(position = row_number()) |>
  ungroup()

finals_points <- world_cup_24_elite_men_points_scale |>
  filter(round_type == "Final") |>
  select(-round_type)

simulated_points <- top_30_each_race |>
  left_join(finals_points)

# Insight: tell a story around Dak. We all know he had the speed. This just
# proves it further. But, interesting that Bruni really is just king. He has the
# fastest pure speed across all races. Period.
simulated_overall <- simulated_points |>
  summarise(simulated_points = sum(points), .by = name) |>
  arrange(desc(simulated_points)) |>
  mutate(simulated_rank = row_number())

actual_overall <- world_cup_24_elite_men_results |>
  summarise(actual_points = sum(points, na.rm = TRUE), .by = name) |>
  arrange(desc(actual_points)) |>
  mutate(actual_rank = row_number())

# Update with rider faces/names Colour/style with Vital green branding and fonts
# Annotate with Excalidraw to show riders that left time on track. Don't
# highlight over-performers, as they might tactically ridden within limits to
# achieve the performance they did.
simulated_overall |>
  left_join(actual_overall) |>
  ggplot(aes(x = actual_rank, y = simulated_rank)) +
  geom_point(size = 2, alpha = .7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = .5) +
  labs(
    title = "Most riders did not rank in accordance with their raw speed",
    subtitle = "Points that deviate from the dotted line indicate a difference between potential and actual speed",
    x = "Actual rank",
    y = "Simulated rank"
  ) +
  theme_minimal()

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
# Bump Chart
# Inspiration: https://tanyaviz.com/blog/bump-chart/
# ------------------------------------------------------------------------------
image_data <- tibble(path = dir_ls("inst/rider-images")) |>
  mutate(name = str_remove(path, "^inst/rider-images/")) |>
  mutate(name = str_remove(name, ".png$")) |>
  mutate(name = str_replace(name, "(?<=[a-z])(?=[A-Z])", " "))

bump_data <- simulated_overall |>
  left_join(actual_overall) |>
  mutate(name = str_to_title(name)) |>
  mutate(
    name = map_chr(str_split(name, " "), ~ str_c(rev(.x), collapse = " "))
  ) |>
  slice_head(n = 10) |>
  select(-ends_with("_points")) |>
  pivot_longer(
    ends_with("_rank"),
    names_to = "season", values_to = "rank"
  ) |>
  mutate(season = str_remove_all(season, "_rank$")) |>
  mutate(
    season = if_else(
      season == "actual", "Actual \nrank", "Simulated \nrank"
    )
  ) |>
  mutate(color = case_when(
    name == "Troy Brosnan" ~ "#57106e",
    name == "Dakotah Norton" ~ "#f98e09",
    TRUE ~ "#E7E7E7"
  )) |>
  left_join(image_data)

ggplot() +
  geom_bump(
    aes(season, rank, group = name, color = I(color)),
    data = bump_data,
    linewidth = 1.5
  ) +
  geom_image(
    data = bump_data,
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
    axis.text.x = element_text(size = 10, vjust = 5),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank()
  ) +
  geom_richtext(
    data = filter(bump_data, season == "Actual \nrank"),
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
    data = filter(bump_data, season == "Simulated \nrank"),
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
    World Cup left time on the track. Simulated ranks are calculated by adding
    together each riders fastest split times from across qualies, semi's and
    finals for each race to create their fastest possible run, and
    rescoring the season using these. <span style='color:#57106e;background:red;'>**Troy Brosnan**</span>,
    demonstrates impressive consistency, leaving little time left on track.
    <span style='color:#f98e09;'>**Dakotah Norton**</span> continuously left
    time on the track, falling 6 places behind his potential.</span>"
  )

# To do:
# - Correlation plot with actual results from season
# - Make animated table that shows rows move position from actual results to
#   possible results.
# - Find riders in bottom of the table that show promise, and could be good bets
#   for the Fantasy league.
# - See notes on other questions to ask/answer from simulated season.
