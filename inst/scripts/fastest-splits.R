# ------------------------------------------------------------------------------
# TODO:
# - add timed training splits
# - get rid of Inf values
# - make sure ranks are sequential
# ------------------------------------------------------------------------------

pkgload::load_all()
library(tidyverse)

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
  summarise(points = sum(points), .by = name) |>
  arrange(desc(points))

# To do:
# - Correlation plot with actual results from season
# - Make animated table that shows rows move position from actual results to
#   possible results.
# - Find riders in bottom of the table that show promise, and could be good bets
#   for the Fantasy league.
# - See notes on other questions to ask/answer from simulated season.
