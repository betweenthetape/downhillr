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
fastest_acutal_time_weekend <- world_cup_24_elite_men_results |>
  summarise(
    fastest_actual_time = min(time, na.rm = TRUE),
    .by = c(name, event_name)
  )

finals_results <- world_cup_24_elite_men_results |>
  filter(round_type == "Final")

fastest_possible_time_weekend <- world_cup_24_elite_men_results |>
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

time_left_on_tack <- fastest_acutal_time_weekend |>
  left_join(fastest_possible_time_weekend) |>
  mutate(
    time_left_on_tack = if_else(
      fastest_possible_time < fastest_actual_time, TRUE, FALSE
    )
  )

# This metric doesn't really tell us all too much other than who could have gone
# even faster. Because the winner could go even faster, it doesn't imply that
# standings would have changed. It is more of a "potential index". Still a fun
# story to tell. It could also be interesting for the riders to see. Find a nice
# way to present this in coloured gt table. Calculate actual "time left on track"
# for each rider at each race.
time_left_on_tack |>
  count(name, time_left_on_tack) |>
  mutate(percent = n / sum(n), .by = name) |>
  filter(time_left_on_tack == TRUE) |>
  select(name, percentage_races_time_left_on_track = percent, num_races = n) |>
  print(n = Inf)

# Now let's rebuild the results table just using the fastest possible times for
# each rider over a weekend, and reattribute the points system (reverse
# engineer) it. The calculate total points for the season. We can then compare
# that to the actual season rankings/total points. While the final
# ranking/points take into consideration poitns from qualies and semi's, this
# doesn't matter too much because we only want to know what could have happened
# in the best possible scenario for each rider (assuming they achieved perfect
# performance across each round_type).
