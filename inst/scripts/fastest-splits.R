# ------------------------------------------------------------------------------
# Questions to answer:
# - Is there a difference between fastest possible time and actual results
#   achieved? If so, how would it have changed the points for each race? How
#   would the overall series have changed if they had not made mistakes? Is
#   there a difference between the top and bottom of the table? Are there any
#   stand out races where the results would clearly have been massively
#   different? How would these unique races have affected the overall? Are there
#   any riders showing huge potential that are not making it on the box because
#   they can't put it together on race day? If the answer is no, this is also an
#   interesting story. The fastest racers really are the fastest split to split,
#   not just the most consistent. This implies training for both out and out
#   speed is just as important as consistency. If the story is reversed, we can
#   highlight certain riders who are on the edge of briiliance (Dakota?) but
#   need to work on consistency.
# - Who is the most consistent rider across a race weekend?
# - Who ramps up speed the most across the race weekend and shows the biggest
#   improvements?
# - For any given racer, who gets faster and faster in a single race run,
#   impying they pace well. Who gets slower and slower across a race run,
#   implying they pace badly and fatigue. Could the good pacers give a little
#   more at the start? To calculate this, you could find the fastest time
#   achieved for any split, and then grade all riders across that fastest time
#   to see where they are loosing time compared to the fastest rider. You could
#   either do this with a discrete scale using ranks, or preferaby a continuous
#   scale using time from fastest for each split.
# - For the questions above, can we use a continous red/green colour scale in
#   combination with gganimate to simulate races? Can we use GT to create tables
#   with rider faces, colours, team logos etc. Make it fun an interactive.
# ------------------------------------------------------------------------------

# TODO:
# - add times training splits
# - get rid of Inf values
# - make sure ranks are sequntial

pkgload::load_all()
library(tidyverse)

# Should we pick the fastest result of the weekend, or their actual finals
# results, or both? The first option allows us to play out the scenario of
# what could have been on finals day. The other scenario allows us to better
# compare fastest actual lap completed when all together.
fastest_acutal_time <- world_cup_24_fort_william_men_elite_results |>
  summarise(fastest_actual_time = min(time, na.rm = TRUE), .by = name)

fastest_possible_time <- world_cup_24_fort_william_men_elite_results |>
  select(name, starts_with("split"), time, round_type) |>
  mutate(
    split_1_diff = split_1,
    split_2_diff = split_2 - split_1,
    split_3_diff = split_3 - split_2,
    split_4_diff = split_4 - split_3,
    split_5_diff = time - split_4
  ) |>
  summarise(
    across(ends_with("_diff"), ~ min(.x, na.rm = TRUE)),
    .by = name
  ) |>
  rowwise(name) |>
  summarise(
    fastest_possible_time = sum(c_across(starts_with("split_")), na.rm = TRUE),
    .groups = "drop"
  )

fastest_acutal_time |>
  left_join(fastest_possible_time) |>
  mutate(
    actual_rank = rank(fastest_actual_time, ties.method = "min"),
    possible_rank = rank(fastest_possible_time, ties.method = "min"),
    under_performed = if_else(possible_rank < actual_rank, TRUE, FALSE)
  ) |>
  arrange(actual_rank) |>
  print(n = Inf)
