library(tidyverse)

world_cup_24_elite_men_points_scale <- tibble(
  round_type = rep(c("Final", "Qualifying", "Semi-Final"), c(30L, 20L, 60L)),
  points = c(
    250, 210, 180, 160, 140, 125, 110, 95, 80, 75, 71, 68, 65, 63, 60, 58, 56,
    54, 52, 50, 48, 46, 44, 42, 40, 38, 36, 34, 32, 30, 50, 40, 30, 25, 22, 20,
    18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 100, 80, 70, 65, 60, 58,
    56, 54, 52, 50, 49, 48, 47, 46, 45, 44, 43, 42, 41, 40, 39, 38, 37, 36, 35,
    34, 33, 32, 31, 30, 29, 28, 27, 26, 25, 24, 23, 22, 21, 20, 19, 18, 17, 16,
    15, 14, 13, 12, 11, 10, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5
  ),
) |>
  mutate(position = row_number(), .by = round_type, .after = round_type)


usethis::use_data(world_cup_24_elite_men_points_scale, overwrite = TRUE)
