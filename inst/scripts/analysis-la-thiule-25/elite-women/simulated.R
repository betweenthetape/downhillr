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
library(ggridges)

# ------------------------------------------------------------------------------
# Prepare data sets
# ------------------------------------------------------------------------------
results <- world_cup_25_elite_women_results |>
  mutate(name = str_to_title(name)) |>
  mutate(
    name = map_chr(str_split(name, " "), ~ str_c(rev(.x), collapse = " "))
  ) |>
  mutate(across(everything(), ~ if_else(is.infinite(.x), NA, .x))) |>
  filter(event_name == "La Thiule") |>
  mutate(name = if_else(name == "Nina Hofmann", "Nina Hoffmann", name))

timed_training <- world_cup_25_elite_women_timed_training |>
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
  filter(event_name == "La Thiule") |>
  mutate(name = if_else(name == "Nina Hofmann", "Nina Hoffmann", name))

# Check that later splits are always larger than earlier splits to catch any
# data entry errors
timed_training <- timed_training |>
  mutate(
    split_2 = if_else(!is.na(split_1) & split_2 <= split_1, NA, split_2),
    split_3 = if_else(!is.na(split_2) & split_3 <= split_2, NA, split_3),
    split_4 = if_else(!is.na(split_3) & split_4 <= split_3, NA, split_4),
    time = if_else(!is.na(split_4) & time <= split_4, NA, time)
  )

image_data <- tibble(path = dir_ls("inst/rider-images")) |>
  mutate(name = str_remove(path, "^inst/rider-images/")) |>
  mutate(name = str_remove(name, ".png$")) |>
  mutate(name = str_replace(name, "(?<=[a-z])(?=[A-Z])", " ")) |>
  mutate(
    name = case_when(
      name == "Vali Holl" ~ "Valentina Höll",
      name == "Louise Ferguson" ~ "Louise-Anna Ferguson",
      name == "Valentina RoaSanchez" ~ "Valentina Sanchez Roa",
      name == "Siel VanderVelden" ~ "Siel Velden Der Van",
      name == "Lais Bonnaure" ~ "Laïs Bonnaure",
      .default = name
    )
  )

image_data <- image_data |>
  right_join(results) |>
  distinct(path, name) |>
  mutate(path = coalesce(path, "inst/rider-images/MissingRider.png"))

# ------------------------------------------------------------------------------
# Fastest actual times
# ------------------------------------------------------------------------------
fastest_times_weekend <- results |>
  select(name, starts_with("split"), time, event_name, round_type) |>
  bind_rows(timed_training) |>
  summarise(
    fastest_time_weekend = min(time, na.rm = TRUE),
    .by = c(name, event_name)
  ) |>
  filter(!is.infinite(fastest_time_weekend))

fastest_times_final <- results |>
  filter(round_type == "Final") |>
  summarise(
    fastest_time_finals = min(time, na.rm = TRUE),
    .by = c(name, event_name)
  ) |>
  filter(!is.infinite(fastest_time_finals))

# ------------------------------------------------------------------------------
# Fastest Possible times
# ------------------------------------------------------------------------------
fastest_possible_sections <- results |>
  select(name, starts_with("split"), time, event_name, round_type) |>
  bind_rows(timed_training) |>
  mutate(
    section_1 = split_1,
    section_2 = split_2 - split_1,
    section_3 = split_3 - split_2,
    section_4 = split_4 - split_3,
    section_5 = time - split_4
  ) |>
  reframe(
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

all_section_times <- results |>
  select(name, starts_with("split"), time, event_name, round_type) |>
  mutate(
    section_1 = split_1,
    section_2 = split_2 - split_1,
    section_3 = split_3 - split_2,
    section_4 = split_4 - split_3,
    section_5 = time - split_4
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
      "**The count & percentage of Elite Women with faster simulation times than actual times**"
    ),
    subtitle = md(
      "Actual times determined as each riders fastest time from each event"
    )
  )

# gtsave(
#   table_possible_faster,
#   "inst/scripts/analysis-la-thiule-25/elite-women/table_possible_faster.png",
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
    fastest_time_finals = "Finals Time (s)",
    fastest_time_possible = "Simulated Fastest Time (s)",
    time_left = "Time Left on Track (s)"
  ) |>
  tab_header(
    title = md(
      "**The top 10 Elite Women with the most time left on the track**"
    ),
    subtitle = md(
      "The time left on the track was calculated as the difference between their Finals time and their simulated fastest time"
    )
  )

# gtsave(
#   table_time_left,
#   "inst/scripts/analysis-la-thiule-25/elite-women/table_time_left.png",
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
      "Actual \nrank",
      "Simulated \nrank"
    )
  ) |>
  left_join(image_data)

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
  scale_x_discrete(position = "top") +
  theme_minimal() +
  theme(
    plot.title = element_textbox_simple(
      halign = 0.5,
      margin = margin(b = 10, t = 15),
      size = 20
    ),
    plot.subtitle = element_textbox_simple(
      halign = 0,
      hjust = 0.5,
      margin = margin(b = 10),
      width = grid::unit(8, "in"),
      size = 14,
      color = "#424242"
    ),
    axis.text.x = element_text(size = 12, vjust = 2, face = "bold"),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank()
  ) +
  geom_richtext(
    data = filter(bump_data, type == "Actual \nrank"),
    hjust = 1,
    nudge_x = -0.1,
    mapping = aes(
      x = type,
      y = rank,
      label.size = NA,
      label = glue(
        "<span style='font-size:14px;'>{name}<span style='color:white;'>...</span><span style='font-size:16px;'>**{rank}**</span></span>"
      )
    )
  ) +
  geom_richtext(
    data = filter(bump_data, type == "Simulated \nrank"),
    nudge_x = 0.1,
    hjust = 0,
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
    title = "<span>**Elite Women: Simulated Leader Board**</span>",
    subtitle = "<span> Each riders fastest splits from across the La Thiule
    race weekend were combined to simulate their fastest hypothetical runs.
    These runs were then ranked to create a new simulated leaderboard.
    Even in the simulated world, the results would have been the same for the 
    podium spots. The mains differences would have been a shuffle at the bottom of the
    table.",
    caption = "The missing simulated ranks 15 & 16 correspond to Valentina Sanchez Roa & Louise-Anna Ferguson, who did not qualify"
  )

# ggsave(
#   "inst/scripts/analysis-la-thiule-25/elite-women/plot_bump.png",
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
    title = md("## Elite Women: Simulated Race Split Times and Rankings"),
    subtitle = md(
      "### Each split in seconds (with rank in brackets) is colored by split time from fastest (green) to slowest (red)"
    )
  )

# gtsave(
#   table_heat_map,
#   "inst/scripts/analysis-la-thiule-25/elite-women/table_heat_map.png",
#   zoom = 10
# )

# ------------------------------------------------------------------------------
# Fastest run comparison
# ------------------------------------------------------------------------------
fastest_combined_run <- fastest_possible_sections |>
  select(name, ends_with("time")) |>
  pivot_longer(
    cols = ends_with("_time"),
    names_to = "section",
    values_to = "time"
  ) |>
  filter(time == min(time), .by = section) |>
  arrange(section) |>
  mutate(section = str_extract(section, "\\d+"))

table_combined_run <- fastest_combined_run |>
  left_join(image_data) |>
  relocate(path) |>
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
    section = "Section",
    time = "Time (s)"
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
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = "name")
  ) |>
  tab_header(
    title = md("## Elite Women: Simulated Combined Fastest Race Run"),
    subtitle = md(
      "### Combining the fastest section times achieved by any rider to create the ultimate run"
    )
  ) |>
  fmt_number(time, decimals = 2) |>
  grand_summary_rows(
    fns = list(
      "Total Time (s)" = ~ sum(.x, na.rm = TRUE)
    ),
    columns = time,
    fmt = ~ fmt_number(., decimals = 2)
  ) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_grand_summary(columns = "time")
  )

# gtsave(
#   table_combined_run,
#   "inst/scripts/analysis-la-thiule-25/elite-women/table_combined_run.png",
#   zoom = 10
# )

# ------------------------------------------------------------------------------
# Which section was won by the biggest margin, relative to its length?
# ------------------------------------------------------------------------------
final_section_times <- all_section_times |>
  filter(round_type == "Final") |>
  select(name, starts_with("section_")) |>
  pivot_longer(
    cols = starts_with("section_"),
    names_to = "section",
    values_to = "time"
  ) |>
  mutate(section = str_extract(section, "\\d+")) |>
  filter(!is.na(time))

final_section_times_from_leader <- final_section_times |>
  mutate(fastest_section_time = min(time), .by = section) |>
  mutate(time_from_leader = time - fastest_section_time)

plot_ridges <- final_section_times_from_leader |>
  filter(time_from_leader < 8) |>
  ggplot(aes(x = time_from_leader, y = section, fill = section)) +
  geom_density_ridges(scale = 2, alpha = .5) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(
    expand = c(0, 0),
    breaks = seq(0, 7, by = 1)
  ) +
  coord_cartesian(clip = "off") +
  theme_ridges() +
  scale_fill_viridis_d(option = "C", begin = .3, end = .8, guide = "none") +
  labs(
    title = "Elite Women: Distribution of Time Gaps from Leader Across \nSections in Finals",
    subtitle = "Outlier time gaps > 8s (e.g., due to crashes) removed for clarity.",
    y = "Race Section",
    x = "Time from leader (s)"
  )

# ggsave(
#   "inst/scripts/analysis-la-thiule-25/elite-women/plot_ridges.png",
#   plot = plot_ridges,
#   width = 2200,
#   height = 1800,
#   units = "px",
#   bg = "white",
#   limitsize = FALSE,
#   dpi = 330
# )

final_section_times_from_leader |>
  select(name, section, time_from_leader) |>
  slice_min(time_from_leader, n = 2, by = section)
