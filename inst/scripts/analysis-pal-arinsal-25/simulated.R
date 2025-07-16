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
library(ggforce)
# ------------------------------------------------------------------------------
# Prepare data set
# ------------------------------------------------------------------------------
results <- world_cup_25_elite_men_results |>
  mutate(name = str_to_title(name)) |>
  mutate(
    name = map_chr(str_split(name, " "), ~ str_c(rev(.x), collapse = " "))
  ) |>
  mutate(across(everything(), ~ if_else(is.infinite(.x), NA, .x))) |>
  filter(event_name == "Pal Arinsal") |>
  mutate(name = if_else(name == "Oisin Callaghan O", "Oisin O'Callaghan", name))

timed_training <- world_cup_25_elite_men_timed_training |>
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
  filter(event_name == "Pal Arinsal") |>
  mutate(name = if_else(name == "Oisin Callaghan O", "Oisin O'Callaghan", name))

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
      name == "Oisin OCallaghan" ~ "Oisin O'Callaghan",
      name == "Remi Thirion" ~ "Rémi Thirion",
      name == "Ethan Craik" ~ "Ethan George Craik",
      name == "Tuhoto ArikiPene" ~ "Tuhoto-Ariki Pene",
      name == "Luke MeierSmith" ~ "Luke Meier-Smith",
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
  filter(!is.infinite(fastest_time_weekend)) |>
  suppressWarnings()

fastest_times_final <- results |>
  filter(round_type == "Final") |>
  summarise(
    fastest_time_finals = min(time, na.rm = TRUE),
    .by = c(name, event_name)
  ) |>
  filter(!is.infinite(fastest_time_finals)) |>
  suppressWarnings()

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
  filter(if_all(starts_with("section_"), ~ !is.infinite(.x))) |>
  suppressWarnings()

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
# Table showing origin of fastest splits
# ------------------------------------------------------------------------------
# Find the riders with the largest variation across round types:
max_round_type_variations <- fastest_possible_sections |>
  select(-ends_with("_time"), -event_name) |>
  pivot_longer(cols = ends_with("_round"), names_to = "round_type") |>
  summarise(distinct_sections = n_distinct(value), .by = name) |>
  arrange(desc(distinct_sections))

max_variation_rider_name <- max_round_type_variations |>
  slice(1) |>
  pull(name)

# Show where fastest splits are for rider
fastest_split_locations <- fastest_possible_sections |>
  select(-ends_with("_time"), -event_name) |>
  pivot_longer(cols = ends_with("_round"), names_to = "round_type") |>
  filter(name == max_variation_rider_name) |>
  select(-name) |>
  deframe()

table_fastest_splits <- results |>
  select(name, starts_with("split"), time, event_name, round_type) |>
  bind_rows(timed_training) |>
  filter(name == max_variation_rider_name) |>
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
        "Timed Training 4",
        "Timed Training 5",
        "Qualifying Round 1",
        "Qualifying Round 2",
        "Final"
      )
    )
  ) |>
  relocate(time, .after = `5`) |>
  arrange(round_type) |>
  gt(rowname_col = "round_type") |>
  cols_label(time = "Time (s)") |>
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
      paste0(
        "**",
        max_variation_rider_name,
        "'s fastest sections were spread across the event**"
      )
    ),
    md("Fastest sections (in seconds) are highlighted in green")
  ) |>
  tab_source_note(
    md("_Finals & Timed Training runs 4 and 5 were not performed_")
  ) |>
  tab_style(
    style = cell_text(align = "left"),
    locations = cells_source_notes()
  ) |>
  data_color(
    columns = `1`,
    rows = round_type == fastest_split_locations[["section_1_round"]],
    palette = "#4daf4a"
  ) |>
  data_color(
    columns = `2`,
    rows = round_type == fastest_split_locations[["section_2_round"]],
    palette = "#4daf4a"
  ) |>
  data_color(
    columns = `3`,
    rows = round_type == fastest_split_locations[["section_3_round"]],
    palette = "#4daf4a"
  ) |>
  data_color(
    columns = `4`,
    rows = round_type == fastest_split_locations[["section_4_round"]],
    palette = "#4daf4a"
  ) |>
  data_color(
    columns = `5`,
    rows = round_type == fastest_split_locations[["section_5_round"]],
    palette = "#4daf4a"
  )

# gtsave(
#   table_fastest_splits,
#   "inst/scripts/analysis-pal-arinsal-25/table_fastest_splits.png",
#   zoom = 10
# )

# Simualted time
fastest_possible_times |>
  filter(name == max_variation_rider_name) |>
  pull(fastest_time_possible)

# ------------------------------------------------------------------------------
# Table showing % riders with fastest simulated results
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
      "**The count & percentage of Elite Men with faster simulation times than actual times**"
    ),
    subtitle = md(
      "Actual times determined as each riders fastest time from each event"
    )
  )

# gtsave(
#   table_possible_faster,
#   "inst/scripts/analysis-pal-arinsal-25/table_possible_faster.png",
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
      "**The top 10 Elite Men with the most time left on the track**"
    ),
    subtitle = md(
      "The time left on the track was calculated as the difference between their Finals time and their simulated fastest time"
    )
  )

# gtsave(
#   table_time_left,
#   "inst/scripts/analysis-pal-arinsal-25/table_time_left.png",
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
  filter(rank_simulated <= 30) |>
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
  mutate(
    color = case_when(
      name == "Loic Bruni" ~ "#57106e",
      name == "Amaury Pierron" ~ "#f98e09",
      TRUE ~ "#E7E7E7"
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
    title = "<span>**Elite Men: Simulated Leader Board**</span>",
    subtitle = "<span> Each riders fastest splits from across the Pal Arinsal
    race weekend were combined to simulate their fastest hypothetical runs.
    These runs were then ranked to create a new simulated leaderboard.
    <span style='color:#57106e;'>**Loic Bruni**</span> demonstrates why he is the
    king of Andorra: even if Jackson could have put in his best run, these
    simulations hint that Loic may have still been unbeatable here.
    <span style='color:#f98e09;'>**Amaury Pierron**</span> was showing
    blistering pace, especially for a man recovering from surgery! A small
    mistake ruined his chance for what looked set to be a top 5 finish.</span>",
    caption = "Missing ranks due to DNS, DNQ, DNF, or omitted low-ranking riders."
  )

# ggsave(
#   "inst/scripts/analysis-pal-arinsal-25/plot_bump.png",
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
    title = md("## Elite Men: Simulated Race Split Times and Rankings"),
    subtitle = md(
      "### Each split in seconds (with rank in brackets) is colored by split time from fastest (green) to slowest (red)"
    )
  )

# gtsave(
#   table_heat_map,
#   "inst/scripts/analysis-pal-arinsal-25/table_heat_map.png",
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
    title = md("## Simulated Combined Fastest Race Run"),
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
#   "inst/scripts/analysis-pal-arinsal-25/table_combined_run.png",
#   zoom = 10
# )

# ------------------------------------------------------------------------------
# Which section was won by the biggest margin, relative to its length?
# ------------------------------------------------------------------------------
all_section_times <- results |>
  select(name, starts_with("split"), time, event_name, round_type) |>
  mutate(
    section_1 = split_1,
    section_2 = split_2 - split_1,
    section_3 = split_3 - split_2,
    section_4 = split_4 - split_3,
    section_5 = time - split_4
  )

final_section_times <- all_section_times |>
  filter(round_type == "Final") |>
  slice(-28:-30) |>
  select(name, starts_with("section_")) |>
  pivot_longer(
    cols = starts_with("section_"),
    names_to = "section",
    values_to = "time"
  ) |>
  mutate(section = str_extract(section, "\\d+"))

final_section_times_from_leader <- final_section_times |>
  mutate(fastest_section_time = min(time), .by = section) |>
  mutate(time_from_leader = time - fastest_section_time)

plot_ridges <- final_section_times_from_leader |>
  filter(time_from_leader < 7) |>
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
    title = "Elite Men: Distribution of Time Gaps from Leader Across Sections \nin Finals",
    subtitle = "Outlier time gaps > 7s (e.g., due to crashes) removed for clarity.",
    y = "Race Section",
    x = "Time from leader (s)"
  )

# ggsave(
#   "inst/scripts/analysis-pal-arinsal-25/plot_ridges.png",
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

# ---- Additional Questions ----------------------------------------------------
# Could Davide Palazzari have won if we didn't crash?
bruni_final_time <- fastest_times_final |>
  slice(1) |>
  pull(fastest_time_finals)

palazzari_splits_1_4 <- c(37.319, 82.524, 112.061, 138.703)

bruni_palazzari_splits_ranked <- fastest_possible_sections |>
  select(-ends_with("_round")) |>
  rename_with(\(x) str_remove_all(x, "_time"), starts_with("section_")) |>
  filter(name == "Loic Bruni") |>
  rowwise() |>
  mutate(
    section_2 = section_1 + section_2,
    section_3 = section_2 + section_3,
    section_4 = section_3 + section_4,
    section_5 = section_4 + section_5
  ) |>
  ungroup() |>
  add_row(
    name = "Davide Palazzari",
    event_name = "Pal Arinsal",
    section_1 = 37.319,
    section_2 = 82.524,
    section_3 = 112.061,
    section_4 = 138.703,
    section_5 = NA,
  ) |>
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

table_bruni_palazzari <- bruni_palazzari_splits_ranked |>
  left_join(image_data) |>
  select(path, name, ends_with("_gap"), ends_with("_rank")) |>
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
    title = md("## Could Palazzari have beaten Bruni?"),
    subtitle = md(
      "### The gap (with rank in brackets) is shown for each section in Finals until split 4"
    )
  ) |>
  data_color(
    columns = section_1_gap,
    rows = name == 'Loic Bruni',
    palette = "#4daf4a"
  ) |>
  data_color(
    columns = section_2_gap,
    rows = name == 'Loic Bruni',
    palette = "#e41a1c"
  ) |>
  data_color(
    columns = section_3_gap,
    rows = name == 'Loic Bruni',
    palette = "#e41a1c"
  ) |>
  data_color(
    columns = section_4_gap,
    rows = name == 'Loic Bruni',
    palette = "#4daf4a"
  ) |>
  data_color(
    columns = section_1_gap,
    rows = name == 'Davide Palazzari',
    palette = "#e41a1c"
  ) |>
  data_color(
    columns = section_2_gap,
    rows = name == 'Davide Palazzari',
    palette = "#4daf4a"
  ) |>
  data_color(
    columns = section_3_gap,
    rows = name == 'Davide Palazzari',
    palette = "#4daf4a"
  ) |>
  data_color(
    columns = section_4_gap,
    rows = name == 'Davide Palazzari',
    palette = "#e41a1c"
  ) |>
  text_transform(
    locations = cells_body(
      columns = section_5_gap,
      rows = name == "Loic Bruni"
    ),
    fn = function(x) "?"
  ) |>
  text_transform(
    locations = cells_body(
      columns = section_5_gap,
      rows = name == "Davide Palazzari"
    ),
    fn = function(x) "?"
  )

# gtsave(
#   table_bruni_palazzari,
#   "inst/scripts/analysis-pal-arinsal-25/table_bruni_palazzari.png",
#   zoom = 10
# )

palazzari_fastest_split_5 <- fastest_possible_sections |>
  filter(name == "Davide Palazzari") |>
  pull(section_5_time)

palazzari_mean_decrease <- fastest_possible_sections |>
  filter(name == "Davide Palazzari") |>
  select(ends_with("_time")) |>
  mutate(id = "fastest_splits", .before = section_1_time) |>
  add_row(
    id = "final_splits",
    section_1_time = palazzari_splits_1_4[1],
    section_2_time = palazzari_splits_1_4[2] - palazzari_splits_1_4[1],
    section_3_time = palazzari_splits_1_4[3] - palazzari_splits_1_4[2],
    section_4_time = palazzari_splits_1_4[4] - palazzari_splits_1_4[3],
    section_5_time = NA
  ) |>
  select(-section_5_time) |>
  pivot_longer(!id) |>
  pivot_wider(names_from = id, values_from = value) |>
  mutate(
    percentage_difference = (fastest_splits - final_splits) /
      fastest_splits
  ) |>
  summarise(mean_difference = mean(percentage_difference)) |>
  pull()

# Scenario 1: Actual finals split times from 1-4 + simulated 5th
palazzari_simulated_finish <- palazzari_splits_1_4[4] +
  palazzari_fastest_split_5

print(bruni_final_time)
print(palazzari_simulated_finish)

# Scenario 2: Actual finals split times from 1-4 + simulated 5th * expected % increase
palazzari_simulated_finish_decreased <- palazzari_splits_1_4[4] +
  (palazzari_fastest_split_5 * (1 - palazzari_mean_decrease))

print(bruni_final_time)
print(palazzari_simulated_finish_decreased)
print(bruni_final_time - palazzari_simulated_finish_decreased)

# Did Andorra have the tightest winning margins this year?
# What about the previous two years?
gaps <- world_cup_25_elite_men_results |>
  select(rank, round_type, event_name, time_from_leader) |>
  filter(round_type == "Final") |>
  filter(rank == 5 | rank == 10 | rank == 20) |>
  mutate(
    race_group = case_when(
      rank == 5 ~ "Top 5",
      rank == 10 ~ "Top 10",
      rank == 20 ~ "Top 20",
    ),
    .by = event_name
  )

table_race_gaps <- gaps |>
  select(race_group, Event = event_name, gap = time_from_leader) |>
  mutate(
    race_group = factor(
      race_group,
      levels = c("Top 5", "Top 10", "Top 20")
    )
  ) |>
  arrange(race_group, gap) |>
  pivot_wider(names_from = Event, values_from = gap) |>
  gt() |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) |>
  opt_row_striping() |>
  tab_options(
    table.font.size = px(12),
    column_labels.font.weight = "bold"
  ) |>
  cols_label(race_group = "") |>
  data_color(
    columns = Leogang,
    rows = race_group == 'Top 5',
    palette = "#4daf4a"
  ) |>
  data_color(
    columns = Leogang,
    rows = race_group == 'Top 10',
    palette = "#4daf4a"
  ) |>
  data_color(
    columns = Leogang,
    rows = race_group == 'Top 20',
    palette = "#4daf4a"
  ) |>
  data_color(
    columns = `Val di Sole`,
    rows = race_group == 'Top 5',
    palette = "#e41a1c"
  ) |>
  data_color(
    columns = `Val di Sole`,
    rows = race_group == 'Top 10',
    palette = "#e41a1c"
  ) |>
  data_color(
    columns = `La Thuile`,
    rows = race_group == 'Top 20',
    palette = "#e41a1c"
  ) |>
  tab_header(
    title = md(
      "**Leogang is so far the tighest race in 2025 (green) with La Thuile and Val di Sole being the least (red)**"
    ),
    subtitle = md("Values in cells show the gap from the leader in seconds")
  )

# gtsave(
#   table_race_gaps,
#   "inst/scripts/analysis-pal-arinsal-25/table_race_gaps.png",
#   zoom = 10
# )

plot_race_gaps <- world_cup_25_elite_men_results |>
  select(rank, round_type, event_name, time_from_leader) |>
  filter(round_type == "Final") |>
  filter(!is.na(time_from_leader)) |>
  filter(time_from_leader <= 50) |>
  mutate(
    event_name = factor(
      event_name,
      levels = c(
        "Bielsko-Biala",
        "Loudenvielle",
        "Leogang",
        "Val di Sole",
        "La Thuile",
        "Pal Arinsal"
      )
    )
  ) |>
  ggplot(aes(x = event_name, y = time_from_leader)) +
  geom_violin(alpha = 0.01, linewidth = 0.1) +
  geom_sina(alpha = 0.7, size = 2, color = "steelblue") +
  labs(
    title = "Spread of Final times in Elite Men",
    subtitle = "Gaps more than 50s (e.g., due to crashes) have been omitted",
    x = NULL,
    y = "Time from Leader (s)"
  ) +
  theme_ridges() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 14, face = "bold"),
    panel.grid.minor = element_blank()
  )

# ggsave(
#   "inst/scripts/analysis-pal-arinsal-25/plot_race_gaps.png",
#   plot = plot_race_gaps,
#   width = 2200,
#   height = 1800,
#   units = "px",
#   bg = "white",
#   limitsize = FALSE,
#   dpi = 330
# )
