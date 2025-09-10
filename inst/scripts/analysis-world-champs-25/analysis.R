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
# Prepare data sets
# ------------------------------------------------------------------------------
results <- world_championships_25 |>
  mutate(name = str_to_title(name)) |>
  mutate(
    name = map_chr(str_split(name, " "), ~ str_c(rev(.x), collapse = " "))
  ) |>
  mutate(across(everything(), ~ if_else(is.infinite(.x), NA, .x))) |>
  mutate(
    name = if_else(name == "Oisin Callaghan O", "Oisin O'Callaghan", name)
  ) |>
  mutate(
    section_1 = split_1,
    section_2 = split_2 - split_1,
    section_3 = split_3 - split_2,
    section_4 = split_4 - split_3,
    section_5 = time - split_4,
    .after = time_from_leader
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
      name == "Vali Holl" ~ "Valentina Höll",
      .default = name
    )
  )

image_data <- image_data |>
  right_join(results) |>
  distinct(path, name) |>
  mutate(path = coalesce(path, "inst/rider-images/MissingRider.png"))

# ------------------------------------------------------------------------------
# The Ultimate Run
# ------------------------------------------------------------------------------
# - Elite men -
men_ultimate_run <- results |>
  filter(round_category == "Men Elite") |>
  select(name, starts_with("section_")) |>
  pivot_longer(
    cols = starts_with("section_"),
    names_to = "section",
    values_to = "time"
  ) |>
  filter(time == min(time, na.rm = TRUE), .by = section) |>
  arrange(section) |>
  mutate(section = str_extract(section, "\\d+"))

men_table_ultimate_run <- men_ultimate_run |>
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
    title = md("## The Ultimate Race Run"),
    subtitle = md(
      "### Combining the fastest section times achieved by any rider"
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

gtsave(
  men_table_ultimate_run,
  "inst/scripts/analysis-world-champs-25/men_table_ultimate_run.png",
  zoom = 10
)

# Jackson's actual time: 174.153
results |>
  filter(name == "Jackson Goldstone") |>
  pull(time)

# Ultimate time: 173.45
men_ultimate_run |>
  summarise(time = sum(time)) |>
  pull(time)

# - Elite Women -
women_ultimate_run <- results |>
  filter(round_category == "Women Elite") |>
  select(name, starts_with("section_")) |>
  pivot_longer(
    cols = starts_with("section_"),
    names_to = "section",
    values_to = "time"
  ) |>
  filter(time == min(time, na.rm = TRUE), .by = section) |>
  arrange(section) |>
  mutate(section = str_extract(section, "\\d+"))

women_table_ultimate_run <- women_ultimate_run |>
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
    title = md("## The Ultimate Race Run"),
    subtitle = md(
      "### Combining the fastest section times achieved by any rider"
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

gtsave(
  women_table_ultimate_run,
  "inst/scripts/analysis-world-champs-25/women_table_ultimate_run.png",
  zoom = 10
)

# Jackson's actual time: 204.715
results |>
  filter(name == "Valentina Höll") |>
  pull(time)

# Ultimate time: 173.45
women_ultimate_run |>
  summarise(time = sum(time)) |>
  pull(time)

# ------------------------------------------------------------------------------
# Distribution: points
# ------------------------------------------------------------------------------
# - Elite men -
men_time_from_leader <- results |>
  filter(round_category == "Men Elite") |>
  slice(1:30) |>
  select(name, starts_with("section_")) |>
  mutate(
    across(
      .cols = starts_with("section_"),
      .fns = ~ .x - min(.x, na.rm = TRUE)
    )
  ) |>
  pivot_longer(
    !name,
    names_to = "section",
    values_to = "time_from_leader"
  ) |>
  mutate(section = str_extract(section, "\\d+"))

men_plot_violin <- men_time_from_leader |>
  ggplot(aes(x = section, y = time_from_leader)) +
  geom_violin(
    alpha = 0.3,
    linewidth = 0,
    fill = "grey"
  ) +
  geom_sina(alpha = 0.7, size = 3, color = "steelblue") +
  labs(
    title = "Jackson secured the race in section three",
    subtitle = "Spread of Time Gaps in Top 30 Elite Men",
    x = "Section",
    y = "Time from Leader (s)"
  ) +
  theme_ridges()

ggsave(
  "inst/scripts/analysis-world-champs-25/men_plot_violin.png",
  plot = men_plot_violin,
  width = 2200,
  height = 1800,
  units = "px",
  bg = "white",
  limitsize = FALSE,
  dpi = 330
)

# Examine gaps
men_time_from_leader |>
  group_by(section) |>
  arrange(time_from_leader, .by_group = TRUE) |>
  slice(1:2)

# - Elite Women -
women_time_from_leader <- results |>
  filter(round_category == "Women Elite") |>
  slice(1:30) |>
  select(name, starts_with("section_")) |>
  mutate(
    across(
      .cols = starts_with("section_"),
      .fns = ~ .x - min(.x, na.rm = TRUE)
    )
  ) |>
  pivot_longer(
    !name,
    names_to = "section",
    values_to = "time_from_leader"
  ) |>
  mutate(section = str_extract(section, "\\d+"))

women_plot_violin <- women_time_from_leader |>
  ggplot(aes(x = section, y = time_from_leader)) +
  geom_violin(
    alpha = 0.3,
    linewidth = 0,
    fill = "grey"
  ) +
  geom_sina(alpha = 0.7, size = 3, color = "steelblue") +
  labs(
    title = "Valetina secured the race in section two",
    subtitle = "Spread of Time Gaps in Top 30 Elite Women",
    x = "Section",
    y = "Time from Leader (s)"
  ) +
  theme_ridges()

ggsave(
  "inst/scripts/analysis-world-champs-25/women_plot_violin.png",
  plot = women_plot_violin,
  width = 2200,
  height = 1800,
  units = "px",
  bg = "white",
  limitsize = FALSE,
  dpi = 330
)

# Examine gaps
women_time_from_leader |>
  group_by(section) |>
  arrange(time_from_leader, .by_group = TRUE) |>
  slice(1:2)

# ------------------------------------------------------------------------------
# Story of the race: ridgeline plot telling different story of points.
# ------------------------------------------------------------------------------
#- Elite men -
men_plot_ridges <- men_time_from_leader |>
  # filter(time_from_leader < 7) |>
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
    y = "Race Section",
    x = "Time from leader (s)"
  )

ggsave(
  "inst/scripts/analysis-world-champs-25/men_plot_ridges.png",
  plot = men_plot_ridges,
  width = 2200,
  height = 1800,
  units = "px",
  bg = "white",
  limitsize = FALSE,
  dpi = 330
)

# - Elite women -
women_plot_ridges <- women_time_from_leader |>
  # filter(time_from_leader < 7) |>
  ggplot(aes(x = time_from_leader, y = section, fill = section)) +
  geom_density_ridges(scale = 2, alpha = .5) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_ridges() +
  scale_fill_viridis_d(option = "C", begin = .3, end = .8, guide = "none") +
  labs(
    title = "Elite Women: Distribution of Time Gaps from Leader Across Sections \nin Finals",
    y = "Race Section",
    x = "Time from leader (s)"
  )

ggsave(
  "inst/scripts/analysis-world-champs-25/women_plot_ridges.png",
  plot = women_plot_ridges,
  width = 2200,
  height = 1800,
  units = "px",
  bg = "white",
  limitsize = FALSE,
  dpi = 330
)

# ------------------------------------------------------------------------------
# Young podium: did age predict performance?
# ------------------------------------------------------------------------------
results_w_age <- results |>
  mutate(
    age = as.numeric(event_year) - as.numeric(yob),
    .after = yob
  ) |>
  left_join(image_data)

# - Elite Men -
men_plot_age_time <- results_w_age |>
  filter(round_category == "Men Elite") |>
  slice(1:30) |>
  ggplot(aes(x = age, y = time)) +
  geom_image(
    size = .1,
    aes(image = path),
    alpha = .7
  ) +
  theme_ridges() +
  labs(
    title = "Age does not appear to correlate with performance in the top 30 Elite Men",
    x = "Age",
    y = "Time (s)"
  )

ggsave(
  "inst/scripts/analysis-world-champs-25/men_plot_age_time.png",
  plot = men_plot_age_time,
  width = 2200,
  height = 1800,
  units = "px",
  bg = "white",
  limitsize = FALSE,
  dpi = 330
)

# While the model's coefficient for time was 0.4269, suggesting a slight
# positive trend where a longer race time was associated with older riders, this
# finding was not statistically significant (p=0.226). Therefore, based on this
# specific dataset, we cannot conclude that age is a reliable predictor of
# performance.
results_w_age |>
  filter(round_category == "Men Elite") |>
  slice(1:30) |>
  lm(age ~ time, data = _) |>
  summary()

# - Elite Women -
women_plot_age_time <- results_w_age |>
  filter(round_category == "Women Elite") |>
  slice(1:30) |>
  ggplot(aes(x = age, y = time)) +
  geom_image(
    size = .1,
    aes(image = path),
    alpha = .7
  ) +
  theme_ridges() +
  labs(
    title = "Age does not appear to correlate with performance in the top 30 Elite Women",
    x = "Age",
    y = "Time (s)"
  )

ggsave(
  "inst/scripts/analysis-world-champs-25/women_plot_age_time.png",
  plot = women_plot_age_time,
  width = 2200,
  height = 1800,
  units = "px",
  bg = "white",
  limitsize = FALSE,
  dpi = 330
)
