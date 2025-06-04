#' Clean a results table extracted from a PDF for the 2024 season
#'
#' Use this function to clean qualifying, semi-final, and final result tables
#' after they have been extracted from a PDF and comined into a single table in
#' their raw format.
#'
#' @param data A data frame containing all combined tables from a PDF in their
#'   raw, unprocessed form.
#' @return A tibble with cleaned results.
#' @noRd
#' @autoglobal
clean_results_24 <- function(data) {
  data <- data |>
    fill(`UCI ID`)

  odd_rows <- data |>
    filter(row_number() %% 2 == 1) |>
    separate_wider_regex(
      cols = `Nr Name / UCI MTB Team`,
      patterns = c(Nr = "^\\d+", `Name / UCI MTB Team` = "\\D+")
    ) |>
    janitor::clean_names() |>
    rename(
      protected = x2,
      name = name_uci_mtb_team,
      split_1 = i1_i2,
      split_3 = i3_i4
    ) |>
    mutate(protected = if_else(!is.na(protected), TRUE, FALSE)) |>
    mutate(name = str_remove(name, "\\*$")) |>
    mutate(name = str_squish(name))

  even_rows <- data |>
    filter(row_number() %% 2 == 0) |>
    select(
      uci_team = `Nr Name / UCI MTB Team`,
      uci_id = `UCI ID`,
      split_2 = `I1 / I2`,
      split_4 = `I3 / I4`,
      time_from_leader = Time
    )

  odd_rows |>
    left_join(even_rows) |>
    mutate(
      dnf = if_else(time == "DNF", TRUE, FALSE),
      dsq = if_else(time == "DSQ", TRUE, FALSE),
      dns = if_else(time == "DNS", TRUE, FALSE),
      across(everything(), ~ if_else(.x == "-", NA, .x)),
      across(starts_with("sp"), ~ str_remove(.x, "\\s*\\(\\d+\\)$")),
      time_from_leader = str_remove(time_from_leader, "^\\+"),
      across(
        c(starts_with("split_"), starts_with("time")),
        ~ convert_to_seconds(.x)
      ),
      across(c(rank, nr, uci_id, speed, points), ~ as.numeric(.x))
    ) |>
    select(
      rank,
      protected,
      nr,
      name,
      uci_team,
      uci_id,
      nat,
      yob,
      speed,
      split_1,
      split_2,
      split_3,
      split_4,
      time,
      time_from_leader,
      dnf,
      dsq,
      dns,
      points
    )
}

#' Clean a results table extracted from a PDF for the 2025 season
#'
#' Use this function to clean qualifying, semi-final, and final result tables
#' after they have been extracted from a PDF and comined into a single table in
#' their raw format.
#'
#' @param data A data frame containing all combined tables from a PDF in their
#'   raw, unprocessed form.
#' @return A tibble with cleaned results.
#' @noRd
#' @autoglobal
clean_results_25 <- function(data) {
  data <- data |>
    fill(`UCI ID`)

  odd_rows <- data |>
    filter(row_number() %% 2 == 1) |>
    separate_wider_regex(
      cols = `Nr Name / UCI MTB Team`,
      patterns = c(Nr = "^\\d+", `Name / UCI MTB Team` = "\\D+")
    ) |>
    janitor::clean_names() |>
    rename(
      name = name_uci_mtb_team,
      split_1 = i1_i2,
      split_3 = i3_i4
    ) |>
    mutate(name = str_remove(name, "\\*$")) |>
    mutate(name = str_squish(name))

  even_rows <- data |>
    filter(row_number() %% 2 == 0) |>
    select(
      uci_team = `Nr Name / UCI MTB Team`,
      uci_id = `UCI ID`,
      split_2 = `I1 / I2`,
      split_4 = `I3 / I4`,
      time_from_leader = Time
    )

  odd_rows |>
    left_join(even_rows) |>
    mutate(
      dnf = if_else(time == "DNF", TRUE, FALSE),
      dsq = if_else(time == "DSQ", TRUE, FALSE),
      dns = if_else(time == "DNS", TRUE, FALSE),
      across(everything(), ~ if_else(.x == "-", NA, .x)),
      across(starts_with("sp"), ~ str_remove(.x, "\\s*\\(\\d+\\)$")),
      time_from_leader = str_remove(time_from_leader, "^\\+"),
      across(
        c(starts_with("split_"), starts_with("time")),
        ~ convert_to_seconds(.x)
      ),
      across(c(rank, nr, uci_id, speed, points), ~ as.numeric(.x))
    ) |>
    select(
      rank,
      nr,
      name,
      uci_team,
      uci_id,
      nat,
      yob,
      speed,
      split_1,
      split_2,
      split_3,
      split_4,
      time,
      time_from_leader,
      dnf,
      dsq,
      dns,
      points
    )
}

#' Clean timed training results extracted from a PDF for the 2024 season
#'
#' Use this function to clean timed training results tables after they have been
#' extracted from a PDF and comined into a single table in their raw format.
#'
#' @param data A data frame containing all combined tables from a PDF in their
#'   raw, unprocessed form.
#' @return A tibble with cleaned results.
#' @noRd
#' @autoglobal
clean_timed_training_24 <- function(data) {
  data <- data |>
    fill(Nr)

  split_1 <- data |>
    filter((row_number() - 1) %% 4 == 0) |>
    rename(
      rank = Rank,
      nr = Nr,
      name = `Name / UCI MTB Team`,
      nat = NAT,
      run_1_speed = Speed...5,
      run_1_split_1 = Splits...6,
      run_1_time = Time...7,
      run_2_speed = Speed...8,
      run_2_split_1 = Splits...9,
      run_2_time = Time...10,
      run_3_speed = Speed...11,
      run_3_split_1 = Splits...12,
      run_3_time = Time...13,
      best_time = Time...14
    )

  split_2 <- data |>
    filter((row_number() - 2) %% 4 == 0) |>
    select(
      nr = Nr,
      uci_team = `Name / UCI MTB Team`,
      run_1_split_2 = Splits...6,
      run_2_split_2 = Splits...9,
      run_3_split_2 = Splits...12,
      best_time_from_leader = Time...14
    )

  split_3 <- data |>
    filter((row_number() - 3) %% 4 == 0) |>
    select(
      nr = Nr,
      run_1_split_3 = Splits...6,
      run_2_split_3 = Splits...9,
      run_3_split_3 = Splits...12,
    )

  split_4 <- data |>
    filter((row_number() - 4) %% 4 == 0) |>
    select(
      nr = Nr,
      run_1_split_4 = Splits...6,
      run_2_split_4 = Splits...9,
      run_3_split_4 = Splits...12,
    )

  list(split_1, split_2, split_3, split_4) |>
    reduce(left_join) |>
    mutate(
      name = str_squish(str_remove(name, "\\*$")),
      best_time_from_leader = str_remove(best_time_from_leader, "^\\+"),
      across(everything(), ~ if_else(.x == "-", NA, .x)),
      across(
        c(starts_with("run_"), starts_with("best_time")),
        ~ convert_to_seconds(.x)
      ),
      across(c(rank, nr), ~ as.numeric(.x))
    ) |>
    select(
      rank,
      nr,
      name,
      uci_team,
      nat,
      run_1_speed,
      starts_with("run_1_split"),
      run_1_time,
      run_2_speed,
      starts_with("run_2_split"),
      run_2_time,
      run_3_speed,
      starts_with("run_3_split"),
      run_3_time,
      best_time,
      best_time_from_leader
    )
}

#' Clean timed training results extracted from a PDF for the 2025 season
#'
#' Use this function to clean timed training results tables after they have been
#' extracted from a PDF and comined into a single table in their raw format.
#'
#' @param data A data frame containing all combined tables from a PDF in their
#'   raw, unprocessed form.
#' @return A tibble with cleaned results.
#' @noRd
#' @autoglobal
clean_timed_training_25 <- function(data) {
  # There is a data entry error for Christian Hauser in Loudenvielle where his
  # Nr is inputted as 10 on his first row, and 1 on his second row, rather than
  # 101 on his first row. Use a combination of columns as a unique indentifier:
  data <- data |>
    mutate(
      Nr = case_when(
        Rank == "84" &
          `Name / UCI MTB Team` == "HAUSER Christian * (ITA)" &
          Nr == "10" &
          `Time...14` == "6:38.697" ~
          "101",
        is.na(Rank) &
          `Name / UCI MTB Team` == "TREK FACTORY RACING DH" &
          Nr == "1" &
          `Time...14` == "+3:19.842" ~
          NA_character_,
        .default = Nr
      )
    )

  data <- data |>
    fill(Nr)

  split_1 <- data |>
    filter((row_number() - 1) %% 4 == 0) |>
    rename(
      rank = Rank,
      nr = Nr,
      name = `Name / UCI MTB Team`,
      run_1_split_1 = Splits...4,
      run_1_time = Time...5,
      run_2_split_1 = Splits...6,
      run_2_time = Time...7,
      run_3_split_1 = Splits...8,
      run_3_time = Time...9,
      run_4_split_1 = Splits...10,
      run_4_time = Time...11,
      run_5_split_1 = Splits...12,
      run_5_time = Time...13,
      best_time = Time...14
    )

  split_2 <- data |>
    filter((row_number() - 2) %% 4 == 0) |>
    select(
      nr = Nr,
      uci_team = `Name / UCI MTB Team`,
      run_1_split_2 = Splits...4,
      run_2_split_2 = Splits...6,
      run_3_split_2 = Splits...8,
      run_4_split_2 = Splits...10,
      run_5_split_2 = Splits...12,
      run_1_speed_kmh = Time...5,
      run_2_speed_kmh = Time...7,
      run_3_speed_kmh = Time...9,
      run_4_speed_kmh = Time...11,
      run_5_speed_kmh = Time...13,
      best_time_from_leader = Time...14
    )

  split_3 <- data |>
    filter((row_number() - 3) %% 4 == 0) |>
    select(
      nr = Nr,
      run_1_split_3 = Splits...4,
      run_2_split_3 = Splits...6,
      run_3_split_3 = Splits...8,
      run_4_split_3 = Splits...10,
      run_5_split_3 = Splits...12,
    )

  split_4 <- data |>
    filter((row_number() - 4) %% 4 == 0) |>
    select(
      nr = Nr,
      run_1_split_4 = Splits...4,
      run_2_split_4 = Splits...6,
      run_3_split_4 = Splits...8,
      run_4_split_4 = Splits...10,
      run_5_split_4 = Splits...12,
    )

  list(split_1, split_2, split_3, split_4) |>
    reduce(left_join) |>
    mutate(name = str_replace_all(name, "\\s*\\*\\s*", " ")) |>
    mutate(name = str_replace_all(name, "\\s*\\([A-Z]{3}\\)", "")) |>
    mutate(name = str_squish(name)) |>
    mutate(best_time_from_leader = str_remove(best_time_from_leader, "^\\+")) |>
    mutate(across(ends_with("_kmh"), ~ str_remove(.x, "kmh$"))) |>
    mutate(across(
      !c(rank, nr, name, uci_team, ends_with("_speed")),
      ~ convert_to_seconds(.x)
    )) |>
    mutate(across(c(rank, nr), ~ as.numeric(.x))) |>
    select(
      rank,
      nr,
      name,
      uci_team,
      run_1_speed_kmh,
      starts_with("run_1_split"),
      run_1_time,
      run_2_speed_kmh,
      starts_with("run_2_split"),
      run_2_time,
      run_3_speed_kmh,
      starts_with("run_3_split"),
      run_3_time,
      run_4_speed_kmh,
      starts_with("run_4_split"),
      run_4_time,
      run_5_speed_kmh,
      starts_with("run_5_split"),
      run_5_time,
      best_time,
      best_time_from_leader
    )
}
