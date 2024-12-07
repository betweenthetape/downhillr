#' Clean a results table extracted from a pdf
#'
#' Use this function to clean qualifying, semi-final, and final result tables
#' after they have been extracted from a pdf and comined into a single table in
#' their raw format.
#'
#' @param data A data frame containing all combined tables from a pdf in their
#'   raw, unprocessed form.
#' @noRd
clean_results_table <- function(data) {
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

  all_rows <- odd_rows |>
    left_join(even_rows) |>
    mutate(
      dnf = if_else(time == "DNF", TRUE, FALSE),
      dsq = if_else(time == "DSQ", TRUE, FALSE),
      dns = if_else(time == "DNS", TRUE, FALSE),
      across(everything(), ~ if_else(.x == "-", NA, .x)),
      across(starts_with("sp"), ~ str_remove(.x, "\\s*\\(\\d+\\)$")),
      time_from_leader = str_remove(time_from_leader, "^\\+"),
      across(
        c(starts_with("split_"), starts_with("time")), ~ convert_to_seconds(.x)
      ),
      speed = as.numeric(speed)
    ) |>
    select(
      rank, protected, nr, name, uci_team, uci_id, nat, yob, speed, split_1,
      split_2, split_3, split_4, time, time_from_leader, dnf, dsq, dns, points
    )
}
