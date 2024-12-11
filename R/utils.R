#' @noRd
#' @examples
#' convert_to_seconds(c("3:14.668", "14.590"))
convert_to_seconds <- function(x) {
  purrr::map_dbl(
    x,
    .f = \(x) {
      if (grepl(":", x)) {
        parts <- stringr::str_split_1(x, ":")
        minutes <- as.numeric(parts[1])
        seconds <- as.numeric(parts[2])
        (minutes * 60) + seconds
      } else {
        as.numeric(x)
      }
    }
  )
}

#' Bind a list of tables extracted from a PDF.
#'
#' @param tables A list of data frames extracted from a PDF.
#' @noRd
bind_tables <- function(tables) {
  tables |>
    map(~ mutate(.x, across(everything(), as.character))) |>
    list_rbind()
}
