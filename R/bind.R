#' Bind a list of tables extracted from a PDF.
#'
#' @param tables A list of data frames extracted from a PDF.
#' @noRd
bind_tables <- function(tables) {
  tables |>
    map(~ mutate(.x, across(everything(), as.character))) |>
    list_rbind()
}
