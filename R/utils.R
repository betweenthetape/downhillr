#' Bind a list of tables extracted from a PDF.
#'
#' @param tables A list of data frames extracted from a PDF.
#' @noRd
bind_tables <- function(tables) {
  tables |>
    map(~ mutate(.x, across(everything(), as.character))) |>
    list_rbind()
}

#' @noRd
#' @examples
#' convert_to_seconds(c("3:14.668", "14.590"))
convert_to_seconds <- function(x) {
  map_dbl(
    x,
    .f = \(x) {
      if (grepl(":", x)) {
        parts <- str_split_1(x, ":")
        minutes <- as.numeric(parts[1])
        seconds <- as.numeric(parts[2])
        (minutes * 60) + seconds
      } else {
        as.numeric(x)
      }
    }
  )
}

#' Extract table areas from a PDF
#'
#' Run this function to interactively extract table areas from a PDF and print
#' the result in a format which can be copied and pasted directly into
#' `tabluapdf::extract_tables()`.
#'
#' @param pdf path to PDF file with tables to extract.
#' @param clipboard Should the outputs be copied to the clipboard?
#'
#' @noRd
pdf_areas <- function(pdf, clipboard = TRUE) {
  output <- tabulapdf::locate_areas(pdf) |>
    map(\(x) round(unname(x), 4))

  print(dput(output))

  if (clipboard) clipr::write_clip(deparse(output))
}
