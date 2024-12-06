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
