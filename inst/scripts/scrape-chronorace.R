library(tidyverse)
library(httr2)

download_urls <- function(url) {
  response <-
    url |>
    request() |>
    req_perform() |>
    resp_body_json()

  unlist(response) |>
    tibble::enframe() |>
    filter(str_detect(name, ".Link$")) |>
    filter(str_detect(value, "_results_")) |>
    select(urls = value) |>
    mutate(
      path = str_c(
        "inst/extdata/",
        str_extract(urls, "(?<=webresources/)[0-9]+"),
        "-",
        str_extract(urls, "(?<=_mtb/).*")
      )
    )
}

download_pdf <- function(data) {
  walk2(
    .x = data$urls,
    .y = data$path,
    .f = \(x, y) {
      x |>
        request() |>
        req_perform() |>
        resp_body_raw() |>
        writeBin(y)
      Sys.sleep(2)
    }
  )
}

base_urls <- c(
  "https://prod.chronorace.be/api/results/uci/dh/cms/20240503_mtb",
  "https://prod.chronorace.be/api/results/uci/dh/cms/20240518_dhi",
  "https://prod.chronorace.be/api/results/uci/dh/cms/20240607_mtb",
  "https://prod.chronorace.be/api/results/uci/dh/cms/20240614_mtb",
  "https://prod.chronorace.be/api/results/uci/dh/cms/20240704_mtb",
  "https://prod.chronorace.be/api/results/uci/dh/cms/20240906_mtb",
  "https://prod.chronorace.be/api/results/uci/dh/cms/20241004_mtb"
)

walk(
  .x = base_urls,
  .f = \(x) {
    x |>
      download_urls() |>
      download_pdf()
    Sys.sleep(10)
  }
)
