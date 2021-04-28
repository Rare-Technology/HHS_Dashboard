read_multi_csv <- function(urls, col_type_spec = NULL, rowmax = 1000000) {
  dat <- purrr::map_dfr(1:length(urls), function(i) {
    iso3 <- names(urls)[i]
    url <- urls[i]

    dat <- readr::read_csv(
      url,
      col_types = col_type_spec,
      locale = readr::locale(encoding = "UTF-8"),
      guess_max = rowmax
    )


  })

  dat
}

apply_PLW_adjustment <- function(.data){
  .data %>% 
    dplyr::mutate(
      level2_name = ifelse(country == "PLW", level1_name, level2_name),
      ma_name = ifelse(country == "PLW", level1_name, ma_name)
    )
}
