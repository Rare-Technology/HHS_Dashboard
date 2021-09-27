#
# prep_q64_wrong_fishing_reserve <- function(.data){
#
# }

plot_q64_wrong_fishing_reserve <- function(.data, ...) {
  codes <- c(
    "Not wrong at all" = "1. Nada malo",
    "Not wrong at all" = "not at all",
    "Slightly wrong" = "Un poquito malo",
    "Slightly wrong" = "slightly",
    "Moderately wrong" = "3. Moderadamente malo",
    "Moderately wrong" = "moderately",
    "Very wrong" = "4. Muy malo",
    "Very wrong" = "very wrong",
    "Extremely wrong" = "5. Extremadamente malo",
    "Extremely wrong" = "extremely wrong"
  )

  .data_plot <- prep_data_for_plot(
    .data,
    focus_var = `64_wrong_fishing_reserve`,
    recoding = codes,
    type = "facet",
    key_order = names(codes) %>% unique()
  )

  p <- plot_horiz_bar(
    .data_plot,
    title = "Proportion of community members who think is wrong to fish in the reserve",
    facet_var = key
  )

  result <- list(
    plot = p,
    data = .data_plot
  )
}
