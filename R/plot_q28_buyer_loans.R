plot_q28_buyer_loans <- function(.data, ...) {
  .data_plot <- prep_data_for_plot(
    .data,
    `28_buyer_loans`,
    type = "bar",
    bar_column = `1`
  )

  p <- plot_horiz_bar(
    .data_plot,
    title = "Proportion of household that \ntake out loans from fish buyers or traders"
  )
  
  result <- list(
    plot = p,
    data = .data_plot
  )
}
