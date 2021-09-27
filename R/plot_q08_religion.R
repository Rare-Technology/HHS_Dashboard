
plot_q08_religion <- function(.data, ...) {

  .data_plot <- prep_data_for_plot(.data, `8_religion`, type = "stacked")
  
  p <- plot_bubble(
    .data_plot,
    title = "Religion of the head of the Household",
  )
  
  result <- list(
    plot = p,
    data = .data_plot
  )
}
