
plot_q18_hh_main_fisher <- function(.data, ...) {

  .data_plot <- prep_data_for_plot(.data, `18_hh_main_fisher`, type = "stacked")

  p <- plot_bubble(
    .data_plot,
    title = "Proportion of main fishers in the household \nthat fish as member of a group",
  )
  
  result <- list(
    plot = p,
    data = .data_plot
  )
}
