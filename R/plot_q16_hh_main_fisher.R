
plot_q16_hh_main_fisher <- function(.data, ...) {

  .data_plot <- prep_data_for_plot(.data, `16_hh_main_fisher`, type = "stacked")
  # plot_horiz_bar(
  #   .data_plot,
  #   title = "Proportion of main fishers in the household \nthat fish as member of a group",
  #   type = "stacked"
  # )
  plot_bubble(
    .data_plot,
    title = "Proportion of main fishers in the household \nthat fish as member of a group",
  )
}
