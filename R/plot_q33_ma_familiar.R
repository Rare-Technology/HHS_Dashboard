
plot_q33_ma_familiar <- function(.data, ...) {
  .data_plot <- .data %>%
    dplyr::filter(`33_ma_familiar` %in% c(0, 1)) %>%
    prep_data_for_plot(
      `33_ma_familiar`,
      type = "bar",
      bar_column = `1`
    )

  plot_horiz_bar(
    .data_plot,
    title = "Proportion of community familiar with the \nmanaged access and reserve area management approach"
  )
}
