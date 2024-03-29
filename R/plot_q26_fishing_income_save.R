plot_q26_fishing_income_save <- function(.data, ...) {
  .data[["26_fishing_income_save"]] <- ifelse(.data$`26_fishing_income_save` > 0, 1, 0)

  .data_plot <- prep_data_for_plot(
    .data,
    `26_fishing_income_save`,
    include_summary_line = FALSE,
    type = "bar",
    bar_column = `1`
  )

  p <- plot_horiz_bar(
    .data_plot,
    title = "Proportion of households with enough income to save"
  )
  
  result <- list(
    plot = p,
    data = .data_plot
  )
}
