plot_q30_save_monthly_income <- function(.data, ...) {
  .data <- .data %>% 
    dplyr::select(maa, `30_save_monthly_income`) %>% 
    dplyr::mutate(
      `30_save_monthly_income` = dplyr::recode(
        `30_save_monthly_income`,
        "Yes" = 1,
        "No" = 0,
        .missing = as.double(NA)
      )
    )

  .data_plot <- prep_data_for_plot(
    .data,
    `30_save_monthly_income`,
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
