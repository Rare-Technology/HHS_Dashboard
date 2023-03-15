plot_q44_ma_familiar <- function(.data, ...) {
  .data_plot <- .data %>%
    dplyr::select(maa, `44_ma_familiar`) %>% 
    dplyr::mutate(
      `44_ma_familiar` = dplyr::recode(
        `44_ma_familiar`,
        "Yes" = 1,
        "No" = 0,
        .default = as.double(NA)
      )
    ) %>% 
    dplyr::filter(`44_ma_familiar` %in% c(0, 1)) %>%
    prep_data_for_plot(
      `44_ma_familiar`,
      type = "bar",
      bar_column = `1`
    )

  p <- plot_horiz_bar(
    .data_plot,
    title = "Proportion of community familiar with the \nmanaged access and reserve area management approach"
  )
  
  result <- list(
    plot = p,
    data = .data_plot
  )
}
