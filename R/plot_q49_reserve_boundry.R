plot_q49_reserve_boundry <- function(.data, ...){
  .data_plot <- .data %>%
    dplyr::select(maa, `49_reserve_boundry`) %>% 
    dplyr::mutate(
      `49_reserve_boundry` = dplyr::recode(
        `49_reserve_boundry`,
        "Yes" = 1,
        "No" = 0,
        .default = as.double(NA)
      )
    ) %>% 
    dplyr::filter(`49_reserve_boundry` %in% c(0, 1)) %>%
    prep_data_for_plot(
      `49_reserve_boundry`,
      type = "bar",
      bar_column = `1`
    )
  
  p <- plot_horiz_bar(
    .data_plot,
    title = "Proportion of community members who are aware of the reserve boundary"
  )
  
  result <- list(
    plot = p,
    data = .data_plot
  )
}