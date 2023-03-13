plot_q53_ma_benefits <- function(.data, ...){
  .data_plot <-  .data %>% 
    dplyr::select(maa, `53_ma_benefits`) %>% 
    dplyr::mutate(
      `53_ma_benefits` = dplyr::recode(
        `53_ma_benefits`,
        "Yes" = 1,
        "No" = 0
      )
    ) %>% 
    dplyr::filter(`53_ma_benefits` %in% c(0,1)) %>%
    prep_data_for_plot(
      `53_ma_benefits`,
      type = "bar",
      bar_column = `1`
    )
  
  p <- plot_horiz_bar(
    .data_plot,
    title = "\nProportion of community members that think there is benefit \nto regulating fishing via a managed access area and a reserve area"
  )
  
  result <- list(
    plot = p,
    data = .data_plot
  )
}