plot_q43_fishery_benefit_equal <- function(.data, ...){
  
.data_plot <- .data %>% 
  dplyr::select(maa, `43_fishery_benefit_equal`) %>% 
  dplyr::mutate(
    `43_fishery_benefit_equal` = dplyr::recode(
      `43_fishery_benefit_equal`,
      "Yes" = 1,
      "No" = 0,
      .default = as.double(NA)
    )
  ) %>% 
  dplyr::filter(`43_fishery_benefit_equal` %in% c(0,1)) %>%
  prep_data_for_plot(
    `43_fishery_benefit_equal`,
    type = "bar",
    bar_column = `1`
  )
  
  p <- plot_horiz_bar(
    .data_plot,
    title = "Proportion of community that believes \nthey benefit equally from fishery as other households"
  )

  result <- list(
    plot = p,
    data = .data_plot
  )
}