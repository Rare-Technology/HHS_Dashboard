plot_q32_fishery_benefit_equal <- function(.data, ...){
  
.data_plot <- .data %>% 
    dplyr::filter(`32_fishery_benefit_equal` %in% c(0,1)) %>%
    prep_data_for_plot(
    `32_fishery_benefit_equal`,
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