
# prep_q43_ma_benefits <- function(.data){
# 
# }

plot_q43_ma_benefits <- function(.data, ...){
  
  .data_plot <-  .data %>% 
    dplyr::filter(`43_ma_benefits` %in% c(0,1)) %>%
    prep_data_for_plot(
      `42d_problem_inside_reserve`,
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