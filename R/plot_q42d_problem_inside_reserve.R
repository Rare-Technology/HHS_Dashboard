plot_q42d_problem_inside_reserve <- function(.data, ...){
  
  .data_plot <-  .data %>% 
    dplyr::filter(`42d_problem_inside_reserve` %in% c(0,1)) %>%
    prep_data_for_plot(
      `42d_problem_inside_reserve`,
      type = "bar",
      bar_column = `1`
    )
  
  p <- plot_horiz_bar(
    .data_plot,
    title = "Proportion of community members that \nknow how fishing inside the reserve affect the fishery"
  )
  
  result <- list(
    plot = p,
    data = .data_plot
  )
}