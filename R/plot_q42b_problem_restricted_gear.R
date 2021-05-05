plot_q42b_problem_restricted_gear <- function(.data, ...){


  .data_plot <-  .data %>% 
    dplyr::filter(`42b_problem_restricted_gear` %in% c(0,1)) %>%
    prep_data_for_plot(
    `42b_problem_restricted_gear`,
    type = "bar",
    bar_column = `1`
  )
  
  plot_horiz_bar(
    .data_plot,
    title = "Proportion of community members that \nknow how fishing with restricted gear affect the fishery"
  )

}