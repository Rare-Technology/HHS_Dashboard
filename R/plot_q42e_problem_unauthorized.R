plot_q42e_problem_unauthorized <- function(.data, ...){
  .data_plot <-  .data %>% 
    dplyr::filter(`42e_problem_unauthorized` %in% c(0,1)) %>%
    prep_data_for_plot(
      `42e_problem_unauthorized`,
      type = "bar",
      bar_column = `1`
    )
  
  plot_horiz_bar(
    .data_plot,
    title = "Proportion of community members that know how \nunauthorized fishers fishing inside the managed access area affect the fishery?"
  )

}