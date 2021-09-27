plot_q42c_problem_undersize <- function(.data, ...){
  
  
  .data_plot <-  .data %>% 
    dplyr::filter(`42c_problem_undersize` %in% c(0,1)) %>%
    prep_data_for_plot(
      `42c_problem_undersize`,
      type = "bar",
      bar_column = `1`
    )
  
  p <- plot_horiz_bar(
    .data_plot,
    title = "Proportion of community members that \nknow how fishing undersize fish affect the fishery"
  )

  result <- list(
    plot = p,
    data = .data_plot
  )
}