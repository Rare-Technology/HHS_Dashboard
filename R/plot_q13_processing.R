plot_q13_processing <- function(.data, ...){

    .data_plot <- prep_data_for_plot(.data,
               select_vars = c(`13a_processing_men`,
               `13b_processing_women`,
               `13c_processing_children`),
               var_names = c("men", "women", "children"),
               key_name = "Processors",
               values_name = "Average")
  
  
    plot_horiz_bar(
      .data_plot,
      y_var = Average,
      facet_var = Processors,
      title = "Average number of household members \nthat participate in post-procesing activities",
      limits = c(0, 3),
      breaks = seq(0, 3, 1),
      y_title = "\nNumber of people"
    )
    

}