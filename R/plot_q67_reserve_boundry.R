plot_q67_reserve_boundry <- function(.data, ...){
  
  .data_plot <-  prep_data_for_plot(
    .data,
    `67_reserve_boundry`,
    type = "bar",
    bar_column = `1`
  )
  
  plot_horiz_bar(
    .data_plot,
    title = "Proportion of community members who are aware of the reserve boundary"
  )
  
}