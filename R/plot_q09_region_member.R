
plot_q09_region_member <- function(.data, ...){

  .data_plot <-  prep_data_for_plot(
   .data,
   `9_region_member`,
   type = "bar",
   bar_column = `1`
  )
  
  p <- plot_horiz_bar(
   .data_plot,
   title = "Proportion of households that identify themselves \nas member of a specific region"
  )

  result <- list(
    plot = p,
    data = .data_plot
  )
}