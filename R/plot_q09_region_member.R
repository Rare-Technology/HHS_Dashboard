
plot_q09_region_member <- function(.data){

         .data_plot <-  prep_data_for_plot(
           .data,
           `9_region_member`,
           include_summary_line = FALSE,
           type = "bar",
           bar_column = `1`
         )
         
         plot_horiz_bar(
           .data_plot,
           title = "Proportion of households that identify themselves \nas member of a specific region"
           )

}