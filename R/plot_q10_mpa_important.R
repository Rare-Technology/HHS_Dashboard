plot_q10_mpa_important <- function(.data, ...){

  
         .data_plot <-prep_data_for_plot(
           .data, 
           focus_var = `10_mpa_important`, 
           recoding = c("Neutral" = -1, "No" = 0, "Yes" = 1),
           type = "facet"
         )
         
         plot_horiz_bar(
           .data_plot,
           title = "Proportion of households that believe is important that the region is managed and protected",
           facet_var = key
         )
}



