plot_q10_mpa_important <- function(.data, ...){
  Q10 <- .data %>% 
    dplyr::select(maa, `10_mpa_important`) %>% 
    dplyr::filter(`10_mpa_important` != "Not Answered") %>% 
    rbind(c(NA, "No"), c(NA, "Neutral"), c(NA, "Yes"))

  .data_plot <-prep_data_for_plot(
   Q10, 
   focus_var = `10_mpa_important`, 
   recoding = c("Neutral" = -1, "No" = 0, "Yes" = 1),
   type = "facet"
  )
  
  p <- plot_horiz_bar(
   .data_plot,
   title = "Proportion of households that believe it is important that the region is managed and protected",
   fill_var = key,
   type = 'grouped'
  )
  
  result <- list(
    plot = p,
    data = .data_plot
  )
}



