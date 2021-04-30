
prep_q10_mpa_important <- function(.data){
  
  res <- .data %>%
    dplyr::filter(`10_mpa_important` != "") %>%
    dplyr::select(maa, `10_mpa_important`) %>% 
    rbind(c(NA,1), c(NA,0), c(NA,-1))

  res <- proportion(
    res$`10_mpa_important`,
    res$maa,
    3,
    type= 3
    )
  
  colnames(res) <- c("MA name", "N", "Neutral", "No", "Yes")
  
  res_long <- res %>% 
    tidyr::pivot_longer(
      cols = c("Neutral", "No", "Yes"),
      names_to = "key",
      values_to = "Proportion (%)"
    )
  
  clean_plot_data(res_long)
}

plot_q10_mpa_important <- function(.data, use_plotly = TRUE){

         .data_plot <- prep_q10_mpa_important(.data)
         p <- plot_horiz_bar(
           .data_plot,
           title = "Proportion of households that believe is important \nthat the region is managed and protected",
           facet_var = key
         )

         
         if(use_plotly)
           ggplotly(p)
         
         p
      
}



