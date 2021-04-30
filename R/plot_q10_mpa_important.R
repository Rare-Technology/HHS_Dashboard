
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
         
         p <- .data_plot %>% 
            ggplot(aes(`MA name`, `Proportion (%)`, N = N)) +
            facet_wrap (~ key) + 
            geom_col(fill = "#005BBB", alpha = 0.8) +  
            labs(
              title = "Proportion of households that believe is important \nthat the region is managed and protected",
              x = NULL,
              y = "Proportion (%)"
            ) +
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            coord_flip(clip = "on") + 
           theme_rare()
         
         if(use_plotly)
           ggplotly(p)
         
         p
      
}



