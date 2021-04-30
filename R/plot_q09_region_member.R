
prep_q09_region_member <- function(.data){
  
  hhs_Q9 <- .data %>% 
    dplyr::filter(`9_region_member` != "") %>%
    dplyr::select("maa", "9_region_member")
  
  Q9_summary <- proportion(hhs_Q9[[2]],
                           hhs_Q9[[1]],
                           3, type = length(unique(hhs_Q9[[2]])))
  colnames(Q9_summary) <- c("MA name", "N", "No", "Yes")
  
  Q9_summary_yes <- Q9_summary[c("MA name", "N", "Yes")]
  colnames(Q9_summary_yes) <- c("MA name", "N", "Proportion (%)")
  
  clean_plot_data(Q9_summary_yes)
  
}

plot_q09_region_member <- function(.data, use_plotly = TRUE){

         .data_plot <- prep_q09_region_member(.data)
         
         p <-.data_plot %>% 
            ggplot(aes(`MA name`, `Proportion (%)`, N = N)) +
            geom_col(fill = "#005BBB", alpha = 0.8) +
            labs(
              title = "Proportion of households that identify themselves \nas member of a specific region",
              x = NULL,
              y = "Proportion (%)"
            ) +
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            coord_flip(clip = "on") +
            theme_rare() +
            theme(legend.position = "right") 
            
         if(use_plotly) 
           ggplotly(p)
         
         p
}