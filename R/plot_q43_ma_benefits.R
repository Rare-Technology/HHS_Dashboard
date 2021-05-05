
# prep_q43_ma_benefits <- function(.data){
# 
# }

plot_q43_ma_benefits <- function(.data, ...){
  
  .data_plot <-  .data %>% 
    dplyr::filter(`43_ma_benefits` %in% c(0,1)) %>%
    prep_data_for_plot(
      `42d_problem_inside_reserve`,
      type = "bar",
      bar_column = `1`
    )
  
  plot_horiz_bar(
    .data_plot,
    title = "\nProportion of community members that think there is benefit \nto regulating fishing via a managed access area and a reserve area"
  )
  
  
# hhs_Q43 <- .data[,c("maa", "43_ma_benefits")] %>%
#                         dplyr::filter(`43_ma_benefits` %in% c(0,1)) %>%
#                            rbind(c(NA,0), c(NA,1)) %>%
#                               droplevels()
#          
#          Q43_summary <- proportion(hhs_Q43[[2]],
#                                    hhs_Q43[[1]],
#                                    3,2)[,-3]
#          
#          colnames(Q43_summary) <- c("MA name", "N", "Proportion (%)")
#          
#          Q43 <- clean_plot_data(Q43_summary)
#          
#          #Plot
#          plot_Q43 <-
#             ggplot(Q43, aes(`MA name`, `Proportion (%)`, N = N)) +
#             theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
#             
#             scale_y_continuous(limits = c(0, 110),
#                                breaks = seq(0, 100, 20)) +
#             ggtitle(
#                "\nProportion of community members that think there is benefit \nto regulating fishing via a managed access area and a reserve area"
#             ) +
#             xlab (NULL) + ylab ("Proportion (%)") + 
#             coord_flip(ylim = c(0, 119))
#          
#          ggplotly (plot_Q43, height = 750)
}