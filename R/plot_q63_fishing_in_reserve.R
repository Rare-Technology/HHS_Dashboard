
prep_q63_fishing_in_reserve <- function(.data){

}

plot_q63_fishing_in_reserve <- function(.data, ...){
  
  .data <- .data %>% 
    dplyr::filter(`63_fishing_in_reserve` %in% c(0,1))
  
  .data_plot <-  prep_data_for_plot(
    .data,
    `9_region_member`,
    type = "bar",
    bar_column = `1`
  )
  
  plot_horiz_bar(
    .data_plot,
    title = "Proportion of fishers that have fished \nin the reserve in the past month"
  )
  
  
# hhs_Q63 <- .data[,c("maa", "63_fishing_in_reserve")] %>%
#                         dplyr::filter(`63_fishing_in_reserve` %in% c(0,1))
#          
#          Q63_summary <- proportion ( hhs_Q63$`63_fishing_in_reserve`,
#                                      hhs_Q63$maa,
#                                      3,2)
#          
#          Q63_summary[Q63_summary == "NaN"] <- 0
#          
#          colnames(Q63_summary) <- c("MA name", "N", "No", "Yes")
#          
#          Q63 <- Q63_summary[,c("MA name", "N", "Yes")] %>% 
#                         dplyr::filter(`MA name` != "Mean ± SE")
#          
#          colnames(Q63) <- c("MA name", "N", "Proportion (%)")
#          
#          Q63 <- clean_plot_data(Q63)
#          
#         #plot
#          plot_Q63 <-
#             ggplot(Q63, aes(`MA name`, `Proportion (%)`, N = N)) +
#             theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
#             
#             scale_y_continuous(limits = c(0, 110),
#                                breaks = seq(0, 100, 20)) +
#             ggtitle("Proportion of fishers that have fished \nin the reserve in the past month") +
#             xlab (NULL) + ylab ("Proportion (%)") + 
#             coord_flip(clip ="on")
#          
#          ggplotly(plot_Q63, height = 750)
}