# 
# prep_q59_food_procurement <- function(.data){
# 
# }

plot_q59_food_procurement <- function(.data, ...){
  
  .data_plot <-prep_data_for_plot(
    .data, 
    focus_var = `59_food_procurement`, 
    recoding = c(
      "Not very confident" = "Certain",
      "Not confident" = "High chance",
      "Confident" = "Confident not",
      "Confident" = "Not confident",
      "Very confident" = "Very not confident",
      "Very confident" = "Very confident not"
    ),
    type = "facet",
    key_order = c("Not very confident",
                  "Not confident",
                  "Uncertain",
                  "Confident",
                  "Very confident")
  )
  
  
  plot_horiz_bar(
    .data_plot,
    title = "Proportion of households who are confident that they will be able to \nprocure enough food for their family in the next 12 months",
    facet_var = key
  )
  
  
# hhs_Q59 <- .data[,c("maa","59_food_procurement")] %>%
#                         dplyr::filter(`59_food_procurement` != "") %>%
#                            rbind(c(NA, "Confident not"),
#                                  c(NA, "Uncertain"),
#                                  c(NA, "High chance"),
#                                  c(NA, "Very confident not"),
#                                  c(NA, "not Certain")) %>%
#                            droplevels()
# 
#          Q59_summary <- proportion (hhs_Q59$`59_food_procurement`,
#                                      hhs_Q59$maa,
#                                      3,5)
#          colnames(Q59_summary) <- c("MA name", "N",
#                                     "Not very confident",
#                                     "Not confident",
#                                     "Uncertain",
#                                     "Confident",
#                                     "Very confident")
#          Q59_summary_long <-
#             Q59_summary %>% tidyr::pivot_longer(
#                cols = c("Not very confident",
#                         "Not confident",
#                         "Uncertain",
#                         "Confident",
#                         "Very confident"),
#                names_to = "key",
#                values_to = "Proportion (%)"
#             )
# 
#          Q59_summary_long$key <-
#             factor(Q59_summary_long$key,
#                    levels = c("Not very confident",
#                               "Not confident",
#                               "Uncertain",
#                               "Confident",
#                               "Very confident")
#             )
# 
#          Q59 <- clean_plot_data(Q59_summary_long)
#          
#          #Plot
#          plot_Q59 <-
#             ggplot(Q59, aes(`MA name`, `Proportion (%)`, N = N)) +
#             theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
#             facet_wrap(~key, ncol= 5)+
#             
#             scale_y_continuous(limits = c(0, 110),
#                                breaks = seq(0, 100, 25)) +
#             ggtitle(
#                "Proportion of households who are confident that they will be able to \nprocure enough food for their family in the next 12 months"
#             ) +
#             xlab (NULL) + ylab ("Proportion (%)") + 
#                coord_flip(clip = "on")
#          
#          ggplotly(plot_Q59, height = 750)
}