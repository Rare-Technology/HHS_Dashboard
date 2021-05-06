
# prep_q52_ma_benefit_5yrs <- function(.data){
# 
# }

plot_q52_ma_benefit_5yrs <- function(.data, ...){
  

  .data <- .data %>% 
    dplyr::filter(`52_ma_benefit_5yrs` %in% c("Yes", "Unsure", "No"))
  
  .data_plot <-prep_data_for_plot(
    .data, 
    focus_var = `52_ma_benefit_5yrs`, 
    type = "facet"
  )
  
  
  plot_horiz_bar(
    .data_plot,
    title = "Proportion of participants who are confident they will continue to \nbenefit from community management of the fishery for the next 5 years",
    facet_var = key
  )
# hhs_Q52 <- .data[,c("maa", "52_ma_benefit_5yrs")] %>%
#                         dplyr::filter(`52_ma_benefit_5yrs` %in% c("Yes", "Unsure", "No")) %>%
#                            rbind (tibble::tibble(maa = c(NA,NA,NA),
#                                          `52_ma_benefit_5yrs` = c("Yes",
#                                                                   "Unsure",
#                                                                   "No"))) %>%
#                         droplevels()
# 
#          Q52_summary <- proportion(hhs_Q52[[2]],
#                                     hhs_Q52[[1]],
#                                    3,3)
#          #pivot table
#          Q52_summary_long <-
#             Q52_summary %>% tidyr::pivot_longer(
#                cols = c("No", "Unsure", "Yes"),
#                names_to = "key",
#                values_to = "Proportion (%)"
#             )
#          Q52 <- clean_plot_data(Q52_summary_long)
         
         #Plot
         # plot_Q52 <-
         #    ggplot(Q52, aes(`MA name`, `Proportion (%)`, N = N)) +
         #    theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
         #    facet_wrap( ~ key, scale = input$x_axis) +
         #    
         #    scale_y_continuous(limits = c(0, 110),
         #                       breaks = seq(0, 100, 20)) +
         #    ggtitle(
         #       "Proportion of participants who are confident they will continue to \nbenefit from community management of the fishery for the next 5 years"
         #    ) +
         #    xlab (NULL) + ylab ("Proportion (%)") + 
         #    coord_flip(clip ="on")
         # 
         # ggplotly(plot_Q52, height = 750)
}