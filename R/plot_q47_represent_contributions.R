
# prep_q47_represent_contributions <- function(.data){
#   hhs_Q47 <- .data[,c("maa", "47_represent_contributions")] %>%
#     rbind(tibble::tibble("maa" = c(NA,NA,NA),
#                  "47_represent_contributions"= c("Agree", 
#                                                  "Neither", 
#                                                  "Disagree"))) %>%
#     dplyr::filter(`47_represent_contributions` %in% c("Agree",  "Neither",  "Disagree")) %>%
#     droplevels()
#   #proportion
#   Q47_summary <- proportion(hhs_Q47[[2]],
#                             hhs_Q47[[1]],
#                             3,3)
#   #pivot table
#   Q47_summary_long <-
#     Q47_summary %>% tidyr::pivot_longer(
#       cols = c("Neither", "Disagree", "Agree"),
#       names_to = "key",
#       values_to = "Proportion (%)"
#     )
#   Q47_summary_long$key <-
#     factor(Q47_summary_long$key,
#            levels = c("Neither", "Disagree", "Agree"))
#   
#   Q47 <- clean_plot_data(Q47_summary_long)
#   Q47
# }

plot_q47_represent_contributions <- function(.data, ...){
  

  .data <- .data %>% 
    dplyr::filter(`47_represent_contributions` %in% c("Agree",  "Neither",  "Disagree")) 
  
  .data_plot <-prep_data_for_plot(
    .data, 
    focus_var = `47_represent_contributions`, 
    type = "facet",
    key_order = c("Neither", "Disagree", "Agree")
  )
  
  plot_horiz_bar(
    .data_plot,
    title = "Proportion of community members who feel that \ntheir contributions to the fishery are recognized",
    facet_var = key
  )
         
         #Plot
         # plot_Q47 <-
         #    ggplot(Q47, aes(`MA name`, `Proportion (%)`, N = N)) +
         #    theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
         #    facet_wrap( ~ key, scale = input$x_axis, ncol = 4) +
         #    
         #    scale_y_continuous(limits = c(0, 110),
         #                       breaks = seq(0, 100, 25)) +
         #    ggtitle(
         #       "Proportion of community members who feel that \ntheir contributions to the fishery are recognized"
         #    ) +
         #    xlab (NULL) + ylab (NULL) + 
         #    coord_flip(clip ="on")
         # 
         # ggplotly(plot_Q47, height = 750)
}