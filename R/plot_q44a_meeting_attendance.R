prep_q44a_meeting_attendance <- function(.data){
  browser()
  hhs_Q44 <- .data[,c("maa", "44_meeting_attendance")] %>%
    tidyr::unnest(cols = `44_meeting_attendance`) %>% 
    dplyr::filter(`44_meeting_attendance` != '') %>%
    dplyr::filter (`44_meeting_attendance` != "na") %>%
    droplevels()
  
  Q44_summary <- proportion(hhs_Q44[[2]],
                            hhs_Q44[[1]],
                            3, length(unique(hhs_Q44[[2]])))
  
  #pivot table
  Q44_summary_long <-
    Q44_summary %>% tidyr::pivot_longer(
      cols = names(Q44_summary[-c(1:2)]),
      names_to = "key",
      values_to = "Proportion (%)"
    )
  
  Q44_summary_long$key <- str_replace (Q44_summary_long$key, "[.]", " ")
  Q44 <- clean_plot_data(Q44_summary_long)
  Q44
}

plot_q44a_meeting_attendance <- function(.data, ...){

  
  .data_plot <- prep_q44_meeting_attendance(.data)
  
  plot_horiz_bar(
    .data_plot,
    title = "Proportion of community members who \nattend management body meetings regularly",
    facet_var = key
  )
         
         #Plot
         # plot_Q44 <-
         #    ggplot(Q44, aes(`MA name`, `Proportion (%)`, N = N)) +
         #    theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
         #    facet_wrap( ~ key, ncol = 6) +
         #    
         #    scale_y_continuous(limits = c(0, 110),
         #                       breaks = seq(0, 100, 25)) +
         #    ggtitle(
         #       "Proportion of community members who \nattend management body meetings regularly"
         #    ) +
         #    xlab (NULL) + ylab (NULL) + 
         #       coord_flip(clip ="on")
         # 
         # ggplotly(plot_Q44, height = 750)
}