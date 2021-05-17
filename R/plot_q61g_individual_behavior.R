
prep_q61g_individual_behavior <- function(.data){
  hhs_Q61g <- .data[,c("maa","61g_individual_behavior")] %>%
    dplyr::filter (`61g_individual_behavior` %in% c(1:5)) %>%
    rbind(c(NA,1),c(NA,2),c(NA,3),c(NA,4),c(NA,5))
  
  Q61g_summary <- proportion(hhs_Q61g$`61g_individual_behavior`,
                             hhs_Q61g$maa,
                             rounding = 3,
                             type = 5)
  
  colnames(Q61g_summary) <-
    c( "MA name",
       "N",
       "Strongly disagree",
       "Disagree",
       "Neither agree nor disagree",
       "Agree",
       "Strongly agree"
    )
  
  Q61g_summary_grouped <- Q61g_summary %>% 
    dplyr::filter (`MA name` != "Mean ± SE")
  
  Q61g_summary_grouped$`Agree (%)` <- as.numeric(Q61g_summary_grouped$Agree) + 
    as.numeric(Q61g_summary_grouped$`Strongly agree`)
  
  Q61g_summary_grouped$`Neither agree nor disagree (%)` <- 
    as.numeric(Q61g_summary_grouped$`Neither agree nor disagree`)
  
  Q61g_summary_grouped$`Disagree (%)` <- as.numeric(Q61g_summary_grouped$Disagree) + 
    as.numeric(Q61g_summary_grouped$`Strongly disagree`)
  
  #pivot table
  Q61g_summary_long <-
    as.data.frame(
      Q61g_summary_grouped[, c("MA name", "N",
                               "Disagree (%)",
                               "Neither agree nor disagree (%)",
                               "Agree (%)" )] %>% 
        tidyr::pivot_longer(
          cols = c(
            "Disagree (%)",
            "Neither agree nor disagree (%)",
            "Agree (%)"
          ),
          names_to = "key",
          values_to = "Proportion (%)"
        )
    )
  Q61g_summary_long$key <-
    factor(
      Q61g_summary_long$key,
      levels = c(
        "Disagree (%)",
        "Neither agree nor disagree (%)",
        "Agree (%)"
      )
    )
  
  Q61g <- clean_plot_data(Q61g_summary_long)
  Q61g
}

plot_q61g_individual_behavior <- function(.data, ...){
  .data_plot <- prep_q61g_individual_behavior(.data)
        plot_horiz_bar(
          .data_plot,
          title = "Proportion of community members who feel that, \nthrough their individual fishing behavior, \ncan make a meaningful contribution to the sustainability of the fishery",
          facet_var = key
        )
         #Plot
         # plot_Q61g <-
         #    ggplot(Q61g, aes(`MA name`, `Proportion (%)`, N = N)) +
         #    theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
         #    facet_wrap( ~ key,
         #                scale = input$x_axis,
         #                labeller = label_wrap_gen(20)) +
         #    
         #    scale_y_continuous(limits = c(0, 110),
         #                       breaks = seq(0, 100, 20)) +
         #    ggtitle(
         #       "Proportion of community members who feel that, \nthrough their individual fishing behavior, \ncan make a meaningful contribution to the sustainability of the fishery"
         #    ) +
         #    xlab (NULL) + ylab ("Proportion (%)") + 
         #    coord_flip(ylim = c(0, 119))
         # 
         # ggplotly(plot_Q61g, height = 750)
}