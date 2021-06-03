
prep_q61e_rights_distribution_fair <- function(.data){
  hhs_Q61e <- .data[,c("maa", "61e_rights_distribution_fair")] %>%
    dplyr::filter(`61e_rights_distribution_fair` %in% c(1:5)) %>%
    rbind(c(NA,1),c(NA,2),c(NA,3),c(NA,4),c(NA,5))
  
  Q61e_summary <- proportion ( hhs_Q61e$`61e_rights_distribution_fair`,
                               hhs_Q61e$maa,
                               3,5)
  
  colnames(Q61e_summary) <-
    c( "MA name",
       "N",
       "Strongly disagree",
       "Disagree",
       "Neither agree nor disagree",
       "Agree",
       "Strongly agree"
    )
  
  Q61e_summary_grouped <- Q61e_summary %>% 
    dplyr::filter (`MA name` != "Mean ± SE")
  
  Q61e_summary_grouped$`Agree (%)` <- as.numeric(Q61e_summary_grouped$Agree) + 
    as.numeric(Q61e_summary_grouped$`Strongly agree`)
  
  Q61e_summary_grouped$`Neither agree nor disagree (%)` <- 
    as.numeric(Q61e_summary_grouped$`Neither agree nor disagree`)
  
  Q61e_summary_grouped$`Disagree (%)` <- as.numeric(Q61e_summary_grouped$Disagree) + 
    as.numeric(Q61e_summary_grouped$`Strongly disagree`)
  
  Q61e_summary <- Q61e_summary_grouped[,c("MA name", "N", 
                                          "Disagree (%)",
                                          "Neither agree nor disagree (%)",
                                          "Agree (%)" )]
  #pivot table
  Q61e_summary_long <-
    as.data.frame(
      Q61e_summary_grouped[, c("MA name", "N",
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
  Q61e_summary_long$key <-
    factor(
      Q61e_summary_long$key,
      levels = c(
        "Disagree (%)",
        "Neither agree nor disagree (%)",
        "Agree (%)"
      )
    )
  
  Q61e <- clean_plot_data(Q61e_summary_long)
  Q61e
}

plot_q61e_rights_distribution_fair <- function(.data, ...){

  .data_plot <- prep_q61e_rights_distribution_fair(.data)
  plot_horiz_bar(
    .data_plot,
    title = "Proportion of fishers who believe that \naccess rights have been distributed fairly",
    facet_var = key
  )
         #Plot
         # plot_Q61e <-
         #    ggplot(Q61e, aes(`MA name`, `Proportion (%)`, N = N)) +
         #    theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
         #    facet_wrap( ~ key,
         #                scale = input$x_axis,
         #                labeller = label_wrap_gen(20)) +
         #    scale_y_continuous(limits = c(0, 110),
         #                       breaks = seq(0, 100, 25)) +
         #    ggtitle(
         #       "Proportion of fishers who believe that \naccess rights have been distributed fairly"
         #    ) +
         #    xlab (NULL) + ylab ("Proportion (%)") + 
         #    coord_flip(ylim = c(0, 119))
         # 
         # ggplotly(plot_Q61e, height = 750)
}