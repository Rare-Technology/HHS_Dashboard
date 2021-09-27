
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
  
  p <- plot_horiz_bar(
    .data_plot,
    title = "Proportion of community members who feel that, \nthrough their individual fishing behavior, \ncan make a meaningful contribution to the sustainability of the fishery",
    facet_var = key
  )

  result <- list(
    plot = p,
    data = .data_plot
        )
}