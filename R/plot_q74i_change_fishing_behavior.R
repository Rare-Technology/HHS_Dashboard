
prep_q74i_fishing_change_behavior <- function(.data){
  hhs_Q74i <- .data %>% 
    dplyr::select(maa, `74i_change_fishing_behavior`) %>%
    dplyr::mutate(`74i_change_fishing_behavior` = dplyr::recode(
      `74i_change_fishing_behavior`,
      "Strongly disagree" = 1,
      "Disagree" = 2,
      "Neither agree nor disagree" = 3,
      "Agree" = 4,
      "Strongly agree" = 5,
      .default = as.double(NA)
    )) %>% 
    dplyr::filter(`74i_change_fishing_behavior` %in% c(1:5)) %>% 
    rbind(c(NA,1),c(NA,2),c(NA,3),c(NA,4),c(NA,5))
  
  Q74i_summary <- proportion (hhs_Q74i$`74i_change_fishing_behavior`,
                              hhs_Q74i$maa,
                              3,5 )
  
  colnames(Q74i_summary) <-
    c( "MA name",
       "N",
       "Strongly disagree",
       "Disagree",
       "Neither agree nor disagree",
       "Agree",
       "Strongly agree"
    )
  
  Q74i_summary_grouped <- Q74i_summary %>% 
    dplyr::filter (`MA name` != "Mean ± SE")
  
  Q74i_summary_grouped$`Agree (%)` <- as.numeric(Q74i_summary_grouped$Agree) + 
    as.numeric(Q74i_summary_grouped$`Strongly agree`)
  
  Q74i_summary_grouped$`Neither agree nor disagree (%)` <- 
    as.numeric(Q74i_summary_grouped$`Neither agree nor disagree`)
  
  Q74i_summary_grouped$`Disagree (%)` <- as.numeric(Q74i_summary_grouped$Disagree) + 
    as.numeric(Q74i_summary_grouped$`Strongly disagree`)
  
  #pivot table
  Q74i_summary_long <-
    as.data.frame(
      Q74i_summary_grouped[, c("MA name", "N",
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
  Q74i_summary_long$key <-
    factor(
      Q74i_summary_long$key,
      levels = c(
        "Disagree (%)",
        "Neither agree nor disagree (%)",
        "Agree (%)"
      )
    )
  
  Q74i <- clean_plot_data(Q74i_summary_long)
  Q74i
}

plot_q74i_fishing_change_behavior <- function(.data, ...){
  .data_plot <- prep_q74i_fishing_change_behavior(.data)
  
  p <- plot_horiz_bar(
    .data_plot,
    title = "Proportion of community members who are \nwilling to change their individual fishing behavior",
    facet_var = key
  )

  result <- list(
    plot = p,
    data = .data_plot
  )
}