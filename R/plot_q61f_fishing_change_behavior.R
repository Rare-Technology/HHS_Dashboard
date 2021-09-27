
prep_q61f_fishing_change_behavior <- function(.data){
  hhs_Q61f <- .data[,c("maa","61f_fishing_change_behavior")] %>%
    dplyr::filter( `61f_fishing_change_behavior` %in% c(1:5)) %>%
    rbind(c(NA,1),c(NA,2),c(NA,3),c(NA,4),c(NA,5))
  
  Q61f_summary <- proportion (hhs_Q61f$`61f_fishing_change_behavior`,
                              hhs_Q61f$maa,
                              3,5 )
  
  colnames(Q61f_summary) <-
    c( "MA name",
       "N",
       "Strongly disagree",
       "Disagree",
       "Neither agree nor disagree",
       "Agree",
       "Strongly agree"
    )
  
  Q61f_summary_grouped <- Q61f_summary %>% 
    dplyr::filter (`MA name` != "Mean ± SE")
  
  Q61f_summary_grouped$`Agree (%)` <- as.numeric(Q61f_summary_grouped$Agree) + 
    as.numeric(Q61f_summary_grouped$`Strongly agree`)
  
  Q61f_summary_grouped$`Neither agree nor disagree (%)` <- 
    as.numeric(Q61f_summary_grouped$`Neither agree nor disagree`)
  
  Q61f_summary_grouped$`Disagree (%)` <- as.numeric(Q61f_summary_grouped$Disagree) + 
    as.numeric(Q61f_summary_grouped$`Strongly disagree`)
  
  #pivot table
  Q61f_summary_long <-
    as.data.frame(
      Q61f_summary_grouped[, c("MA name", "N",
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
  Q61f_summary_long$key <-
    factor(
      Q61f_summary_long$key,
      levels = c(
        "Disagree (%)",
        "Neither agree nor disagree (%)",
        "Agree (%)"
      )
    )
  
  Q61f <- clean_plot_data(Q61f_summary_long)
  Q61f
}

plot_q61f_fishing_change_behavior <- function(.data, ...){

  
  .data_plot <- prep_q61f_fishing_change_behavior(.data)
  
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