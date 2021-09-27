
prep_q61d_strong_enforcement <- function(.data){
  hhs_Q61d <- .data[,c("maa", "61d_strong_enforcement")] %>%
    dplyr::filter (`61d_strong_enforcement` %in% c(1:5)) %>%
    rbind(c(NA,1),c(NA,2),c(NA,3),c(NA,4),c(NA,5))
  
  Q61d_summary <- proportion ( hhs_Q61d$`61d_strong_enforcement`,
                               hhs_Q61d$maa,
                               3,5)
  
  colnames(Q61d_summary) <- c("MA name",
                              "N",
                              "Strongly disagree",
                              "Disagree",
                              "Neither agree nor disagree (%)",
                              "Agree",
                              "Strongly agree")
  
  Q61d_summary_grouped <- Q61d_summary %>%
    dplyr::filter(`MA name` != "Mean ± SE")
  
  Q61d_summary_grouped$`Agree (%)` <- as.numeric(Q61d_summary_grouped$Agree) + 
    as.numeric(Q61d_summary_grouped$`Strongly agree`)
  
  Q61d_summary_grouped$`Neither agree nor disagree (%)` <- 
    as.numeric(Q61d_summary_grouped$`Neither agree nor disagree`)
  
  Q61d_summary_grouped$`Disagree (%)` <- as.numeric(Q61d_summary_grouped$Disagree) + 
    as.numeric(Q61d_summary_grouped$`Strongly disagree`)
  
  #pivot table
  Q61d_summary_long <-
    as.data.frame(
      Q61d_summary_grouped[, c("MA name", "N",
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
  Q61d_summary_long$key <-
    factor(
      Q61d_summary_long$key,
      levels = c(
        "Disagree (%)",
        "Neither agree nor disagree (%)",
        "Agree (%)"
      )
    )
  Q61d <- clean_plot_data(Q61d_summary_long)
  Q61d
}

plot_q61d_strong_enforcement <- function(.data, ...){

  .data_plot <- prep_q61d_strong_enforcement(.data)
  
  p <- plot_horiz_bar(
    .data_plot,
    title = "Proportion of fishers who believe that \nit is important to have a strong enforcement system",
    facet_var = key
  )

  result <- list(
    plot = p,
    data = .data_plot
  )
}