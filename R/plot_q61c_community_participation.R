
prep_q61c_community_participation <- function(.data){
  hhs_Q61c <- .data[,c("maa", "61c_community_participation")] %>%
    dplyr::filter (`61c_community_participation` %in% c(1:5)) %>%
    rbind(c(NA,1),c(NA,2),c(NA,3),c(NA,4),c(NA,5))
  
  Q61c_summary <- proportion (hhs_Q61c$`61c_community_participation`,
                              hhs_Q61c$maa,
                              3,5)
  
  colnames(Q61c_summary) <- c("MA name",
                              "N",
                              "Strongly disagree",
                              "Disagree",
                              "Neither agree nor disagree (%)",
                              "Agree",
                              "Strongly agree")
  
  Q61c_summary_grouped <- Q61c_summary %>%
    dplyr::filter (`MA name` != "Mean ± SE")
  
  Q61c_summary_grouped$`Agree (%)` <- as.numeric(Q61c_summary_grouped$Agree) + 
    as.numeric(Q61c_summary_grouped$`Strongly agree`)
  
  Q61c_summary_grouped$`Neither agree nor disagree (%)` <- 
    as.numeric(Q61c_summary_grouped$`Neither agree nor disagree`)
  
  Q61c_summary_grouped$`Disagree (%)` <- as.numeric(Q61c_summary_grouped$Disagree) + 
    as.numeric(Q61c_summary_grouped$`Strongly disagree`)
  #pivot table
  Q61c_summary_long <-
    as.data.frame(
      Q61c_summary_grouped %>% 
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
  Q61c_summary_long$key <-
    factor(
      Q61c_summary_long$key,
      levels = c(
        "Disagree (%)",
        "Neither agree nor disagree (%)",
        "Agree (%)"
      )
    )
  Q61c <- clean_plot_data(Q61c_summary_long)
  Q61c
}

plot_q61c_community_participation <- function(.data, ...){

  .data_plot <- prep_q61c_community_participation(.data)
  
  p <- plot_horiz_bar(
    .data_plot,
    title = "Proportion of fishers who believe that participation in management \nwill help to maintain or improve fish catch",
    facet_var = key
  )

  result <- list(
    plot = p,
    data = .data_plot
  )
}