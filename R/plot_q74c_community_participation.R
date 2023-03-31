prep_q74c_community_participation <- function(.data){
  hhs_Q74c <- .data %>% 
    dplyr::select(maa, `74c_community_participation`) %>%
    dplyr::mutate(`74c_community_participation` = dplyr::recode(
      `74c_community_participation`,
      "Strongly disagree" = 1,
      "Disagree" = 2,
      "Neither agree nor disagree" = 3,
      "Agree" = 4,
      "Strongly agree" = 5,
      .default = as.double(NA)
    )) %>% 
    dplyr::filter (`74c_community_participation` %in% c(1:5)) %>% 
    rbind(c(NA,1),c(NA,2),c(NA,3),c(NA,4),c(NA,5))
  
  Q74c_summary <- proportion (hhs_Q74c$`74c_community_participation`,
                              hhs_Q74c$maa,
                              3,5)
  
  colnames(Q74c_summary) <- c("MA name",
                              "N",
                              "Strongly disagree",
                              "Disagree",
                              "Neither agree nor disagree (%)",
                              "Agree",
                              "Strongly agree")
  
  Q74c_summary_grouped <- Q74c_summary %>%
    dplyr::filter (`MA name` != "Mean ± SE")
  
  Q74c_summary_grouped$`Agree (%)` <- as.numeric(Q74c_summary_grouped$Agree) + 
    as.numeric(Q74c_summary_grouped$`Strongly agree`)
  
  Q74c_summary_grouped$`Neither agree nor disagree (%)` <- 
    as.numeric(Q74c_summary_grouped$`Neither agree nor disagree`)
  
  Q74c_summary_grouped$`Disagree (%)` <- as.numeric(Q74c_summary_grouped$Disagree) + 
    as.numeric(Q74c_summary_grouped$`Strongly disagree`)
  #pivot table
  Q74c_summary_long <-
    as.data.frame(
      Q74c_summary_grouped %>% 
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
  Q74c_summary_long$key <-
    factor(
      Q74c_summary_long$key,
      levels = c(
        "Disagree (%)",
        "Neither agree nor disagree (%)",
        "Agree (%)"
      )
    )
  Q74c <- clean_plot_data(Q74c_summary_long)
  Q74c
}

plot_q74c_community_participation <- function(.data, ...){

  .data_plot <- prep_q74c_community_participation(.data)
  
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