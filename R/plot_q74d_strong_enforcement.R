
prep_q74d_strong_enforcement <- function(.data){
  hhs_Q74d <- .data %>% 
    dplyr::select(maa, `74d_strong_enforcement`) %>%
    dplyr::mutate(`74d_strong_enforcement` = dplyr::recode(
      `74d_strong_enforcement`,
      "Strongly disagree" = 1,
      "Disagree" = 2,
      "Neither agree nor disagree" = 3,
      "Agree" = 4,
      "Strongly agree" = 5,
      .default = as.double(NA)
    )) %>% 
    dplyr::filter(`74d_strong_enforcement` %in% c(1:5)) %>% 
    rbind(c(NA,1),c(NA,2),c(NA,3),c(NA,4),c(NA,5))
  
  Q74d_summary <- proportion ( hhs_Q74d$`74d_strong_enforcement`,
                               hhs_Q74d$maa,
                               3,5)
  
  colnames(Q74d_summary) <- c("MA name",
                              "N",
                              "Strongly disagree",
                              "Disagree",
                              "Neither agree nor disagree (%)",
                              "Agree",
                              "Strongly agree")
  
  Q74d_summary_grouped <- Q74d_summary %>%
    dplyr::filter(`MA name` != "Mean ± SE")
  
  Q74d_summary_grouped$`Agree (%)` <- as.numeric(Q74d_summary_grouped$Agree) + 
    as.numeric(Q74d_summary_grouped$`Strongly agree`)
  
  Q74d_summary_grouped$`Neither agree nor disagree (%)` <- 
    as.numeric(Q74d_summary_grouped$`Neither agree nor disagree`)
  
  Q74d_summary_grouped$`Disagree (%)` <- as.numeric(Q74d_summary_grouped$Disagree) + 
    as.numeric(Q74d_summary_grouped$`Strongly disagree`)
  
  #pivot table
  Q74d_summary_long <-
    as.data.frame(
      Q74d_summary_grouped[, c("MA name", "N",
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
  Q74d_summary_long$key <-
    factor(
      Q74d_summary_long$key,
      levels = c(
        "Disagree (%)",
        "Neither agree nor disagree (%)",
        "Agree (%)"
      )
    )
  Q74d <- clean_plot_data(Q74d_summary_long)
  Q74d
}

plot_q74d_strong_enforcement <- function(.data, ...){

  .data_plot <- prep_q74d_strong_enforcement(.data)
  
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