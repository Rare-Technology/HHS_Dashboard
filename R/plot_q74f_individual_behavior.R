prep_q74f_individual_behavior <- function(.data){
  hhs_Q74f <- .data %>% 
    dplyr::select(maa, `74f_individual_behavior`) %>%
    dplyr::mutate(`74f_individual_behavior` = dplyr::recode(
      `74f_individual_behavior`,
      "Strongly disagree" = 1,
      "Disagree" = 2,
      "Neither agree nor disagree" = 3,
      "Agree" = 4,
      "Strongly agree" = 5,
      .default = as.double(NA)
    )) %>% 
    dplyr::filter(`74f_individual_behavior` %in% c(1:5)) %>% 
    rbind(c(NA,1),c(NA,2),c(NA,3),c(NA,4),c(NA,5))
  
  Q74f_summary <- proportion(
    hhs_Q74f$`74f_individual_behavior`,
    hhs_Q74f$maa,
    rounding = 3,
    type = 5
  )
  
  colnames(Q74f_summary) <-
    c( "MA name",
       "N",
       "Strongly disagree",
       "Disagree",
       "Neither agree nor disagree",
       "Agree",
       "Strongly agree"
    )
  
  Q74f_summary_grouped <- Q74f_summary %>% 
    dplyr::filter (`MA name` != "Mean ± SE")
  
  Q74f_summary_grouped$`Agree (%)` <- as.numeric(Q74f_summary_grouped$Agree) + 
    as.numeric(Q74f_summary_grouped$`Strongly agree`)
  
  Q74f_summary_grouped$`Neither agree nor disagree (%)` <- 
    as.numeric(Q74f_summary_grouped$`Neither agree nor disagree`)
  
  Q74f_summary_grouped$`Disagree (%)` <- as.numeric(Q74f_summary_grouped$Disagree) + 
    as.numeric(Q74f_summary_grouped$`Strongly disagree`)
  
  #pivot table
  Q74f_summary_long <-
    as.data.frame(
      Q74f_summary_grouped[, c("MA name", "N",
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
  Q74f_summary_long$key <-
    factor(
      Q74f_summary_long$key,
      levels = c(
        "Disagree (%)",
        "Neither agree nor disagree (%)",
        "Agree (%)"
      )
    )
  
  Q74f <- clean_plot_data(Q74f_summary_long)
  Q74f
}

plot_q74f_individual_behavior <- function(.data, ...){
  .data_plot <- prep_q74f_individual_behavior(.data)
  
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