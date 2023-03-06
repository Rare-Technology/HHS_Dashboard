prep_q74b_catch_recording <- function(.data){
  hhs_Q74b <- .data %>% 
    dplyr::select(maa, `74b_catch_recording`) %>%
    dplyr::mutate(`74b_catch_recording` = dplyr::recode(
      `74b_catch_recording`,
      "Strongly disagree" = 1,
      "Disagree" = 2,
      "Neither agree nor disagree" = 3,
      "Agree" = 4,
      "Strongly agree" = 5,
      .default = as.double(NA)
    )) %>% 
    dplyr::filter(`74b_catch_recording` %in% c(1:5))
  
  Q74b_summary <- proportion(hhs_Q74b$`74b_catch_recording`,
                             hhs_Q74b$maa,
                             3,5)
  
  colnames(Q74b_summary) <-
    c( "MA name",
       "N",
       "Strongly disagree",
       "Disagree",
       "Neither agree nor disagree (%)",
       "Agree",
       "Strongly agree"
    )
  
  Q74b_summary_grouped <- Q74b_summary %>%
    dplyr::filter(`MA name` != "Mean ± SE")
  
  Q74b_summary_grouped$`Disagree (%)` <- as.numeric(Q74b_summary_grouped$Disagree) +
    as.numeric(Q74b_summary_grouped$`Strongly disagree`)
  
  Q74b_summary_grouped$`Neither agree nor disagree (%)` <- 
    as.numeric(Q74b_summary_grouped$`Neither agree nor disagree`)
  
  Q74b_summary_grouped$`Agree (%)`<- as.numeric(Q74b_summary_grouped$Agree) +
    as.numeric(Q74b_summary_grouped$`Strongly agree`)
  
  #pivot table
  Q74b_summary_long <-
    as.data.frame(
      Q74b_summary_grouped [, c("MA name", "N", 
                                "Disagree (%)", 
                                "Neither agree nor disagree (%)",
                                "Agree (%)")] %>% 
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
  Q74b_summary_long$key <-
    factor(
      Q74b_summary_long$key,
      levels = c(
        "Disagree (%)",
        "Neither agree nor disagree (%)",
        "Agree (%)"
      )
    )
  Q74b <- clean_plot_data(Q74b_summary_long)
  Q74b
}

plot_q74b_catch_recording <- function(.data, ...){

  .data_plot <- prep_q74b_catch_recording(.data)
  p <- plot_horiz_bar(
    .data_plot,
    title = "\nProportion of fishers who believe that registering and recording \nwill help to maintain or improve fish catch",
    facet_var = key
  )

  result <- list(
    plot = p,
    data = .data_plot
  )
}