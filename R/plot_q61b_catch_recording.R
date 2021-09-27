
prep_q61b_catch_recording <- function(.data){
  hhs_Q61b <- .data[,c("maa", "61b_catch_recording")] %>%
    dplyr::filter( `61b_catch_recording` %in% c(1:5)) %>%
    rbind(c(NA,1),c(NA,2),c(NA,3),c(NA,4),c(NA,5)) %>%
    droplevels()
  
  Q61b_summary <- proportion(hhs_Q61b$`61b_catch_recording`,
                             hhs_Q61b$maa,
                             3,5)
  
  colnames(Q61b_summary) <-
    c( "MA name",
       "N",
       "Strongly disagree",
       "Disagree",
       "Neither agree nor disagree (%)",
       "Agree",
       "Strongly agree"
    )
  
  Q61b_summary_grouped <- Q61b_summary %>%
    dplyr::filter(`MA name` != "Mean ± SE")
  
  Q61b_summary_grouped$`Disagree (%)` <- as.numeric(Q61b_summary_grouped$Disagree) +
    as.numeric(Q61b_summary_grouped$`Strongly disagree`)
  
  Q61b_summary_grouped$`Neither agree nor disagree (%)` <- 
    as.numeric(Q61b_summary_grouped$`Neither agree nor disagree`)
  
  Q61b_summary_grouped$`Agree (%)`<- as.numeric(Q61b_summary_grouped$Agree) +
    as.numeric(Q61b_summary_grouped$`Strongly agree`)
  
  #pivot table
  Q61b_summary_long <-
    as.data.frame(
      Q61b_summary_grouped [, c("MA name", "N", 
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
  Q61b_summary_long$key <-
    factor(
      Q61b_summary_long$key,
      levels = c(
        "Disagree (%)",
        "Neither agree nor disagree (%)",
        "Agree (%)"
      )
    )
  Q61b <- clean_plot_data(Q61b_summary_long)
  Q61b
}

plot_q61b_catch_recording <- function(.data, ...){

  .data_plot <- prep_q61b_catch_recording(.data)
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