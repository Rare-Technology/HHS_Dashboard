
prep_q61h_help_neighbors <- function(.data){
  hhs_Q61h <- .data[,c("maa", "61h_help_neighbors")] %>%
    dplyr::filter(`61h_help_neighbors` %in% c(1:5)) %>%
    rbind(c(NA,1),c(NA,2),c(NA,3),c(NA,4),c(NA,5))
  
  Q61h_summary <- proportion(hhs_Q61h$`61h_help_neighbors`,
                             hhs_Q61h$maa,
                             rounding = 3,
                             type = 5)
  
  colnames(Q61h_summary) <-
    c( "MA name",
       "N",
       "Strongly disagree",
       "Disagree",
       "Neither agree nor disagree",
       "Agree",
       "Strongly agree"
    )
  
  Q61h_summary_grouped <- Q61h_summary %>% 
    dplyr::filter (`MA name` != "Mean ± SE")
  
  Q61h_summary_grouped$`Agree (%)` <- as.numeric(Q61h_summary_grouped$Agree) + 
    as.numeric(Q61h_summary_grouped$`Strongly agree`)
  
  Q61h_summary_grouped$`Neither agree nor disagree (%)` <- 
    as.numeric(Q61h_summary_grouped$`Neither agree nor disagree`)
  
  Q61h_summary_grouped$`Disagree (%)` <- as.numeric(Q61h_summary_grouped$Disagree) + 
    as.numeric(Q61h_summary_grouped$`Strongly disagree`)
  
  #pivot table
  Q61h_summary_long <-
    as.data.frame(
      Q61h_summary_grouped[, c("MA name", "N",
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
  Q61h_summary_long$key <-
    factor(
      Q61h_summary_long$key,
      levels = c(
        "Disagree (%)",
        "Neither agree nor disagree (%)",
        "Agree (%)"
      )
    )
  
  Q61h <- clean_plot_data(Q61h_summary_long)
  Q61h
}

plot_q61h_help_neighbors <- function(.data, ...){
  .data_plot <- prep_q61h_help_neighbors(.data)
  
  p <- plot_horiz_bar(
    .data_plot,
    title = "Proportion of community members who feel that it is important for them \nto be able to help their neighbors in times of need",
    facet_var = key
  )
  
  result <- list(
    plot = p,
    data = .data_plot
  )
}