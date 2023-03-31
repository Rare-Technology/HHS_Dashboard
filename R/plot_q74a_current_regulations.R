prep_q74a_current_regulations <- function(.data){
  hhs_Q74a <- .data %>% 
    dplyr::select(maa, `74a_current_regulations`) %>% 
    dplyr::mutate(`74a_current_regulations` = dplyr::recode(
      `74a_current_regulations`,
      "Strongly disagree" = 1,
      "Disagree" = 2,
      "Neither agree nor disagree" = 3,
      "Agree" = 4,
      "Strongly agree" = 5
    )) %>% 
    dplyr::filter(`74a_current_regulations` %in% c(1:5)) %>% 
    rbind(c(NA,1),c(NA,2),c(NA,3),c(NA,4),c(NA,5))

    Q74a_summary <- proportion (hhs_Q74a$`74a_current_regulations`,
                                hhs_Q74a$maa,
                                3,5)
    
    Q74a_summary_grouped <- Q74a_summary %>%
                             dplyr::filter (`MA name` != "Mean ± SE")
    #grouped
    Q74a_summary_grouped$Disagree <-  as.numeric(Q74a_summary_grouped$X1) +
                                     as.numeric(Q74a_summary_grouped$X2)
    
    Q74a_summary_grouped$Neither <-
      as.numeric(Q74a_summary_grouped$X3)
    
    Q74a_summary_grouped$Agree <-  as.numeric(Q74a_summary_grouped$X4) +
                                  as.numeric(Q74a_summary_grouped$X5)
    
    Q74a_summary <-
      rbind(
         Q74a_summary_grouped[,c("MA name", "N",
                                 "Disagree",
                                 "Neither",
                                 "Agree")],
          c(NA,
            sum(as.numeric(Q74a_summary_grouped$N)),
            compute_summary_line(Q74a_summary_grouped$Disagree, 1),
            compute_summary_line(Q74a_summary_grouped$Neither, 1),
            compute_summary_line(Q74a_summary_grouped$Agree, 1)
         )
      )
    
    colnames(Q74a_summary) <-
      c("MA name",
        "N",
        "Disagree (%)",
        "Neither (%)",
        "Agree (%)")
    
    #pivot table
    Q74a_summary_long <-
      Q74a_summary %>% tidyr::pivot_longer(
         cols = c(
            "Disagree (%)",
            "Neither (%)",
            "Agree (%)"
         ),
         names_to = "key",
         values_to = "Proportion (%)"
      )
    
    Q74a_summary_long$key <-
      factor(
         Q74a_summary_long$key,
         levels = c(
            "Disagree (%)",
            "Neither (%)",
            "Agree (%)"
         )
      )
    
    Q74a <- clean_plot_data(Q74a_summary_long)
    Q74a
}

plot_q74a_current_regulations <- function(.data, ...){
  .data_plot <-prep_q74a_current_regulations(.data)
  
  p <- plot_horiz_bar(
    .data_plot,
    title = "Proportion of community members who feel that current fishing regulations \nare effective at managing the fishery and at ensuring catches remain stable",
    facet_var = key
  )
  
  result <- list(
    plot = p,
    data = .data_plot
  )
}