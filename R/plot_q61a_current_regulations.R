
prep_q61a_current_regulations <- function(.data){
  hhs_Q61a <- .data[,c("maa", "61a_current_regulations")] %>%
                             dplyr::filter(`61a_current_regulations` %in% c(1:5)) %>%
                                rbind(c(NA,1),c(NA,2),c(NA,3),c(NA,4),c(NA,5))

           Q61a_summary <- proportion (hhs_Q61a$`61a_current_regulations`,
                                        hhs_Q61a$maa,
                                        3,5)

           Q61a_summary_grouped <- Q61a_summary %>%
                                     dplyr::filter (`MA name` != "Mean ± SE")
           #grouped
           Q61a_summary_grouped$Disagree <-  as.numeric(Q61a_summary_grouped$X1) +
                                             as.numeric(Q61a_summary_grouped$X2)

           Q61a_summary_grouped$Neither <-
              as.numeric(Q61a_summary_grouped$X3)

           Q61a_summary_grouped$Agree <-  as.numeric(Q61a_summary_grouped$X4) +
                                          as.numeric(Q61a_summary_grouped$X5)

           Q61a_summary <-
              rbind(
                 Q61a_summary_grouped[,c("MA name", "N",
                                         "Disagree",
                                         "Neither",
                                         "Agree")],
                  c(NA,
                    sum(as.numeric(Q61a_summary_grouped$N)),
                    compute_summary_line(Q61a_summary_grouped$Disagree, 1),
                    compute_summary_line(Q61a_summary_grouped$Neither, 1),
                    compute_summary_line(Q61a_summary_grouped$Agree, 1)
                 )
              )

           colnames(Q61a_summary) <-
              c("MA name",
                "N",
                "Disagree (%)",
                "Neither (%)",
                "Agree (%)")
           #pivot table
           Q61a_summary_long <-
              Q61a_summary %>% tidyr::pivot_longer(
                 cols = c(
                    "Disagree (%)",
                    "Neither (%)",
                    "Agree (%)"
                 ),
                 names_to = "key",
                 values_to = "Proportion (%)"
              )
           Q61a_summary_long$key <-
              factor(
                 Q61a_summary_long$key,
                 levels = c(
                    "Disagree (%)",
                    "Neither (%)",
                    "Agree (%)"
                 )
              )

           Q61a <- clean_plot_data(Q61a_summary_long)
           Q61a
}

plot_q61a_current_regulations <- function(.data, ...){
  
  
  .data_plot <-prep_q61a_current_regulations(.data)
  
  
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