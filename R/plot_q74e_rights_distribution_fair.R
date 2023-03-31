
prep_q74e_rights_distribution_fair <- function(.data){
  hhs_Q74e <- .data %>% 
    dplyr::select(maa, `74e_rights_distribution_fair`) %>% 
    dplyr::mutate(`74e_rights_distribution_fair` = dplyr::recode(
      `74e_rights_distribution_fair`,
      "Strongly disagree" = 1,
      "Disagree" = 2,
      "Neither agree nor disagree" = 3,
      "Agree" = 4,
      "Strongly agree" = 5
    )) %>% 
    dplyr::filter(`74e_rights_distribution_fair` %in% c(1:5)) %>% 
    rbind(c(NA,1),c(NA,2),c(NA,3),c(NA,4),c(NA,5))
  
  Q74e_summary <- proportion ( hhs_Q74e$`74e_rights_distribution_fair`,
                               hhs_Q74e$maa,
                               3,5)
  
  colnames(Q74e_summary) <-
    c( "MA name",
       "N",
       "Strongly disagree",
       "Disagree",
       "Neither agree nor disagree",
       "Agree",
       "Strongly agree"
    )
  
  Q74e_summary_grouped <- Q74e_summary %>% 
    dplyr::filter (`MA name` != "Mean ± SE")
  
  Q74e_summary_grouped$`Agree (%)` <- as.numeric(Q74e_summary_grouped$Agree) + 
    as.numeric(Q74e_summary_grouped$`Strongly agree`)
  
  Q74e_summary_grouped$`Neither agree nor disagree (%)` <- 
    as.numeric(Q74e_summary_grouped$`Neither agree nor disagree`)
  
  Q74e_summary_grouped$`Disagree (%)` <- as.numeric(Q74e_summary_grouped$Disagree) + 
    as.numeric(Q74e_summary_grouped$`Strongly disagree`)
  
  Q74e_summary <- Q74e_summary_grouped[,c("MA name", "N", 
                                          "Disagree (%)",
                                          "Neither agree nor disagree (%)",
                                          "Agree (%)" )]
  
  #pivot table
  Q74e_summary_long <-
    as.data.frame(
      Q74e_summary_grouped[, c("MA name", "N",
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
  Q74e_summary_long$key <-
    factor(
      Q74e_summary_long$key,
      levels = c(
        "Disagree (%)",
        "Neither agree nor disagree (%)",
        "Agree (%)"
      )
    )
  
  Q74e <- clean_plot_data(Q74e_summary_long)
  Q74e
}

plot_q74e_rights_distribution_fair <- function(.data, ...){

  .data_plot <- prep_q74e_rights_distribution_fair(.data)
 
  p <- plot_horiz_bar(
    .data_plot,
    title = "Proportion of fishers who believe that \naccess rights have been distributed fairly",
    facet_var = key
  )

  result <- list(
    plot = p,
    data = .data_plot
  )
}