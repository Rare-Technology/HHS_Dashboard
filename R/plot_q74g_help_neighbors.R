prep_q74g_help_neighbors <- function(.data){
  hhs_Q74g <- .data %>% 
    dplyr::select(maa, `74g_help_neighbors`) %>%
    dplyr::mutate(`74g_help_neighbors` = dplyr::recode(
      `74g_help_neighbors`,
      "Strongly disagree" = 1,
      "Disagree" = 2,
      "Neither agree nor disagree" = 3,
      "Agree" = 4,
      "Strongly agree" = 5,
      .default = as.double(NA)
    )) %>% 
    dplyr::filter(`74g_help_neighbors` %in% c(1:5)) %>% 
    rbind(c(NA,1),c(NA,2),c(NA,3),c(NA,4),c(NA,5))
  
  Q74g_summary <- proportion(hhs_Q74g$`74g_help_neighbors`,
                             hhs_Q74g$maa,
                             rounding = 3,
                             type = 5)
  
  colnames(Q74g_summary) <-
    c( "MA name",
       "N",
       "Strongly disagree",
       "Disagree",
       "Neither agree nor disagree",
       "Agree",
       "Strongly agree"
    )
  
  Q74g_summary_grouped <- Q74g_summary %>% 
    dplyr::filter (`MA name` != "Mean ± SE")
  
  Q74g_summary_grouped$`Agree (%)` <- as.numeric(Q74g_summary_grouped$Agree) + 
    as.numeric(Q74g_summary_grouped$`Strongly agree`)
  
  Q74g_summary_grouped$`Neither agree nor disagree (%)` <- 
    as.numeric(Q74g_summary_grouped$`Neither agree nor disagree`)
  
  Q74g_summary_grouped$`Disagree (%)` <- as.numeric(Q74g_summary_grouped$Disagree) + 
    as.numeric(Q74g_summary_grouped$`Strongly disagree`)
  
  #pivot table
  Q74g_summary_long <-
    as.data.frame(
      Q74g_summary_grouped[, c("MA name", "N",
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
  Q74g_summary_long$key <-
    factor(
      Q74g_summary_long$key,
      levels = c(
        "Disagree (%)",
        "Neither agree nor disagree (%)",
        "Agree (%)"
      )
    )
  
  Q74g <- clean_plot_data(Q74g_summary_long)
  Q74g
}

plot_q74g_help_neighbors <- function(.data, ...){
  .data_plot <- prep_q74g_help_neighbors(.data)
  
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