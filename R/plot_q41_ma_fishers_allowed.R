
prep_q41_ma_fishers_allowed <- function(.data){
  hhs_Q41 <- .data[,c("maa", "41_ma_fishers_allowed")] %>%
    dplyr::filter(`41_ma_fishers_allowed` %in% 
                    c("Community only",
                      "Don't know",
                      "No managed access",
                      "No restrictions",
                      "With authorization",
                      "Without authorization")) %>%
    rbind(c(NA, "Community only"),
          c(NA, "Don't know"),
          c(NA, "No managed access"),
          c(NA, "No restrictions"),
          c(NA, "With authorization"),
          c(NA, "Without authorization")) %>%
    droplevels()
  
  Q41_summary <- proportion (hhs_Q41[[2]],
                             hhs_Q41[[1]],
                             3,6)
  
  colnames(Q41_summary) <- c("MA name", "N", 
                             "Community only",
                             "Don't know",
                             "No managed access",
                             "No restrictions",
                             "With authorization",
                             "Without authorization")
  #pivot table
  Q41_summary_long <-
    Q41_summary %>% tidyr::pivot_longer(
      cols = c(
        "Community only",
        "Don't know",
        "No managed access",
        "No restrictions",
        "With authorization",
        "Without authorization"
      ),
      names_to = "key",
      values_to = "Proportion (%)"
    )
  Q41_summary_long$key <-
    factor(
      Q41_summary_long$key,
      levels = c(
        "Community only",
        "With authorization",
        "Without authorization",
        "No restrictions",
        "Don't know",
        "No managed access"
      )
    )
  
  Q41 <- clean_plot_data(Q41_summary_long)
  
  Q41
}

plot_q41_ma_fishers_allowed <- function(.data, ...){

         .data_plot <- prep_q41_ma_fishers_allowed(.data)
         
         plot_horiz_bar(
           .data_plot,
           title = "Proportion of households that are aware of who is allowed \nto fish in the fisheries management/managed access area?",
           facet_var = `key`,
           breaks = seq(0, 100, 30)
         )

}