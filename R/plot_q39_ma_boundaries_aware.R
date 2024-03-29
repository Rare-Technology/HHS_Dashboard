
prep_q39_ma_boundaries_aware <- function(.data){
  hhs_Q39 <- .data[,c("maa", "39_ma_boundaries_aware")] %>%
    subset(`39_ma_boundaries_aware` %in% c("Agree", "Neither", 
                                           "Disagree",
                                           "Strongly agree",
                                           "Strongly disagree")) %>%
    rbind(c(NA, "Agree"), 
          c(NA, "Neither"), 
          c(NA, "Disagree"), 
          c(NA, "Strongly agree"), 
          c(NA, "Strongly disagree")) %>%
    droplevels()
  
  #proportion
  Q39_summary <-
    proportion(hhs_Q39[[2]],
               hhs_Q39[[1]],
               3,5)
  #rename
  colnames(Q39_summary) <-
    c(
      "MA name",
      "N",
      "Agree",
      "Disagree",
      "Neither",
      "Strongly agree",
      "Strongly disagree"
    )
  
  #pivot table
  Q39_summary_long <-
    Q39_summary %>% tidyr::pivot_longer(
      cols = c(
        "Strongly disagree",
        "Disagree",
        "Neither",
        "Agree",
        "Strongly agree"
      ),
      names_to = "key",
      values_to = "Proportion (%)"
    )
  Q39_summary_long$key <-
    factor(
      Q39_summary_long$key,
      levels = c(
        "Strongly disagree",
        "Disagree",
        "Neither",
        "Agree",
        "Strongly agree"
      )
    )
  
  Q39 <- clean_plot_data (Q39_summary_long)
  Q39
}

plot_q39_ma_boundaries_aware <- function(.data, ...){
  .data_plot <- prep_q39_ma_boundaries_aware(.data)
  
  p <- plot_horiz_bar(
    .data_plot,
    title = "Proportion of fishers aware of the boundaries \nof the fisheries management/managed access area",
    facet_var = key
  )
  
  result <- list(
    plot = p,
    data = .data_plot
  )
}