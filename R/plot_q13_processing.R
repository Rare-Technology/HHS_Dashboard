prep_q13_processing <- function(.data, ...){
  hhs_Q13 <- .data[,c("maa", 
                               "13a_processing_men",
                               "13b_processing_women",
                               "13c_processing_children")]
  
  Q13_summary <- hhs_Q13 %>%
    dplyr::group_by(maa) %>%
    dplyr::summarise(
      "N" = dplyr::n(),
      "men" = round(mean(`13a_processing_men`, na.rm =
                           TRUE), 1),
      "women" = round(mean(`13b_processing_women`, na.rm =
                             TRUE), 1),
      "children" = round(mean(`13c_processing_children`, na.rm =
                                TRUE), 1)
    )
  
  Q13_summary <- rbind(Q13_summary,
                       c(
                         NA,
                         sum(Q13_summary$N),
                         compute_summary_line(Q13_summary$`men`, 1),
                         compute_summary_line(Q13_summary$`women`, 1),
                         compute_summary_line(Q13_summary$`children`, 1)
                       ))
  #plot
  Q13_summary_long <-
    Q13_summary %>% tidyr::pivot_longer(
      cols = c("men", "women", "children"),
      names_to = "key",
      values_to = "Proportion (%)"
    )
  Q13_summary_long$key <-
    factor(
      Q13_summary_long$key,
      levels = c("men", "women", "children")
    )
  
  colnames(Q13_summary_long) <- c("MA name", "N", "Processors", "Proportion (%)")
  
  Q13 <- clean_plot_data(Q13_summary_long)
  
  colnames(Q13) <- c("MA name", "N", "Processors", "Average")
  Q13
}


plot_q13_processing <- function(.data, ...){

  .data_plot <- prep_q13_processing(.data)
  
  
  p <- plot_horiz_bar(
    .data_plot,
    y_var = Average,
    facet_var = Processors,
    title = "Average number of household members that participate in post-procesing activities",
    limits = c(0, 3),
    breaks = seq(0, 3, 1),
    y_title = "\nNumber of people"
  )

  result <- list(
    plot = p,
    data = .data_plot
  )

}