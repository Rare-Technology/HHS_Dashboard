prep_q16_processing <- function(.data, ...){
  ### Tue Mar 7, 2023
  # When the legacy data was combined with the 2022 revision data, missing values for Q15 were
  # mapped to 0, so figured obtained from the dashboard previously may not match exactly.
  hhs_Q16 <- .data[,c("maa", 
                               "16a_processing_men",
                               "16b_processing_women",
                               "16c_processing_children")]
  
  Q16_summary <- hhs_Q16 %>%
    dplyr::group_by(maa) %>%
    dplyr::summarise(
      "N" = dplyr::n(),
      "men" = round(mean(`16a_processing_men`, na.rm =
                           TRUE), 1),
      "women" = round(mean(`16b_processing_women`, na.rm =
                             TRUE), 1),
      "children" = round(mean(`16c_processing_children`, na.rm =
                                TRUE), 1)
    )
  
  Q16_summary <- rbind(Q16_summary,
                       c(
                         NA,
                         sum(Q16_summary$N),
                         compute_summary_line(Q16_summary$`men`, 1),
                         compute_summary_line(Q16_summary$`women`, 1),
                         compute_summary_line(Q16_summary$`children`, 1)
                       ))
  #plot
  Q16_summary_long <-
    Q16_summary %>% tidyr::pivot_longer(
      cols = c("men", "women", "children"),
      names_to = "key",
      values_to = "Proportion (%)"
    )
  Q16_summary_long$key <-
    factor(
      Q16_summary_long$key,
      levels = c("men", "women", "children")
    )
  
  colnames(Q16_summary_long) <- c("MA name", "N", "Processors", "Proportion (%)")
  
  Q16 <- clean_plot_data(Q16_summary_long)
  
  colnames(Q16) <- c("MA name", "N", "Processors", "Average")
  Q16
}


plot_q16_processing <- function(.data, ...){

  .data_plot <- prep_q16_processing(.data)
  
  
  p <- plot_horiz_bar(
    .data_plot,
    y_var = Average,
    facet_var = Processors,
    title = "Average number of household members that participate in post-processing activities",
    limits = c(0, 3),
    breaks = seq(0, 3, 1),
    y_title = "\nNumber of people"
  )

  result <- list(
    plot = p,
    data = .data_plot
  )

}