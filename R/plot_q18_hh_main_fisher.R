plot_q18_hh_main_fisher <- function(.data, ...) {
  hhs_Q18 <- .data %>% 
    dplyr::filter(!(`18_hh_main_fisher` %in% c("Not a fisher", "Not answered")))
  
  .data_plot <- prep_data_for_plot(hhs_Q18, `18_hh_main_fisher`, type = "stacked")

  p <- plot_bubble(
    .data_plot,
    title = "Proportion of main fishers in the household \nthat fish as member of a group",
  )
  
  result <- list(
    plot = p,
    data = .data_plot
  )
}
