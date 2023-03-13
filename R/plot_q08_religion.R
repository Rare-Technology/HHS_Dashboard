
plot_q08_religion <- function(.data, ...) {
  Q08 <- .data %>% 
    dplyr::select(maa, `8_religion`) %>% 
    dplyr::filter(`8_religion` != "Not Answered")
  
  .data_plot <- prep_data_for_plot(Q08, `8_religion`, type = "stacked")
  
  p <- plot_bubble(
    .data_plot,
    title = "Religion of the head of the Household",
  )
  
  result <- list(
    plot = p,
    data = .data_plot
  )
}
