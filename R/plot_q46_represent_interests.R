plot_q46_represent_interests <- function(.data, ...){
  
  
  .data <- .data %>% 
    dplyr::select(maa, `46_represent_interests`)
  
  .data_plot <-prep_data_for_plot(
    .data, 
    focus_var = `46_represent_interests`, 
    type = "facet"
  )
  
  p <- plot_horiz_bar(
    .data_plot,
    title = "Proportion of community members who agree that the \nfisheries management body represents their interests to the fishery",
    facet_var = key,
    breaks = seq(0, 100, 25)
  )
  
  result <- list(
    plot = p,
    data = .data_plot
  )
}