prep_q48_reserve_fishing_allowed <- function(.data){
  hhs_Q48 <- .data %>%
    dplyr::filter(`48_reserve_fishing_allowed` %in% c("No", "Yes")) %>% 
    dplyr::select(maa, `48_reserve_fishing_allowed`)
  
  #proportion
  Q48_summary <-
    proportion(hhs_Q48[[2]],
               hhs_Q48[[1]],
               3,2)[,-4]
  
  #rename
  colnames(Q48_summary) <-
    c("MA name", "N", "Proportion (%)")
  
  #data for plot
  Q48 <- clean_plot_data(Q48_summary)
  Q48
}

plot_q48_reserve_fishing_allowed <- function(.data, ...){
  
  .data_plot <- prep_q48_reserve_fishing_allowed(.data)
  
  p <- plot_horiz_bar(
    .data_plot,
    title = "Proportion of community members who are aware \nthat fishing is not allowed in the reserve area"
  )

  result <- list(
    plot = p,
    data = .data_plot
  )
}