
prep_q08_religion <- function(.data) {
  hhs_Q8 <- .data %>%
    dplyr::filter(`8_religion` != "") %>%
    dplyr::select(maa, `8_religion`)

  Q8_summary <- proportion(hhs_Q8[[2]],
    hhs_Q8[[1]],
    3,
    type = length(unique(hhs_Q8[[2]]))
  )
  Q8_summary_long <- Q8_summary %>%
    tidyr::pivot_longer(
      cols = c(3:ncol(Q8_summary)),
      names_to = "key",
      values_to = "Proportion (%)"
    )

  Q8 <- clean_plot_data(Q8_summary_long)
  Q8$key <- stringr::str_replace_all(Q8$key, "[.]", " ")

  Q8
}

plot_q08_religion <- function(.data, use_plotly = TRUE) {


  .data_plot <- prep_q08_religion(.data)

  
  p <- plot_horiz_bar_stacked(
    .data_plot,
    title = "Religion of the head of the Household"
  )
  
  

  if(use_plotly)
    ggplotly(p)
  
  p
  
}
