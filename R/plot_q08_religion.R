
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

  .data_plot <- prep_q8_religion(.data)

  p <- .data_plot %>%
    ggplot(aes(`MA name`, `Proportion (%)`, N = N)) +
    geom_bar(aes(fill = key),
      position = position_stack(reverse = TRUE),
      stat = "identity",
      alpha = 0.8
    ) +
    labs(
      title = "Religion of the head of the Household",
      x = NULL,
      y = "Proportion (%)"
    ) +
    scale_fill_brewer(palette = "Blues", direction = -1) +
    scale_y_continuous(
      limits = c(0, 105),
      breaks = seq(0, 100, 20)
    ) +
    coord_flip(clip = "on") +
    guides(fill = guide_legend(reverse = TRUE)) +
    theme_rare() +
    theme(legend.position = "right")

  if (use_plotly) {
    return(ggplotly(p, height = 750))
  }

  p
}
