plot_q34_gear_restrictions <- function(.data, ...) {
  
  .data_plot <- .data %>%
    dplyr::filter(`34_gear_restrictions` %in% c(0, 1, -1)) %>%
    dplyr::mutate(
      `34_gear_restrictions` = ifelse(`34_gear_restrictions` == -1, 0, `34_gear_restrictions`)
    ) %>%
    prep_data_for_plot(
      `34_gear_restrictions`,
      type = "bar",
      bar_column = `1`
    )

  p <- plot_horiz_bar(
    .data_plot,
    title = "Proportion of community members who are aware of gear restrictions in the managed access area"
  )
  
  result <- list(
    plot = p,
    data = .data_plot
  )
}
