plot_q45_gear_restrictions <- function(.data, ...) {
  
  .data_plot <- .data %>%
    dplyr::filter(`45_gear_restrictions` %in% c("Yes", "No")) %>%
    # dplyr::mutate(
    #   `45_gear_restrictions` = ifelse(`45_gear_restrictions` == -1, 0, `45_gear_restrictions`)
    # ) %>%
    prep_data_for_plot(
      `45_gear_restrictions`,
      type = "bar",
      bar_column = `Yes`
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
