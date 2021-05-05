
prep_q29_family_income <- function(.data) {
  hhs_Q29 <- .data[, c("maa", "29_family_income")] %>%
    dplyr::filter(`29_family_income` %in% c(
      "Sufficient",
      "Insufficient",
      "Tight"
    )) %>%
    rbind(
      c(NA, "Sufficient"),
      c(NA, "Insufficient"),
      c(NA, "Tight")
    ) %>%
    droplevels()
  # proportion
  Q29_summary <-
    proportion(hhs_Q29[[2]], hhs_Q29[[1]], 3, 3)
  # rename column
  colnames(Q29_summary) <-
    c(
      "MA name",
      "N",
      "Insufficient (%)",
      "Sufficient (%)",
      "Tight (%)"
    )

  # plot set up
  Q29_longer <-
    Q29_summary %>% tidyr::pivot_longer(
      cols = c("Insufficient (%)", "Sufficient (%)", "Tight (%)"),
      names_to = "key",
      values_to = "Proportion (%)"
    )
  Q29_longer$key <-
    factor(Q29_longer$key,
      levels = c("Insufficient (%)", "Tight (%)", "Sufficient (%)")
    )

  Q29 <- clean_plot_data(Q29_longer)
  Q29
}

plot_q29_family_income <- function(.data, ...) {
  .data_plot <- prep_q29_family_income(.data)

  plot_horiz_bar(
    .data_plot,
    title = "Proportion of community members that have \nsufficient income to cover their family's needs",
    facet_var = key
  )
}
