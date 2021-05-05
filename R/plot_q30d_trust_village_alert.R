
prep_q30d_trust_village_alert <- function(.data) {
  hhs_Q30d <- .data[, c("maa", "30_trust_village_alert")] %>%
    dplyr::filter(`30_trust_village_alert` %in% c(1:5)) %>%
    rbind(c(NA, 1), c(NA, 2), c(NA, 3), c(NA, 4), c(NA, 5)) %>%
    droplevels()
  # proportion
  Q30d_summary <-
    proportion(hhs_Q30d[[2]],
      hhs_Q30d[[1]],
      3,
      type = 5
    )

  colnames(Q30d_summary) <- c(
    "MA name",
    "N",
    "Strongly disagree",
    "Disagree",
    "Neither agree nor disagree",
    "Agree",
    "Strongly agree"
  )
  Q30d <- Q30d_summary %>%
    dplyr::filter(`MA name` != "Mean ± SE")

  # sum agree and strongly agree
  Q30d$`Social Trust` <- as.numeric(Q30d$Agree) + as.numeric(Q30d$`Strongly agree`)
  Q30d_summary <- Q30d[, c("MA name", "N", "Social Trust")]

  # rename columns for shiny app
  colnames(Q30d_summary) <- c("MA name", "N", "Proportion (%)")

  Q30d <- clean_plot_data(Q30d_summary)
  Q30d
}

plot_q30d_trust_village_alert <- function(.data, ...) {
  .data_plot <- prep_q30d_trust_village_alert(.data)
  plot_horiz_bar(
    .data_plot,
    title = "Proportion of community members who believe that \nyou have that to be alert to someone taking advantage of you"
  )
}
