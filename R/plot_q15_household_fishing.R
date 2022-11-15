
prep_q15_household_fishing <- function(.data) {

  hhs_Q15 <- .data %>% 
    dplyr:: select(maa, `15a_fishing_men`, `15b_fishing_women`, `15c_fishing_children`) %>% 
    dplyr::filter(`15a_fishing_men` < 10 & `15a_fishing_men` != "") %>%
    dplyr::filter(`15b_fishing_women` < 10 & `15b_fishing_women` != "") %>%
    dplyr::filter(`15c_fishing_children` < 10 & `15c_fishing_children` != "")

  Q15_summary <- hhs_Q15 %>%
    dplyr::group_by(maa) %>%
    dplyr::summarise(
      "N" = dplyr::n(),
      "fisher men" = round(mean(`15a_fishing_men`,
        na.rm =
          TRUE
      ), 1),
      "fisher women" = round(mean(`15b_fishing_women`,
        na.rm =
          TRUE
      ), 1),
      "fisher children" = round(mean(`15c_fishing_children`,
        na.rm =
          TRUE
      ), 1)
    )

  Q15_summary <- rbind(
    Q15_summary,
    c(
      NA,
      sum(Q15_summary$N),
      compute_summary_line(Q15_summary$`fisher men`, 1),
      compute_summary_line(Q15_summary$`fisher women`, 1),
      compute_summary_line(Q15_summary$`fisher children`, 1)
    )
  )

  # plot
  Q15_summary_long <-
    Q15_summary %>% tidyr::pivot_longer(
      cols = c("fisher men", "fisher women", "fisher children"),
      names_to = "key",
      values_to = "Proportion (%)"
    )
  Q15_summary_long$key <-
    factor(
      Q15_summary_long$key,
      levels = c("fisher men", "fisher women", "fisher children")
    )

  colnames(Q15_summary_long) <- c("MA name", "N", "Fishers", "Proportion (%)")

  Q15 <- clean_plot_data(Q15_summary_long)

  colnames(Q15) <- c("MA name", "N", "Fishers", "Average")
  
  Q15
}

plot_q15_household_fishing <- function(.data, ...) {
  .data_plot <- prep_q15_household_fishing(.data)
  
  p <- .data_plot %>%
    plot_horiz_bar(
      y_var = Average,
      title = glue::glue("Average number of household members \nthat go fishing regularly"),
      y_title = "\nNumber of people",
      facet_var = Fishers,
      limits = c(0, 3),
      breaks = seq(0, 3, 1)
    )
  
  result <- list(
    plot = p,
    data = .data_plot
  )
}
