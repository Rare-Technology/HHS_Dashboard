
prep_q12_household_fishing <- function(.data) {

  hhs_Q12 <- .data %>% 
    dplyr:: select(maa, `12a_fishing_men`, `12b_fishing_women`, `12c_fishing_children`) %>% 
    dplyr::filter(`12a_fishing_men` < 10 & `12a_fishing_men` != "") %>%
    dplyr::filter(`12b_fishing_women` < 10 & `12b_fishing_women` != "") %>%
    dplyr::filter(`12c_fishing_children` < 10 & `12c_fishing_children` != "")

  Q12_summary <- hhs_Q12 %>%
    dplyr::group_by(maa) %>%
    dplyr::summarise(
      "N" = dplyr::n(),
      "fisher men" = round(mean(`12a_fishing_men`,
        na.rm =
          TRUE
      ), 1),
      "fisher women" = round(mean(`12b_fishing_women`,
        na.rm =
          TRUE
      ), 1),
      "fisher children" = round(mean(`12c_fishing_children`,
        na.rm =
          TRUE
      ), 1)
    )

  Q12_summary <- rbind(
    Q12_summary,
    c(
      NA,
      sum(Q12_summary$N),
      compute_summary_line(Q12_summary$`fisher men`, 1),
      compute_summary_line(Q12_summary$`fisher women`, 1),
      compute_summary_line(Q12_summary$`fisher children`, 1)
    )
  )

  # plot
  Q12_summary_long <-
    Q12_summary %>% tidyr::pivot_longer(
      cols = c("fisher men", "fisher women", "fisher children"),
      names_to = "key",
      values_to = "Proportion (%)"
    )
  Q12_summary_long$key <-
    factor(
      Q12_summary_long$key,
      levels = c("fisher men", "fisher women", "fisher children")
    )

  colnames(Q12_summary_long) <- c("MA name", "N", "Fishers", "Proportion (%)")

  Q12 <- clean_plot_data(Q12_summary_long)

  colnames(Q12) <- c("MA name", "N", "Fishers", "Average")
  
  Q12
}

plot_q12_household_fishing <- function(.data, ...) {
  .data_plot <- prep_q12_household_fishing(.data)
  
  .data_plot %>%
    plot_horiz_bar(
      y_var = Average,
      title = glue::glue("Average number of household members \nthat go fishing regularly"),
      y_title = "\nNumber of people",
      facet_var = Fishers,
      limits = c(0, 3),
      breaks = seq(0, 3, 1)
    )
}
