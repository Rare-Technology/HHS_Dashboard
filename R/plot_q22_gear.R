plot_q22_gear <- function(.data, ...){
  .data <- .data %>%
    dplyr::mutate(
      dplyr::across(
        stringr::str_subset(names(hhs_data), "^22[a-z]"),
        ~ dplyr::recode(
          .x,
          "No" = 0,
          "Yes" = 1,
          "Not answered" = as.double(NA),
          "Not a fisher" = as.double(NA)
        )
      )
    )
  .data_plot <- prep_data_for_plot_multivar(
    .data,
    select_vars= c(`22a_gear_hand`, `22b_gear_stationary_net`, `22c_gear_mobile_net`,
                   `22d_gear_stationary_line`, `22e_gear_mobile_line`,
                   `22f_gear_explosives`, `22g_gear_other`),
    recoding = c(
      "Hand" = "22a_gear_hand",
      "Stationary net" = "22b_gear_stationary_net",
      "Mobile net" = "22c_gear_mobile_net",
      "Stationary line" = "22d_gear_stationary_line",
      "Mobile line" = "22e_gear_mobile_line",
      "Explosives" = "22f_gear_explosives",
      "Other" = "22g_gear_other")
  
  )
  
  p <- plot_horiz_bar(
    .data_plot,
    y_var = `Percentage (%)`,
    title = "Fishing gear used",
    facet_var = key,
    breaks = c(0, 50, 100)
  )
  
  result <- list(
    plot = p,
    data = .data_plot
  )
  
}
