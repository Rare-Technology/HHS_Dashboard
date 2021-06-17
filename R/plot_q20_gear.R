
plot_q20_gear <- function(.data, ...){


.data_plot <- prep_data_for_plot_multivar(
  .data,
  select_vars= c(`20a_gear_hand`, `20b_gear_stationary_net`, `20c_gear_mobile_net`,
                 `20d_gear_stationary_line`, `20e_gear_mobile_line`,
                 `20f_gear_explosives`, `20g_gear_other`),
  recoding = c(
    "Hand" = "20a_gear_hand",
    "Stationary net" = "20b_gear_stationary_net",
    "Mobile net" = "20c_gear_mobile_net",
    "Stationary line" = "20d_gear_stationary_line",
    "Mobile line" = "20e_gear_mobile_line",
    "Explosives" = "20f_gear_explosives",
    "Other" = "20g_gear_other")

)

plot_horiz_bar(
  .data_plot,
  y_var = `Percentage (%)`,
  title = "Fishing gear used",
  facet_var = key,
  breaks = c(0, 50, 100)
)
  
  
  
}
