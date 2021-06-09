


prep_data_for_plot_multivar(
  hhs_data,
  select_vars= c(`20a_gear_hand`, `20b_gear_stationary_net`, `20c_gear_mobile_net`,
                 `20d_gear_stationary_line`, `20e_gear_mobile_line`,
                 `20f_gear_explosives`, `20g_gear_other`),
  group_by_var = maa,
  var_names,
  key_name = "key",
  values_name = "Proportion (%)",
  my_func = function(x){round(mean(x, na.rm = TRUE), 1)}
)
