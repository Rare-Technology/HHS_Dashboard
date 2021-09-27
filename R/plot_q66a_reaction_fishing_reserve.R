
# prep_q66_reaction_fishing_reserve <- function(.data) {
# 
# }

plot_q66a_reaction_fishing_reserve <- function(.data, ...) {
  codes <- c(
    "No sanction" = "1. Non-sanction",
    "No sanction" = "non-sanction",
    "No sanction" = "Non-saction",
    "No sanction" = "1. No aplicaría ninguna sanción",
    "Informal sanction" = "2. Negative informal sanction",
    "Informal sanction" = "2. Aplicaría una sanción negativa informal",
    "Informal sanction" = "negative informal sanction",
    "Informal sanction" = "negative formal sanction",
    "Formal sanction" = "3. Aplicaría una sanción negativa formal",
    "Formal sanction" = "Negative formal sanction"
  )

  .data_plot <- prep_data_for_plot(
    .data,
    focus_var = `66_reaction_fishing_reserve`,
    recoding = codes,
    type = "facet",
    key_order = names(codes) %>% unique()
  )

  p <- plot_horiz_bar(
    .data_plot,
    title = "Proportion of households that answered yes to the previous question and would sanction fishers fishing in the reserve",
    facet_var = key
  )

  result <- list(
    plot = p,
    data = .data_plot
  )
}
