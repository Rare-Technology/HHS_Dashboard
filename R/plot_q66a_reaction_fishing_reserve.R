
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

  #          Q66_summary_long$key <-
  #             recode_factor (Q66_summary_long$key,
  #                            "Non.sanction" =  "No sanction",
  #                            "Negative.informal.sanction" = "Informal sanction",
  #                            "Negative.formal.sanction" =  "Formal sanction")


  .data_plot <- prep_data_for_plot(
    .data,
    focus_var = `66_reaction_fishing_reserve`,
    recoding = codes,
    type = "facet",
    key_order = names(codes) %>% unique()
  )

  plot_horiz_bar(
    .data_plot,
    title = "Proportion of households that answered yes to the previous question and would sanction fishers fishing in the reserve",
    facet_var = key
  )




  # hhs_Q66 <- .data[,c("maa", "66_reaction_fishing_reserve")] %>%
  #             dplyr::filter(`66_reaction_fishing_reserve` != "" ) %>%
  #                droplevels()
  #
  #          hhs_Q66$`66_reaction_fishing_reserve` <-
  # recode_factor (hhs_Q66$`66_reaction_fishing_reserve`,
  #                "1. Non-sanction" = "Non-sanction",
  #                "2. Negative informal sanction" = "Negative informal sanction",
  #                "2. Aplicaría una sanción negativa informal" = "Negative informal sanction",
  #                "3. Aplicaría una sanción negativa formal" = "Negative formal sanction",
  #                "negative informal sanction" = "Negative informal sanction",
  #                "negative formal sanction" = "Negative informal sanction",
  #                "1. No aplicaría ninguna sanción" = "Non-sanction",
  #                "non-sanction" =  "Non-sanction",
  #                "Non-saction" = "Non-sanction")
  #
  #          Q66_summary <- proportion(hhs_Q66$`66_reaction_fishing_reserve`,
  #                                    hhs_Q66$maa,
  #                                    3,
  #                                    length(unique(hhs_Q66$`66_reaction_fishing_reserve`))
  #          )
  #
  #          Q66_summary_long <-
  #             tibble(
  #                Q66_summary %>%
  #                   tidyr::pivot_longer(
  #                      cols = c(
  #                         "Non.sanction",
  #                         "Negative.informal.sanction",
  #                         "Negative.formal.sanction"),
  #                      names_to = "key",
  #                      values_to = "Proportion (%)"
  #                   )
  #             )
  #
  #          Q66_summary_long$key <-
  #             recode_factor (Q66_summary_long$key,
  #                            "Non.sanction" =  "No sanction",
  #                            "Negative.informal.sanction" = "Informal sanction",
  #                            "Negative.formal.sanction" =  "Formal sanction")
  #
  #          Q66 <- clean_plot_data(Q66_summary_long)
  #
  #          #plot
  #          plot_Q66 <-
  #             ggplot(Q66, aes(`MA name`, `Proportion (%)`, N = N)) +
  #             facet_wrap(
  #                ~ key,
  #                labeller = label_wrap_gen(20),
  #                ncol = 5
  #             ) +
  #             theme_rare +
  #             geom_col(fill = "#005BBB", alpha = 0.8) +
  #             scale_y_continuous(limits = c(0, 110),
  #                                breaks = seq(0, 100, 20)) +
  #             ggtitle("Proportion of households that anwsered yes to the previous question \nand would sanction fishers fishing in the reserve") +
  #             xlab (NULL) + ylab ("") +
  #             coord_flip(clip = "on")

}
