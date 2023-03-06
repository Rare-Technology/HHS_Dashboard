prep_q47_ability_min_size <- function(.data){
  hhs_Q47min <- .data %>% 
    dplyr::select(maa, `47_ability_min_size`) %>% 
    dplyr::mutate(`47_ability_min_size` = dplyr::recode(
      `47_ability_min_size`,
      "Not Answered" = "No"
    ))
  
  #proportion
  Q47_summary_min <-
    proportion(hhs_Q47min[[2]],
               hhs_Q47min[[1]],
               3,2)[,-3]
  #rename
  colnames(Q47_summary_min) <-
    c("MA name", "N", "Min size restrictions")
  
  
  ### Q47 Proportion of community members familar with specific max fish size restricions in MA
  hhs_Q47max <- .data %>% 
    dplyr::select(maa, `47_ability_max_size`) %>% 
    dplyr::mutate(`47_ability_max_size` = dplyr::recode(
      `47_ability_max_size`,
      "Not Answered" = "No"
    ))
  
  #proportion
  Q47_summary_max <-
    proportion(hhs_Q47max$`47_ability_max_size`,
               hhs_Q47max$maa,
               3,2)[, -3]
  #rename
  colnames(Q47_summary_max) <-
    c("MA name", "N", "Max size restrictions")
  
  Q47_summary <- dplyr::left_join (Q47_summary_min, Q47_summary_max[,-2], by = "MA name")
  
  Q47_summary_long <-
    Q47_summary %>% tidyr::pivot_longer(
      cols = c(
        "Min size restrictions",
        "Max size restrictions",
      ),
      names_to = "key",
      values_to = "Proportion (%)"
    )
  
  Q47 <- clean_plot_data(Q47_summary_long)
  Q47 <- aggregate(`Proportion (%)` ~ `MA name` + N, data=Q47, FUN=mean)
  Q47$`Proportion (%)` <- round(Q47$`Proportion (%)`, digits=1)
  Q47
}

plot_q47_ability_min_size <- function(.data, ...){
  .data_plot <- prep_q47_ability_min_size(.data)
  
  p <- plot_horiz_bar(
    .data_plot,
    title = "Proportion of community members who know specific \nfish size restrictions in the managed access area"
  )
  
  result <- list(
    plot = p,
    data = .data_plot
  )
}