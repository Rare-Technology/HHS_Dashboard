
prep_q37_ability_min_size <- function(.data){
  hhs_Q37min <- .data[,c("maa", "37_ability_min_size")] %>%
    rbind(c(NA,0), c(NA,1), c(NA,-1))
  hhs_Q37min$`37_ability_min_size`[hhs_Q37min$`37_ability_min_size` == -1] <- 0
  hhs_Q37min$`37_ability_min_size`[hhs_Q37min$`37_ability_min_size` == ''] <- 0
  
  
  #proportion
  Q37_summary_min <-
    proportion(hhs_Q37min[[2]],
               hhs_Q37min[[1]],
               3,2)[,-3]
  #rename
  colnames(Q37_summary_min) <-
    c("MA name", "N", "Min size restrictions")
  
  
  ### Q37 Proportion of community members familar with specific max fish size restricions in MA
  hhs_Q37max <- .data[,c("maa", "37_ability_max_size")] %>%
    rbind(c(NA,0), c(NA,1), c(NA,-1))
  hhs_Q37max$`37_ability_max_size`[hhs_Q37max$`37_ability_max_size` == -1] <- 0
  hhs_Q37max$`37_ability_max_size`[hhs_Q37max$`37_ability_max_size` == ''] <- 0
  
  #proportion
  Q37_summary_max <-
    proportion(hhs_Q37max$`37_ability_max_size`,
               hhs_Q37max$maa,
               3,2)[, -3]
  #rename
  colnames(Q37_summary_max) <-
    c("MA name", "N", "Max size restrictions")
  
  Q37_summary <- dplyr::left_join (Q37_summary_min, Q37_summary_max[,-2], by = "MA name")
  
  Q37_summary_long <-
    Q37_summary %>% tidyr::pivot_longer(
      cols = c(
        "Min size restrictions",
        "Max size restrictions",
      ),
      names_to = "key",
      values_to = "Proportion (%)"
    )
  
  Q37 <- clean_plot_data(Q37_summary_long)
  Q37 <- aggregate(`Proportion (%)` ~ `MA name` + N, data=Q37, FUN=mean)
  Q37$`Proportion (%)` <- round(Q37$`Proportion (%)`, digits=1)
  Q37
}

plot_q37_ability_min_size <- function(.data, ...){
  .data_plot <- prep_q37_ability_min_size(.data)
  
  p <- plot_horiz_bar(
    .data_plot,
    title = "Proportion of community members who know specific \nfish size restrictions in the managed access area"
  )
  
  result <- list(
    plot = p,
    data = .data_plot
  )
}