
prep_q68_fish_sold <- function(.data){

  hhs_Q68 <- .data[,c("maa", "68_fish_eaten", "68_fish_sold")] %>%
    dplyr::filter(!is.na(`68_fish_sold`)) %>%
    dplyr::filter(`68_fish_eaten` < 800000)
  Q68_fish_eaten <- tapply(hhs_Q68$`68_fish_eaten`, hhs_Q68$maa, mean, na.rm = TRUE)
  Q68_fish_sold <-tapply(hhs_Q68$`68_fish_sold`, hhs_Q68$maa, mean, na.rm = TRUE)
  
  Q68_summary_bind <- as.data.frame(cbind(
    as.vector(summary(factor(hhs_Q68$maa))),
    round(Q68_fish_sold / (Q68_fish_eaten + Q68_fish_sold), 3) * 100
    ))
  
  colnames(Q68_summary_bind) <- c("N", "Fish for subsistence")
  Q68_summary <-
    rbind(Q68_summary_bind, "Mean ± SE" = c(
      sum(Q68_summary_bind$N),
      compute_summary_line(Q68_summary_bind$`Fish for subsistence`, 1)
    ))
  
  Q68_summary <-tibble::rownames_to_column(Q68_summary_bind, "MA name")
  colnames(Q68_summary) <- c("MA name", "N", "Proportion (%)")
  
  Q68 <- clean_plot_data(Q68_summary)
  Q68
}

plot_q68_fish_sold <- function(.data, ...){
  # TODO redesign this plot as a horizontal stacked bar plot
         
  .data_plot <- prep_q68_fish_sold(.data)
 
  p <- plot_horiz_bar(
    .data_plot,
    title = "Proportion of catch sold instead of eaten in the household"
  )

  result <- list(
    plot = p,
    data = .data_plot
  )
}