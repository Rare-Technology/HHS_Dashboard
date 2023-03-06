
prep_q21_current_fish_catch <- function(.data){
  hhs_Q21 <- .data[,c("maa", "21_current_fish_catch")] %>%
    dplyr::filter(!(`21_current_fish_catch` %in% c("Not answered", "Not a fisher"))) %>%
    droplevels()
  Q21_summary <- proportion(hhs_Q21[[2]], 
                            hhs_Q21[[1]], 
                            3, type = "5")
  Q21 <- Q21_summary %>% dplyr::filter(`MA name` != "Mean ± SE")
  
  #Combine categories in 3 choices
  Q21$Declined <- as.numeric(Q21$Declined.a.lot) + as.numeric(Q21$Declined.slightly)
  Q21$Improved <- as.numeric(Q21$Improved.slightly) + as.numeric(Q21$Improved.heavily)
  
  #Format table for Rmarkdown report
  Q21_summary <-
    rbind(Q21[, c("MA name",
                  "N",
                  "Declined",
                  "Stayed.the.same",
                  "Improved")],
          `Mean ± SE` = c('',
                          sum(as.numeric(Q21$N)),
                          compute_summary_line(Q21$Declined, 1),
                          compute_summary_line(Q21$Stayed.the.same, 1),
                          compute_summary_line(Q21$Improved, 1)
          ))
  colnames(Q21_summary) <-
    c("MA name",
      "N",
      "Declined",
      "Stayed the same",
      "Improved")
  
  #plot
  Q21_summary_long <-
    Q21_summary %>% tidyr::pivot_longer(
      cols = c("Declined", "Stayed the same", "Improved"),
      names_to = "key",
      values_to = "Proportion (%)"
    )
  Q21_summary_long$key <-
    factor(
      Q21_summary_long$key,
      levels = c("Declined", "Stayed the same", "Improved")
    )
  
  Q21 <- clean_plot_data(Q21_summary_long)
  Q21
}

plot_q21_current_fish_catch <- function(.data, ...){
  
  
  .data_plot <- prep_q21_current_fish_catch(.data)
  
  
  p <- plot_horiz_bar(
    .data_plot,
    title = "Proportion of fishers who perceived that their catch \nremained stable or increased over the past 2 years",
    facet_var = key
  )
        
  result <- list(
    plot = p,
    data = .data_plot
  )
}