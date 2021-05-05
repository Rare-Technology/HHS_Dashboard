
prep_q19_current_fish_catch <- function(.data){
  hhs_Q19 <- .data[,c("maa", "19_current_fish_catch")] %>%
    dplyr::filter(`19_current_fish_catch` != "") %>%
    droplevels()
  Q19_summary <- proportion(hhs_Q19[[2]], 
                            hhs_Q19[[1]], 
                            3, type = "5")
  Q19 <- Q19_summary %>% dplyr::filter(`MA name` != "Mean ± SE")
  
  #Combine categories in 3 choices
  Q19$Declined <- as.numeric(Q19$Declined.a.lot) + as.numeric(Q19$Declined.slightly)
  Q19$Improved <- as.numeric(Q19$Improved.slightly) + as.numeric(Q19$Improved.heavily)
  
  #Format table for Rmarkdown report
  Q19_summary <-
    rbind(Q19[, c("MA name",
                  "N",
                  "Declined",
                  "Stayed.the.same",
                  "Improved")],
          `Mean ± SE` = c('',
                          sum(as.numeric(Q19$N)),
                          compute_summary_line(Q19$Declined, 1),
                          compute_summary_line(Q19$Stayed.the.same, 1),
                          compute_summary_line(Q19$Improved, 1)
          ))
  colnames(Q19_summary) <-
    c("MA name",
      "N",
      "Declined",
      "Stayed the same",
      "Improved")
  
  #plot
  Q19_summary_long <-
    Q19_summary %>% tidyr::pivot_longer(
      cols = c("Declined", "Stayed the same", "Improved"),
      names_to = "key",
      values_to = "Proportion (%)"
    )
  Q19_summary_long$key <-
    factor(
      Q19_summary_long$key,
      levels = c("Declined", "Stayed the same", "Improved")
    )
  
  Q19 <- clean_plot_data(Q19_summary_long)
  Q19
}

plot_q19_current_fish_catch <- function(.data){
  
  
        .data_plot <- prep_q19_current_fish_catch(.data)
  

        plot_horiz_bar(
          .data_plot,
          title = "Proportion of fishers who perceived that their catch \nremained stable or increased over the past 2 years",
          facet_var = key
        )
        
         
}