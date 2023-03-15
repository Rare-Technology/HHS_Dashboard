prep_q24_catch_5yrs <- function(.data){

  hhs_Q24 <-.data %>% 
    dplyr::select(maa, `24_catch_5yrs`) %>%
    dplyr::filter(!(`24_catch_5yrs` %in% c("Not a fisher", "Not answered"))) %>%
    rbind(c(NA,"Declines a lot"), c(NA,"Declines slightly"), 
          c(NA,"Stays the same"), c(NA,"Improves slightly"),
          c(NA,"Improves heavily")) %>%
    droplevels()
  
  #proportion
  Q24_summary <-
    proportion(hhs_Q24[[2]], 
               hhs_Q24[[1]], 
               3,5)
  #remove summary row
  Q24_summary_bind <- Q24_summary %>%
    dplyr::filter(`MA name` != "Mean ± SE") %>%
    droplevels()
  #Combine categories
  Q24_summary_bind$Declines <-
    as.numeric(Q24_summary_bind$Declines.a.lot) + 
    as.numeric(Q24_summary_bind$Declines.slightly)
  
  Q24_summary_bind$Improves <-
    as.numeric(Q24_summary_bind$Improves.slightly) + 
    as.numeric(Q24_summary_bind$Improves.heavily)
  
  #Format table for Rmarkdown report
  Q24_summary <-
    rbind(
      Q24_summary_bind[, c("MA name",
                           "N",
                           "Declines",
                           "Stays.the.same",
                           "Improves")],
      `Mean ± SE` = c('',
                      sum(as.numeric(Q24_summary_bind$N)),
                      compute_summary_line(Q24_summary_bind$Declines, 1),
                      compute_summary_line(Q24_summary_bind$Stays.the.same, 1),
                      compute_summary_line(Q24_summary_bind$Improves), 1)
    )
  
  Q24_summary <- Q24_summary %>% dplyr::filter (`MA name` != "")
  #rename colums
  colnames(Q24_summary) <-
    c("MA name",
      "N",
      "Decline (%)",
      "Stay the same (%)",
      "Improve (%)")
  
  #pivot table
  Q24_summary_long <-
    Q24_summary %>% tidyr::pivot_longer(
      cols = c("Decline (%)", "Stay the same (%)", "Improve (%)"),
      names_to = "key",
      values_to = "Proportion (%)"
    )
  Q24_summary_long$key <-
    factor(
      Q24_summary_long$key,
      levels = c("Decline (%)", "Stay the same (%)", "Improve (%)")
    )
  
  Q24 <- clean_plot_data(Q24_summary_long) 
  Q24
}

plot_q24_catch_5yrs <- function(.data, ...){

         
  .data_plot <- prep_q24_catch_5yrs(.data)
  
  p <- plot_horiz_bar(
    .data_plot,
    title = "Proportion of fishers who perceived that their catch \nwill remain stable or increase over the next 5 years",
    facet_var = key
  )
  
  result <- list(
    plot = p,
    data = .data_plot
  )
}