
prep_q22_catch_5yrs <- function(.data){

  hhs_Q22 <-.data[,c("maa", "22_catch_5yrs")] %>%
    dplyr::filter(`22_catch_5yrs` != '') %>%
    rbind(c(NA,"Declines a lot"), c(NA,"Declines slightly"), 
          c(NA,"Stays the same"), c(NA,"Improves slightly"),
          c(NA,"Improves heavily")) %>%
    droplevels()
  #proportion
  Q22_summary <-
    proportion(hhs_Q22[[2]], 
               hhs_Q22[[1]], 
               3,5)
  #remove summary row
  Q22_summary_bind <- Q22_summary %>%
    dplyr::filter(`MA name` != "Mean ± SE") %>%
    droplevels()
  #Combine categories
  Q22_summary_bind$Declines <-
    as.numeric(Q22_summary_bind$Declines.a.lot) + 
    as.numeric(Q22_summary_bind$Declines.slightly)
  
  Q22_summary_bind$Improves <-
    as.numeric(Q22_summary_bind$Improves.slightly) + 
    as.numeric(Q22_summary_bind$Improves.heavily)
  
  #Format table for Rmarkdown report
  Q22_summary <-
    rbind(
      Q22_summary_bind[, c("MA name",
                           "N",
                           "Declines",
                           "Stays.the.same",
                           "Improves")],
      `Mean ± SE` = c('',
                      sum(as.numeric(Q22_summary_bind$N)),
                      compute_summary_line(Q22_summary_bind$Declines, 1),
                      compute_summary_line(Q22_summary_bind$Stays.the.same, 1),
                      compute_summary_line(Q22_summary_bind$Improves), 1)
    )
  
  Q22_summary <- Q22_summary %>% dplyr::filter (`MA name` != "")
  #rename colums
  colnames(Q22_summary) <-
    c("MA name",
      "N",
      "Decline (%)",
      "Stay the same (%)",
      "Improve (%)")
  
  #pivot table
  Q22_summary_long <-
    Q22_summary %>% tidyr::pivot_longer(
      cols = c("Decline (%)", "Stay the same (%)", "Improve (%)"),
      names_to = "key",
      values_to = "Proportion (%)"
    )
  Q22_summary_long$key <-
    factor(
      Q22_summary_long$key,
      levels = c("Decline (%)", "Stay the same (%)", "Improve (%)")
    )
  
  Q22 <- clean_plot_data(Q22_summary_long) 
  Q22
}

plot_q22_catch_5yrs <- function(.data, ...){

         
        .data_plot <- prep_q22_catch_5yrs(.data)
        
        plot_horiz_bar(
          .data_plot,
          title = "Proportion of fishers who perceived that their catch \nwill remain stable or increase over the next 5 years",
          facet_var = key
        )

}