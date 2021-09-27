
prep_q54_food_availability <- function(.data){
  hhs_Q54 <- .data[,c("maa", "54_food_availability")] %>%
    dplyr::filter(`54_food_availability` != "") %>%
    droplevels()
  
  Q54_summary_bind <- proportion (hhs_Q54$`54_food_availability` ,
                                  hhs_Q54$maa,
                                  3,5)
  Q54_summary_bind <-
    Q54_summary_bind[-dim(Q54_summary_bind)[1], ]
  Q54_summary_bind$Good_ <-
    as.numeric(Q54_summary_bind$Good) + as.numeric(Q54_summary_bind$Very.good)
  Q54_summary_bind$Bad <-
    as.numeric(Q54_summary_bind$Rather.bad) + as.numeric(Q54_summary_bind$Very.bad)
  Q54_summary_bind$OK <- as.numeric(Q54_summary_bind$OK)
  Q54_summary <-
    Q54_summary_bind[, c("MA name", "N", "Bad", "OK", "Good_")]
  colnames(Q54_summary) <-
    c("MA name", "N", "Bad", "Normal", "Good")
  #pivot
  Q54_summary_long <-
    Q54_summary %>% tidyr::pivot_longer(
      cols = c(`Good`, `Bad`, `Normal`),
      names_to = "key",
      values_to = "Proportion (%)"
    )
  Q54_summary_long$key <-
    factor(Q54_summary_long$key,
           levels = c("Bad", "Normal", "Good"))
  
  Q54 <- clean_plot_data(Q54_summary_long)
  
  Q54
}

plot_q54_food_availability <- function(.data, ...){

  
  .data_plot <- prep_q54_food_availability(.data)
  
  p <- plot_horiz_bar(
    .data_plot,
    title = "Proportion of households who think that \nfood availability was good in the past year",
    facet_var = key
  )
  
  result <- list(
    plot = p,
    data = .data_plot
  )
}