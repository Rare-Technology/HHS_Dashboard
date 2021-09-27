
prep_q38_reserve_fishing_allowed <- function(.data){

  hhs_Q38 <- .data[,c("maa", "38_reserve_fishing_allowed")] %>%
    dplyr::filter(`38_reserve_fishing_allowed` != "" &
             `38_reserve_fishing_allowed` != -1) %>%
    rbind(c(NA,0), c(NA,1)) %>%
    droplevels()
  #proportion
  Q38_summary <-
    proportion(hhs_Q38[[2]],
               hhs_Q38[[1]],
               3,2)[,-4]
  #rename
  colnames(Q38_summary) <-
    c("MA name", "N", "Proportion (%)")
  #data for plot
  Q38 <- clean_plot_data(Q38_summary)
  Q38
}

plot_q38_reserve_fishing_allowed <- function(.data, ...){
  
  .data_plot <- prep_q38_reserve_fishing_allowed(.data)
  
  p <- plot_horiz_bar(
    .data_plot,
    title = "Proportion of community members who are aware \nthat fishing is not allowed in the reserve area"
  )

  result <- list(
    plot = p,
    data = .data_plot
  )
}