plot_q25_job_secure <- function(.data, ...){
  .data <- .data %>% 
    dplyr::filter(`25_job_secure` %in% c("No","Yes"))
  
  .data_plot <-  prep_data_for_plot(
    .data,
    `25_job_secure`,
    type = "bar",
    bar_column = `Yes`,
    rounding = 2
  )
  
  p <- plot_horiz_bar(
    .data_plot,
    title = "Proportion of fishers who are confident \nthat their jobs are secure"
  )
  
  result <- list(
    plot = p,
    data = .data_plot
  )
}