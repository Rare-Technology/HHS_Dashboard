

plot_q23_job_secure <- function(.data, ...){

  .data <- .data %>% 
    dplyr::filter(`23_job_secure`%in% c(0,1))
  
  .data_plot <-  prep_data_for_plot(
    .data,
    `9_region_member`,
    type = "bar",
    bar_column = `1`,
    rounding = 2,
    
  )
  
  plot_horiz_bar(
    .data_plot,
    title = "Proportion of fishers who are confident \nthat their jobs are secure"
  )
  
}