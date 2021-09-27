# 
# prep_q66b_response_fishing_reserve <- function(.data){
# 
# }

plot_q66b_response_fishing_reserve <- function(.data, ...){
  
  .data <- .data %>% 
    dplyr::filter(`66_response_fishing_reserve` %in% c(0,1))
  
  .data_plot <-  prep_data_for_plot(
    .data,
    `66_response_fishing_reserve`,
    type = "bar",
    bar_column = `1`
  )
  
  p <- plot_horiz_bar(
    .data_plot,
    title = "Proportion of households believing that if a fisher was fishing in the reserve, they would say or do anything in response"
  )

  result <- list(
    plot = p,
    data = .data_plot
  )
}