# 
# prep_q63_fishing_in_reserve <- function(.data){
# 
# }

plot_q63_fishing_in_reserve <- function(.data, ...){
  
  .data <- .data %>% 
    dplyr::filter(`63_fishing_in_reserve` %in% c(0,1))
  
  .data_plot <-  prep_data_for_plot(
    .data,
    `63_fishing_in_reserve`,
    type = "bar",
    bar_column = `1`
  )
  
  p <- plot_horiz_bar(
    .data_plot,
    title = "Proportion of fishers that have fished \nin the reserve in the past month"
  )
  result <- list(
    plot = p,
    data = .data_plot
  )
}