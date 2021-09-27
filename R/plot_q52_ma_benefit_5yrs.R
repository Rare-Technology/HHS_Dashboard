
# prep_q52_ma_benefit_5yrs <- function(.data){
# 
# }

plot_q52_ma_benefit_5yrs <- function(.data, ...){
  

  .data <- .data %>% 
    dplyr::filter(`52_ma_benefit_5yrs` %in% c("Yes", "Unsure", "No"))
  
  .data_plot <-prep_data_for_plot(
    .data, 
    focus_var = `52_ma_benefit_5yrs`, 
    type = "facet"
  )
  
  p <- plot_horiz_bar(
    .data_plot,
    title = "Proportion of participants who are confident they will continue to \nbenefit from community management of the fishery for the next 5 years",
    facet_var = key
  )
  
  result <- list(
    plot = p,
    data = .data_plot
  )
}