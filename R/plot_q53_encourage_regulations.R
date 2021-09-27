# 
# prep_q53_encourage_regulations <- function(.data){
# 
# }

plot_q53_encourage_regulations <- function(.data, ...){
  
  
  .data <- .data %>% 
    dplyr::filter(`53_encourage_regulations` != "" &
               `53_encourage_regulations` != 'No regulations')
  
  .data_plot <-prep_data_for_plot(
    .data, 
    focus_var = `53_encourage_regulations`, 
    type = "facet"
  )
  
  
  p <- plot_horiz_bar(
    .data_plot,
    title = "Proportion of fishers that encourage others \nto participate in sustainable/responsible activity",
    facet_var = key
  )
  
  result <- list(
    plot = p,
    data = .data_plot
  )
}