# 
# prep_q60_hh_fish_consumption <- function(.data){
# 
# }

plot_q60_hh_fish_consumption <- function(.data, ...){
  
  

  .data_plot <-prep_data_for_plot(
    .data, 
    focus_var = `60_hh_fish_consumption`, 
    type = "facet"
  )
  
  
  p <- plot_horiz_bar(
    .data_plot,
    title = "Proportion of households that consume fish \nmore than few times per week",
    facet_var = key
  )
  
  result <- list(
    plot = p,
    data = .data_plot
  )
}