plot_q45_leadership_position <- function(.data, ...){

  .data <- .data %>% 
    dplyr::select(maa, `45_leadership_position`) %>% 
    tidyr::unnest(cols = `45_leadership_position`)
  
  if(nrow(.data) == 0) return(list(plot = NO_PLOT_ATTEMPT, data = NO_PLOT_ATTEMPT))
  
  .data_plot <-prep_data_for_plot(
    .data, 
    focus_var = `45_leadership_position`, 
    type = "facet"
  )
  
  p <- plot_horiz_bar(
    .data_plot,
    title = "Proportion of community members that \nhold leadership positions in the management body",
    facet_var = key
  )
    
  result <- list(
    plot = p,
    data = .data_plot
  )        
}