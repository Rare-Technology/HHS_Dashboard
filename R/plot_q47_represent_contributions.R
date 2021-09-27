plot_q47_represent_contributions <- function(.data, ...){
  

  .data <- .data %>% 
    dplyr::filter(`47_represent_contributions` %in% c("Agree",  "Neither",  "Disagree")) 
  
  .data_plot <-prep_data_for_plot(
    .data, 
    focus_var = `47_represent_contributions`, 
    type = "facet",
    key_order = c("Neither", "Disagree", "Agree")
  )
  
  p <- plot_horiz_bar(
    .data_plot,
    title = "Proportion of community members who feel that \ntheir contributions to the fishery are recognized",
    facet_var = key
  )
     
  result <- list(
    plot = p,
    data = .data_plot
  )
}