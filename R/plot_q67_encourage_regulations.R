plot_q67_encourage_regulations <- function(.data, ...){
  ok_values <- c("Never", "Rarely", "Sometimes" ,"Often", "Very often")
  
  .data <- .data %>% 
    dplyr::select(maa, `67_encourage_regulations`) %>% 
    dplyr::filter(`67_encourage_regulations` %in% ok_values)
  
  .data_plot <-prep_data_for_plot(
    .data, 
    focus_var = `67_encourage_regulations`, 
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