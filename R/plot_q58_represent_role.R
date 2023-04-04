plot_q58_represent_role <- function(.data, ...){
  .data <- .data %>% 
    dplyr::filter(`58_represent_role` %in% c("Agree",  "Neither",  "Disagree")) %>%
    dplyr::select(maa, `58_represent_role`) %>% 
    rbind(c(NA, "Disagree"), c(NA, "Neither"), c(NA, "Agree"))
  
  .data_plot <-prep_data_for_plot(
    .data, 
    focus_var = `58_represent_role`, 
    type = "facet",
    key_order = c("Neither", "Disagree", "Agree")
  )
  
  p <- plot_horiz_bar(
    .data_plot,
    title = "Proportion of community members who feel that \ntheir role in the fishery is represented",
    facet_var = key
  )
     
  result <- list(
    plot = p,
    data = .data_plot
  )
}