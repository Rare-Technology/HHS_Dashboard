# 
# prep_q55_worry_food <- function(.data){
# 
# }

plot_q55_worry_food <- function(.data, ...){
  
  
  .data_plot <-prep_data_for_plot(
    .data, 
    focus_var = `55_worry_food`, 
    type = "facet",
    key_order = c("Never", "Sometimes", "Often")
  )
  
  
  p <- plot_horiz_bar(
    .data_plot,
    title = "Proportion of fisher households who often worry about \nnot having enough food for everyone in the household",
    facet_var = key,
  )
  
  result <- list(
    plot = p,
    data = .data_plot
  )
}