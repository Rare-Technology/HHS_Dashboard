# 
# prep_q56_reduce_meal_size_adult <- function(.data){
# 
# }

plot_q56_reduce_meal_size_adult <- function(.data, ...){
  
  
  .data_plot <-prep_data_for_plot(
    .data, 
    focus_var = `56_reduce_meal_size_adult`
  )
  
  
  p <- plot_horiz_bar(
    .data_plot,
    title = "Proportion of fisher households that had to reduce meal size \ndue to not having enough food in the last 12 months",
  )
  
  result <- list(
    plot = p,
    data = .data_plot
  )
}