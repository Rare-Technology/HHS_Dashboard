# 
# prep_q59_food_procurement <- function(.data){
# 
# }

plot_q59_food_procurement <- function(.data, ...){
  
  .data_plot <-prep_data_for_plot(
    .data, 
    focus_var = `59_food_procurement`, 
    recoding = c(
      "Not very confident" = "Certain",
      "Not confident" = "High chance",
      "Confident" = "Confident not",
      "Confident" = "Not confident",
      "Very confident" = "Very not confident",
      "Very confident" = "Very confident not"
    ),
    type = "facet",
    key_order = c("Not very confident",
                  "Not confident",
                  "Uncertain",
                  "Confident",
                  "Very confident")
  )
  
  
  p <- plot_horiz_bar(
    .data_plot,
    title = "Proportion of households who are confident that they will be able to \nprocure enough food for their family in the next 12 months",
    facet_var = key
  )
  
  result <- list(
    plot = p,
    data = .data_plot
  )
}