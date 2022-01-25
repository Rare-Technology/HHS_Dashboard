
plot_q18_fishing_high_profit <- function(.data, ...){

  
  .data_plot <- prep_data_for_plot(
    .data, 
    `18_fishing_high_profit`, 
    type = "stacked",
    key_name = "Frequency",
    recoding = c("Everyday" = "7 per week",
                 "Everyday" = "7 times per week",
                 "5-6 times per week" = "6 times per week",
                 "5-6 times per week" = "5-6 per week",
                 "3-4 times per week" = "3-4 per week",
                 "3-4 times per week" = "3-4 times per week",
                 "1-2 times per week" = "1-2 per week",
                 "More than few times per week" = "More than 1-2 times per week"),
    key_order = rev(c("Everyday",
                      "5-6 times per week",
                      "3-4 times per week",
                      "More than few times per week",
                      "1-2 times per week",
                      "A few times per month",
                      "A few times" ,
                      "Once or never"))
  )
  
  p <- plot_bubble(
    .data_plot,
    title = "Frequency that the main fisher goes fishing \nduring the high profitable fishing season",
    x_var = Frequency
  )
  
  result <- list(
    plot = p,
    data = .data_plot
  )
}