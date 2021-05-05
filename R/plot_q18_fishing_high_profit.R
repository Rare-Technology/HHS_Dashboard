
plot_q18_fishing_high_profit <- function(.data, ...){

  
  .data_plot <- prep_data_for_plot(
    .data, 
    `18_fishing_high_profit`, 
    type = "stacked",
    key_name = "Frequency",
    recoding = c("7 per week" = "Everyday",
                 "7 times per week" = "Everyday",
                 "6 times per week" = "Five or six times per week",
                 "5-6 per week" = "Five or six times per week",
                 "3-4 per week" = "Three or four times per week",
                 "3-4 times per week" = "Three or four times per week",
                 "1-2 per week" = "One or two times per week",
                 "More than 1-2 times per week" = "More than few times per week"),
    key_order = rev(c("Everyday",
                      "Five or six times per week",
                      "Three or four times per week",
                      "More than few times per week",
                      "One or two times per week",
                      "A few times per month",
                      "A few times" ,
                      "Once or never"))
  )
  

  
  plot_horiz_bar(
    .data_plot,
    title = "Frequency that the main fisher go fishing \nduring the high profitable fishing season",
    type = "stacked",
    stack_var = Frequency
  )
  
  
  
}