
prep_q17_fishing_low_profit <- function(.data){

}

plot_q17_fishing_low_profit <- function(.data, use_plotly = TRUE){


  .data_plot <- prep_data_for_plot(
    .data, 
    `17_fishing_low_profit`, 
    type = "stacked",
    key_name = "Frequency",
    recoding = c("Everyday" = "7 per week",
                 "Five or six times per week" = "5-6 per week",
                 "Three or four times per week" = "3-4 per week",
                 "One or two times per week" = "1-2 per week",
                 "More than few times per week" = "More than 1-2 times per week",
                 "Three or four times per week",
                 "Five or six times per week",
                 "Everyday"),
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
    title = "Frequency that the main fisher go fishing \nduring less profitable fishing season",
    type = "stacked",
    stack_var = Frequency,
    guide_reverse = TRUE
  )
  
  
}