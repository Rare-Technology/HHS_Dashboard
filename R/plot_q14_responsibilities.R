
prep_q14_responsibilities <- function(.data){
  

  .data <- .data %>% 
    dplyr::select(maa, submissionid, `14_responsibility`) %>% 
    tidyr::unnest(cols = `14_responsibility`)
    

  .data$`14_responsibility` <- 
    dplyr::recode_factor(.data$`14_responsibility`,
                  "Prepare fishing gear" = 'Preparing gear for fishing',
                  "caring for house" = 'Keeping the house')
  
  

  
  .data_summary <- proportion_Q14(.data$submissionid, 
                                .data$`14_responsibility`,
                                .data$maa,
                                rounding = 3, 
                                type = length(unique(.data$`14_responsibility`)))
  
  .data_long <-
    .data_summary %>% tidyr::pivot_longer(
      cols = c(3:ncol(.data_summary)),
      names_to = "Activity",
      values_to = "Proportion (%)"
    )
  
  .data_final <- clean_plot_data(.data_long)
  .data_final$Activity <- stringr::str_replace_all(.data_final$Activity, "[.]", " ")
  .data_final
}

plot_q14_responsibilities <- function(.data, ...){

      .data_plot <- prep_q14_responsibilities(.data)
         
       plot_horiz_bar(
         .data_plot,
          title = "Proportion of activities that are the responsibility \nof the women in the household in a typical week",
          breaks = seq(0, 100, 25),
         type= "stacked",
          stack_var = Activity
        )

}