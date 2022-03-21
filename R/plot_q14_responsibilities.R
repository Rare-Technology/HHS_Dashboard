
prep_q14_responsibilities <- function(.data){
  
  ## figure out how to filter out rows where 14_responsibility are just length-1
  ## lists with NULL as the only element (so, all of Honduras's rows)
  
  .data <- .data %>% 
    dplyr::select(maa, submissionid, `14_responsibility`) %>% 
    tidyr::unnest(cols = `14_responsibility`)
    
  if(nrow(.data) == 0) return(NO_PLOT_ATTEMPT)
  
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
   
  if(is.null(.data_plot)) return(list(plot = NO_PLOT_ATTEMPT, data = NO_PLOT_ATTEMPT))
  
   p <- plot_bubble(
     .data_plot,
     title = "Proportion of activities that are the responsibility \nof the women in the household in a typical week",
     x_var = Activity
   )

   result <- list(
     plot = p,
     data = .data_plot
   )
}