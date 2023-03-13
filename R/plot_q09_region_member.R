plot_q09_region_member <- function(.data, ...){
  Q09 <- .data %>% 
    dplyr::select(
      maa, `9_region_member`
    ) %>% 
    dplyr::mutate(
      `9_region_member` = dplyr::recode(
        `9_region_member`,
        "Yes" = 1,
        "No" = 0,
        "Not Answered" = as.double(NA)
      )
    )
  
  .data_plot <-  prep_data_for_plot(
   Q09,
   `9_region_member`,
   type = "bar",
   bar_column = `1`
  )
  
  p <- plot_horiz_bar(
   .data_plot,
   title = "Proportion of households that identify themselves \nas member of a specific region"
  )

  result <- list(
    plot = p,
    data = .data_plot
  )
}