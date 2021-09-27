# 
# prep_q36_fish_size_restriction <- function(.data){
# 
# }

plot_q36_fish_size_restriction <- function(.data, ...){
  
  
  .data_plot <- .data %>%
    dplyr::filter(`36_fish_size_restriction` %in% c(0, 1, -1)) %>%
    dplyr::mutate(
      `36_fish_size_restriction` = ifelse(`36_fish_size_restriction` == -1, 0, `36_fish_size_restriction`)
    ) %>%
    prep_data_for_plot(
      `36_fish_size_restriction`,
      type = "bar",
      bar_column = `1`
    )
  
  p <- plot_horiz_bar(
    .data_plot,
    title = "Proportion of community members who are aware \nof fish size restrictions in the managed access area"
  )
  
  result <- list(
    plot = p,
    data = .data_plot
  )
}