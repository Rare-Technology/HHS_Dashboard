plot_bubble <- function(
  .data,
  x_var = `key`,
  y_var = `MA name`,
  size_var = `Proportion (%)`,
  title = ""
){
  title <- stringr::str_wrap(title, width = 65)
  subtitle <- 'Proportion (%)'
  
  num_y <- .data %>% dplyr::pull({{y_var}}) %>% unique() %>% length()

  .data <- .data %>% 
    dplyr::mutate(
      `MA name` := factor(`MA name`)
    )
  
  .data <- .data %>% 
    dplyr::mutate(
      `MA name` := forcats::fct_rev(`MA name`)
    )
  
  p <- .data %>% 
    ggplot(aes(x={{ x_var }}, y={{ y_var }}))
  
  p <- p +
    geom_point(aes(size={{size_var}}, fill={{x_var}}),
               shape=21, color='black', show.legend=FALSE) +
    scale_size_area(max_size=20, guide = 'none') +
    geom_text(aes(
      y = as.numeric(as.factor({{y_var}})) - num_y * sqrt({{size_var}})/200, label={{size_var}}),
      vjust = 1.3,
      size = 4
    ) +
    scale_fill_brewer(palette='Set3') +
    labs(title = title, subtitle = subtitle) +
    theme_rare() +
    theme(
      panel.grid.major.y = element_blank(),
      axis.text.x = element_text(angle=30, hjust=1),
      axis.title.y = element_blank()
    )
  
  p
}