plot_horiz_bar <- function(
  .data,
  x_var = `MA name`,
  y_var = `Proportion (%)`,
  n_var = `N`,
  title = "",
  x_title = NULL,
  y_title = "Proportion (%)",
  fill_col = "#005BBB",
  facet_var = ` `
  
){
  

  # TODO: must be a better way!
  facet_var_str <- quo_name(enquo(facet_var))
  
  p <- .data %>% 
    ggplot(aes({{ x_var }}, {{ y_var }}, N = {{ n_var }})) +
    geom_col(fill = fill_col, alpha = 0.8) +
    labs(
      title = title,
      x = x_title,
      y = y_title
    ) +
    scale_y_continuous(
      limits = c(0, 110),
      breaks = seq(0, 100, 20)
      ) +
    coord_flip(clip = "on") +
    theme_rare() +
    theme(legend.position = "right") 
  
  if(facet_var_str != " ")
    p <- p + facet_wrap( vars({{ facet_var }}))
  
  p
}