plot_horiz_bar_facetted <- function(
  .data,
  xvar = `MA name`,
  yvar = `Proportion (%)`,
  N = `N`,
  title = "",
  x_title = NULL,
  y_title = "Proportion (%)",
  fill_col = "#005BBB"
  
){
  

  p <- .data %>% 
    ggplot(aes({{ xvar }}, {{ yvar }}, N = {{ N }})) +
    #facet_wrap (~ key) + 
    geom_col(fill =  fill_col, alpha = 0.8) +  
    labs(
      title = title,
      x = x_title,
      y = y_title
    ) +
    scale_y_continuous(
      limits = c(0, 110),
      breaks = seq(0, 100, 20)) +
    coord_flip(clip = "on") + 
    theme_rare()
  
  

}