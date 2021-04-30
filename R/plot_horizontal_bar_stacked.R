plot_horiz_bar_stacked <- function(
  .data,
  xvar = `MA name`,
  yvar = `Proportion (%)`,
  N = `N`,
  title = "",
  x_title = NULL,
  y_title = "Proportion (%)",
  palette = "Blues",
  palette_direction = -1,
  breaks = seq(0, 100, 20),
  limits = c(0, 105),
  stack_var = `key`,
  guide_reverse = TRUE
  ){


  p <- .data %>% 
    ggplot(aes({{ xvar }}, {{ yvar }}, N = {{ N }})) +
    geom_bar(aes(fill = {{stack_var}}),
             position = position_stack(reverse = guide_reverse),
             stat = "identity",
             alpha = 0.8
    ) +
    labs(
      title = title,
      x = x_title,
      y = y_title
    ) +
    scale_fill_brewer(palette = palette, direction = palette_direction) +
    scale_y_continuous(
      limits = limits,
      breaks = breaks
    ) +
    coord_flip(clip = "on") +
    guides(fill = guide_legend(reverse = TRUE)) +
    theme_rare() +
    theme(legend.position = "right")
  
  p

}